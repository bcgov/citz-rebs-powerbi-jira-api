# Load necessary packages
library(base64enc, quietly = TRUE, warn.conflicts = FALSE)
library(plyr, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(httr2, quietly = TRUE, warn.conflicts = FALSE)
library(jsonlite, quietly = TRUE, warn.conflicts = FALSE)
library(lubridate, quietly = TRUE, warn.conflicts = FALSE)
library(purrr, quietly = TRUE, warn.conflicts = FALSE)
library(tibble, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = TRUE, warn.conflicts = FALSE)

# Will need this function later for extracting values from list columns
safe_hoist <- function(.data, .col, ...) {
  .col <- tidyselect::vars_pull(names(.data), {{ .col }})
  if (is.list(.data[[.col]])) {
    hoist(.data, .col, ...)
  } else {
    dot_args <- list(...)
    dot_args <- dot_args[setdiff(names(dot_args), names(formals(hoist)))]
    mutate(.data, !!!replace(dot_args, TRUE, NA))
  }
}

email <- read.csv("C:/Projects/credentials/email_address.csv") |> pull()
api_key <- read.csv("C:/Projects/credentials/jira_api_token.csv") |> pull()
token <- base64encode(charToRaw(paste0(email, ":", api_key)))
token_string <- paste("Basic", token)

# Setup API parameters ####
query_url = "https://citz-rpd.atlassian.net/rest/api/3/"
project_id = "search?jql=project=PAR&expand=changelog,names"
max_results = 100
start_at = 1
total_results = 25
progress = 0

# Issues Loop ####
# while (total_results > progress) {
req <- request(query_url) |>
  req_headers(Authorization = token_string) |>
  # configure project, max_results, and start_at
  req_url_path_append(paste0(
    project_id,
    "&maxResults=",
    max_results,
    "&startAt=",
    start_at
  )) |>
  req_perform()

resp <- req |> resp_body_json()

# Used to update total_results in while loop
total_results <- resp["total"][[1]]

names <- resp |>
  purrr::pluck("names") |>
  tibble::enframe() |>
  safe_hoist(value, Value = 1L) |>
  group_by(Value) |>
  mutate(row_name = row_number(), row_count = n()) |>
  mutate(
    Value = case_when(
      row_count > 1 ~ paste0(Value, "-", row_name),
      .default = Value
    )
  ) |>
  select(-c(row_name, row_count)) |>
  tibble::deframe()

issues <- resp |>
  purrr::pluck("issues") |>
  tibble::enframe() |>
  tidyr::unnest_wider(value) |>
  tidyr::unnest_wider(fields) |>
  plyr::rename(names) |>
  select(
    IssueID = id,
    IssueKey = key,
    Changelog = changelog,
    Organization = `Ministry/BPS Organization`,
    ProjectEffectiveDate = `Project Effective Date`,
    StatusCategory = `Status Category`,
    Resolution,
    Assignee,
    TimeToResolution = `Time to resolution`,
    TimeToFirstResponse = `Time to first response`,
    Reporter,
    Urgency,
    RequestLanguage = `Request language`,
    Progress,
    IssueType = `Issue Type`,
    Project,
    RequestParticipants = `Request participants`,
    Resolved,
    Updated,
    Description,
    RequestType = `Request Type`,
    Summary,
    StatusCategoryChanged = `Status Category Changed`,
    PrimaryUse = `Primary Use`,
    BuildingNumber = `Building Number`,
    Priority,
    Status,
    Creator,
    TotalProgress = `Σ Progress`,
    Created,
    Tenure,
    ServiceProvider = `Service Provider`,
    PropertyDescription = `Description of Property`,
    Area,
    LeaseNumber = `Lease Number`,
    UnitAddress = `Suite or Unit Address`,
    Floors = `Floors/Rooms`,
    BuildingType = `Building Type`,
    City = `City-2`,
    Address
  )
#   if (start_at == 1) {
#     Issues <- issues
#   } else {
#     Issues <- full_join(Issues, issues)
#   }
#   progress <- progress + max_results
#   start_at <- progress + 1
# }
