# Load necessary packages
library(base64enc, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(httr2, quietly = TRUE, warn.conflicts = FALSE)
library(jsonlite, quietly = TRUE, warn.conflicts = FALSE)
library(lubridate, quietly = TRUE, warn.conflicts = FALSE)
library(purrr, quietly = TRUE, warn.conflicts = FALSE)
library(tibble, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = TRUE, warn.conflicts = FALSE)

# Server Run
# source("E:/Projects/citz-rebs-jira-api/utilities.R")

# Local Run
source(here::here("R/utilities.R"))

# Update email/api_key as needed
# Server Run
# email <- read.csv("E:/Projects/credentials/service_email_address.csv") |> pull()
# api_key <- read.csv("E:/Projects/credentials/jira_api_service_token.csv") |>
#   pull()

# Local Run
email <- read.csv("C:/Projects/credentials/email_address.csv") |> pull()
api_key <- read.csv("C:/Projects/credentials/jira_api_token.csv") |> pull()

# Encode token
token <- base64encode(charToRaw(paste0(email, ":", api_key)))
token_string <- paste("Basic", token)

# Setup API parameters ####
query_url = "https://citz-rpd.atlassian.net/rest/api/3/search/jql"
# https://developer.atlassian.com/cloud/jira/platform/rest/v3/api-group-issue-search/#api-rest-api-3-search-jql-get
project_id = "RBAS"
expand_opts = c("changelog", "names", "fields")
max_results = 100
nextPageToken = NULL
total_results = 1
progress = 0
round = 1

# Issues Loop ####
while (total_results > progress) {
  req <- request(query_url) |>
    req_headers(Authorization = token_string) |>
    # configure project, max_results, and start_at
    req_url_query(
      jql = I(paste0("project=", project_id)), # I wrapper skips auto-formatting of the extra "=" sign
      expand = expand_opts,
      maxResults = max_results,
      fields = "*all",
      # startAt = start_at, #deprecated for nextPageToken
      nextPageToken = nextPageToken,
      .multi = "comma" # control how vectors are appended, for expand_opts
    ) |>
    # req_proxy("142.34.229.249", 8080) |> # Use Server Proxy to connect
    req_perform()

  resp <- req |> resp_body_json()

  # Used to update total_results in while loop
  nextPageToken <- resp["nextPageToken"][[1]]

  # total results isn't always accurate, check that response has issues
  if (length(resp$issues) == 0) {
    break
  }

  if (is.null(nextPageToken)) {
    progress <- 2
  }

  # pull the names attribute and prep to rename the issue columns.
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

  # pull rows of issues, rename, unnest columns and format.
  issues <- resp |>
    purrr::pluck("issues") |>
    tibble::enframe() |>
    tidyr::unnest_wider(value) |>
    tidyr::unnest_wider(fields) |>
    plyr::rename(names) |>
    rename_with(~ gsub(" ", "", .)) |>
    select(
      IssueID = id,
      IssueKey = key,
      Created,
      EndDate,
      RequestedDueDate,
      Duedate,
      Updated,
      Resolved,
      Resolution,
      TimeToResolution = Timetoresolution,
      ServiceRequested,
      ProjectType,
      Organization = `Ministry/BPSOrganization`,
      BranchDivision = `Branch/Division`,
      RPDBranch,
      MYSCReq = `MYSCReq#`,
      RequestType,
      RequestSubtype,
      Casetype,
      Status,
      StatusCategory,
      StatusCategoryChanged,
      Assignee,
      EmployeeID,
      Reporter,
      Summary
    ) |>
    safe_hoist(Resolution, Resolution = "name", .remove = FALSE) |>
    safe_hoist(
      TimeToResolution,
      TimeToResolution = list("ongoingCycle", "elapsedTime", "millis"),
      .remove = FALSE
    ) |>
    safe_hoist(RPDBranch, RPDBranch = "value", .remove = FALSE) |>
    safe_hoist(
      RequestType,
      RequestType = list("requestType", "name"),
      .remove = FALSE
    ) |>
    safe_hoist(Status, Status = "name", .remove = FALSE) |>
    safe_hoist(StatusCategory, StatusCategory = "name", .remove = FALSE) |>
    safe_hoist(Assignee, Assignee = "displayName", .remove = FALSE) |>
    safe_hoist(Reporter, Reporter = "displayName", .remove = FALSE) |>
    safe_hoist(Organization, Organization = "value", .remove = FALSE) |>
    safe_hoist(RequestedDueDate, RequestedDueDate = "value", .remove = FALSE) |>
    mutate(
      Created = as.POSIXct(
        Created,
        tz = Sys.timezone()
      ),
      Updated = as.POSIXct(
        Updated,
        tz = Sys.timezone()
      ),
      StatusCategoryChanged = as.POSIXct(
        StatusCategoryChanged,
        tz = Sys.timezone()
      )
    ) |>
    mutate(
      MinutesToResolution = (TimeToResolution / 1000) / 60,
      .keep = "unused",
      .after = Resolution
    )

  if (round == 1) {
    Issues <- issues
  } else {
    Issues <- full_join(Issues, issues)
  }

  round <- 2
}

Issues <- Issues %>%
  # Add a filter step to remove all the test tickets prior to launch on Aug 18th 2025
  filter(
    !IssueKey %in% c("RBAS-1", "RBAS-2", "RBAS-3", "RBAS-4", "RBAS-5", "RBAS-6")
  )

# Server Save
# write.csv(Issues, "E:/Projects/PBI-Gateway/RBAS_Issues.csv", row.names = FALSE)

# Local Save
write.csv(Issues, here::here("RBAS_output.csv"))
