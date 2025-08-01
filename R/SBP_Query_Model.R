# Load necessary packages
library(base64enc, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(httr2, quietly = TRUE, warn.conflicts = FALSE)
library(jsonlite, quietly = TRUE, warn.conflicts = FALSE)
library(lubridate, quietly = TRUE, warn.conflicts = FALSE)
library(purrr, quietly = TRUE, warn.conflicts = FALSE)
library(tibble, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = TRUE, warn.conflicts = FALSE)

source(here::here("R/utilities.R"))

email <- read.csv("C:/Projects/credentials/email_address.csv") |> pull()
api_key <- read.csv("C:/Projects/credentials/jira_api_token.csv") |> pull()
token <- base64encode(charToRaw(paste0(email, ":", api_key)))
token_string <- paste("Basic", token)

# Setup API parameters ####
query_url = "https://citz-rpd.atlassian.net/rest/api/3/"
project_id = "search?jql=project=SBP&expand=changelog,names"
max_results = 100
start_at = 1
total_results = 25
progress = 0

# Issues Loop ####
while (total_results > progress) {
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
    # Parent column is sometimes missing as sparsely populated
    mutate(
      Parent = if ("Parent" %in% names(pick(everything()))) Parent else NA
    ) |>
    # Select fields of interest
    select(
      IssueKey = key,
      IssueType = `Issue Type`,
      Address,
      Assignee,
      Created,
      RequestedDueDate = `Requested Due Date`,
      SpaceBookingAdmin = `Name of Space Booking Admin`,
      NumberOfSpaces = `Number of Spaces to Onboard`,
      FloorPlan = `Do you have a floor plan?`,
      FurniturePlan = `Do you have a furniture plan?`,
      LastUpdatedStatus = `Last Updated Status`,
      Department = `Department-1`,
      DueDate = `Due date`,
      Organization = `Ministry/BPS Organization`,
      Priority,
      Reporter,
      RequestParticipants = `Request participants`,
      RequestType = `Request Type`,
      Resolved,
      Status,
      Summary,
      Updated,
      Parent
    ) |>
    safe_hoist(IssueType, IssueType = "name", .remove = FALSE) |>
    safe_hoist(
      Address,
      Address = list("content", 1L, "content", 1L, "text"),
      .remove = FALSE
    ) |>
    safe_hoist(Assignee, Assignee = "displayName", .remove = FALSE) |>
    safe_hoist(RequestedDueDate, RequestedDueDate = "value", .remove = FALSE) |>
    safe_hoist(FloorPlan, FloorPlan = "value", .remove = FALSE) |>
    safe_hoist(FurniturePlan, FurniturePlan = "value", .remove = FALSE) |>
    safe_hoist(Organization, Organization = "value", .remove = FALSE) |>
    safe_hoist(Priority, Priority = "name", .remove = FALSE) |>
    safe_hoist(Reporter, Reporter = "displayName", .remove = FALSE) |>
    safe_hoist_all(
      RequestParticipants,
      path = list("displayName"),
      .remove = FALSE
    ) |>
    mutate(
      RequestParticipants = RequestParticipants_displayName,
      .keep = "unused"
    ) |>
    safe_hoist(
      RequestType,
      RequestType = list("requestType", "name"),
      .remove = FALSE
    ) |>
    safe_hoist(Status, Status = "name", .remove = FALSE) |>
    safe_hoist(Parent, Parent = "key", .remove = FALSE)

  if (start_at == 1) {
    Issues <- issues
  } else {
    Issues <- full_join(Issues, issues)
  }
  progress <- progress + max_results
  start_at <- progress + 1
}

# write.csv(Issues, here::here("SBP_output.csv"))
