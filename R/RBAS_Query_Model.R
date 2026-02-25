# For server logging
# Begin timer
# task_start <- Sys.time()

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
# source("E:/Projects/citz-rpd-utilities/event_logger.R")
# api_id <- "Jira" # for event_logger
# project_id <- "RBAS" # for event_logger
# email <- "rpd.spbooking@gov.bc.ca"
# api_key <- keyring::key_get(
#   service = "JIRA_API",
#   username = email,
#   keyring = NULL
# )

# Local Run
source("C:/Projects/citz-rpd-utilities/safe_hoist.R")
# email <- "david.rattray@gov.bc.ca"
# They kicked me out of their board so either setup for service acct or get reinstated
api_key <- keyring::key_get(
  service = "JIRA_API",
  username = email,
  keyring = NULL
)

# Encode token
token <- base64encode(charToRaw(paste0(email, ":", api_key)))
token_string <- paste("Basic", token)

# Setup API parameters ####
query_url = "https://citz-rpd.atlassian.net/rest/api/3/search/jql"
# https://developer.atlassian.com/cloud/jira/platform/rest/v3/api-group-issue-search/#api-rest-api-3-search-jql-get
# https://developer.atlassian.com/changelog/#CHANGE-2046
dashboard_id = "RBAS"
expand_opts = c("changelog", "names", "fields")
max_results = 100
nextPageToken = NULL
progress = 0
round = 1

# Issues Loop ####
while (progress < 2) {
  req <- request(query_url) |>
    req_headers(Authorization = token_string) |>
    # configure project, max_results, and start_at
    req_url_query(
      jql = I(paste0("project=", dashboard_id)), # I wrapper skips auto-formatting of the extra "=" sign
      expand = expand_opts,
      maxResults = max_results,
      fields = "*all",
      # startAt = start_at, #deprecated for nextPageToken
      nextPageToken = nextPageToken,
      .multi = "comma" # control how vectors are appended, for expand_opts
    ) |>
    # Server side logging
    # req_proxy("142.34.229.249", 8080) |> # Use Server Proxy to connect
    #   req_error(is_error =
    #               function(resp) {
    #                 lr <- resp_header(resp, "x-seraph-loginreason")
    #                 bad_auth <- !is.null(lr) && grepl("AUTHENTICATED_FAILED|AUTHENTICATION_DENIED", lr)
    #                 empty_ok <- FALSE  # we only care about bad_auth here
    #                 bad_auth || empty_ok
    #               },

    #             body = function(resp) {
    #               paste0(
    #                 "Auth Failure for ",
    #                 api_id,
    #                 " reason: ",
    #                 resp_header(resp, "x-seraph-loginreason") %||% "UNKNOWN",
    #                 " traceid: ",
    #                 resp_header(resp, "atl-traceid") %||% "NA",
    #                 " url: ",
    #                 resp_url(resp)
    #               )
    #             }
    #   )

    # # Perform request with error handling and structured logging
    # resp <- tryCatch(
    #   req_perform(req) |> resp_body_json(),
    #   error = function(e) {
    #     # Compose a one-line description with context
    #     desc <- if (!is.null(e$body) && is.character(e$body)) e$body else e$message

    #     # Log the error to daily CSV
    #     event_logger(
    #       api        = api_id,
    #       subset     = project_id,         # PAR / SBP / RBAS
    #       event_type = "error",
    #       description = desc
    #     )
    #     stop(e)  # rethrow so task scheduler flags a failure (current monitoring is by Nagios)
    #   }
    # )
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

# Server side logging
# task_time <- round(as.numeric(difftime(Sys.time(), task_start, units = "secs")), digits = 2)
# desc <- paste0("Run Completed, ", nrow(Issues), " tickets processed. Runtime: ", task_time)
# event_logger(api_id, project_id, "success", desc)

# Server Save
# write.csv(Issues, "E:/Projects/PBI-Gateway/RBAS_Issues.csv", row.names = FALSE)

# Local Save
# write.csv(Issues, here::here("RBAS_output.csv"))
