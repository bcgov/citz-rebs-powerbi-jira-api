# Load necessary packages
library(base64enc, quietly = TRUE, warn.conflicts = FALSE)
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
query_url = "https://citz-rpd.atlassian.net/rest/api/3/search/jql"
# https://developer.atlassian.com/cloud/jira/platform/rest/v3/api-group-issue-search/#api-rest-api-3-search-jql-get
# https://developer.atlassian.com/changelog/#CHANGE-2046
project_id = "CSR"
expand_opts = c("names", "fields", "changelog")
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
    plyr::rename(names) #|>

  test <- issues |>
    select_if(~ !all(is.na(.))) |>
    rename_with(~ gsub(" ", "", .)) |>
    select(
      IssueID = id,
      IssueKey = key,
      Changelog = changelog,
      Assignee,
      Creator,
      CSM,
      CSRIssueSubtype,
      Reporter,
      Organization = `Ministry/BPSOrganization`,
      ResponsibleGroup,
      ProjectEffectiveDate,
      StatusCategory,
      Resolution,
      Progress,
      IssueType,
      Project,
      PIN = `PIN(ARENumber)`,
      Resolved,
      Updated,
      Description,
      RequestType,
      Summary,
      StatusCategoryChanged,
      Priority,
      Status,
      TotalProgress = `ΣProgress`,
      Created
    ) #|>
  safe_hoist(Organization, Organization = "value", .remove = FALSE) |>
    safe_hoist(StatusCategory, StatusCategory = "name", .remove = FALSE) |>
    safe_hoist(Status, Status = "name", .remove = FALSE) |>
    safe_hoist(Assignee, Assignee = "displayName", .remove = FALSE) |>
    safe_hoist(Reporter, Reporter = "displayName", .remove = FALSE) |>
    safe_hoist(IssueType, IssueType = "name", .remove = FALSE) |>
    safe_hoist(
      RequestType,
      RequestType = list("requestType", "name"),
      .remove = FALSE
    ) |>
    safe_hoist(Resolution, Resolution = "name", .remove = FALSE)
  # Pin Name
  # creator
  # csm
  # csmissue
  # responsible
  # progress
  # priority
  # Total progress
  test$Description[43]
  test2$Description2[2]
  test2 <- test |>

    safe_hoist(
      Description,
      Description2 = list("content", 1L, "content", 1L, "text"),
      .remove = FALSE
    ) #|>
  mutate(
    ProjectEffectiveDate = as.Date(ProjectEffectiveDate, format = "%Y-%m-%d")
  ) |>
    mutate(
      Resolved = as.POSIXct(
        Resolved,
        tz = Sys.timezone()
      )
    ) |>
    mutate(
      Created = as.POSIXct(
        Created,
        tz = Sys.timezone()
      )
    ) |>
    mutate(
      Updated = as.POSIXct(
        Updated,
        tz = Sys.timezone()
      )
    ) |>
    mutate(
      StatusCategoryChanged = as.POSIXct(
        StatusCategoryChanged,
        tz = Sys.timezone()
      )
    ) #|>
  # mutate(Created = parse_date_time(Created, "%Y-%m-%d %H:%M")) |>
  # mutate(Updated = parse_date_time(Updated, "%Y-%m-%d %H:%M")) |>
  # mutate(
  #   StatusCategoryChanged = parse_date_time(
  #     StatusCategoryChanged,
  #     "%Y-%m-%d %H:%M"
  #   )
  # ) |>
  # select(
  #   # Shed what seems like excess for now
  #   -c()
  # )
  if (round == 1) {
    Issues <- issues
  } else {
    Issues <- full_join(Issues, issues)
  }

  round <- 2
}

# Replace NA values with NaN, think this plays better with PBI
Issues <- Issues %>%
  replace_na(replace = list(TimeToCompletion = NaN))

# Server Save
# write.csv(Issues, "E:/Projects/PBI-Gateway/CSR_Issues.csv", row.names = FALSE)

# Local Save
write.csv(Issues, here::here("CSR_output.csv"))
