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
query_url = "https://citz-rpd.atlassian.net/rest/api/3/search"
project_id = "SBP"
expand_opts = c("changelog", "names")
max_results = 100
start_at = 1
total_results = 25
progress = 0

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
      Parent,
      changelog
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

  if (round == 1) {
    Issues <- issues
  } else {
    Issues <- full_join(Issues, issues)
  }

  round <- 2
}

# Calculate time spent in status for each ticket.
# timestamp is used for tickets that have only been open, calc time from creation to sys.time for total elapsed time.
timestamp <- format(Sys.time(), format = "%Y-%m-%dT%H:%M:%OS3%z")

StatusChange <- Issues |>
  select(IssueKey, Status, TicketCreated = Created, changelog) |>
  safe_hoist(changelog, histories = list("histories"), .remove = FALSE) |>
  select(-c(changelog)) |>
  unnest_longer(col = histories, values_to = "values") |>
  unnest_longer(col = values, values_to = "values", indices_to = "column") |>
  filter(column %in% c("created", "items")) |>
  pivot_wider(names_from = column, values_from = values, values_fn = list) |>
  unnest_longer(col = created:items) |>
  select(-c(created_id, items_id)) |>
  unnest_longer(items) |>
  safe_hoist(
    items,
    item_field = list("field"),
    .remove = FALSE
  ) |>
  safe_hoist(
    items,
    item_fromString = list("fromString"),
    .remove = FALSE
  ) |>
  safe_hoist(
    items,
    item_toString = list("toString"),
    .remove = FALSE
  ) |>
  group_by(IssueKey) |>
  mutate(rowNum = row_number()) |>
  mutate(
    created = case_when(
      !any(item_field == "status") & rowNum == 1 ~ timestamp,
      .default = created
    ),
    item_toString = case_when(
      !any(item_field == "status") & rowNum == 1 ~ "No-Change",
      .default = item_toString
    ),
    item_fromString = case_when(
      !any(item_field == "status") & rowNum == 1 ~ "Open",
      .default = item_fromString
    ),
    item_field = case_when(
      !any(item_field == "status") & rowNum == 1 ~ "status",
      .default = item_field
    )
  ) |>
  ungroup() |>
  filter(item_field == "status") |>
  select(-c(items, rowNum)) |>
  mutate(
    TicketCreated = ymd_hms(TicketCreated),
    created = ymd_hms(created)
  ) |>
  arrange(IssueKey, created) |>
  group_by(IssueKey) |>
  mutate(
    TimeElapsed = as.numeric(difftime(
      created,
      lag(created, n = 1, default = first(TicketCreated)),
      units = "days"
    ))
  ) |>
  ungroup() |>
  select(-c(item_toString, item_field, created, Status, TicketCreated)) |>
  mutate(
    item_fromString = gsub(" ", "", tools::toTitleCase(item_fromString))
  ) |> # deal with variable capitalization
  group_by(IssueKey, item_fromString) |>
  summarise(TimeElapsed = sum(TimeElapsed, na.rm = TRUE)) |>
  pivot_wider(names_from = item_fromString, values_from = TimeElapsed) |>
  ungroup()

# Deal with issues where extra newline characters screwed up the read in of data to power bi
Issues <- Issues |>
  mutate(across(where(is.character), ~ gsub(",", "", .x))) |>
  mutate(across(where(is.character), ~ trimws(.x))) |>
  left_join(StatusChange, by = join_by(IssueKey)) |>
  select(-changelog)

# Server Save
# write.csv(Issues, "E:/Projects/PBI-Gateway/SBP_Issues.csv", row.names = FALSE)

# Local Save
write.csv(Issues, here::here("SBP_output.csv"))
