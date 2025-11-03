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
      startAt = start_at,
      .multi = "comma" # control how vectors are appended, for expand_opts
    ) |>
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
      # StatusCategoryChanged = `Status Category Changed`,
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

  if (start_at == 1) {
    Issues <- issues
  } else {
    Issues <- full_join(Issues, issues)
  }
  progress <- progress + max_results
  start_at <- progress + 1
}

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
  group_by(IssueKey, item_fromString) |>
  summarise(TimeElapsed = sum(TimeElapsed, na.rm = TRUE)) |>
  pivot_wider(names_from = item_fromString, values_from = TimeElapsed) |>
  ungroup() |>
  rename_with(~ gsub(" ", "", .x), where(is.numeric))

Issues <- Issues |>
  left_join(StatusChange, by = join_by(IssueKey)) |>
  select(-changelog)

# write.csv(Issues, here::here("SBP_output.csv"))

# test <- issues |>
#   # select(where(is.list)) |>
#   safe_hoist(Status, Status = "name", .remove = FALSE) |>
#   filter(Status == "Closed") |>
#   select(key, Status, StatusCategoryChanged = `Status Category Changed`, TimeToResolution = `Time to resolution`) |>
#   safe_hoist(TimeToResolution, TimeToResolution = list("completedCycles"), .remove = FALSE)
