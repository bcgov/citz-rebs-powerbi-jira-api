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
project_id = "search?jql=project=COM&expand=changelog,names"
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
    # Select fields of interest
    select(
      IssueKeyNumber = id,
      IssueKey = key,
      Priority,
      Assignee,
      Resolved,
      Frequency,
      `Initial Target Date`,
      `Request Type`,
      Summary,
      `Issue Type`,
      Priority,
      Assignee,
      Reporter,
      Creator,
      Created,
      Description,
      `RPD Branch`,
      `Comms Plan Required?`,
      `Communication Lead`,
      `Communication Topic`,
      `Topic of Communication`,
      Status #,
      # `Status Category Changed`,
      # Updated
    ) |>
    # begin retrieving values of interest and flattening list columns
    # Assignee ####
    safe_hoist(Assignee, AssigneeName = "displayName", .remove = FALSE) |>
    select(-c(Assignee)) |>
    # Comms Plan Required?
    safe_hoist(
      `Comms Plan Required?`,
      RequireCommsPlan = "value",
      .remove = FALSE
    ) |>
    select(-c(`Comms Plan Required?`)) |>
    # Communication Lead
    safe_hoist(
      `Communication Lead`,
      CommunicationLead = "displayName",
      .remove = FALSE
    ) |>
    # Communication Topic
    safe_hoist(
      `Topic of Communication`,
      # Check if there is ever more than one
      TopicOfCommunication = list("content", 1L, "content", 1L, "text"),
      .remove = FALSE
    ) |>
    select(-c(`Topic of Communication`)) |>
    # Creator
    safe_hoist(Creator, CreatorName = "displayName", .remove = FALSE) |>
    select(-c(Creator)) |>
    # Created - format as date
    mutate(Created = as.Date(Created, format = "%Y-%m-%d")) |>
    # Description may need fine tuning with an unnest_wider if it needs more than one
    safe_hoist(
      Description,
      Description_val = list("content", 1L, "content", 1L, "text"),
      .remove = FALSE
    ) |>
    select(-c(Description)) |>
    # Frequency
    safe_hoist(
      Frequency,
      Frequency_val = list(1L, "value"),
      .remove = FALSE
    ) |>
    select(-c(Frequency)) |>
    # Initial Target Date - convert to date format
    mutate(
      `Initial Target Date` = as.Date(
        `Initial Target Date`,
        format = "%Y-%m-%d"
      )
    ) |>
    # Issue Type
    safe_hoist(
      `Issue Type`,
      IssueType = "name",
      .remove = FALSE
    ) |>
    select(-c(`Issue Type`)) |>
    # Priority
    safe_hoist(Priority, Priority_name = "name", .remove = FALSE) |>
    select(-c(Priority)) |>
    # Reporter
    safe_hoist(Reporter, Reporter_val = "displayName", .remove = FALSE) |>
    select(-c(Reporter)) |>
    # Request Type
    safe_hoist(`Request Type`, RequestType = list("requestType", "name")) |>
    select(-c(`Request Type`)) |>
    # Resolved - single date value, format as date
    mutate(Resolved = as.Date(Resolved, format = "%Y-%m-%d")) |>
    # RPD Branch
    safe_hoist(`RPD Branch`, RPD_Branch = "value", .remove = FALSE) |>
    select(-c(`RPD Branch`)) |>
    # Status
    safe_hoist(Status, Status_val = "name", .remove = FALSE) |>
    select(-c(Status)) |>
    # start cleaning up the columns
    select(-c(where(is.list))) |>
    select_all(~ gsub("_name|_text|_val|_value", "", .)) |>
    select_all(~ gsub("_", " ", .))
  if (start_at == 1) {
    Issues <- issues
  } else {
    Issues <- full_join(Issues, issues)
  }
  progress <- progress + max_results
  start_at <- progress + 1
}

# Tactics Loop ####
max_results = 100
start_at = 1
total_results = 25
progress = 0

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

  tactics <- resp |>
    purrr::pluck("issues") |>
    tibble::enframe() |>
    tidyr::unnest_wider(value) |>
    tidyr::unnest_wider(fields) |>
    # Rename custom fields
    plyr::rename(names) |>
    # Select fields of interest
    select(
      IssueKeyNumber = id,
      Tactics
    ) |>
    unnest_wider(starts_with("Tactics"), names_sep = "_") |>
    unnest_wider(starts_with("Tactics"), names_sep = "_") |>
    select(
      -c(starts_with("Tactics") & !ends_with("value"))
    ) |>
    pivot_longer(
      cols = starts_with("Tactics"),
      names_to = "aud_names",
      values_to = "aud_val"
    ) |>
    group_by(IssueKeyNumber) |>
    mutate(rownumber = row_number()) |>
    ungroup() |>
    mutate(
      Value = case_when(
        rownumber == 1 & is.na(aud_val) ~ "Blank",
        rownumber == 1 & !is.na(aud_val) ~ aud_val,
        rownumber != 1 & !is.na(aud_val) ~ aud_val,
        .default = NA
      )
    ) |>
    select(-c(aud_names, aud_val, rownumber)) |>
    filter(!is.na(Value)) |>
    mutate(count = 1) |>
    pivot_wider(names_from = Value, values_from = count)
  if (start_at == 1) {
    Tactics <- tactics
  } else {
    Tactics <- full_join(Tactics, tactics) |>
      mutate(across(everything(), ~ replace_na(.x, 0)))
  }
  progress <- progress + max_results
  start_at <- progress + 1
}

# IntendedAudiences Loop ####
max_results = 100
start_at = 1
total_results = 25
progress = 0
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

  intendedAudiences <- resp |>
    purrr::pluck("issues") |>
    tibble::enframe() |>
    tidyr::unnest_wider(value) |>
    tidyr::unnest_wider(fields) |>
    # Rename custom fields
    plyr::rename(names) |>
    select(
      IssueKeyNumber = id,
      `Intended audiences`,
      `Intended Audience(s)`
    ) |>
    unnest_wider(starts_with("Intended Audience"), names_sep = "_") |>
    unnest_wider(starts_with("Intended Audience"), names_sep = "_") |>
    select(
      -c(starts_with("Intended Audience") & !ends_with("value"))
    ) |>
    # Clean up Intended Audience(s) (the column numbering is bad after filtering)
    pivot_longer(
      cols = starts_with("Intended Audience"),
      names_to = "aud_names",
      values_to = "aud_val"
    ) |>
    group_by(IssueKeyNumber) |>
    mutate(rownumber = row_number()) |>
    ungroup() |>
    mutate(
      Value = case_when(
        rownumber == 1 & is.na(aud_val) ~ "Blank",
        rownumber == 1 & !is.na(aud_val) ~ aud_val,
        rownumber != 1 & !is.na(aud_val) ~ aud_val,
        .default = NA
      )
    ) |>

    select(-c(aud_names, aud_val, rownumber)) |>
    filter(!is.na(Value)) |>
    # For some reason duplicate values
    distinct() |>
    ungroup() |>
    mutate(count = 1) |>
    pivot_wider(
      id_cols = IssueKeyNumber,
      names_from = Value,
      values_from = count
    )

  if (start_at == 1) {
    IntendedAudiences <- intendedAudiences
  } else {
    IntendedAudiences <- full_join(IntendedAudiences, intendedAudiences) |>
      mutate(across(everything(), ~ replace_na(.x, 0)))
  }
  progress <- progress + max_results
  start_at <- progress + 1
}

Example <- IntendedAudiences |>
  rowwise() |>
  mutate(Count = sum(c_across(`All RPD Employees`:Other))) |>
  ungroup()
