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

# Setup variables needed for API calls
# https://developer.atlassian.com/cloud/jira/platform/basic-auth-for-rest-apis/
# API key will need to be generated on the Jira website
# Eventual goal will be to have the token and email associated with a service account
# For the M query version will omit email and api_key and put the encoded token directly in
email <- read.csv("C:/Projects/credentials/email_address.csv") |> pull()
api_key <- read.csv("C:/Projects/credentials/jira_api_token.csv") |> pull()
token <- base64encode(charToRaw(paste0(email, ":", api_key)))
token_string <- paste("Basic", token)
query_url = "https://citz-rpd.atlassian.net/rest/api/3/field"

# Query API to get the names for custom fields
req <- request(query_url) |>
  req_headers(Authorization = token_string) |>
  req_perform()

# fields is used to overwrite custom field columns with actual column name in subsequent queries
fields <- req |>
  resp_body_json() |>
  tibble::enframe() |>
  tidyr::unnest_wider(value, names_sep = "_") |>
  select(id = value_id, name = value_name) |>
  add_row(id = "changelog", name = "changelog")

# Setup API retrieving all issues/tickets from dashboard
query_url = "https://citz-rpd.atlassian.net/rest/api/3/"
project_id = "search?jql=project=COM&expand=changelog"
max_results = 100
start_at = 1
total_results = 25
progress = 0

# Loop query to get all issues in a dashboard
while (total_results > progress) {
  print(paste0("StartAt: ", start_at))
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
  total_results <- resp["total"]

  # Start unnesting returned json before customfield renaming
  step_one <- resp |>
    purrr::pluck("issues") |>
    tibble::enframe() |>
    tidyr::unnest_wider(value) |>
    tidyr::unnest_wider(fields)

  # Setup values to rename custom fields
  colnames <- step_one |>
    select(-c(1:5)) |>
    colnames() |>
    tibble::enframe() |>
    select(-name) |>
    left_join(fields, by = join_by(value == id)) |>
    group_by(name) |>
    mutate(row_name = row_number(), row_count = n()) |>
    mutate(
      name = case_when(
        row_count > 1 ~ paste0(name, "-", row_name),
        .default = name
      )
    ) |>
    select(-c(row_name, row_count)) |>
    tibble::deframe()

  issueDimension <- step_one |>
    # Rename custom fields
    plyr::rename(colnames) |>
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
      `Topic of Communication` #,
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
      InitialTargetDate = as.Date(
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
    # start cleaning up the columns
    select(-c(where(is.list))) |>
    select_all(~ gsub("_name|_text|_val|_value", "", .)) |>
    select_all(~ gsub("_", " ", .))

  additionalContributors <- step_one |>
    # Rename custom fields
    plyr::rename(colnames) |>
    # Select fields of interest
    select(
      IssueKeyNumber = id,
      `Additional Contributors (SMEs)`
    ) |>
    # Additional Contributors ####
    safe_hoist(
      `Additional Contributors (SMEs)`,
      Additional_Contributors_SMEs = list("content", 1L, "content"),
      .remove = FALSE
    ) |>
    unnest_wider(Additional_Contributors_SMEs, names_sep = "_") |>
    unnest_wider(
      starts_with("Additional_Contributors_SMEs"),
      names_sep = "_"
    ) |>
    select(
      -c(starts_with("Additional_Contributors_SMEs") & !ends_with("text"))
    ) |>
    select(-c(`Additional Contributors (SMEs)`)) |>
    # Clean up Additional Contributors (column numbers are weird)
    pivot_longer(
      cols = starts_with("Additional_Contributors"),
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
    filter(!is.na(Value))

  branchOrGroup <- step_one |>
    # Rename custom fields
    plyr::rename(colnames) |>
    # Select fields of interest
    select(
      IssueKeyNumber = id,
      `Branch or Group`
    ) |>
    # begin retrieving values of interest and flattening list columns
    # Branch or Group ####
    unnest_wider(`Branch or Group`, names_sep = "_") |>
    unnest_wider(starts_with("Branch or Group"), names_sep = "_") |>
    select(-c(starts_with("Branch or Group") & !ends_with("value"))) |>
    pivot_longer(
      cols = starts_with("Branch or Group"),
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
    filter(!is.na(Value))

  intendedAudiences <- step_one |>
    # Rename custom fields
    plyr::rename(colnames) |>
    # Select fields of interest
    select(
      IssueKeyNumber = id,
      `Intended audiences`,
      `Intended Audience(s)`
    ) |>
    # Intended Audiences ####
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
    filter(!is.na(Value))

  tactics <- step_one |>
    # Rename custom fields
    plyr::rename(colnames) |>
    # Select fields of interest
    select(
      IssueKeyNumber = id,
      Tactics
    ) |>
    # Tactics ####
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
    pivot_wider(names_from = Value, values_from = count) |>
    mutate(across(everything(), ~ replace_na(.x, 0)))

  ticketStatus <- step_one |>
    # Rename custom fields
    plyr::rename(colnames) |>
    # Select fields of interest
    select(
      IssueKeyNumber = id,
      changelog,
      changelog_historiesFirstcreated = Created
    ) |>
    # changelog ####
    safe_hoist(
      changelog,
      changelog_histories = list("histories"),
      .remove = FALSE
    ) |>
    unnest_wider(changelog_histories, names_sep = "_") |>
    unnest_wider(contains("changelog_histories_"), names_sep = "_") |>
    select(
      -c(
        starts_with("changelog_histories") &
          !ends_with("items") &
          !ends_with("created")
      )
    ) |>
    unnest_wider(contains("items"), names_sep = "_") |>
    unnest_wider(contains("items_"), names_sep = "_") |>
    select(-changelog) |>
    pivot_longer(
      cols = contains("changelog_histories"),
      names_to = "column_name",
      values_to = "column_value"
    ) |>
    filter(!is.na(column_value)) |>
    # match first number in name string
    mutate(
      col_number = stringr::str_extract(column_name, "[0-9]+"),
      col_first_type = stringr::str_extract(
        column_name,
        "([0-9]+_)([a-z]+)",
        group = 2
      ),
      sub_col_number = stringr::str_extract(
        column_name,
        "([0-9]+_[a-z]+_)([0-9]+)",
        group = 2
      ),
      col_second_type = stringr::str_extract(
        column_name,
        "([0-9]+_[a-z]+_[0-9]+_)([a-zA-Z]+)",
        group = 2
      )
    ) |>
    mutate(
      col_number = case_when(!is.na(col_number) ~ col_number, .default = "0")
    ) |>
    group_by(IssueKeyNumber, col_number) |>
    filter(any(column_value == 'status') | col_number == 0) |>
    group_by(IssueKeyNumber, col_number, sub_col_number) |>
    filter(all(!column_value == 'resolution')) |>
    filter(col_second_type %in% c('toString', NA)) |>
    mutate(
      col_name = case_when(
        col_number == 0 ~ 'Created',
        col_number != 0 & !is.na(col_second_type) ~ col_second_type,
        col_number != 0 & is.na(col_second_type) ~ 'TimeStamp'
      )
    ) |>
    ungroup() |>
    select(-c(column_name, col_first_type, sub_col_number, col_second_type)) |>
    relocate(col_name, column_value, col_number, .after = IssueKeyNumber) |>
    pivot_wider(
      names_from = col_name,
      values_from = column_value
    ) |>
    mutate(
      TimeStamp = case_when(
        !is.na(Created) ~ Created,
        is.na(Created) ~ TimeStamp
      ),
      Value = case_when(!is.na(toString) ~ toString, .default = "Open")
    ) |>
    select(-c(col_number, toString, Created)) |>
    mutate(
      TimeStamp = ymd_hms(TimeStamp)
    ) |>
    arrange(IssueKeyNumber, TimeStamp)

  if (start_at == 1) {
    Issues <- issueDimension
    AdditionalContributors <- additionalContributors
    BranchOrGroup <- branchOrGroup
    IntendedAudiences <- intendedAudiences
    Status <- ticketStatus
    Tactics <- tactics
  } else {
    Issues <- full_join(Issues, issueDimension)
    AdditionalContributors <- full_join(
      AdditionalContributors,
      additionalContributors
    )
    BranchOrGroup <- full_join(BranchOrGroup, branchOrGroup)
    IntendedAudiences <- full_join(IntendedAudiences, intendedAudiences)
    Status <- full_join(Status, ticketStatus)
    Tactics <- full_join(Tactics, tactics)
  }

  progress <- progress + max_results
  start_at <- progress + 1
}

rm(
  list = c(
    'additionalContributors',
    'branchOrGroup',
    'intendedAudiences',
    'issueDimension',
    'step_one',
    'tactics',
    'ticketStatus'
  )
)
