query_url = "https://citz-rpd.atlassian.net/rest/api/3/issue/SBP-878?expand=names,changelog"
project_id = "SBP"
expand_opts = c("changelog", "names")
max_results = 100
start_at = 1
total_results = 25
progress = 0
issue_key = "SBP-878"

req <- request(query_url) |>
  req_headers(Authorization = token_string) |>
  req_perform()

resp <- req |> resp_body_json()

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
  purrr::pluck("fields") |>
  tibble::enframe() |>
  pivot_wider(names_from = name, values_from = value) |>
  plyr::rename(names) |>
  rename_with(~ gsub(" ", "", .x), everything())

changelog <- resp |>
  purrr::pluck("changelog") |>
  purrr::pluck("histories") |>
  tibble::enframe()

changelog$value[[1]]$created
changelog$value[[1]]$items[[1]]$field
changelog$value[[1]]$items[[1]]$fromString
changelog$value[[1]]$items[[1]]$toString

changelog2 <- changelog |>
  safe_hoist(value, value_created = list("created"), .remove = FALSE) |>
  safe_hoist(
    value,
    value_field = list("items", 1L, "field"),
    .remove = FALSE
  ) |>
  safe_hoist(
    value,
    value_fromString = list("items", 1L, "fromString"),
    .remove = FALSE
  ) |>
  safe_hoist(
    value,
    value_toString = list("items", 1L, "toString"),
    .remove = FALSE
  )


test$RequestParticipants[72]
safe_hoist_all(
  RequestParticipants,
  path = list("displayName"),
  .remove = FALSE
)


output <- test |>
  safe_hoist(
    changelog,
    changelog_created = list("histories", "created"),
    .remove = FALSE
  ) |>
  safe_hoist(
    changelog,
    changelog_field = list("items", 1L, "field"),
    .remove = FALSE
  ) |>
  safe_hoist(
    changelog,
    changelog_fromString = list("items", 1L, "fromString"),
    .remove = FALSE
  ) |>
  safe_hoist(
    changelog,
    changelog_toString = list("items", 1L, "toString"),
    .remove = FALSE
  )

timestamp <- format(Sys.time(), format = "%Y-%m-%dT%H:%M:%OS3%z")

test <- issues |>
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

test2 <- test |>
  group_by(IssueKey) |>
  mutate(count = n()) |>
  ungroup()
# Finish by figuring out how to pivot to new columns to have one row per ticket
# and the values for each time duration in the correct status column.
