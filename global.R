library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

read_timetable_wide <- function(file) {
  
  raw <- read_excel(file, col_names = FALSE)
  
  day_row <- raw[1, ]
  tt <- raw[-1, ]
  
  # If your first 3 columns are route, location, event
  names(tt)[1:3] <- c("route", "location", "event")
  
  days <- as.character(day_row[1, 4:ncol(raw)])
  days <- str_trim(days)
  
  sailing_col_names <- ave(
    days,
    days,
    FUN = function(x) paste0(x, "_", seq_along(x))
  )
  
  names(tt)[4:ncol(tt)] <- sailing_col_names
  
  tt_long <- tt %>%
    pivot_longer(
      cols = -c(route, location, event),
      names_to = "day_sailing",
      values_to = "time"
    ) %>%
    mutate(
      day = str_remove(day_sailing, "_\\d+$"),
      sailing_number = str_extract(day_sailing, "\\d+$"),
      time = as.character(time)
    ) %>%
    select(route, day, sailing_number, location, event, time) %>%
    filter(!is.na(time), time != "")
  
  return(tt_long)
}

# Read the wide timetable files
eday_sanday_stronsay <- read_timetable_wide("restart-timetables/eday_sanday_stronsay.xlsx")

westray_papa_westray <- read_timetable_wide("restart-timetables/westray_papa_westray.xlsx")

shapinsay <- read_timetable_wide("restart-timetables/shapinsay.xlsx")

# Read North Ronaldsay because it is already long format
north_ronaldsay <- read_excel(
  "restart-timetables/north_ronaldsay_long_format.xlsx"
) %>%
  rename(
    route = Route,
    day = Day,
    month = Month,
    sailing_number = `Stop Order`,
    location = Location,
    event = `arr/dep`,
    time = Time,
    date_header = `Original Date Header`
  ) %>%
  select(route, day, month, date_header, sailing_number, location, event, time) %>%
  mutate(across(everything(), as.character))

# Make all columns character before combining
eday_sanday_stronsay <- eday_sanday_stronsay %>%
  mutate(
    month = NA_character_,
    date_header = NA_character_
  ) %>%
  select(route, day, month, date_header, sailing_number, location, event, time)

westray_papa_westray <- westray_papa_westray %>%
  mutate(
    month = NA_character_,
    date_header = NA_character_
  ) %>%
  select(route, day, month, date_header, sailing_number, location, event, time)

shapinsay <- shapinsay %>%
  mutate(
    month = NA_character_,
    date_header = NA_character_
  ) %>%
  select(route, day, month, date_header, sailing_number, location, event, time)

# Combine all route timetables
all_timetables <- bind_rows(
  eday_sanday_stronsay,
  westray_papa_westray,
  shapinsay,
  north_ronaldsay
)

# -----------------------------
# CLEAN LOCATION NAMES FOR APP DISPLAY
# -----------------------------

all_timetables <- all_timetables %>%
  mutate(
    location = str_trim(as.character(location)),
    location_clean = case_when(
      location %in% c("P.Westray", "P. Westray", "Papa W", "Papa Westray") ~ "Papa Westray",
      TRUE ~ location
    )
  )

# -----------------------------
# CREATE REAL DATE COLUMN
# Also split headers like "Tuesday 8 / 22 September"
# into two separate rows
# -----------------------------

all_timetables <- all_timetables %>%
  mutate(
    date_header = str_squish(as.character(date_header)),
    
    weekday_from_header = str_extract(date_header, "^[A-Za-z]+"),
    day_part = str_extract(date_header, "\\d+(\\s*/\\s*\\d+)*"),
    month_from_header = str_extract(date_header, "[A-Za-z]+$")
  ) %>%
  separate_rows(day_part, sep = "\\s*/\\s*") %>%
  mutate(
    date_header = if_else(
      !is.na(date_header) & date_header != "NA",
      paste(weekday_from_header, day_part, month_from_header),
      date_header
    ),
    
    full_date_text = if_else(
      !is.na(date_header) & date_header != "NA",
      paste(date_header, "2026"),
      NA_character_
    ),
    
    sailing_date = as.Date(full_date_text, format = "%A %d %B %Y")
  ) %>%
  select(-weekday_from_header, -day_part, -month_from_header)

View(all_timetables)

# -----------------------------
# FILTER TO RESTART ORKNEY HOURS
# Keep sailings that LEAVE KIRKWALL during open hours
# -----------------------------

time_to_minutes <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, ":", "")
  x <- str_replace_all(x, "[^0-9]", "")
  x <- ifelse(nchar(x) == 3, paste0("0", x), x)
  
  ifelse(
    nchar(x) == 4,
    as.integer(substr(x, 1, 2)) * 60 + as.integer(substr(x, 3, 4)),
    NA_integer_
  )
}

all_timetables_with_hours <- all_timetables %>%
  mutate(
    time_minutes = time_to_minutes(time),
    
    open_minutes = case_when(
      day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ 9 * 60,
      day == "Saturday" ~ 10 * 60,
      day == "Sunday" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    close_minutes = case_when(
      day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ 16 * 60,
      day == "Saturday" ~ 16 * 60,
      day == "Sunday" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    sailing_id = if_else(
      !is.na(sailing_date),
      paste(route, sailing_date, sep = "_"),
      paste(route, day, sailing_number, sep = "_")
    )
  )

# Only keep sailings where the Kirkwall departure is during open hours
valid_sailings <- all_timetables_with_hours %>%
  filter(
    location_clean == "Kirkwall",
    event == "dep",
    !is.na(close_minutes),
    !is.na(time_minutes),
    time_minutes <= close_minutes
  ) %>%
  distinct(sailing_id)

all_timetables_filtered <- all_timetables_with_hours %>%
  semi_join(valid_sailings, by = "sailing_id") %>%
  select(-time_minutes, -open_minutes, -close_minutes)

View(all_timetables_filtered)


# -----------------------------
# ROUND TRIP SEARCH FUNCTION
# -----------------------------
# -----------------------------
# ROUND TRIP SEARCH FUNCTION
# -----------------------------
find_round_trips_from_kirkwall <- function(data, selected_date, selected_destination) {
  
  selected_date <- as.Date(selected_date)
  selected_day <- weekdays(selected_date)
  
  open_time <- case_when(
    selected_day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ 900,
    selected_day == "Saturday" ~ 1000,
    TRUE ~ NA_real_
  )
  
  close_time <- case_when(
    selected_day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ 1600,
    selected_day == "Saturday" ~ 1600,
    TRUE ~ NA_real_
  )
  
  if (is.na(open_time) | is.na(close_time)) {
    return(data.frame(
      Message = "Restart Orkney is closed on Sundays."
    ))
  }
  
  clean_time_number <- function(x) {
    x <- as.character(x)
    x <- str_replace_all(x, ":", "")
    x <- str_replace_all(x, "[^0-9]", "")
    x <- ifelse(nchar(x) == 3, paste0("0", x), x)
    as.numeric(x)
  }
  
  data <- data %>%
    mutate(
      time_number = clean_time_number(time),
      sailing_date = as.Date(sailing_date),
      sailing_id = as.character(sailing_id)
    ) %>%
    group_by(sailing_id) %>%
    mutate(
      stop_order = row_number()
    ) %>%
    ungroup()
  
  # Outbound: Kirkwall dep → selected island arr
  outbound_departures <- data %>%
    filter(
      (
        (!is.na(sailing_date) & sailing_date == selected_date) |
          (is.na(sailing_date) & day == selected_day)
      ),
      location_clean == "Kirkwall",
      event == "dep",
      time_number <= close_time
    ) %>%
    select(
      day,
      outbound_sailing_id = sailing_id,
      outbound_departure_order = stop_order,
      outbound_departure = time,
      outbound_departure_number = time_number
    )
  
  outbound_arrivals <- data %>%
    filter(
      (
        (!is.na(sailing_date) & sailing_date == selected_date) |
          (is.na(sailing_date) & day == selected_day)
      ),
      location_clean == selected_destination,
      event == "arr"
    ) %>%
    select(
      day,
      outbound_sailing_id = sailing_id,
      outbound_arrival_order = stop_order,
      outbound_arrival = time,
      outbound_arrival_number = time_number
    )
  
  outbound_trips <- outbound_departures %>%
    inner_join(outbound_arrivals, by = c("day", "outbound_sailing_id")) %>%
    filter(as.numeric(outbound_arrival_order) > as.numeric(outbound_departure_order))
  
  # Return: selected island dep → Kirkwall arr
  return_departures <- data %>%
    filter(
      (
        (!is.na(sailing_date) & sailing_date == selected_date) |
          (is.na(sailing_date) & day == selected_day)
      ),
      location_clean == selected_destination,
      event == "dep"
    ) %>%
    select(
      day,
      return_sailing_id = sailing_id,
      return_departure_order = stop_order,
      return_departure = time,
      return_departure_number = time_number
    )
  
  return_arrivals <- data %>%
    filter(
      (
        (!is.na(sailing_date) & sailing_date == selected_date) |
          (is.na(sailing_date) & day == selected_day)
      ),
      location_clean == "Kirkwall",
      event == "arr"
    ) %>%
    select(
      day,
      return_sailing_id = sailing_id,
      return_arrival_order = stop_order,
      return_arrival = time,
      return_arrival_number = time_number
    )
  
  return_trips <- return_departures %>%
    inner_join(return_arrivals, by = c("day", "return_sailing_id")) %>%
    filter(as.numeric(return_arrival_order) > as.numeric(return_departure_order))
  
  results <- outbound_trips %>%
    inner_join(return_trips, by = "day") %>%
    filter(
      return_departure_number > outbound_arrival_number
    ) %>%
    mutate(
      outbound_departure = str_pad(str_replace_all(as.character(outbound_departure), ":", ""), 4, pad = "0"),
      outbound_arrival = str_pad(str_replace_all(as.character(outbound_arrival), ":", ""), 4, pad = "0"),
      return_departure = str_pad(str_replace_all(as.character(return_departure), ":", ""), 4, pad = "0"),
      return_arrival = str_pad(str_replace_all(as.character(return_arrival), ":", ""), 4, pad = "0"),
      
      outbound_departure = paste0(substr(outbound_departure, 1, 2), ":", substr(outbound_departure, 3, 4)),
      outbound_arrival = paste0(substr(outbound_arrival, 1, 2), ":", substr(outbound_arrival, 3, 4)),
      return_departure = paste0(substr(return_departure, 1, 2), ":", substr(return_departure, 3, 4)),
      return_arrival = paste0(substr(return_arrival, 1, 2), ":", substr(return_arrival, 3, 4))
    ) %>%
    select(
      day,
      outbound_departure,
      outbound_arrival,
      return_departure,
      return_arrival
    )
  
  if (nrow(results) == 0) {
    return(data.frame(
      Message = "No available delivery options found for this date and destination."
    ))
  }
  
  return(results)
}