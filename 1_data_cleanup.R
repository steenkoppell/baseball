## Data Cleanup ----
### Load data, remove unwanted variables, create specified datasets

## Load packages ----
library(tidyverse)
library(kableExtra)
library(here)

# Read in raw data ----
raw_statcast_data <- read_csv("data/raw/statcast_pitch_swing_data_2024_with_arm_angle.csv")
raw_switch_hitters <- read_csv("data/raw/fangraphs-leaderboards.csv")

# Data types ----
statcast_data_types <-
  tibble(
    type = c("date","character","logical","numeric"),
    n = c(1,16,8,88),
    missing_status = c("none","none","all","variable")
  )

kable(statcast_data_types, "html",
      col.names = c("Variable type", "Number", "Missingness trend")) |>
  kable_styling() |>
  save_kable("data/data_types.html")

# Determine removable variables ----
raw_statcast_data |>
  count(pitch_type) |>
  arrange(-n)

# Reformat names
name_format <- function(last_first){
  last <- substr({{last_first}}, 1, str_locate({{last_first}},",") - 1)
  first <- substr({{last_first}}, str_locate({{last_first}},",") + 2, str_length({{last_first}}))
  first_last <- paste0(first, " ", last)
  return(first_last)
}

## Tidy the data ----
statcast_data <-
  raw_statcast_data |>
  filter(pitch_type != "PO", pitch_type != "CS", pitch_type != "FA") |>
  # FA appears to label pitches thrown by position players
  select(-spin_dir,-spin_rate_deprecated, -break_angle_deprecated, 
         -break_length_deprecated,-tfs_deprecated,-tfs_zulu_deprecated,
         -umpire,-sv_id) |>
  mutate(player_name = name_format(player_name))

## Alternative datasets ----
# Plate appearance ----

statcast_data <- statcast_data |>
  filter(player_name != "Josh Naylor" & pitcher != 573009 & at_bat_number != 65 & fielder_7 != 666971)
# fixing an inconsistency in one of Josh Naylor's at-bats on August 7, 2024

plate_appearance_data <- statcast_data |>
  select(game_date, player_name, batter, pitcher, description, at_bat_number, pitch_number, stand, p_throws) |>
  group_by(game_date, batter, at_bat_number) |> # identifies unique plate appearance
  arrange(pitch_number) |> # ensures pitches are in order in data frame
  pivot_wider(
    names_from = pitch_number,
    values_from = description,
    names_prefix = "pitch_"
  ) |> 
  ungroup()

# Switch hitter ----
switch_hitter_data <- statcast_data |>
  filter(batter %in% raw_switch_hitters$MLBAMID) |>
  select(game_date, player_name, batter, stand, pitcher, p_throws, description, at_bat_number, pitch_number) |>
  group_by(game_date, batter, at_bat_number) |> # identifies unique plate appearance
  arrange(pitch_number) |> # ensures pitches are in order in data frame
  pivot_wider(
    names_from = pitch_number,
    values_from = description,
    names_prefix = "pitch_"
  )

# Platoon split ----
statcast_platoon_split <-
  statcast_data |>
  mutate(
    hand_match =
      case_when(
        (stand == "L" & p_throws == "L") ~ "BLPL",
        (stand == "L" & p_throws == "R") ~ "BLPR",
        (stand == "R" & p_throws == "R") ~ "BRPR",
        (stand == "R" & p_throws == "L") ~ "BRPL",
        (stand == NA | p_throws == NA) ~ NA
      )
  )

# Pitch outcomes ----
outcomes <- statcast_platoon_split |>
  filter(!is.na(events)) |>
  relocate(pitch_number, .after = pitch_type) |>
  mutate(outcome = case_when(
    events %in% c("single","home_run","double","triple") ~ "hit",
    events %in% c("strikeout","field_out","force_out","grounded_into_double_play",
                  "double_play","strikeout_double_play","field_error",
                  "fielders_choice_out","fielders_choice","triple_play") ~ "out"
  ), .after = pitch_type)

outcomes <- outcomes |>
  mutate(
    hand_match = ifelse(hand_match %in% c("BLPL","BRPR"), TRUE, FALSE)
  ) |>
  summarize(
    n_hit_match = sum(outcome == "hit" & hand_match == TRUE, na.rm = TRUE),
    n_out_match = sum(outcome == "out" & hand_match == TRUE, na.rm = TRUE),
    n_hit_no_match = sum(outcome == "hit" & hand_match == FALSE, na.rm = TRUE),
    n_out_no_match = sum(outcome == "out" & hand_match == FALSE, na.rm = TRUE),
    prop_hit_match = n_hit_match/(n_hit_match + n_out_match),
    prop_hit_no_match = n_hit_no_match/(n_hit_no_match + n_out_no_match),
    prop_hit_diff = prop_hit_no_match - prop_hit_match,
    .by = c(player_name)
  )

## off_speed vs. fastball classification
fastballs <- c("FA","FF","FS","FC","SI")
off_speeds <- c("SL","CH","CU","KC","KN","EP","ST","SV","FO")

speed_class <-
  statcast_data |>
  filter(!is.na(pitch_type)) |>
  mutate(
    pitch_speed = case_when(
      pitch_type %in% fastballs ~ "fastball",
      pitch_type %in% off_speeds ~ "off_speed",
      pitch_type == NA ~ NA
    ),
    pitch_speed = as.factor(pitch_speed),
    .after = pitch_type
  )

## Variation of plate_appearance_data, each pitch has type listed
pitch_progression <- statcast_data |>
  select(game_date, player_name, batter, pitcher, pitch_type, at_bat_number, pitch_number) |>
  group_by(game_date, batter, at_bat_number) |> # identifies unique plate appearance
  arrange(pitch_number) |> # ensures pitches are in order in data frame
  pivot_wider(
    names_from = pitch_number,
    values_from = pitch_type,
    names_prefix = "pitch_"
  )

## Same as pitch_progression but each pitch has its description listed
pitch_outcome_progression <- statcast_data |>
  select(game_date, player_name, batter, pitcher, description, at_bat_number, pitch_number) |>
  group_by(game_date, batter, at_bat_number) |> # identifies unique plate appearance
  arrange(pitch_number) |> # ensures pitches are in order in data frame
  pivot_wider(
    names_from = pitch_number,
    values_from = description,
    names_prefix = "pitch_"
  )

## Same as pitch_progression but each pitch has its speed listed
speed_progression <- speed_class |>
  select(game_date, player_name, batter, pitcher, description, at_bat_number, pitch_number, pitch_speed) |>
  group_by(game_date, batter, at_bat_number) |> # identifies unique plate appearance
  arrange(pitch_number) |> # ensures pitches are in order in data frame
  pivot_wider(
    names_from = pitch_number,
    values_from = pitch_speed,
    names_prefix = "pitch_"
  )

# write out files ----
save(raw_statcast_data, file = here("data/raw/raw_statcast_data.rda"))
save(statcast_data, file = here("data/statcast_data.rda"))
save(plate_appearance_data, file = here("data/plate_appearance_data.rda"))
save(switch_hitter_data, file = here("data/switch_hitter_data.rda"))
save(statcast_platoon_split, file = here("data/statcast_platoon_split.rda"))
save(outcomes, file = here("data/outcomes.rda"))
save(speed_class, file = here("data/speed_class.rda"))
save(pitch_progression, file = here("data/pitch_progression.rda"))
save(pitch_outcome_progression, file = here("data/pitch_outcome_progression.rda"))
save(speed_progression, file = here("data/speed_progression.rda"))

# Note: There are other datasets created within the two exploration R scripts.