#### Platoon Advantage Analysis ----

### Load packages ---- 
library(tidyverse)
library(baseballr)
library(kableExtra)
library(here)

### Load relevant data
load(here("data/statcast_data.rda"))
load(here("data/switch_hitter_data.rda"))
load(here("data/statcast_platoon_split.rda"))
load(here("data/outcomes.rda"))
load(here("data/plate_appearance_data.rda"))

### Analysis 1: Effect of handedness/stance split ----
## Pitch outcome by handedness/stance split ---- 
# Creating hands.html table ----
statcast_data |>
  filter((player_name %in% switch_hitter_data$player_name) == FALSE) |>
  select(batter, stand) |>
  distinct(batter, stand) |>
  count(stand)

statcast_data |> distinct(pitcher, p_throws) |> count(p_throws)

hands <- tibble(
  position = c("Pitcher", "Hitter"),
  right = c(625, 364),
  left = c(220, 221),
  switch = c(0,77)
)

knitr::kable(hands, "html", 
             col.names = c("Position", "Right-handed", "Left-handed", "Switch"))  |>
  kable_styling() |>
  save_kable("visualizations/hands.html")

# Batter stance by pitcher handedness ----
plate_appearance_data |>
  ggplot(aes(p_throws, fill= stand)) +
  geom_bar() +
  labs(
    title = "Batter stance (L/R) within pitcher handedness",
    x = "Pitcher handedness",
    y = "Number"
  ) +
  scale_fill_discrete(name = "Batter stance")
ggsave("visualizations/stance_by_pitcher.png", width = 5, height = 4)

# Platoon split events ----
statcast_platoon_split |>
  summarize(
    n = n(),
    foul_pct = sum(description == "foul" | description == "foul_tip")/n(),
    miss_pct = sum(description == "swinging_strike")/n(),
    fair_pct = sum(description == "hit_into_play")/n(),
    no_swing_pct = sum(description %in% c("ball","called_strike","blocked_ball"))/n(),
    .by = hand_match
  )

# Outcomes by platoon split (proportion) ----
statcast_platoon_split |>
  mutate(
    description = as.factor(description),
    description = fct_collapse(description,
                               "foul" = c("foul", "foul_tip"),
                               "miss" = c("swinging_strike","swinging_strike_blocked"),
                               "fair" = "hit_into_play",
                               "no swing" = c("ball","called_strike","blocked_ball"),
                               "bunt_attempt" = c("foul_bunt", "missed_bunt", "bunt_foul_tip"),
                               "pitchout" = "pitchout",
                               "hit_by_pitch" = "hit_by_pitch")
  ) |>
  filter(description %in% c("foul","fair","miss","no swing")) |>
  ggplot(aes(description, fill = hand_match)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proportion of pitch outcomes by platoon split",
    x = "Pitch outcome",
    y = "Proportion"
  ) +
  scale_fill_discrete(name = "Batter/Pitcher Handedness", labels = c("Bats L, Pitches L","Bats L, Pitches R", "Bats R, Pitches L", "Bats R, Pitches R"))
ggsave("visualizations/platoon_proportion.png", width = 5, height = 4)

# Outcomes by platoon split (proportion, simplified) ----
statcast_platoon_split |>
  mutate(
    description = as.factor(description),
    description = fct_collapse(description,
                               "foul" = c("foul", "foul_tip"),
                               "miss" = c("swinging_strike","swinging_strike_blocked"),
                               "fair" = "hit_into_play",
                               "no swing" = c("ball","called_strike","blocked_ball"),
                               "bunt_attempt" = c("foul_bunt", "missed_bunt", "bunt_foul_tip"),
                               "pitchout" = "pitchout",
                               "hit_by_pitch" = "hit_by_pitch"),
    hand_match = case_when(
      (stand == p_throws) ~ "Match",
      (stand != p_throws) ~ "No match",
      (stand == NA | p_throws == NA) ~ NA
    )
  ) |>
  filter(description %in% c("foul","fair","miss","no swing")) |>
  ggplot(aes(description, fill = hand_match)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proportion of pitch outcomes by handedness split",
    x = "Pitch outcome",
    y = "Proportion"
  ) +
  scale_fill_discrete(name = "Batter/Pitcher Handedness", labels = c("Match","No match"))
ggsave("visualizations/hand_match_proportion.png", width = 5, height = 4)

## Proportion of handedness/stance splits ---- 
statcast_data |>
  ggplot(aes(p_throws), position = "fill") +
  geom_bar(aes(fill = stand)) +
  labs(
    title = "Batter stance (L/R) within pitcher handedness",
    x = "Pitcher handedness",
    y = "Number"
  ) +
  scale_fill_discrete(name = "Batter Stance")
ggsave("visualizations/stance_by_pitcher.png", width = 5, height = 4)

statcast_data |>
  ggplot(aes(stand), position = "fill") +
  geom_bar(aes(fill = p_throws))

## Individual hitters vs. pitchers of different handedness ---- 
# Density of platoon split
ggplot(outcomes, aes(prop_hit_diff)) +
  geom_density(na.rm = TRUE) +
  geom_vline(xintercept = median(outcomes$prop_hit_diff, na.rm = TRUE), color = "blue2") +
  labs(
    title = "Distribution of platoon split",
    subtitle = "Effect of batter stance/pitcher handedness on average outcomes",
    x = "Difference in hit proportion",
    y = "Density"
  ) 

ggsave("visualizations/platoon_distribution.png", width = 5, height = 4)

## Location of hits based on split ----  
player_spray <- function(player){
  plot <- statcast_platoon_split |>
    mutate(
      hand_match = ifelse(hand_match %in% c("BLPL","BRPR"), "yes", "no")
    ) |>
    filter(player_name == {{player}}, description == "hit_into_play") |>
    ggplot(aes(x = hc_x, y = -hc_y, fill = hand_match)) +
    geom_point(size = 3, shape = 21, alpha = 0.5) +
    scale_fill_manual(
      name = "Batter stance/pitcher handedness",
      values = c(no = "red", yes = "lightblue"),
      labels = c("No match","Match"),
      na.value = "grey50"
    ) +
    labs(
      title = glue::glue("Spray Chart of {player}, by Batter/Pitcher Handedness Matchup"),
      x = "Horizontal hit coordinate",
      y = "Vertical hit coordinate"
    ) 
  
  return(plot)
}

## Same function, mapped on a field ---- 
platoon_spray <- function(player){
  platoon <- statcast_platoon_split |>
    mutate(location_x = 2.5 * (hc_x - 125.42),
           location_y = 2.5 * (198.27 - hc_y),
           hand_match = ifelse(hand_match %in% c("BLPL","BRPR"), "yes", "no")
    ) |>
    filter(player_name == {{player}})
  plot <- 
    sportyR::geom_baseball(league = "MLB") +
    geom_point(data = platoon, aes(location_x, location_y, color = hand_match)) +
    scale_color_manual(
      values = c(no = "red", yes = "lightblue"),
      name = "Handedness",
      labels = c("No match","Match"),
      na.value = "grey50"
    ) +
    labs(
      title = glue::glue("Spray Chart of {player},  by Batter/Pitcher Handedness"),
      x = "Horizontal hit coordinate",
      y = "Vertical hit coordinate"
    ) 
  return(plot)
}

platoon_spray("Anthony Volpe")
ggsave("visualizations/volpe_spray.png", width = 8, height = 7)

platoon_spray("Gunnar Henderson")
ggsave("visualizations/gunnar_spray.png", width = 8, height = 7)

platoon_spray("Luis Arraez")
ggsave("visualizations/arraez_spray.png", width = 8, height = 7)

platoon_spray("Marcus Semien")
ggsave("visualizations/semien_spray.png", width = 8, height = 7)

## Same function, for switch-hitters ----
switch_spray <- function(player){
  platoon <- statcast_platoon_split |>
    mutate(location_x = 2.5 * (hc_x - 125.42),
           location_y = 2.5 * (198.27 - hc_y)
    ) |>
    filter(player_name == {{player}})
  plot <- 
    sportyR::geom_baseball(league = "MLB") +
    geom_point(data = platoon, aes(location_x, location_y, color = p_throws)) +
    scale_color_manual(
      values = c(R = "red", L = "lightblue"),
      name = "Batter handedness",
      labels = c("Right","Left"),
      na.value = "grey50"
    ) +
    labs(
      title = glue::glue("Spray chart of {player}, Batter handedness"),
      x = "Horizontal hit coordinate",
      y = "Vertical hit coordinate"
    ) 
  return(plot)
}

switch_spray("Francisco Lindor")
ggsave("visualizations/lindor_spray.png", width = 8, height = 7)

switch_spray("Elly De La Cruz")
ggsave("visualizations/elly_spray.png", width = 8, height = 7)

switch_spray("Jurickson Profar")
ggsave("visualizations/profar_spray.png", width = 8, height = 7)

switch_spray("Ian Happ")
ggsave("visualizations/happ_spray.png", width = 8, height = 7)


### Switch hitter behavior ----
switch_hitter_data |>
  mutate(
    hand_match =
      case_when(
        (stand == "L" & p_throws == "L") ~ "BLPL",
        (stand == "L" & p_throws == "R") ~ "BLPR",
        (stand == "R" & p_throws == "R") ~ "BRPR",
        (stand == "R" & p_throws == "L") ~ "BRPL",
        (stand == NA | p_throws == NA) ~ NA
      )
  ) |>
  ggplot(aes(p_throws), position = "fill") +
  geom_bar(aes(fill = stand)) + labs(
    title = "Switch-hitting",
    subtitle = "Batter stance (L/R) within pitcher handedness",
    x = "Pitcher handedness",
    y = "Number"
  ) +
  scale_fill_discrete(name = "Batter stance")

ggsave("visualizations/switch_hit_splits.png", width = 5, height = 4)

statcast_platoon_split |>
  ggplot(aes(p_throws), position = "fill") +
  geom_bar(aes(fill = stand))


## Checking platoon spray ----
# gunnar
statcast_data |>
  filter(player_name == "Gunnar Henderson", description == "hit_into_play") |>
  count(p_throws)

player_spray("Gunnar Henderson")
platoon_spray("Gunnar Henderson")

statcast_data |>
  filter(player_name == "Gunnar Henderson", description == "hit_into_play") |>
  count(p_throws, events) |>
  arrange(events, -n)

statcast_platoon_split |>
  mutate(
    hand_match = ifelse(hand_match %in% c("BLPL","BRPR"), "yes", "no")
  ) |>
  filter(player_name == "Gunnar Henderson", description == "hit_into_play") |>
  count(hand_match)

# semien
statcast_data |>
  filter(player_name == "Marcus Semien", description == "hit_into_play") |>
  count(p_throws)

statcast_data |>
  filter(player_name == "Marcus Semien", description == "hit_into_play") |>
  count(p_throws, events) |>
  arrange(events, -n)

player_spray("Marcus Semien")
platoon_spray("Marcus Semien")

statcast_platoon_split |>
  mutate(
    hand_match = ifelse(hand_match %in% c("BLPL","BRPR"), "no", "yes")
  ) |>
  filter(player_name == "Marcus Semien", description == "hit_into_play") |>
  count(hand_match)

## Function to map only safe base hits by platoon ----
safe_spray <- function(player){
  platoon <- statcast_platoon_split |>
    mutate(location_x = 2.5 * (hc_x - 125.42),
           location_y = 2.5 * (198.27 - hc_y),
           hand_match = ifelse(hand_match %in% c("BLPL","BRPR"), "yes", "no")
    ) |>
    filter(player_name == {{player}}, events %in% c("single","double","triple","home_run"))
  plot <- 
    sportyR::geom_baseball(league = "MLB") +
    geom_point(data = platoon, aes(location_x, location_y, color = hand_match)) +
    scale_color_manual(
      values = c(no = "red", yes = "lightblue"),
      name = "Handedness",
      labels = c("No match","Match"),
      na.value = "grey50"
    ) +
    labs(
      title = glue::glue("Spray Chart of {player}, by Batter/Pitcher Handedness"),
      subtitle = "Location of base hits and home runs",
      x = "Horizontal hit coordinate",
      y = "Vertical hit coordinate"
    ) 
  return(plot)
}


safe_spray("Anthony Volpe")
#platoon_spray("Anthony Volpe")
ggsave("visualizations/volpe_safe_spray.png", width = 8, height = 7)

safe_spray("Gunnar Henderson")
#platoon_spray("Gunnar Henderson")
ggsave("visualizations/gunnar_safe_spray.png", width = 8, height = 7)

safe_spray("Luis Arraez")
ggsave("visualizations/arraez_safe_spray.png", width = 8, height = 7)

safe_spray("Marcus Semien")
#platoon_spray("Marcus Semien")
ggsave("visualizations/semien_safe_spray.png", width = 8, height = 7)

## Same function but for switch hitters ----
switch_safe_spray <- function(player){
  platoon <- statcast_platoon_split |>
    mutate(location_x = 2.5 * (hc_x - 125.42),
           location_y = 2.5 * (198.27 - hc_y),
    ) |>
    filter(player_name == {{player}}, events %in% c("single","double","triple","home_run"))
  plot <- 
    sportyR::geom_baseball(league = "MLB") +
    geom_point(data = platoon, aes(location_x, location_y, color = p_throws)) +
    scale_color_manual(
      values = c(R = "red", L = "lightblue"),
      name = "Batter handedness",
      labels = c("Right","Left"),
      na.value = "grey50"
    ) +
    labs(
      title = glue::glue("Spray Chart of {player}, by Batter Handedness"),
      subtitle = "Location of base hits and home runs",
      x = "Horizontal hit coordinate",
      y = "Vertical hit coordinate"
    ) 
  return(plot)
}

switch_safe_spray("Francisco Lindor")
ggsave("visualizations/lindor_safe_spray.png", width = 8, height = 7)

switch_safe_spray("Elly De La Cruz")
ggsave("visualizations/elly_safe_spray.png", width = 8, height = 7)

switch_safe_spray("Jurickson Profar")
ggsave("visualizations/profar_safe_spray.png", width = 8, height = 7)

switch_safe_spray("Ian Happ")
ggsave("visualizations/happ_safe_spray.png", width = 8, height = 7)

# checking Lindor
statcast_platoon_split |>
  mutate(location_x = 2.5 * (hc_x - 125.42),
         location_y = 2.5 * (198.27 - hc_y)) |>
  filter(player_name == "Elly De La Cruz", events %in% c("single","double","triple","home_run")) |>
  select(player_name, stand, p_throws, location_x, location_y, events) |>
  arrange(-location_x)

# Verify players who switch hit and throw left
# baseballr::playerid_lookup("Hughes", "Brandon")
# statcast_data |> filter(pitcher == 676714) |> count() # 80
# Verify players who bat right and throw left
# baseballr::playerid_lookup("McCormick", "Chas")
# statcast_data |> filter(batter == 676801) |> count() # 946
# baseballr::playerid_lookup("Meyers", "Jake")
# statcast_data |> filter(batter == 676694) |> count() # 1762
