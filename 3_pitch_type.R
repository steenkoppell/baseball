#### Pitch Type Analysis ----

### Load packages ----
library(tidyverse)
library(baseballr)
library(dplyr)
library(kableExtra)
library(here)

### Load data ----
load(here("data/raw/raw_statcast_data.rda"))
load(here("data/statcast_data.rda"))
load(here("data/switch_hitter_data.rda"))
load(here("data/plate_appearance_data.rda"))
load(here("data/speed_class.rda"))
load(here("data/pitch_outcome_progression.rda"))
load(here("data/pitch_progression.rda"))

### Classifying pitches by speed ----
fastballs <- c("FA","FF","FS","FC","SI")
off_speeds <- c("SL","CH","CU","KC","KN","EP","ST","SV","FO")


pitch_types <- statcast_data |> distinct(pitch_type)
pitch_types <- left_join(pitch_types, mlb_pitch_types(), join_by(pitch_type == pitch_type_code))

speed_vec <- NULL
for (i in 1:14){
  if (pitch_types[[1]][i] %in% fastballs){
    speed_vec[i] <- "Fastball"
  } else {
    speed_vec[i] <- "Off-speed"
  }}

pitch_types <- cbind(pitch_types, speed_vec)
knitr::kable(pitch_types, col.names = c("MLB Code", "Pitch Type", "Speed Class")) |>
  kable_styling() |>
  save_kable(here("visualizations/pitch_types.html"))


# Checking FA
raw_statcast_data |>
  filter(pitch_type != "PO", pitch_type != "CS") |>
  select(pitch_type, release_speed) |>
  summarize(
    median_speed = median(release_speed, na.rm = TRUE),
    .by = pitch_type
  ) |>
  mutate(
    pitch_type = as.factor(pitch_type),
    pitch_type = fct_reorder(pitch_type, -median_speed)
  ) |>
  ggplot(aes(pitch_type, median_speed)) +
  geom_point() +
  coord_cartesian(ylim = c(45,95)) +
  labs(
    title = "Median speed by pitch type",
    x = "Pitch type",
    y = "Median speed (mph)"
  )

ggsave(here("visualizations/fa_median_speed.png"), width = 5, height = 4)


# Speed by pitch type

statcast_data |>
  summarize(
    median_speed = median(release_speed, na.rm = TRUE),
    .by = pitch_type
  ) |>
  mutate(
    pitch_type = as.factor(pitch_type),
    pitch_type = fct_reorder(pitch_type, -median_speed)
  ) |>
  ggplot(aes(pitch_type, median_speed)) +
  geom_point() +
  coord_cartesian(ylim = c(45,95)) +
  labs(
    title = "Median speed by pitch type",
    x = "Pitch type",
    y = "Median speed (mph)"
  )

ggsave(here("visualizations/median_speed.png"), width = 5, height = 4)


### Analysis 1: Pitch Speed ----
speed_class |>
  filter(pitch_type != "EP") |>
  select(pitch_type, release_speed, pitch_speed) |>
  ggplot(aes(release_speed, fill = pitch_speed)) +
  geom_density(alpha = 0.7) +
  labs(
    title = "Distribution of release speed, fastball vs. off-speed",
    x = "Pitch speed when released (mph)",
    y = "Density"
  ) +
  scale_fill_discrete(name = "Pitch Type", labels = c("Fastball", "off-speed"))

ggsave(here("visualizations/speed_by_type.png"), width = 5, height = 4)

## Launch speed by release speed
speed_class |>
  summarize(
    n = n(),
    .by = pitch_type
  ) |>
  arrange(-n)
# Four-seam fastballs (FF) and sliders (SL) are the most popular pitches

speed_class |>
  filter(launch_speed != 0, 
         str_detect(description, "bunt") == FALSE, 
         str_detect(description, "foul") == FALSE,
         pitch_type %in% c("FF", "SL"))  |>
  ggplot(aes(release_speed, launch_speed, color = pitch_type)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  scale_color_manual(
    name = "Pitch type",
    values = c(FF = "blue", SL = "red"),
    labels = c("Four-seam fastball", "Slider (off-speed)")
  ) +
  labs(
    title = "Release and launch speed, fastball vs. slider",
    x = "Release speed (mph)",
    y = "Launch speed (mph)"
  )

ggsave(here("visualizations/release_launch_speed.png"), width = 6, height = 4)

# same as above but curveball and fastball
speed_class |>
  filter(launch_speed != 0, 
         str_detect(description, "bunt") == FALSE, 
         str_detect(description, "foul") == FALSE,
         pitch_type %in% c("FF", "CU"))  |>
  mutate(
    pitch_type = as.factor(pitch_type),
    pitch_type = pitch_type |> fct_infreq()) |>
  ggplot(aes(release_speed, launch_speed, color = pitch_type)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  scale_color_manual(
    name = "Pitch type",
    values = c(FF = "blue", CU = "red"),
    labels = c("Four-seam fastball", "Curveball")
  ) +
  labs(
    title = "Release and launch speed, fastball vs. curveball",
    x = "Release speed (mph)",
    y = "Launch speed (mph)"
  )

ggsave(here("visualizations/fb_curve.png"), width = 6, height = 4)

## Launch speed determiners
png(here("visualizations/correlations.png"), width = 800, height = 600, res = 100)

statcast_data |>
  filter(!is.na(launch_speed)) |>
  select(launch_speed, launch_angle, release_speed, release_spin_rate, bat_speed) |>
  cor(use = "pairwise.complete.obs") |>
  corrplot::corrplot(type = "lower", order = "FPC")

dev.off()

## Location of hit by pitch speed ----
spray_field <- function(player){
  speed1 <- speed_class |>
    mutate(location_x = 2.5 * (hc_x - 125.42),
           location_y = 2.5 * (198.27 - hc_y)) |>
    filter(
      player_name == {{player}},
      !is.na(pitch_speed)
    )
  
  plot <- 
    sportyR::geom_baseball(league = "MLB") +
    geom_point(data = speed1, aes(location_x, location_y, color = pitch_speed)) +
    scale_color_manual(
      values = c(off_speed = "red", fastball = "lightblue"),
      name = "Pitch Speed",
      labels = c("Off-speed","Fastball"),
      na.value = "grey50"
    ) +
    labs(
      title = glue::glue("Spray Chart of {player}, by Pitch Speed"),
      x = "Horizontal hit coordinate",
      y = "Vertical hit coordinate"
    ) 
  return(plot)
}

spray_field("Shohei Ohtani")
ggsave(here("visualizations/ohtani_speed_spray.png"), width = 8, height = 7)

spray_field("Brice Turang")
ggsave(here("visualizations/turang_speed_spray.png"), width = 8, height = 7)

spray_field("Aaron Judge")
ggsave(here("visualizations/judge_speed_spray.png"), width = 8, height = 7)

spray_field("Anthony Volpe")
ggsave(here("visualizations/volpe_speed_spray.png"), width = 8, height = 7)

spray_field("Bobby Witt Jr.")
ggsave(here("visualizations/witt_speed_spray.png"), width = 8, height = 7)

## Location of safe hit by pitch speed
safe_spray_field <- function(player){
  speed1 <- speed_class |>
    mutate(location_x = 2.5 * (hc_x - 125.42),
           location_y = 2.5 * (198.27 - hc_y)) |>
    filter(
      player_name == {{player}},
      events %in% c("single","double","triple","home_run"),
      !is.na(pitch_speed))
  
  plot <- 
    sportyR::geom_baseball(league = "MLB") +
    geom_point(data = speed1, aes(location_x, location_y, color = pitch_speed)) +
    scale_color_manual(
      values = c(off_speed = "red", fastball = "lightblue"),
      name = "Pitch Speed",
      labels = c("Off-speed","Fastball"),
      na.value = "grey50"
    ) +
    labs(
      title = glue::glue("Spray Chart of {player}, by Pitch Speed"),
      subtitle = "Location of base hits and home runs",
      x = "Horizontal hit coordinate",
      y = "Vertical hit coordinate"
    ) 
  return(plot)
}


safe_spray_field("Shohei Ohtani")
ggsave(here("visualizations/ohtani_safe_speed_spray.png"), width = 8, height = 7)

safe_spray_field("Brice Turang")
ggsave(here("visualizations/turang_safe_speed_spray.png"), width = 8, height = 7)

safe_spray_field("Aaron Judge")
ggsave(here("visualizations/judge_safe_speed_spray.png"), width = 8, height = 7)

safe_spray_field("Anthony Volpe")
ggsave(here("visualizations/volpe_safe_speed_spray.png"), width = 8, height = 7)

safe_spray_field("Bobby Witt Jr.")
ggsave(here("visualizations/witt_safe_speed_spray.png"), width = 8, height = 7)

### Analysis 2: Pitch Movement ----
speed_class |>
  select(pitch_speed, pfx_x, pfx_z) |>
  ggplot(aes(pfx_x, pfx_z, color = pitch_speed)) +
  geom_point(alpha = 0.2) +
  labs(
    title = "Pitch movement, fastball vs. off-speed",
    subtitle = "(Movement from the catcher's perspective)",
    x = "Horizontal movement (ft)",
    y = "Vertical movement (ft)"
  ) +
  scale_color_discrete(name = "Pitch Speed", labels = c("Fastball", "off-speed"))

ggsave(here("visualizations/movement_by_type.png"), width = 5, height = 4)

# faceted version of above plot
speed_class |>
  select(pitch_speed, pfx_x, pfx_z) |>
  ggplot(aes(pfx_x, pfx_z, color = pitch_speed)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~pitch_speed)
labs(
  title = "Pitch movement, fastball vs. off-speed",
  subtitle = "(Movement from the catcher's perspective)",
  x = "Horizontal movement (ft)",
  y = "Vertical movement (ft)"
) +
  scale_color_discrete(name = "Pitch Speed", labels = c("Fastball", "Off-speed"))

speed_class |>
  filter(player_name == "Shohei Ohtani") |>
  ggplot(aes(x = hc_x, y = -hc_y, fill = pitch_speed)) +
  geom_point(size = 3, shape = 21, alpha = 0.5) +
  scale_fill_manual(
    values = c(off_speed = "red", fastball = "lightblue"),
    na.value = "grey50"
  )

speed1 <- speed_class |>
  mutate(location_x = 2.5 * (hc_x - 125.42),
         location_y = 2.5 * (198.27 - hc_y)) |>
  filter(player_name == "Shohei Ohtani")
sportyR::geom_baseball(league = "MLB") +
  geom_point(data = speed1, aes(location_x, location_y, color = pitch_speed)) +
  scale_color_manual(values = c(off_speed = "red", fastball = "lightblue"))


### Analysis 3: Pitch Type Progression throughout a Plate Appearance ----
plate_appearance_data |>
  ungroup() |>
  summarize(
    n_01 = sum(is.na(pitch_2)),
    n_02 = sum(is.na(pitch_3)) - sum(is.na(pitch_2)),
    n_03 = sum(is.na(pitch_4)) - sum(is.na(pitch_3)),
    n_04 = sum(is.na(pitch_5)) - sum(is.na(pitch_4)),
    n_05 = sum(is.na(pitch_6)) - sum(is.na(pitch_5)),
    n_06 = sum(is.na(pitch_7)) - sum(is.na(pitch_6)),
    n_07 = sum(is.na(pitch_8)) - sum(is.na(pitch_7)),
    n_08 = sum(is.na(pitch_9)) - sum(is.na(pitch_8)),
    n_09 = sum(is.na(pitch_10)) - sum(is.na(pitch_9)),
    n_10 = sum(is.na(pitch_11)) - sum(is.na(pitch_10)),    
    n_11 = sum(is.na(pitch_12)) - sum(is.na(pitch_11)),
    n_12 = sum(is.na(pitch_13)) - sum(is.na(pitch_12)),
    n_13 = sum(is.na(pitch_14)) - sum(is.na(pitch_13)),
    n_14 = sum(!is.na(pitch_14)),
  ) |>
  pivot_longer(
    names_to = "pitch_number",
    cols = c(n_01,n_02,n_03,n_04,n_05,n_06,n_07,n_08,n_09,n_10,n_11,n_12,n_13,n_14),
    values_to = "n"
  ) |>
  ggplot(aes(pitch_number, n)) +
  geom_point() +
  labs(
    title = "Plate appearances by number of pitches",
    x = "Pitches in plate appearance",
    y = "Number of at-bats"
  )
ggsave(here("visualizations/pa_by_pitch_count.png"), width = 5, height = 4)

pitch_progression |>
  ungroup() |>
  filter(!is.na(pitch_1), !is.na(pitch_2), !is.na(pitch_3), !is.na(pitch_4), is.na(pitch_5)) |>
  select(pitch_1, pitch_2, pitch_3, pitch_4) |>
  group_by(pitch_1, pitch_2, pitch_3, pitch_4) |>
  count() |>
  arrange(-n)

speed_progression <- statcast_data |>
  mutate(
    pitch_speed = case_when(
      pitch_type %in% fastballs ~ "fastball",
      pitch_type %in% off_speeds ~ "off-speed",
      pitch_type == NA ~ NA
    )
  ) |>
  select(game_date, player_name, batter, pitcher, pitch_speed, at_bat_number, pitch_number) |>
  group_by(game_date, batter, at_bat_number) |> # identifies unique plate appearance
  arrange(pitch_number) |> # ensures pitches are in order in data frame
  pivot_wider(
    names_from = pitch_number,
    values_from = pitch_speed,
    names_prefix = "pitch_"
  )

## Effect of previous pitch's speed ----
speed_progression |>
  ungroup() |>
  filter(!is.na(pitch_1)) |>
  ggplot(aes(pitch_1)) +
  geom_bar() +
  labs(
    title = "First pitch speed",
    x = "Pitch 1 speed",
    y = "Number of pitches"
  )

ggsave(here("visualizations/pitch1_speed.png"), width = 5, height = 4)

speed_progression |>
  ungroup() |>
  filter(!is.na(pitch_2), !is.na(pitch_1)) |>
  ggplot(aes(pitch_1, fill = pitch_2)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Second pitch speed, by first pitch speed",
    x = "Pitch 1 speed",
    y = "Number of pitches"
  ) +
  scale_fill_discrete(name = "Pitch 2 speed", labels = c("Fastball", "off-speed"))

ggsave(here("visualizations/pitch2_by_pitch1_speed.png"), width = 5, height = 4)

speed_progression |>
  ungroup() |>
  filter(!is.na(pitch_3), !is.na(pitch_2), !is.na(pitch_1)) |>
  ggplot(aes(pitch_2, fill = pitch_3)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Third pitch speed, by second pitch speed",
    x = "Pitch 2 speed",
    y = "Number of pitches"
  ) +
  scale_fill_discrete(name = "Pitch 3 speed", labels = c("Fastball", "off-speed"))

ggsave(here("visualizations/pitch3_by_pitch2_speed.png"), width = 5, height = 4)

speed_progression |>
  ungroup() |>
  filter(!is.na(pitch_3), !is.na(pitch_2), !is.na(pitch_1)) |>
  mutate(pitch12 = case_when(
    (pitch_1 == "fastball" & pitch_2 == "fastball") ~ "fast-fast",
    (pitch_1 == "fastball" & pitch_2 == "off-speed") ~ "fast-off",
    (pitch_1 == "off-speed" & pitch_2 == "fastball") ~ "off-fast",
    (pitch_1 == "off-speed" & pitch_2 == "off-speed") ~ "off-off"
  )) |>
  ggplot(aes(pitch12, fill = pitch_3)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Third pitch speed, by first two pitches' speed",
    x = "Pitch 1 - Pitch 2",
    y = "Number of pitches"
  ) +
  scale_fill_discrete(name = "Pitch 3 speed", labels = c("Fastball", "off-speed"))

ggsave(here("visualizations/pitch3_by_pitch12_speed.png"), width = 5, height = 4)

## Effect of previous pitch's outcome ----
speed_progression |>
  ungroup() |>
  filter(is.na(pitch_5), !is.na(pitch_4), !is.na(pitch_3), !is.na(pitch_2), !is.na(pitch_1)) |>
  ggplot(aes(pitch_3, fill = pitch_4)) +
  geom_bar(position = "dodge")

speed_combos <- speed_progression |>
  ungroup() |>
  filter(!is.na(pitch_2), !is.na(pitch_1)) |>
  count(pitch_1, pitch_2) |>
  mutate(
    prop = n/sum(n)
  )
knitr::kable(speed_combos, col.names = c("Pitch 1", "Pitch 2", "Total", "Proportion"))



## Full join speed_progression and pitch_outcome_progression ----
type_event_prog <-
  inner_join(speed_progression, pitch_outcome_progression, 
             join_by(game_date, player_name, batter, pitcher, at_bat_number))

type_event_prog |>
  ungroup() |>
  filter(!is.na(pitch_2.x), !is.na(pitch_1.x)) |>
  filter(pitch_1.y %in% c("ball","called_strike","foul","swinging_strike")) |>
  count(pitch_1.y, pitch_2.x) |>
  reframe(
    prop = n/sum(n),
    .by = pitch_1.y
  )

second_pitch <- tibble(
  first_pitch = c("Ball", "Strike (looking)", "Strike (swinging)", "Foul"), 
  fastball_prop = c(0.622, 0.530, 0.543, 0.580),
  off_speed_prop = c(0.378, 0.470, 0.457, 0.420)
)

knitr::kable(second_pitch, "html",
             col.names = c("Pitch 1 Outcome", "Proportion of Pitch 2 Fastballs", "Proportion of Pitch 2 Off-speeds")) |>
  kable_styling() |>
  save_kable(here("visualizations/pitch12.html"))

## Checking bat speed ----
# curveballs and fastballs 
speed_class |>
  filter(bat_speed != 0, 
         str_detect(description, "bunt") == FALSE, 
         str_detect(description, "foul") == FALSE,
         pitch_type %in% c("FF", "CU"))  |>
  mutate(
    pitch_type = as.factor(pitch_type),
    pitch_type = pitch_type |> fct_infreq()) |>
  ggplot(aes(release_speed, bat_speed, color = pitch_type)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  scale_color_manual(
    name = "Pitch type",
    values = c(FF = "blue", CU = "red"),
    labels = c("Four-seam fastball", "Curveball")
  ) +
  labs(
    title = "Release and bat speed, fastball vs. curveball",
    x = "Release speed (mph)",
    y = "Bat speed (mph)"
  )

ggsave(here("visualizations/fb_curve_bat.png"), width = 6, height = 4)

# fastballs and sliders ----
speed_class |>
  filter(bat_speed != 0, 
         str_detect(description, "bunt") == FALSE, 
         str_detect(description, "foul") == FALSE,
         pitch_type %in% c("FF", "SL"))  |>
  mutate(
    pitch_type = as.factor(pitch_type),
    pitch_type = pitch_type |> fct_infreq()) |>
  ggplot(aes(release_speed, bat_speed, color = pitch_type)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  scale_color_manual(
    name = "Pitch type",
    values = c(FF = "blue", SL = "red"),
    labels = c("Four-seam fastball", "Slider (off-speed)")
  ) +
  labs(
    title = "Release and bat speed, fastball vs. slider",
    x = "Release speed (mph)",
    y = "Bat speed (mph)"
  )

ggsave(here("visualizations/fb_slide_bat.png"), width = 6, height = 4)

# Missingness of bat speed and launch speed ----
miss_data <- statcast_data |> 
  mutate(
    bat = ifelse(is.na(bat_speed), FALSE, TRUE),
    launch = ifelse(is.na(launch_speed), FALSE, TRUE),
    .before = game_date
  ) |> 
  select(bat, launch, bat_speed, launch_speed) |> 
  count(bat, launch) |>
  mutate(
    prop = round(n/672836, digits = 4),
    bat = ifelse(bat, "Listed", "Missing"),
    launch = ifelse(launch, "Listed", "Missing")
  )

bat <- miss_data |> pull(bat)
launch <- miss_data |> pull(launch)
n <- miss_data |> pull(n)
prop <- miss_data |> pull(prop)
miss_tab <- cbind(bat, launch, n, prop)
knitr::kable(miss_tab, col.names = c("Bat speed", "Launch speed", "Number", "Proportion")) |>
  kable_styling() |>
  save_kable(here("data/speed_na.html"))

