## Important Disclaimer

The initial dataset and created datasets are incredibly large (see below). As such, I will not be uploading them to the GitHub repository. Instead, I've included details instructions for downloading the data should one be interested in viewing the full project. The **Folder Contents** section describes my file system, and all of the `.rda` files created in the R scripts. The `visualizations` folder and the `.html` files in this folder contain all of the relevant images needed to render the `Executive_Summary.qmd` and `Final_Report.qmd` files.

## Data Overview

### Statcast data

The primary Statcast data was sourced from Baseball Savant via the [2025 Connecticut Sports Analytics Symposium](https://statds.org/events/csas2025/challenge.html) website. The data challenge organizers uploaded all of the datasets to this [shared OneDrive folder](https://yaleedu-my.sharepoint.com/personal/brian_macdonald_yale_edu/_layouts/15/onedrive.aspx?id=%2Fpersonal%2Fbrian%5Fmacdonald%5Fyale%5Fedu%2FDocuments%2Fservice%2FCSAS%2F2025%2D04%2D11%20%2D%20CSAS%20at%20Yale%2Fdata%2Echallenge%2Fdata&ga=1), owned by Brian Macdonald, Co-Director of Undergraduate Studies for the Yale Department of Statistics and Data Science.

The dataset contains pitch-level data for 701,557 pitches, thrown in Major League Baseball games between April 2, 2024 and October 30, 2024.

### Switch hitter data

The supplementary dataset was obtained from FANGRAPHS, a baseball statistics website and blog. I ran a query for Major League Baseball players who were active between April 2, 2024 and October 30, 2024 and who were listed as switch hitters. Switch hitters are identified with a "B" or "S" as their batting stance value.

The dataset contains the names and 2024 batting statistics of 77 switch hitters. I used the dataset to identify switch hitters in the larger `statcast_data` dataset and create the `switch_hitter_data` dataset.

## Folder Contents

### `raw` sub-folder

The `raw` subfolder contains: - The unedited `.csv` files I downloaded from the 2025 CSAS website. - An `.rda` file containing the raw dataset `raw_statcast_data`.

### `.rda` files

Below is a description of the supplementary datasets created in `1_data_cleanup.R` and saved in the `.rda` files.

| `.rda` file name | Description |
|-------------------------|----------------------------------------------|
| `statcast_data` | Tidy version of original `raw_statcast_data` , reformatted `player_name`, logical variables and irrelevant observations removed. |
| `statcast_platoon_split` | Version of `statcast_data`. Includes new factor variable `hand_match` with four levels (`BRPR`, `BRPL`, `BLPR`, `BLPL`), indicates the batter/pitcher handedness match-up. |
| `speed_class` | Version of `statcast_data`. Includes new factor variable `pitch_speed` with two levels (`fastball` and `off_speed`). |
| `plate_appearance_data` | Used `pivot_wider()` to create dataset in which each observation represents a single plate appearance, cells within a row contain the `description` of each pitch in that appearance. |
| `switch_hitter_data` | Version of `plate_appearance_data`. Only includes plate appearances by switch hitters, as determined by the FANGRAPHS data. |
| `speed_progression` | Version of `plate_appearance_data`. Cell values pulled from `pitch_speed`, factor created in `speed_class.rda`. |
| `pitch_outcome_progression` | Version of `plate_appearance_data`. Cell values pulled from `description`, contains the narrative of each plate appearance. |
| `pitch_progression` | Version of `plate_appearance_data`. Cell values pulled from `pitch_type`, used to analyze effect of pitch speed on the speed of the next pitches. |
| `outcomes` | Derived from `statcast_platoon_split`. Each row represents a batter, contains their rates of success based on the value of `hand_match`. |

### `.html` files

The `.html` files in this data folder were created to save tables pertaining to the data that would be used in the final report. The were created with `knitr::kable()`.

| `.html` file name | Description |
|-------------------------|----------------------------------------------|
| `data_types` | Contains table describing variable missingness by type. |
| `speed_na` | Contains table enumerating the joint missingness of `bat_speed` and `launch_speed`. |
