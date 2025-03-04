## STAT 301-1 Final Project

### General Project Information

This project began as a final project for STAT 301-1 at Northwestern. We were asked to select external data, tidy it, and conduct an elementary data analysis on it. The `Final_Report.qmd` and `Executive_Summary.qmd` were submitted, along with the R scripts. I have continued to work on the project beyond my class submission

### Basic Repository Setup

-   R scripts to store all code.

    | R script name | Purpose |
    |------------------------------------|------------------------------------|
    | `1_data_cleanup.R` | Tidy the data from `raw_statcast_data`, create modified datasets for analysis. |
    | `2_platoon_analysis.R` | Code for EDA on platoon advantage, create and save visualizations. |
    | `3_pitch_type_analysis.R` | Code for EDA on pitch characteristics, create and save visualizations. |

    -   Note that `2_platoon_analysis.R` and `3_pitch_type_analysis.R` are numbered arbitrarily. They need not be run in that order. Each loads `.rds` files created in `1_data_cleanup.R`.

-   `Final_Report.qmd` and `Executive_Summary.qmd`. Names are self-explanatory.

-   Folders for further organization.

    | Folder name      | Purpose                                                  |
    |--------------------|----------------------------------------------------|
    | `data`           | Store all data, including new written `.csv` files.      |
    | `raw`            | Subfolder of `data`, holds the original `.csv` datasets. |
    | `visualizations` | Store all EDA visualizations, in alphabetical order.     |

    -   Note: Due to size concerns and my desire to not break my laptop, I did not commit the original, tidy, and modified datasets. The code used to create them, however, can be found in the R scripts. I have linked to the original datasets in the **References** section of the final report.

## Other information

-   The CSADS data did not include a codebook when I downloaded it. However, because it is data from Baseball Savant, I was able to find a webpage that lists the Statcast variables and what they represent. [Here](https://baseballsavant.mlb.com/csv-docs) is the link to that webpage.
-   Due to the size of the datasets, I am not including them in my repository. I have included links to the data sources in `Final_Project.qmd` to compensate for this.
