library(tidyverse)
library(rio)
library(testthat)
library(ggpubr)

setwd("C:/Users/rache/OneDrive/Desktop/programming/final_project/final_project/")
source("./Renv/activate.R")



# Read in files
filetype <- c('act', 'resp', 'rost')
year <- c('2019', '2020', '2021', '2022')
for (i in year) {
    for (j in filetype) {
        setwd("C:/Users/rache/OneDrive/Desktop/programming/final_project/final_project/downloads/")
        name <- paste('atus', j, '_', i, '.dat', sep = "")
        df <- read.csv(name)
        filename <- paste('atus', j, '_', i, '.RData', sep = "")
        setwd("C:/Users/rache/OneDrive/Desktop/programming/final_project/final_project/data/")
        save(df, file = filename)
    
    } 
    }

    

# Assign dataframe names
setwd("C:/Users/rache/OneDrive/Desktop/programming/final_project/final_project/data/")

atusact2019 <- rio::import("atusact_2019.RData")
atusact2020 <- rio::import("atusact_2020.RData")
atusact2021 <- rio::import("atusact_2021.RData")
atusact2022 <- rio::import("atusact_2022.RData")
atusresp2019 <- rio::import("atusresp_2019.RData")
atusresp2020 <- rio::import("atusresp_2020.RData")
atusresp2021 <- rio::import("atusresp_2021.RData")
atusresp2022 <- rio::import("atusresp_2022.RData")
atusrost2019 <- rio::import("atusrost_2019.RData")
atusrost2020 <- rio::import("atusrost_2020.RData")
atusrost2021 <- rio::import("atusrost_2021.RData")
atusrost2022 <- rio::import("atusrost_2022.RData")

setwd("C:/Users/rache/OneDrive/Desktop/programming/final_project/final_project/")


# Clean 2019 data
dplyr::full_join(atusrost2019, atusresp2019,
    by = c("TUCASEID", "TULINENO")) |>
dplyr::filter(TULINENO == 1) |>
dplyr::right_join(atusact2019, by = "TUCASEID") |>
dplyr::mutate(YEAR = 2019) |>
dplyr::mutate(MALE = case_when(
    TESEX == 2 ~ 0,
    TESEX == 1 ~ 1)) |>
dplyr::mutate(ACTIVITY_GRP = case_when(
    (TRCODE >= 30101 & TRCODE <= 30399) |
        TRCODE %in% c(180301, 180302, 180303) ~ 1,
    TRCODE >= 50101 & TRCODE <= 59999 ~ 3,
    TRCODE >= 20101 & TRCODE <= 29999 ~ 4,
    TRUE ~ 0)) |>
dplyr::mutate(TUSTARTHOUR =
    as.integer(substr(TUSTARTTIM, 1, 2))) |>
dplyr::mutate(TUSTARTHOUR = case_when(
    TUSTARTHOUR == 0 ~ 24,
    TUSTARTHOUR == 1 ~ 25,
    TUSTARTHOUR == 2 ~ 26,
    TUSTARTHOUR == 3 ~ 27,
    TRUE ~ TUSTARTHOUR
)) |>
dplyr::mutate(TUSTOPHOUR =
    as.integer(substr(TUSTOPTIME, 1, 2))) |>
dplyr::mutate(TUSTOPHOUR = case_when(
    TUSTOPHOUR == 0 ~ 24,
    TUSTOPHOUR == 1 ~ 25,
    TUSTOPHOUR == 2 ~ 26,
    TUSTOPHOUR == 3 ~ 27,
    TRUE ~ TUSTOPHOUR
)) ->
atus2019

for (iHour in 4:27) {
    TUHour <- as.integer((iHour >= atus2019$TUSTARTHOUR & iHour <= atus2019$TUSTOPHOUR) & atus2019$ACTIVITY_GRP == 1)
    atus2019[paste("TU", iHour, sep = "")] <- TUHour

    TUCaseHour <- data.frame(dplyr::bind_cols(
        atus2019$TUCASEID, TUHour))
    colnames(TUCaseHour) <- c("TUCASEID", "TUHOUR")
    TUCaseHour |>
        dplyr::group_by(TUCASEID) |>
        mutate(TUHOURMAX = max(TUHOUR)) |>
        dplyr::ungroup() ->
    TUCaseHour
    atus2019[paste("TUMAX", iHour, sep = "")] <-
        TUCaseHour$TUHOURMAX
}

atus2019 |>
   dplyr::filter(TUACTIVITY_N == 1) ->
atus2019

