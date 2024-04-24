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



# Clean 2020 data
dplyr::full_join(atusrost2020, atusresp2020,
    by = c("TUCASEID", "TULINENO")) |>
dplyr::filter(TULINENO == 1) |>
dplyr::right_join(atusact2020, by = "TUCASEID") |>
dplyr::rename(TUFINLWGT = TU20FWGT) |>
dplyr::mutate(YEAR = 2020) |>
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
atus2020

for (iHour in 4:27) {
    TUHour <- as.integer((iHour >= atus2020$TUSTARTHOUR & iHour <= atus2020$TUSTOPHOUR) & atus2020$ACTIVITY_GRP == 1)
    atus2020[paste("TU", iHour, sep = "")] <- TUHour

    TUCaseHour <- data.frame(dplyr::bind_cols(
        atus2020$TUCASEID, TUHour))
    colnames(TUCaseHour) <- c("TUCASEID", "TUHOUR")
    TUCaseHour |>
        dplyr::group_by(TUCASEID) |>
        mutate(TUHOURMAX = max(TUHOUR)) |>
        dplyr::ungroup() ->
    TUCaseHour
    atus2020[paste("TUMAX", iHour, sep = "")] <-
        TUCaseHour$TUHOURMAX
}

atus2020 |>
   dplyr::filter(TUACTIVITY_N == 1) ->
atus2020





## Clean 2021 data
dplyr::full_join(atusrost2021, atusresp2021,
    by = c("TUCASEID", "TULINENO")) |>
dplyr::filter(TULINENO == 1) |>
dplyr::right_join(atusact2021, by = "TUCASEID") |>
dplyr::mutate(YEAR = 2021) |>
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
atus2021

for (iHour in 4:27) {
    TUHour <- as.integer((iHour >= atus2021$TUSTARTHOUR & iHour <= atus2021$TUSTOPHOUR) & atus2021$ACTIVITY_GRP == 1)
    atus2021[paste("TU", iHour, sep = "")] <- TUHour

    TUCaseHour <- data.frame(dplyr::bind_cols(
        atus2021$TUCASEID, TUHour))
    colnames(TUCaseHour) <- c("TUCASEID", "TUHOUR")
    TUCaseHour |>
        dplyr::group_by(TUCASEID) |>
        mutate(TUHOURMAX = max(TUHOUR)) |>
        dplyr::ungroup() ->
    TUCaseHour
    atus2021[paste("TUMAX", iHour, sep = "")] <-
        TUCaseHour$TUHOURMAX
}

atus2021 |>
   dplyr::filter(TUACTIVITY_N == 1) ->
atus2021





## Clean 2022 data
dplyr::full_join(atusrost2022, atusresp2022,
    by = c("TUCASEID", "TULINENO")) |>
dplyr::filter(TULINENO == 1) |>
dplyr::right_join(atusact2022, by = "TUCASEID") |>
dplyr::mutate(YEAR = 2022) |>
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
atus2022

for (iHour in 4:27) {
    TUHour <- as.integer((iHour >= atus2022$TUSTARTHOUR & iHour <= atus2022$TUSTOPHOUR) & atus2022$ACTIVITY_GRP == 1)
    atus2022[paste("TU", iHour, sep = "")] <- TUHour

    TUCaseHour <- data.frame(dplyr::bind_cols(
        atus2022$TUCASEID, TUHour))
    colnames(TUCaseHour) <- c("TUCASEID", "TUHOUR")
    TUCaseHour |>
        dplyr::group_by(TUCASEID) |>
        mutate(TUHOURMAX = max(TUHOUR)) |>
        dplyr::ungroup() ->
    TUCaseHour
    atus2022[paste("TUMAX", iHour, sep = "")] <-
        TUCaseHour$TUHOURMAX
}

atus2022 |>
   dplyr::filter(TUACTIVITY_N == 1) ->
atus2022




## Put years together and collapse
atus <- bind_rows(
    atus2019, atus2020, atus2021, atus2022
)

atus |>
    dplyr::filter(TUDIARYDAY %in% 2:6) |>
    dplyr::filter(TRHOLIDAY == 0) |>
    dplyr::filter(TRHHCHILD == 1) |>
    dplyr::filter(TUACTIVITY_N == 1) ->
atus_filter

## Run unit test
source("tests/unit_test.r")
#testthat::test_dir("tests")

atus_filter |>
    dplyr::mutate(EMPLOYED = case_when(
        TRDPFTPT %in% c(1,2) ~ 1,
        TRUE ~ 0)) ->
atus_filter

atus_filter |>
    dplyr::mutate(MALEYEAR = paste(MALE, YEAR, sep = "")) |>
    dplyr::select(MALEYEAR, TUFINLWGT, TUMAX4, TUMAX5, TUMAX6, TUMAX7, TUMAX8, TUMAX9, TUMAX10, TUMAX11, TUMAX12, TUMAX13, TUMAX14, TUMAX15, TUMAX16, TUMAX17, TUMAX18, TUMAX19, TUMAX20, TUMAX21, TUMAX22, TUMAX23, TUMAX24, TUMAX25, TUMAX26, TUMAX27) |>
    dplyr::group_by(MALEYEAR) |>
    dplyr::summarize(
        TUMEAN4 = weighted.mean(TUMAX4, TUFINLWGT), TUMEAN5 = weighted.mean(TUMAX5, TUFINLWGT),
        TUMEAN6 = weighted.mean(TUMAX6, TUFINLWGT), TUMEAN7 = weighted.mean(TUMAX7, TUFINLWGT),
        TUMEAN8 = weighted.mean(TUMAX8, TUFINLWGT), TUMEAN9 = weighted.mean(TUMAX9, TUFINLWGT),
        TUMEAN10 = weighted.mean(TUMAX10, TUFINLWGT), TUMEAN11 = weighted.mean(TUMAX11, TUFINLWGT),
        TUMEAN12 = weighted.mean(TUMAX12, TUFINLWGT), TUMEAN13 = weighted.mean(TUMAX13, TUFINLWGT),
        TUMEAN14 = weighted.mean(TUMAX14, TUFINLWGT), TUMEAN15 = weighted.mean(TUMAX15, TUFINLWGT),
        TUMEAN16 = weighted.mean(TUMAX16, TUFINLWGT), TUMEAN17 = weighted.mean(TUMAX17, TUFINLWGT),
        TUMEAN18 = weighted.mean(TUMAX18, TUFINLWGT), TUMEAN19 = weighted.mean(TUMAX19, TUFINLWGT),
        TUMEAN20 = weighted.mean(TUMAX20, TUFINLWGT), TUMEAN21 = weighted.mean(TUMAX21, TUFINLWGT),
        TUMEAN22 = weighted.mean(TUMAX22, TUFINLWGT), TUMEAN23 = weighted.mean(TUMAX23, TUFINLWGT),
        TUMEAN24 = weighted.mean(TUMAX24, TUFINLWGT), TUMEAN25 = weighted.mean(TUMAX25, TUFINLWGT),
        TUMEAN26 = weighted.mean(TUMAX26, TUFINLWGT), TUMEAN27 = weighted.mean(TUMAX27, TUFINLWGT)
    ) ->
atus_maleyear_means





