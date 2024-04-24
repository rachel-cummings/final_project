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

