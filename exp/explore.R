# library(dplyr)
# library(readxl)
# Exploration -------------------------------------------------------------
# source("R/utils_processing.R")

## data path ---------------------------------------------------------------

path_data_raw <- "./data/raw/"



## read in -----------------------------------------------------------------


path_raw_files <- list.files(path_data_raw, full.names = TRUE)


li_ghg_sheets <- lapply(
  path_raw_files,
  function(prf){
    readxl::excel_sheets(prf)
  }
)


li_ghg <- lapply(
  path_raw_files,
  function(prf){
    readxl::read_xlsx(prf)
  }
)


# table building test -----------------------------------------------------


trends_annual <- read_emission_trends(path = "./data/raw/DEU_2022_2020_14012022_064619_started.xlsx",
                               table = "Table10s1")

trends$sector


trends_total <- read_emission_trends(path = "./data/raw/DEU_2022_2020_14012022_064619_started.xlsx",
                               table = "Table10s1",
                               total = TRUE)


trends_total_co2 <- read_emission_trends(path = "./data/raw/DEU_2022_2020_14012022_064619_started.xlsx",
                                     table = "Table10s2",
                                     total = TRUE)


trends_total_ch4 <- read_emission_trends(path = "./data/raw/DEU_2022_2020_14012022_064619_started.xlsx",
                                         table = "Table10s3",
                                         total = TRUE)



# find CRF-Catergoy Supra
# find CRF-Category Sub (A, B)
# ...
# iterative function? pre-defined?

# assume max depth of 5




# Testing -----------------------------------------------------------------

sheetnames <- readxl::excel_sheets("./data/raw/DEU_2022_2020_14012022_064619_started.xlsx")
sheetnames <- sheetnames[-1]
names(sheetnames) <- sheetnames


meta_headers <- purrr::map_dfr(sheetnames, function(x){
  readxl::read_excel(
    path = "./data/raw/DEU_2022_2020_14012022_064619_started.xlsx",
    sheet = x,
    col_names = "content",
    range = "A1:A4")
}, .id = "sheet")
