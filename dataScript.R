#####################################################################
## Data processing script for School Immunization Dashboard        ##
#####################################################################
# Process data for Kindergarten and 7th grade immunization waivers from MDHHS
# To run, ensure that there is a folder called "data", which contains the Kindergarten Immunization Status by Building, and Seventh Grade Immunization Status by Building files from
# https://www.michigan.gov/mdhhs/adult-child-serv/childrenfamilies/immunizations/data-statistics/schoolimmsdata
# These files have been included in the dashboard .zip file
#
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(readxl)) install.packages("readxl")

importPath <- file.path(getwd(), "data")
if(!dir.exists(importPath)){
  dir.create(importPath)
  stop("Please check to ensure that your working directory is set to the file containing app.R and that data for kindergarten and 7th grade immunization is downloaded to a folder called data.
  Data can be downloaded from https://www.michigan.gov/mdhhs/adult-child-serv/childrenfamilies/immunizations/data-statistics/schoolimmsdata")
  }

dfK <- read_xlsx(file.path(importPath, "immunizationKindergarten2024.xlsx"), skip = 7, col_names = TRUE) %>%
  rename(
    nCOMP           = "COMP",
    pctCOMP         = "%COMP",
    nWaiver         = "n...10",
    pctWaiver       = "%...11",
    nMedical        = "n...12",
    pctMedical      = "%...13",
    nReligious      = "n...14",
    pctReligious    = "%...15",
    nPhilosophical  = "n...16",
    pctPhilosophical= "%...17"
  )

df7 <- read_xlsx(file.path(importPath, "immunizationSeventh2024.xlsx"), skip = 7, col_names = TRUE) %>%
  rename(
    nCOMP           = "COMP",
    pctCOMP         = "%COMP",
    nWaiver         = "n...10",
    pctWaiver       = "%...11",
    nMedical        = "n...12",
    pctMedical      = "%...13",
    nReligious      = "n...14",
    pctReligious    = "%...15",
    nPhilosophical  = "n...16",
    pctPhilosophical= "%...17"
  )

dfK$Group <- "K"
df7$Group <- "7"

df <- bind_rows(dfK, df7)

#Rename Grand Traverse
df$COUNTY[which(df$COUNTY == "Gd. Traverse")] <- "Grand Traverse"


df <- df %>% 
  mutate(
    pctPROV  = 100 * PROV / N,
    pctINCOM = 100 * INCOM / N
  )

dfDistrict <- df %>%
  group_by(DISTRICT, Group) %>%
  summarize(
    N = sum(N, na.rm = TRUE),
    nCOMP = sum(nCOMP, na.rm = TRUE),
    PROV = sum(PROV, na.rm = TRUE),
    INCOM = sum(INCOM, na.rm = TRUE),
    nWaiver = sum(nWaiver, na.rm = TRUE),
    nMedical = sum(nMedical, na.rm = TRUE),
    nReligious = sum(nReligious, na.rm = TRUE),
    nPhilosophical = sum(nPhilosophical, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pctWaiver         = 100 * nWaiver / N,
    pctMedical        = 100 * nMedical / N,
    pctReligious      = 100 * nReligious / N,
    pctPhilosophical  = 100 * nPhilosophical / N,
    pctCOMP           = 100 * nCOMP / N,
    pctPROV           = 100 * PROV / N,
    pctINCOM          = 100 * INCOM / N
  ) %>%
  pivot_wider(
    names_from   = Group,
    values_from  = c(N, nCOMP, PROV, INCOM, nWaiver, nMedical, nReligious, nPhilosophical,
                     pctWaiver, pctMedical, pctReligious, pctPhilosophical,
                     pctCOMP, pctPROV, pctINCOM),
    names_glue   = "{Group}_{.value}",
    values_fill  = 0
  )

district_numeric_cols <- names(dfDistrict)[str_detect(names(dfDistrict), "_n|_N|_PROV|_INCOM|_COMP|_Medical|_Religious|_Philosophical|_pct")]
dfDistrict[district_numeric_cols] <- lapply(dfDistrict[district_numeric_cols], function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  x_num[is.na(x_num)] <- 0
  return(x_num)
})

vars <- gsub("K_", "", grep("^K_", names(dfDistrict), value = TRUE))
vars <- vars[!str_detect(vars, "pct")]

for(var in vars){
  k_col <- paste0("K_", var)
  seven_col <- paste0("7_", var)
  total <- paste0("tot_", var)
  if (all(c(k_col, seven_col) %in% names(dfDistrict))) {
    dfDistrict[[total]] <- dfDistrict[[k_col]] + dfDistrict[[seven_col]]
  }
}

dfDistrict <- dfDistrict %>%
  mutate(
    tot_pctWaiver         = tot_nWaiver / tot_N * 100,
    tot_pctMedical        = tot_nMedical / tot_N * 100,
    tot_pctReligious      = tot_nReligious / tot_N * 100,
    tot_pctPhilosophical  = tot_nPhilosophical / tot_N * 100,
    tot_pctCOMP           = tot_nCOMP / tot_N * 100,
    tot_pctPROV           = tot_PROV / tot_N * 100,
    tot_pctINCOM          = tot_INCOM / tot_N * 100
  )

dfDistrict <- dfDistrict %>%
  select(DISTRICT, starts_with("tot"), starts_with("K"), starts_with("7"))

dfDistrict <- df %>%
  select(DISTRICT, COUNTY) %>%
  distinct(DISTRICT, COUNTY) %>%
  right_join(dfDistrict, by = "DISTRICT")

dfCounty <- df %>%
  group_by(COUNTY, Group) %>%
  summarize(
    N = sum(N, na.rm = TRUE),
    nCOMP = sum(nCOMP, na.rm = TRUE),
    PROV = sum(PROV, na.rm = TRUE),
    INCOM = sum(INCOM, na.rm = TRUE),
    nWaiver = sum(nWaiver, na.rm = TRUE),
    nMedical = sum(nMedical, na.rm = TRUE),
    nReligious = sum(nReligious, na.rm = TRUE),
    nPhilosophical = sum(nPhilosophical, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pctWaiver         = 100 * nWaiver / N,
    pctMedical        = 100 * nMedical / N,
    pctReligious      = 100 * nReligious / N,
    pctPhilosophical  = 100 * nPhilosophical / N,
    pctCOMP           = 100 * nCOMP / N,
    pctPROV           = 100 * PROV / N,
    pctINCOM          = 100 * INCOM / N
  ) %>%
  pivot_wider(
    names_from   = Group,
    values_from  = c(N, nCOMP, PROV, INCOM, nWaiver, nMedical, nReligious, nPhilosophical,
                     pctWaiver, pctMedical, pctReligious, pctPhilosophical,
                     pctCOMP, pctPROV, pctINCOM),
    names_glue   = "{Group}_{.value}",
    values_fill  = 0
  )

county_numeric_cols <- names(dfCounty)[str_detect(names(dfCounty), "_n|_N|_PROV|_INCOM|_COMP|_Medical|_Religious|_Philosophical|_pct")]
dfCounty[county_numeric_cols] <- lapply(dfCounty[county_numeric_cols], function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  x_num[is.na(x_num)] <- 0
  return(x_num)
})

for(var in vars){
  k_col <- paste0("K_", var)
  seven_col <- paste0("7_", var)
  total <- paste0("tot_", var)
  if (all(c(k_col, seven_col) %in% names(dfCounty))) {
    dfCounty[[total]] <- dfCounty[[k_col]] + dfCounty[[seven_col]]
  }
}

dfCounty <- dfCounty %>%
  mutate(
    tot_pctWaiver         = tot_nWaiver / tot_N * 100,
    tot_pctMedical        = tot_nMedical / tot_N * 100,
    tot_pctReligious      = tot_nReligious / tot_N * 100,
    tot_pctPhilosophical  = tot_nPhilosophical / tot_N * 100,
    tot_pctCOMP           = tot_nCOMP / tot_N * 100,
    tot_pctPROV           = tot_PROV / tot_N * 100,
    tot_pctINCOM          = tot_INCOM / tot_N * 100
  ) %>%
  select(COUNTY, starts_with("tot"), starts_with("K"), starts_with("7"))

rm(df7, dfK, k_col, seven_col, total, var, vars)

save(df, dfCounty, dfDistrict, file = file.path(getwd(), "data", "immunodata.RData"))