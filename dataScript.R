#####################################################################
## Script to process CSVs into dashboard-friendly tabular dataframe##
## Called by App.R                                                 ##
## Creates ./data/immunodata.RData to be used by App.R             ##
#####################################################################

#Store this script in the same directory as a folder called "data" containing .xlsx data files downloaded from  https://web.archive.org/web/20250212143312/https://www.michigan.gov/mdhhs/adult-child-serv/childrenfamilies/immunizations/data-statistics/schoolimmsdata


# Set-up ------------------------------------------------------------------
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(readxl)){install.packages("readxl")}
if(!require(naniar)){install.packages("naniar")}


importPath <- file.path(getwd(), "data")



# Data import & QC --------------------------------------------------------


#Datafiles taken from https://web.archive.org/web/20250212143312/https://www.michigan.gov/mdhhs/adult-child-serv/childrenfamilies/immunizations/data-statistics/schoolimmsdata
dfK <- read_xlsx(file.path(importPath, "immunizationKindergarten2024.xlsx"), skip = 7, col_names = T) %>%
  rename(nCOMP = "COMP", pctCOMP = "%COMP", nWaiver = 'n...10' , pctWaiver = '%...11', nMedical = 'n...12', pctMedical = '%...13', nReligious = 'n...14', pctReligious = '%...15', nPhilosophical = 'n...16', pctPhilosophical = '%...17')

df7 <- read_xlsx(file.path(importPath, "immunizationSeventh2024.xlsx"), skip = 7, col_names = T) %>%
  rename(nCOMP = "COMP", pctCOMP = "%COMP", nWaiver = 'n...10' , pctWaiver = '%...11', nMedical = 'n...12', pctMedical = '%...13', nReligious = 'n...14', pctReligious = '%...15', nPhilosophical = 'n...16', pctPhilosophical = '%...17')

#QC for waiver counts equal to sum of all reasons
which(dfK$nMedical + dfK$nReligious + dfK$nPhilosophical != dfK$nWaiver) #all K schools correctly count waivers
which(df7$nMedical+ df7$nReligious+ df7$nPhilosophical != (df7$nWaiver))#all 7th schools correctly count waivers

which(dfK$N != (dfK$INCOM + dfK$nWaiver + dfK$nCOMP + dfK$PROV))
dfK[c(401,1145,1340),] #3 K schools fail QC  for total students, have students unaccounted for

which(df7$N != (df7$INCOM + df7$nWaiver + df7$nCOMP + df7$PROV))
df7[111,] #1 7th school fails QC for total studentw, has students unaccounted for


dfK["Group"] <- "K"
df7["Group"] <- "7"

#overall dataframe, with each school as an observation
df <- bind_rows(dfK, df7)

#vis_miss(df) #no missing values

#Create a district df - aggregate by district and age-group, then pivot on groups

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
    pctWaiver = 100 * nWaiver / N,
    pctMedical = 100 * nMedical / N,
    pctReligious = 100 * nReligious / N,
    pctPhilosophical = 100 * nPhilosophical / N
  ) %>%
  pivot_wider(
    names_from = Group,
    values_from = c(N, nCOMP, PROV, INCOM, nWaiver, nMedical, nReligious, nPhilosophical,
                    pctWaiver, pctMedical, pctReligious, pctPhilosophical),
    names_glue = "{Group}_{.value}",
    values_fill = 0)




#Create totals for each District (summing K+7), remove pct variables to be calculated later
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

#Create total percent variables
dfDistrict <- dfDistrict %>%
  mutate(tot_pctWaiver = tot_nWaiver / tot_N  * 100,
         tot_pctMedical = tot_nMedical / tot_N * 100,
         tot_pctReligious = tot_nReligious / tot_N  * 100,
         tot_pctPhilosophical = tot_nPhilosophical / tot_N * 100,
         tot_pctCOMP = tot_nCOMP / tot_N * 100)

#rearrange so its pretty, dfDistrict is tabular with each district as an observation
dfDistrict <- dfDistrict %>%
  select(DISTRICT, starts_with("tot"), starts_with("K"), starts_with("7"))




#add back county labels
dfDistrict <- df %>%
  select(DISTRICT, COUNTY) %>%
  distinct(DISTRICT, COUNTY) %>%
  right_join(dfDistrict, by = "DISTRICT")#depends on all schools in a district being within a county








#repeat above steps for counties
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
    pctWaiver = 100 * nWaiver / N,
    pctMedical = 100 * nMedical / N,
    pctReligious = 100 * nReligious / N,
    pctPhilosophical = 100 * nPhilosophical / N
  ) %>%
  pivot_wider(
    names_from = Group,
    values_from = c(N, nCOMP, PROV, INCOM, nWaiver, nMedical, nReligious, nPhilosophical,
                    pctWaiver, pctMedical, pctReligious, pctPhilosophical),
    names_glue = "{Group}_{.value}",
    values_fill = 0)

for(var in vars){
  k_col <- paste0("K_", var)
  seven_col <- paste0("7_", var)
  total <- paste0("tot_", var)
  if (all(c(k_col, seven_col) %in% names(dfCounty))) {
    dfCounty[[total]] <- dfCounty[[k_col]] + dfCounty[[seven_col]]
  }
}

dfCounty <- dfCounty %>%
  mutate(tot_pctWaiver = tot_nWaiver / tot_N * 100,
         tot_pctMedical = tot_nMedical / tot_N * 100,
         tot_pctReligious = tot_nReligious / tot_N  * 100,
         tot_pctPhilosophical = tot_nPhilosophical / tot_N * 100,
         tot_pctCOMP = tot_nCOMP / tot_N * 100)

#dfCounty tabular data frame with each county as an observation
dfCounty <- dfCounty %>%
  select(COUNTY, starts_with("tot"), starts_with("K"), starts_with("7"))

#clean up some useless workspace stuff
rm(df7, dfK, k_col, seven_col, total, var, vars)

#save clean workspace image to .RData to be accessed by shiny dashboard
save(df, dfCounty, dfDistrict, file = file.path(getwd(), "data", "immunodata.RData"))



###TODO: Should also create pct variables for PROV and INCOM, right now these are excluded
