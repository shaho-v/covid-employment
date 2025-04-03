# WOMEN'S EMPLOYMENT DURING COVID #
## Notes ##
# Employment is in thousands

#### Setup ####

library(tidyverse)
library(readr)
library(janitor)
library(rsdmx)
library(countrycode)
library(plm)
library(modelsummary)
library(kableExtra)

df_ilo <- read_csv("https://rplumber.ilo.org/data/indicator/?id=EMP_TEMP_SEX_AGE_OCU_NB_Q&ref_area=ALB+AUT+BGD+BEL+BIH+BRA+BGR+HRV+CYP+CZE+DNK+EGY+EST+FIN+FRA+DEU+GRC+HUN+ISL+IND+IDN+IRN+IRL+ITA+JPN+JOR+KOR+KOS+LVA+LTU+LUX+MLT+MDA+MNG+MNE+NLD+NOR+PAK+POL+PRT+ROU+RUS+SRB+SGP+SVK+SVN+ZAF+ESP+SWE+CHE+GBR+VNM&sex=SEX_T+SEX_M+SEX_F&classif1=AGE_AGGREGATE_TOTAL+AGE_AGGREGATE_Y15-24+AGE_AGGREGATE_Y25-54+AGE_AGGREGATE_Y55-64+AGE_AGGREGATE_YGE65&classif2=OCU_SKILL_TOTAL+OCU_SKILL_L3-4+OCU_SKILL_L2+OCU_SKILL_L1&timefrom=2015&type=label&format=.csv") %>%
  select(c(1,4),c(5:8))

df_sector <- read_csv("https://rplumber.ilo.org/data/indicator/?id=EMP_TEMP_SEX_AGE_ECO_NB_Q&ref_area=AUT+BEL+BRA+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IND+IDN+ITA+JPN+KOR+LVA+LTU+LUX+NLD+NOR+POL+PRT+RUS+SVK+SVN+ZAF+ESP+SWE+CHE+GBR&sex=SEX_T+SEX_M+SEX_F&classif1=AGE_AGGREGATE_TOTAL+AGE_AGGREGATE_Y15-24+AGE_AGGREGATE_Y25-54+AGE_AGGREGATE_Y55-64+AGE_AGGREGATE_YGE65&classif2=ECO_AGGREGATE_TOTAL+ECO_AGGREGATE_AGR+ECO_AGGREGATE_MAN+ECO_AGGREGATE_CON+ECO_AGGREGATE_MEL+ECO_AGGREGATE_MKT+ECO_AGGREGATE_PUB&timefrom=2015&type=label&format=.csv") %>%
  select(c(1,4),c(5:8))

df_gdp <- readSDMX("https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA_EXPENDITURE_GROWTH_OECD,/Q..ZAF+RUS+IDN+IND+BRA+AUT+BEL+CHE+CZE+DEU+DNK+ESP+FIN+EST+FRA+GBR+GRC+HUN+ISL+LTU+IRL+ITA+JPN+KOR+LUX+LVA+NLD+NOR+POL+PRT+SVK+SVN+SWE.S1..P51G+B1GQ......G1.?startPeriod=2015-Q1") %>%
  as_tibble() %>%
  clean_names() %>%
  select(c(3,6,17,18))


#### Cleaning ####
##### ILO Employment #####
df_ilo <- df_ilo %>%
  rename(country = ref_area.label,
         sex = sex.label,
         age = classif1.label,
         skill = classif2.label,
         employment = obs_value) %>% 
  mutate(sex = str_remove(sex, ".*: "),
         age = str_remove(age, ".*: "),
         skill = str_remove(skill, ".*: ")) %>%
  mutate(sex = as.factor(sex),
         age = as.factor(age),
         skill = as.factor(skill)) %>%
  filter(time >= "2015Q1") %>%
  mutate(country  = str_replace(country, "United Kingdom of Great Britain and Northern Ireland", "United Kingdom"),
         country = str_replace(country, "Republic of Korea", "South Korea"),
         country = str_replace(country, "Russian Federation", "Russia")) %>% 
  filter(sex != "Total")

df_ilo <- df_ilo %>%
  pivot_wider(names_from = sex, values_from = employment) %>%
  mutate(fmratio = Female / Male) 

## Sectors ##
df_sector <- df_sector %>%
  rename(country = ref_area.label,
         sex = sex.label,
         age = classif1.label,
         sector = classif2.label,
         employment = obs_value) %>%
  mutate(sex = str_remove(sex, ".*: "),
         age = str_remove(age, ".*: "),
         sector = str_remove(sector, ".*: ")) %>%
  mutate(sex = as.factor(sex),
         age = as.factor(age),
         sector = as.factor(sector)) %>%
  filter(time >= "2015Q1") %>%
  mutate(country  = str_replace(country, "United Kingdom of Great Britain and Northern Ireland", "United Kingdom"),
         country = str_replace(country, "Republic of Korea", "South Korea"),
         country = str_replace(country, "Russian Federation", "Russia")) %>% 
  filter(sex != "Total")

df_sector <- df_sector %>%
  pivot_wider(names_from = sex, values_from = employment) %>%
  mutate(fmratio = Female / Male) 

##### OECD GDP GFCF #####
df_gdp <- df_gdp %>%
  left_join(codelist %>% select(iso3c,country.name.en), by = c("ref_area" = "iso3c")) %>%
  select(2:5) %>%
  rename(country = country.name.en) %>%
  pivot_wider(names_from = transaction, values_from = obs_value) %>% 
  rename(time = obs_time,
         gdp_g = B1GQ,
         gfcf_g = P51G) %>% 
  mutate(time = str_remove(time, "-"))

# Merge ILO with GDP
df_ilo_gdp <- left_join(df_ilo, df_gdp, by = join_by(country, time)) %>% 
  na.omit(gdp_growth)

df_sector_gdp <- left_join(df_sector, df_gdp, by = join_by(country, time)) %>% 
  na.omit(gdp_growth)

##### Creation of COVID time periods ######
df_covid <- tibble(
  time = 
    paste0(year(seq(ymd("2015-01-01"), ymd("2024-12-31"), by = "quarter")),
           "Q",
           quarter(seq(ymd("2015-01-01"), ymd("2024-12-31"), by = "quarter")))
) %>%
  mutate(covid = case_when(
    time < "2020Q1" ~ 0,
    time >= "2020Q1" & time < "2020Q3" ~ 1,
    time >= "2020Q3" & time < "2021Q1" ~ 2,
    time >= "2021Q1" & time < "2021Q3" ~ 3,
    time >= "2021Q3" & time < "2022Q1" ~ 4,
    time >= "2022Q1" & time < "2022Q3" ~ 5,
    time >= "2022Q3" & time < "2023Q1" ~ 6,
    time >= "2023Q1" & time < "2023Q3" ~ 7,
    time >= "2023Q3" ~ 8
     ),
    postcovid = ifelse(covid >= 1, 1, 0),
    covid = as.factor(covid)
  )
# Joining COVID
df_ilo_gdp <- left_join(df_ilo_gdp, df_covid, by = join_by(time))
df_sector_gdp <- left_join(df_sector_gdp, df_covid, by = join_by(time))

#### Models ####

df_test <- df_sector_gdp %>%
  filter(age != "Total" & sector != "Total") %>%
  mutate(time = as.factor(time))
#df_test$sector <- relevel(df_test$sector, ref = "Public Administration, Community, Social and other Services and Activities")
df_test$sector <- relevel(df_test$sector, ref = "Trade, Transportation, Accommodation and Food, and Business and Administrative Services")

df_test2 <- df_ilo_gdp %>%
  filter(age != "Total" & skill != "Total") %>%
  mutate(time = as.factor(time))
#df_test2$time <- factor(df_test2$time, ordered = FALSE) # Include these
#df_test2$time <- relevel(df_test2$time, ref = "2019Q4")


##### OLS #####
m1 <- list(
  lm(fmratio ~ covid, data = df_test),
  lm(fmratio ~ covid + age, data = df_test),
  #lm(data = df_test, fmratio ~ postcovid + sector), #AdjR2 0.741
  lm(fmratio ~ postcovid*sector, data = df_test) #AdjR2 0.742
)
modelsummary(m1, stars = T, vcov = "HC1", title = "Selected regressions using sectoral data", output = "latex_tabular")

m2 <- list(
  lm(fmratio ~ covid, data = df_test2),
  #lm(fmratio ~ covid*skill, data = df_test2), #AdjR2 0.152 
  lm(fmratio ~ postcovid*skill, data = df_test2), #AdjR2 0.152
  lm(fmratio ~ postcovid*skill + age + gdp_g, data=df_test2), #AdjR2 0.174
  lm(fmratio ~ covid*skill + age + gfcf_g, data=df_test2) #AdjR2 0.174
)
modelsummary(m2, stars = T, vcov = "HC1", title = "Selected regressions using skill data", output = "latex_tabular")


##### Fixed Effects #####
m3 <- list(
  plm(fmratio ~ covid*skill, data = df_test2, index = c("country", "time"), model = "within"), #AdjR2 0.166
  "Skill" = plm(fmratio ~ postcovid*skill + age + gdp_g, data = df_test2, index = c("country", "time"), model = "within"), #AdjR2 0.191
  #plm(fmratio ~ time*skill + age + gdp_g, data = df_test2, index = c("country", "time"), model = "within") #AdjR2 0.189
  "Sector" = plm(fmratio ~ postcovid*sector + age, data = df_test, index = c("country", "time"), model = "within") #AdjR2 0.778
)
modelsummary(m3, stars = T, vcoc = "HC1", title = "Selected fixed effects models", output = "latex_tabular")
#fixef(m3$Skill)


##### Tables #####
table1 <- df_sector_gdp %>%
  group_by(postcovid, country) %>%
  summarise(
    mean_ratio = mean(fmratio, na.rm = TRUE),
    sd_ratio = sd(fmratio, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = postcovid,
    values_from = c(mean_ratio, sd_ratio),
    names_prefix = "postcovid_"
  ) %>%
  rename(
    mean = mean_ratio_postcovid_0,
    sd = sd_ratio_postcovid_0,
    mean_ = mean_ratio_postcovid_1,
    sd_ = sd_ratio_postcovid_1
  ) %>% 
  select(country, mean, sd, mean_, sd_) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
table1 %>%
  kable("latex", booktabs = TRUE, caption = "Female-to-male employment ratios") %>%
  kable_styling(latex_options = c("hold_position", "basic")) %>%
  add_header_above(c(" " = 1, "Pre-COVID" = 2, "Post-COVID" = 2))


## Things to do ##
# https://ourworldindata.org/covid-cases 
# Fixed effects for countries
# Adding additional vars e.g. sector