# WOMEN'S EMPLOYMENT DURING COVID #
## Notes ##
# Employment is in thousands

#### Setup ####
setwd("~/Documents/F2023-W2025 Master in Economics/M2 S2/Transversal Project")

library(tidyverse); library(readr); library(ggrepel); library(ggpubr); library(janitor)
df_ilo <- read_csv("https://rplumber.ilo.org/data/indicator/?id=EMP_TEMP_SEX_AGE_OCU_NB_Q&ref_area=ALB+AUT+BGD+BEL+BIH+BRA+BGR+HRV+CYP+CZE+DNK+EGY+EST+FIN+FRA+DEU+GRC+HUN+ISL+IND+IDN+IRN+IRL+ITA+JPN+JOR+KOR+KOS+LVA+LTU+LUX+MLT+MDA+MNG+MNE+NLD+NOR+PAK+POL+PRT+ROU+RUS+SRB+SGP+SVK+SVN+ZAF+ESP+SWE+CHE+GBR+VNM&sex=SEX_T+SEX_M+SEX_F&classif1=AGE_AGGREGATE_TOTAL+AGE_AGGREGATE_Y15-24+AGE_AGGREGATE_Y25-54+AGE_AGGREGATE_Y55-64+AGE_AGGREGATE_YGE65&classif2=OCU_SKILL_TOTAL+OCU_SKILL_L3-4+OCU_SKILL_L2+OCU_SKILL_L1&timefrom=2015&type=label&format=.csv")

df_ilo <- df_ilo %>% select(c(1,4),c(5:8))

df_gdp <- read_csv("Gdp_growth.csv") %>% clean_names() %>% mutate(time = as.factor(time))

#### Cleaning ####
# ILO
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
         country = str_replace(country, "Republic of Korea", "Korea"),
         country = str_replace(country, "Russian Federation", "Russia")) %>% 
  filter(sex != "Total")

df_ilo <- df_ilo %>%
  pivot_wider(names_from = sex, values_from = employment) %>%
  mutate(fmratio = Female / Male) 

# GDP


# Merge ILO with GDP
df_ilo_gdp <- left_join(df_ilo, df_gdp, by = join_by(country, time)) %>% 
  na.omit(gdp_growth)
#write.csv(df_ilo_gdp, file = "ilo_emp_gdp.csv")
# Creation of COVID time periods
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


#### Models ####

df_test <- df_ilo_gdp %>%
  filter(age == "Total" & skill == "Total") %>%
  mutate(time = as.factor(time))

df_test2 <- df_ilo_gdp %>%
  filter(age != "Total" & skill != "Total") %>%
  mutate(time = as.factor(time))

df_test$time <- factor(df_test$time, ordered = FALSE)
df_test$time <- relevel(df_test$time, ref = "2019Q4")
m1 <- list(
  lm(data = df_test, f_m_ratio ~ time),
  lm(data = df_test, f_m_ratio ~ covid), 
  lm(data = df_test, f_m_ratio ~ postcovid),
  lm(data = df_test, f_m_ratio ~ postcovid + gdp_growth)
)
modelsummary::modelsummary(m1, stars = T, vcov = "HC1")

m2 <- list(
  lm(data=df_test2, f_m_ratio ~ covid),
  lm(data=df_test2, f_m_ratio ~ covid*skill), #AdjR2 0.15 
  lm(data=df_test2, f_m_ratio ~ postcovid*skill), #AdjR2 0.151
  lm(data=df_test2, f_m_ratio ~ postcovid*skill + age + gdp_growth) #AdjR2 0.171
)
modelsummary::modelsummary(m2, stars = T, vcov = "HC1")

## Things to do ##
# https://ourworldindata.org/covid-cases 
# Fixed effects for countries
# Adding additional countries e.g. family size, sector, investment