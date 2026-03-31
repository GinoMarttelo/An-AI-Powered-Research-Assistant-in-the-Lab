#Template for intercoder reliability
#0. Setup----
pacman::p_load(tidyverse, readxl, PerformanceAnalytics, corrplot, irr, writexl, krippendorff)

#1. Data Loading----
#Adjust your dataframe so that each response to be classified has its own row, 
#with one column for each combination of category and coder. Make sure to label
#each column in a way that clearly identifies both the coder and the category.
run1 <- read.csv("Classifications - Run 1.csv")
colnames(run1)[3:12] <- paste0("r1_", c(
  "education_learning",
  "financial_stability",
  "career_development",
  "health_wellbeing",
  "family_relationships",
  "travel_exploration",
  "material_acquisition",
  "personal_development",
  "not_applicable",
  "orphans"
))

run2 <- read.csv("Classifications - Run 2.csv")
colnames(run2)[3:12] <- paste0("r2_", c(
  "education_learning",
  "financial_stability",
  "career_development",
  "health_wellbeing",
  "family_relationships",
  "travel_exploration",
  "material_acquisition",
  "personal_development",
  "not_applicable",
  "orphans"
))

run3 <- read.csv("Classifications - Run 3.csv")
colnames(run3)[3:12] <- paste0("r3_", c(
  "education_learning",
  "financial_stability",
  "career_development",
  "health_wellbeing",
  "family_relationships",
  "travel_exploration",
  "material_acquisition",
  "personal_development",
  "not_applicable",
  "orphans"
))

run4 <- read.csv("Classifications - Run 4.csv")
colnames(run4)[3:12] <- paste0("r4_", c(
  "education_learning",
  "financial_stability",
  "career_development",
  "health_wellbeing",
  "family_relationships",
  "travel_exploration",
  "material_acquisition",
  "personal_development",
  "not_applicable",
  "orphans"
))

run5 <- read.csv("Classifications - Run 5.csv")
colnames(run5)[3:12] <- paste0("r5_", c(
  "education_learning",
  "financial_stability",
  "career_development",
  "health_wellbeing",
  "family_relationships",
  "travel_exploration",
  "material_acquisition",
  "personal_development",
  "not_applicable",
  "orphans"
))

gpt <- run1 %>% 
  full_join(run2, by = c ("X", "ResponseId"))

gpt <- gpt %>% 
  full_join(run3, by = c ("X", "ResponseId"))

gpt <- gpt %>% 
  full_join(run4, by = c ("X", "ResponseId"))

gpt <- gpt %>% 
  full_join(run5, by = c ("X", "ResponseId"))

gpt <- gpt %>%
  mutate(
    education_learning = case_when(
      rowSums(select(., r1_education_learning, r2_education_learning, r3_education_learning, r4_education_learning, r5_education_learning) == 2) >= 3 ~ 2,
      rowSums(select(., r1_education_learning, r2_education_learning, r3_education_learning, r4_education_learning, r5_education_learning) == 1) >= 3 ~ 1,
      rowSums(select(., r1_education_learning, r2_education_learning, r3_education_learning, r4_education_learning, r5_education_learning) == 0) >= 3 ~ 0,
      TRUE ~ NA_real_
    ),
    
    financial_stability = case_when(
      rowSums(select(., r1_financial_stability, r2_financial_stability, r3_financial_stability, r4_financial_stability, r5_financial_stability) == 2) >= 3 ~ 2,
      rowSums(select(., r1_financial_stability, r2_financial_stability, r3_financial_stability, r4_financial_stability, r5_financial_stability) == 1) >= 3 ~ 1,
      rowSums(select(., r1_financial_stability, r2_financial_stability, r3_financial_stability, r4_financial_stability, r5_financial_stability) == 0) >= 3 ~ 0,
      TRUE ~ NA_real_
    ),
    
    career_development = case_when(
      rowSums(select(., r1_career_development, r2_career_development, r3_career_development, r4_career_development, r5_career_development) == 2) >= 3 ~ 2,
      rowSums(select(., r1_career_development, r2_career_development, r3_career_development, r4_career_development, r5_career_development) == 1) >= 3 ~ 1,
      rowSums(select(., r1_career_development, r2_career_development, r3_career_development, r4_career_development, r5_career_development) == 0) >= 3 ~ 0,
      TRUE ~ NA_real_
    ),
    
    health_wellbeing = case_when(
      rowSums(select(., r1_health_wellbeing, r2_health_wellbeing, r3_health_wellbeing, r4_health_wellbeing, r5_health_wellbeing) == 2) >= 3 ~ 2,
      rowSums(select(., r1_health_wellbeing, r2_health_wellbeing, r3_health_wellbeing, r4_health_wellbeing, r5_health_wellbeing) == 1) >= 3 ~ 1,
      rowSums(select(., r1_health_wellbeing, r2_health_wellbeing, r3_health_wellbeing, r4_health_wellbeing, r5_health_wellbeing) == 0) >= 3 ~ 0,
      TRUE ~ NA_real_
    ),
    
    family_relationships = case_when(
      rowSums(select(., r1_family_relationships, r2_family_relationships, r3_family_relationships, r4_family_relationships, r5_family_relationships) == 2) >= 3 ~ 2,
      rowSums(select(., r1_family_relationships, r2_family_relationships, r3_family_relationships, r4_family_relationships, r5_family_relationships) == 1) >= 3 ~ 1,
      rowSums(select(., r1_family_relationships, r2_family_relationships, r3_family_relationships, r4_family_relationships, r5_family_relationships) == 0) >= 3 ~ 0,
      TRUE ~ NA_real_
    ),
    
    travel_exploration = case_when(
      rowSums(select(., r1_travel_exploration, r2_travel_exploration, r3_travel_exploration, r4_travel_exploration, r5_travel_exploration) == 2) >= 3 ~ 2,
      rowSums(select(., r1_travel_exploration, r2_travel_exploration, r3_travel_exploration, r4_travel_exploration, r5_travel_exploration) == 1) >= 3 ~ 1,
      rowSums(select(., r1_travel_exploration, r2_travel_exploration, r3_travel_exploration, r4_travel_exploration, r5_travel_exploration) == 0) >= 3 ~ 0,
      TRUE ~ NA_real_
    ),
    
    material_acquisition = case_when(
      rowSums(select(., r1_material_acquisition, r2_material_acquisition, r3_material_acquisition, r4_material_acquisition, r5_material_acquisition) == 2) >= 3 ~ 2,
      rowSums(select(., r1_material_acquisition, r2_material_acquisition, r3_material_acquisition, r4_material_acquisition, r5_material_acquisition) == 1) >= 3 ~ 1,
      rowSums(select(., r1_material_acquisition, r2_material_acquisition, r3_material_acquisition, r4_material_acquisition, r5_material_acquisition) == 0) >= 3 ~ 0,
      TRUE ~ NA_real_
    ),
    
    personal_development = case_when(
      rowSums(select(., r1_personal_development, r2_personal_development, r3_personal_development, r4_personal_development, r5_personal_development) == 2) >= 3 ~ 2,
      rowSums(select(., r1_personal_development, r2_personal_development, r3_personal_development, r4_personal_development, r5_personal_development) == 1) >= 3 ~ 1,
      rowSums(select(., r1_personal_development, r2_personal_development, r3_personal_development, r4_personal_development, r5_personal_development) == 0) >= 3 ~ 0,
      TRUE ~ NA_real_
    ),
    
    not_applicable = case_when(
      rowSums(select(., r1_not_applicable, r2_not_applicable, r3_not_applicable, r4_not_applicable, r5_not_applicable) == 2) >= 3 ~ 2,
      rowSums(select(., r1_not_applicable, r2_not_applicable, r3_not_applicable, r4_not_applicable, r5_not_applicable) == 1) >= 3 ~ 1,
      rowSums(select(., r1_not_applicable, r2_not_applicable, r3_not_applicable, r4_not_applicable, r5_not_applicable) == 0) >= 3 ~ 0,
      TRUE ~ NA_real_
    ),
    
    orphans = case_when(
      rowSums(select(., r1_orphans, r2_orphans, r3_orphans, r4_orphans, r5_orphans) == 2) >= 3 ~ 2,
      rowSums(select(., r1_orphans, r2_orphans, r3_orphans, r4_orphans, r5_orphans) == 1) >= 3 ~ 1,
      rowSums(select(., r1_orphans, r2_orphans, r3_orphans, r4_orphans, r5_orphans) == 0) >= 3 ~ 0,
      TRUE ~ NA_real_
    )
  )

gpt <- gpt %>% 
  select(1:2,53:62)

colnames(run2)[3:12] <- paste0("r2_", c(
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "orphans"
))


gpt$main_catg <- ifelse(grepl("2", gpt$education_learning), "Education and learning", 
                              ifelse(grepl("2", gpt$financial_stability), "Financial stability", 
                                     ifelse(grepl("2", gpt$career_development), "Career and professional development", 
                                            ifelse(grepl("2", gpt$health_wellbeing), "Health and well-being", 
                                                   ifelse(grepl("2", gpt$family_relationships), "Family and social relationships", 
                                                          ifelse(grepl("2", gpt$travel_exploration), "Travel and exploration", 
                                                                 ifelse(grepl("2", gpt$material_acquisition), "Material acquisitions", 
                                                                        ifelse(grepl("2", gpt$personal_development), "Personal development", 
                                                                               ifelse(grepl("2", gpt$not_applicable), "Not applicable", 
                                                                                      ifelse(grepl("2", gpt$orphans), "Orphans", NA))))))))))





prop.table(table(gpt$main_catg))

write_xlsx(gpt, "Goals classified.xlsx")
