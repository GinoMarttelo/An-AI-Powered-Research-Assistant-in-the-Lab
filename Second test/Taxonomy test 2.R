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

colnames(gpt)[3:12] <- paste0("c1_", c(
  "cat1",
  "cat2",
  "cat3",
  "cat4",
  "cat5",
  "cat6",
  "cat7",
  "cat8",
  "cat9",
  "cat10"
))


human1 <- read_xlsx("Classifications - Human 1.xlsx")

human1$X <- c(0:149)

colnames(human1)[3:12] <- paste0("c2_", c(
  "cat1",
  "cat2",
  "cat3",
  "cat4",
  "cat5",
  "cat6",
  "cat7",
  "cat8",
  "cat9",
  "cat10"
))

gpt <- gpt %>% 
  full_join(human1, by = c ("X", "ResponseId"))

human2 <- read_xlsx("Classifications - Human 2.xlsx")

human2$X <- c(0:149)

colnames(human2)[3:12] <- paste0("c3_", c( 
  "cat1",
  "cat2",
  "cat3",
  "cat4",
  "cat5",
  "cat6",
  "cat7",
  "cat8",
  "cat9",
  "cat10"
))

gpt <- gpt %>% 
  full_join(human2, by = c ("X", "ResponseId", "goal"))

gpt <- gpt[, c("goal", setdiff(names(gpt), "goal"))]

write_xlsx(gpt, "gpt.xlsx")


gpt <- read_xlsx("gpt.xlsx")


#2. Percentage of agreement - Ideal for discrete scoring------

#Adjust if you have more coders adding more combinations
for (i in 1:10) {
  gpt[[paste0("c1Xc2_cat", i)]] <- ifelse(gpt[[paste0("c1_cat", i)]] == gpt[[paste0("c2_cat", i)]], TRUE, FALSE)
  gpt[[paste0("c1Xc3_cat", i)]] <- ifelse(gpt[[paste0("c1_cat", i)]] == gpt[[paste0("c3_cat", i)]], TRUE, FALSE)
  gpt[[paste0("c2Xc3_cat", i)]] <- ifelse(gpt[[paste0("c2_cat", i)]] == gpt[[paste0("c3_cat", i)]], TRUE, FALSE)
}

# Empty gptframe. Add more combinations if you have more coders
results <- data.frame(Category = paste0("cat", 1:10), #Adjust based on the number of categotries
                      c1Xc2 = NA,
                      c1Xc3 = NA,
                      c2Xc3 = NA)

# Percentage of true per comparisson and category
#Adjust based on the number of categories and coders
for (i in 1:10) {
  results$c1Xc2[i] <- mean(gpt[[paste0("c1Xc2_cat", i)]], na.rm = TRUE) * 100
  results$c1Xc3[i] <- mean(gpt[[paste0("c1Xc3_cat", i)]], na.rm = TRUE) * 100
  results$c2Xc3[i] <- mean(gpt[[paste0("c2Xc3_cat", i)]], na.rm = TRUE) * 100
}

#Calculate average of agreement per category
results <- rbind(results, 
                 data.frame(Category = "Average",
                            c1Xc2 = mean(results$c1Xc2, na.rm = TRUE),
                            c1Xc3 = mean(results$c1Xc3, na.rm = TRUE),
                            c2Xc3 = mean(results$c2Xc3, na.rm = TRUE)))

#Calculate average of agreement per pairwise of coders
results$mean_agreement <- rowMeans(results[, c("c1Xc2", "c1Xc3", "c2Xc3")], na.rm = TRUE)

results

#3. Intercoder reliability indexes------
#Creating a longdata for intercoder reliability indexes
longdata <- gpt %>% 
  pivot_longer(
    cols = starts_with("c"), 
    names_to = c("coder", "cat"), 
    names_pattern = "(c\\d+)_(cat\\d+)", 
    values_to = "value"
  ) %>% 
  pivot_wider(names_from = coder, values_from = value) %>% 
  rename(coder1 = c1, coder2 = c2)#, coder3 = c3) #Adjust based on the number of coders

longdata <- gpt %>% 
  pivot_longer(cols = c("c1_cat1","c1_cat2","c1_cat3","c1_cat4","c1_cat5","c1_cat6","c1_cat7","c1_cat8","c1_cat9","c1_cat10"
  ), names_to ="cat", values_to ="coder1") %>% 
  select(goal, cat, coder1)

longdata$cat <- gsub("c1_", "", longdata$cat)

longdata2 <- gpt %>% 
  pivot_longer(cols = c("c2_cat1","c2_cat2","c2_cat3","c2_cat4","c2_cat5","c2_cat6","c2_cat7","c2_cat8","c2_cat9","c2_cat10"
  ), names_to ="cat", values_to ="coder2") %>% 
  select(goal, cat, coder2)

longdata2$cat <- gsub("c2_", "", longdata2$cat)

longdata <- longdata %>%
  left_join(longdata2, by = c("goal", "cat"))

longdata3 <- gpt %>% 
  pivot_longer(cols = c("c3_cat1","c3_cat2","c3_cat3","c3_cat4","c3_cat5","c3_cat6","c3_cat7","c3_cat8","c3_cat9","c3_cat10"
  ), names_to ="cat", values_to ="coder3") %>% 
  select(goal, cat, coder3)

longdata3$cat <- gsub("c3_", "", longdata3$cat)

longdata <- longdata %>%
  left_join(longdata3, by = c("goal", "cat"))

#Create a dataframe with coders columns only
longdata2 <- longdata %>% 
  select(-c("goal", "cat"))

##Intraclass Correlation Coefficient  - For continuum scoring and pairwise comparisons
icc(longdata[, c("coder1", "coder2")], model = "twoway", type = "agreement")
icc(longdata[, c("coder1", "coder3")], model = "twoway", type = "agreement")
icc(longdata[, c("coder2", "coder3")], model = "twoway", type = "agreement")

#Intraclass Correlation Coefficient - For continuum scoring and multiple comparisons
icc(longdata2, model = "twoway", type = "agreement")

#4. Realibility per category-----
#Create a dataframe
# cat1
longdata_cat1 <- longdata %>% 
  filter(cat == "cat1") %>% 
  select(-c("goal", "cat"))
icc(longdata_cat1[, c("coder1", "coder2")], model = "twoway", type = "agreement")
icc(longdata_cat1[, c("coder1", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat1[, c("coder2", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat1, model = "twoway", type = "agreement")

# cat2
longdata_cat2 <- longdata %>% 
  filter(cat == "cat2") %>% 
  select(-c("goal", "cat"))
icc(longdata_cat2[, c("coder1", "coder2")], model = "twoway", type = "agreement")
icc(longdata_cat2[, c("coder1", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat2[, c("coder2", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat2, model = "twoway", type = "agreement")

# cat3
longdata_cat3 <- longdata %>% 
  filter(cat == "cat3") %>% 
  select(-c("goal", "cat"))
icc(longdata_cat3[, c("coder1", "coder2")], model = "twoway", type = "agreement")
icc(longdata_cat3[, c("coder1", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat3[, c("coder2", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat3, model = "twoway", type = "agreement")

# cat4
longdata_cat4 <- longdata %>% 
  filter(cat == "cat4") %>% 
  select(-c("goal", "cat"))
icc(longdata_cat4[, c("coder1", "coder2")], model = "twoway", type = "agreement")
icc(longdata_cat4[, c("coder1", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat4[, c("coder2", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat4, model = "twoway", type = "agreement")

# cat5
longdata_cat5 <- longdata %>% 
  filter(cat == "cat5") %>% 
  select(-c("goal", "cat"))
icc(longdata_cat5[, c("coder1", "coder2")], model = "twoway", type = "agreement")
icc(longdata_cat5[, c("coder1", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat5[, c("coder2", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat5, model = "twoway", type = "agreement")

# cat6
longdata_cat6 <- longdata %>% 
  filter(cat == "cat6") %>% 
  select(-c("goal", "cat"))
icc(longdata_cat6[, c("coder1", "coder2")], model = "twoway", type = "agreement")
icc(longdata_cat6[, c("coder1", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat6[, c("coder2", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat6, model = "twoway", type = "agreement")

# cat7
longdata_cat7 <- longdata %>% 
  filter(cat == "cat7") %>% 
  select(-c("goal", "cat"))
icc(longdata_cat7[, c("coder1", "coder2")], model = "twoway", type = "agreement")
icc(longdata_cat7[, c("coder1", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat7[, c("coder2", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat7, model = "twoway", type = "agreement")

# cat8
longdata_cat8 <- longdata %>% 
  filter(cat == "cat8") %>% 
  select(-c("goal", "cat"))
icc(longdata_cat8[, c("coder1", "coder2")], model = "twoway", type = "agreement")
icc(longdata_cat8[, c("coder1", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat8[, c("coder2", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat8, model = "twoway", type = "agreement")


alpha <- kripp.alpha(t(as.matrix(longdata_cat8)), method = "nominal")
print(alpha)

# cat9
longdata_cat9 <- longdata %>% 
  filter(cat == "cat9") %>% 
  select(-c("goal", "cat"))
icc(longdata_cat9[, c("coder1", "coder2")], model = "twoway", type = "agreement")
icc(longdata_cat9[, c("coder1", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat9[, c("coder2", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat9, model = "twoway", type = "agreement")

# cat10
longdata_cat10 <- longdata %>% 
  filter(cat == "cat10") %>% 
  select(-c("goal", "cat"))
icc(longdata_cat10[, c("coder1", "coder2")], model = "twoway", type = "agreement")
icc(longdata_cat10[, c("coder1", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat10[, c("coder2", "coder3")], model = "twoway", type = "agreement")
icc(longdata_cat10, model = "twoway", type = "agreement")
