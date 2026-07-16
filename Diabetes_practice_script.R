library(readxl)
diabetic_data <- read_excel("diabetic_data.xlsx")
View(diabetic_data) 
summary(diabetic_data)
colSums(is.na(diabetic_data))
diabetic_data<- diabetic_data %>% mutate(across(where(is.character), ~ na_if(.x, "?")))
diabetic_data<- diabetic_data %>% distinct()
diabetic_data <- diabetic_data %>% mutate(payer_code = replace_na(payer_code, "Missing")) 
table(diabetic_data$payer_code, useNA = "always")
diabetic_data <- diabetic_data %>% mutate(weight = replace_na(weight, "Missing")) 
table(diabetic_data$weight, useNA = "always")
diabetic_data <- diabetic_data %>% mutate(medical_specialty = replace_na(medical_specialty, "Missing"))  
table(diabetic_data$medical_specialty, useNA = "always")
most_common_payer <- diabetic_data %>% filter(payer_code != "Missing") %>% count(payer_code) %>%  arrange(desc(n)) %>% slice(1) %>% pull(payer_code) 
most_common_payer 
diabetic_data <- diabetic_data %>%
  mutate( payer_code = if_else(payer_code == "Missing",most_common_payer, payer_code))
diabetic_data <- diabetic_data %>% mutate(race = replace_na(race, "Missing"))
table(diabetic_data$race, useNA = "always")
diabetic_data <- diabetic_data %>% mutate(diag_1 = replace_na(diag_1, "Missing"))
table(diabetic_data$diag_1, useNA = "always")
diabetic_data <- diabetic_data %>% mutate(diag_2 = replace_na(diag_2, "Missing"))
table(diabetic_data$diag_2, useNA = "always")
diabetic_data <- diabetic_data %>% mutate(diag_3 = replace_na(diag_3, "Missing")) 
table(diabetic_data$diag_3, useNA = "always") 
diabetic_data <- diabetic_data %>% mutate( readmitted_30 = if_else(readmitted == "<30", 1, 0)) 
table(diabetic_data$readmitted_30)
set.seed(123)
all_patients <- unique(diabetic_data$patient_nbr)
training_patients <- sample(all_patients, size = round(0.80 * length(all_patients)))
training_data <- diabetic_data %>% filter(patient_nbr %in% training_patients)
testing_data <- diabetic_data %>% filter(!(patient_nbr %in% training_patients))
nrow(training_data)
nrow(testing_data)
most_common <- function(column) {
  column <- column[ !is.na(column) &column != "Missing"]   
  names(sort(table(column), decreasing = TRUE ))[1]}
race_value <- most_common(training_data$race)
weight_value <- most_common(training_data$weight)
payer_value <- most_common(training_data$payer_code)
specialty_value <- most_common(training_data$medical_specialty)
diag1_value <- most_common(training_data$diag_1)
diag2_value <- most_common(training_data$diag_2)
diag3_value <- most_common(training_data$diag_3)
training_data <- training_data %>%
  mutate(
    race = if_else(is.na(race) | race == "Missing", race_value, race),
    weight = if_else(is.na(weight) | weight == "Missing", weight_value, weight),
    payer_code = if_else(is.na(payer_code) | payer_code == "Missing", payer_value, payer_code),
    medical_specialty = if_else(is.na(medical_specialty) | medical_specialty == "Missing", specialty_value, medical_specialty),
    diag_1 = if_else(is.na(diag_1) | diag_1 == "Missing", diag1_value, diag_1),
    diag_2 = if_else(is.na(diag_2) | diag_2 == "Missing", diag2_value, diag_2),
    diag_3 = if_else(is.na(diag_3) | diag_3 == "Missing", diag3_value, diag_3))
testing_data <- testing_data %>%
  mutate(
    race = if_else(is.na(race) | race == "Missing", race_value, race),
    weight = if_else(is.na(weight) | weight == "Missing", weight_value, weight),
    payer_code = if_else(is.na(payer_code) | payer_code == "Missing", payer_value, payer_code),
    medical_specialty = if_else(is.na(medical_specialty) | medical_specialty == "Missing", specialty_value, medical_specialty),
    diag_1 = if_else(is.na(diag_1) | diag_1 == "Missing", diag1_value, diag_1),
    diag_2 = if_else(is.na(diag_2) | diag_2 == "Missing", diag2_value, diag_2),
    diag_3 = if_else(is.na(diag_3) | diag_3 == "Missing", diag3_value, diag_3))
View(training_data)
View(testing_data)
logistic_model <- glm(
  readmitted_30 ~ age + gender + race + time_in_hospital,
  data = training_data,
  family = binomial)
summary(logistic_model)
testing_data <- testing_data %>%
  mutate(
    predicted_probability = predict(
      logistic_model,
      newdata = testing_data,
      type = "response"))
testing_data <- testing_data %>%
  mutate(
    predicted_readmitted = if_else(
      predicted_probability >= 0.50,1,0))
table(
  Actual = testing_data$readmitted_30,
  Predicted = testing_data$predicted_readmitted)
accuracy <- mean(
  testing_data$readmitted_30 == testing_data$predicted_readmitted)
accuracy * 100