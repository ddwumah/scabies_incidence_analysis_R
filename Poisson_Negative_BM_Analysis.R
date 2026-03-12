install.packages(c("MASS", "LRTesteR", "tidyverse", "readxl", 
                   "forcats", "patchwork", "AER"))


library(MASS)
library(LRTesteR)
library(tidyverse)
library(readxl)
library(forcats)
library(patchwork)
library(AER)

# Importing the Scabies datasets

scabies_data <- read_excel("D:/Project_MK_Ultra/Project_Trials/Project_SCABIES/Original_data_project.xlsx")
scabies_data

str(scabies_data)

scabies <- scabies_data %>% 
  select(months, SCABIES)
attach(scabies)

months_factor <- factor(months, 
                        levels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                   "Jun","Jul", "Aug", "Sep", "Oct", 
                                   "Nov", "Dec"), 
                        labels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                   "Jun","Jul", "Aug", "Sep", "Oct", 
                                   "Nov", "Dec"))

  scabies <- scabies %>% 
    mutate(months = months_factor)
scabies



# Fitting the Poisson Regression model.

poisson_model <- glm(SCABIES ~ months, 
                     family = "poisson", data = scabies)
summary(poisson_model)


# Dispersion Parameter (Overdispersion test)

dispersiontest(poisson_model)



# Fitting a Negative Binomial Regression model.

negative_binomia_model <- glm.nb(SCABIES ~ months,
                              data = scabies)
summary(negative_binomia_model)


 # Performing likelihood ratio test

lr_test <- anova(poisson_model, negative_binomia_model, 
                 test = "Chisq")
lr_test


# Calculate AIC for both models

poisson_model_aic <- AIC(poisson_model) %>% 
  print()

negative_binomia_model_aic <- AIC(negative_binomia_model) %>% 
  print()

# Calculate BIC for both models

poisson_model_bic <- BIC(poisson_model) %>% 
  print()

negative_binomia_model_bic <- BIC(negative_binomia_model) %>% 
  print()



