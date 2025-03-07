## -- this script runs some very basic code on preparing the dataset and updating variable info.

library(magrittr)
library(tidyverse)

# importing data ----------------------------------------------------------

X <- read.csv("data/features.csv")
y <- read.csv("data/target.csv")
var_inf <- read.csv("data/var_info.csv")

df <- cbind(X, y) ## -- concatenate features (X) and target (y)

write.csv(df, file = "data/derived/df_v1.csv") ## -- continue using this for subsequent analysis

## -- fill in missing variable info

var_inf %<>%
  mutate(description = case_when(
    name == "cp" ~ "chest pain type (1-4)",
    name == "restecg" ~ "resting electrocardiographic results (3 values, 0 - normal)",
    name == "slope" ~ "the slope of the peak exercise ST segment (3 values)",
    name == "thal" ~ "thalassemia type: 3 values, coded as 3 (normal), 6 (fixed defect), and 7 (reversible defect)",
    TRUE ~ description
  ))

write.csv(var_inf, file = "data/derived/var_info_upd.csv")



  
