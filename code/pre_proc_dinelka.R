# works on some initial descriptive stats ------------------------

library(tidyverse)
library(ggplot2)

df <- read.csv("data/derived/df_v1.csv")
str(df)

# target class distribution ------------------------------------------------

ggplot(data = df, aes(x = num)) + 
  geom_bar() + 
  labs(title = "Presence of heart disease (0:absent, 1-4: presence)")

# descriptive stats -------------------------------------------------------

  ## -- sex (more males in presence of heart diagnosis)
ggplot(data = df, aes(x = num,  fill = factor(sex))) +
  geom_bar(position = "stack") +
  labs(x = "Heart Diagnosis", fill = "sex") +
  theme_minimal() 

  ## -- age
ggplot(data = df, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "#2b2b2b", color = "white", alpha = 0.8) +
  labs(title = "Age Distribution",
       x = "Age",
       y = "Count") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15))




