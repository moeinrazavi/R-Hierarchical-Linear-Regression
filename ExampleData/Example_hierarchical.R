library(dplyr)
library(tibble)
library(utils)
library(hierarchicalR)

happiness_data = read.csv("./happiness.csv")

Hierarchical_summary_table = hierarchical(happiness_data, happiness_data$happiness_score, "gender")

# Hierarchical_summary_table = hierarchical(happiness_data, happiness_data$happiness_score, c()) --> if there was no categorical variable
# Hierarchical_summary_table = hierarchical(happiness_data, happiness_data$happiness_score, c(Cat_var1, Cat_var2)) --> if there were more than one categorical variables

View(Hierarchical_summary_table)
