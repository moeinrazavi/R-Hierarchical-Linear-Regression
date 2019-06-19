library(dplyr)
library(tibble)
library(utils)
library(hierarchicalR)

happiness_data = read.csv("./happiness.csv")

group1 = happiness_data[1:3]
group2 = happiness_data[4:5]
group3 = happiness_data[6]


Group_Hierarchical_summary_table = group_hierarchical(list(group1,group2,group3), happiness_data$happiness_score, "gender")

# Group_Hierarchical_summary_table = group_hierarchical(list(group1,group2,group3), happiness_data$happiness_score, c()) --> if there was no categorical variable
# Group_Hierarchical_summary_table = group_hierarchical(list(group1,group2,group3), happiness_data$happiness_score, c(Cat_var1, Cat_var2)) --> if there were more than one categorical variables

View(Group_Hierarchical_summary_table)

