# R-Hierarchical-Linear-Regression

This package contains two functions:
1. hierarchical: for automatic hierachical regression (predictors automatically entered for regression one by one)
2. group_hierarchical: for automatic hierachical regression in sets (groups of predictors automatically entered for regression)

## Help for function "hierarchical"

  This function does automatic hierachical regression (predictors automatically entered for regression one by one).
  This function applies hierachical regression on a "dataFrame" with all the predictors included in that dataFrame.
  The dataFrame should include the variables in the order you want to apply the hierachical regression (the order of columns     in dataFrame is important).

Example:

  "happniess_data" is the dataFrame in which all predictors are included
  1. hierarchical(happiness_data, happinessdata$happiness_score, "gender")

  2. if there are more than one categorical variables -->
      hierarchical(dataFrame, DependentVariable, c("Categorical_var_1", "Categorical_var_2"))

  3. if there is no categorical variable -->
      hierarchical(dataFrame, DependentVariable, c())


## Help for function "group_hierarchical"

  This function does automatic hierachical regression in sets (groups of predictors automatically entered for regression).
  This function applies hierachical regression on a "list" of "dataFrame"s with all groups of predictors included in that       "list"
  
 Example:
  1. group_hierarchical(list(happiness_data[1:3],happiness_data[4:5],happiness_data[6]), happiness_data$happiness_score, "gender")

  2. if there are more than one categorical variables -->
    group_hierarchical(list(groupDF1, groupDF2, groupDF3), DependentVariable, c("Categorical_var_1", "Categorical_var_2"))

  3. if there is no categorical variable -->
    group_hierarchical(list(groupDF1, groupDF2, groupDF3), DependentVariable, c())
