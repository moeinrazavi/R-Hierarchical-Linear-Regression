#' @title Hierarchical Regression
#'
#' @description This function does automatic hierachical regression (predictors automatically entered for regression one by one).
#'  This function applies hierachical regression on a "dataFrame" with all the predictors included in that dataFrame.
#'  The dataFrame should include the variables in the order you want to apply the hierachical regression (the order of columns in dataFrame is important).
#'
#' @param dataFrame, dependent variable (Numeric Vector), categorical variables in the dataFrame (if any)
#'
#' @return Hierarchical Regression summary table
#'
#' @examples 1. hierarchical(happniess_data, happinessdata$happiness_score, "gender")
#'
#'           2. if there are more than one categorical variables -->
#'               hierarchical(dataFrame, DependentVariable, c("Categorical_var_1", "Categorical_var_2"))
#'
#'           3. if there is no categorical variable -->
#'               hierarchical(dataFrame, DependentVariable, c())
#'
#' @export


hierarchical <- function(dataFrame, DV,Cat_Var) {


library(dplyr)
library(tibble) # library for adding a column in middle of a dataframe


#Hierarchical Model
H_Model = rep(list(list()), dim(dataFrame)[2]-1)
summary_H_Model= rep( list(list()), dim(dataFrame)[2]-1)
for (i in seq(dim(dataFrame)[2]-1)) {
  H_dataFrame = dataFrame[,c(seq(i),dim(dataFrame)[2])]

  fla_1 <- substitute(DV ~ .)  #formula

  H_Model[[i]] = lm(fla_1, data = H_dataFrame)
  summary_H_Model[[i]] = summary(H_Model[[i]])
  summary_H_Model_Modified = summary_H_Model[[i]] # This variable is defined for the next question
}


# This function will calculate coefficients and their t-score for each variable added in its own step.
# F-statistic for R-square-change in each step equals to t^2 (p-values are the same).

for (i in seq(dim(dataFrame)[2]-1)) {
  summary_H_Model_Modified[["coefficients"]][i+1,]=summary_H_Model[[i]][["coefficients"]][i+1,]
}

if (!is.null(Cat_Var)){
Cat_var_ind=rep(NaN,length(Cat_Var))
for (i in seq(length(Cat_Var))) {
  Cat_var_ind[i] = grep(Cat_Var[i], colnames(dataFrame))

std_dataFrame = as.data.frame(scale(select(dataFrame,-c(Cat_Var[i]))))
std_dataFrame = add_column(std_dataFrame, dataFrame[[Cat_Var[i]]], .after = Cat_var_ind[i]-1)
names(std_dataFrame)[names(std_dataFrame) == "dataFrame[[Cat_Var[i]]]"] = Cat_Var[i]
}
}
else{
  std_dataFrame = as.data.frame(scale(dataFrame))
}
std_H_Model = rep(list(list()), dim(std_dataFrame)[2]-1)
std_summary_H_Model= rep(list(list()), dim(std_dataFrame)[2]-1)
for (i in seq(dim(std_dataFrame)[2]-1)) {
  std_H_dataFrame = std_dataFrame[,c(seq(i),dim(std_dataFrame)[2])]

  fla_2 <- substitute(DV ~ .)  #formula

  std_H_Model[[i]] = lm(fla_2, data = std_H_dataFrame)
  std_summary_H_Model[[i]] = summary(std_H_Model[[i]])
  std_summary_H_Model_Modified = std_summary_H_Model[[i]]
}

for (i in seq(dim(std_dataFrame)[2]-1)) {
  std_summary_H_Model_Modified[["coefficients"]][i+1,]=std_summary_H_Model[[i]][["coefficients"]][i+1,]
}

#print(std_summary_H_Model_Modified)

beta = std_summary_H_Model_Modified[["coefficients"]][,"Estimate"]

# Extracting R-squared and R-squared-change
R_squared = rep(NaN, dim(std_dataFrame)[2])
R_squared_change = rep(NaN, dim(std_dataFrame)[2])

for (i in seq(dim(std_dataFrame)[2]-1)) {
  R_squared[i+1] = std_summary_H_Model[[i]][["r.squared"]]
  R_squared_change[i+1] = ifelse(i == 1, R_squared[i+1], R_squared[i+1]-R_squared[i])
}

# Extracting F-score and p-value for R-squared-change

t_score = std_summary_H_Model_Modified[["coefficients"]][,"t value"]
F_score = std_summary_H_Model_Modified[["coefficients"]][,"t value"]^2
p_value = std_summary_H_Model_Modified[["coefficients"]][,"Pr(>|t|)"]
Coeffs = summary_H_Model_Modified[["coefficients"]][,"Estimate"]
summary_table = data.frame(Beta= beta, Coefficients = as.numeric(Coeffs), R_squared = R_squared,
                           R_squared_change = R_squared_change, t_score = as.numeric(t_score),
                           F_score = as.numeric(F_score), p_value = as.numeric(p_value))
return(summary_table)
#View(summary_table)
}
