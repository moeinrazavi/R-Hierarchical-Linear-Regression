#' @title Hierachical Regression in Sets
#'
#' @description This function does automatic hierachical regression in sets (groups of predictors automatically entered for regression).
#'  This function applies hierachical regression on a "list" of "dataFrame"s with all groups of predictors included in that "list"
#'
#' @param 1. "list" of "dataFrames" (each group of predictors must be included in one "dataFrame" and all these group "dataFrames" must be assigned to a "list"),
#'        2. Dependent variable (Numeric Vector)
#'        3. categorical variables in the dataFrame (if any)
#'
#' @return Hierarchical Regression summary table for sets
#'
#' @examples 1. group_hierarchical(list(happinessdata[1:3],happinessdata[4:5],happinessdata[6]), happinessdata$happiness_score, "gender")
#'
#'           2. if there are more than one categorical variables -->
#'               group_hierarchical(list(groupDF1, groupDF2, groupDF3), DependentVariable, c("Categorical_var_1", "Categorical_var_2"))
#'
#'           3. if there is no categorical variable -->
#'               group_hierarchical(list(groupDF1, groupDF2, groupDF3), DependentVariable, c())
#'
#' @export


group_hierarchical <- function(groups_input,DV,Cat_Var){

  # Grouped Hierarchical Model
  library(dplyr)
  library(tibble) # library for adding a column in middle of a dataframe

  groups_dataFrame = as.data.frame(groups_input)


  if (!is.null(Cat_Var)){
    Cat_var_ind=rep(NaN,length(Cat_Var))

    for (i in seq(length(Cat_Var))){

      Cat_var_ind[i] = grep(Cat_Var[i], colnames(groups_dataFrame))

    }
  }
  else{
    std_groups_dataFrame = as.data.frame(scale(groups_dataFrame))
    Cat_var_ind=c(NaN)
  }

  if (!is.nan(Cat_var_ind[1])){
    std_groups_dataFrame = as.data.frame(scale(select(groups_dataFrame,-Cat_Var)))
    for (i in seq(length(Cat_Var))){
    std_groups_dataFrame = add_column(std_groups_dataFrame, groups_dataFrame[[Cat_Var[i]]], .after = Cat_var_ind[i]-1)
    names(std_groups_dataFrame)[names(std_groups_dataFrame) == "groups_dataFrame[[Cat_Var[i]]]"] = Cat_Var[i]
    }
  }

  std_groups_input = rep(list(list()), length(groups_input))
  std_DV = as.numeric(scale(DV))

  for (i in seq(length(groups_input))){
    std_groups_input[[i]] = select(std_groups_dataFrame, names(groups_input[[i]]))
  }

  # Happiness data grouped into desired groups and DV

  group_H_Model = rep(list(list()), length(groups_input))
  group_summary_H_Model= rep(list(list()), length(groups_input))

  for (i in seq(length(groups_input))) {
    group_H_happiness = as.data.frame(groups_input[c(seq(i))])
    group_H_Model[[i]] = lm(DV ~ ., data = group_H_happiness)
    group_summary_H_Model[[i]] = summary(group_H_Model[[i]])

    # This variable is defined for the following "for"" loop
    group_summary_H_Model_Modified = group_summary_H_Model[[i]]
    print(group_summary_H_Model[[i]])
  }
  # Standardized Grouped Hierarchical Model

  # Happiness data grouped into desired groups and DV

  std_group_H_Model = rep(list(list()), length(std_groups_input)-1)
  std_group_summary_H_Model= rep(list(list()), length(std_groups_input)-1)

  for (i in seq(length(std_groups_input))) {
    std_group_H_happiness = as.data.frame(std_groups_input[c(seq(i))])
    std_group_H_Model[[i]] = lm(std_DV ~ ., data = std_group_H_happiness)
    std_group_summary_H_Model[[i]] = summary(std_group_H_Model[[i]])

    # This variable is defined for the following "for"" loop
    std_group_summary_H_Model_Modified = std_group_summary_H_Model[[i]]
    #print(std_group_summary_H_Model[[i]])
  }


  # This part makes the corrected version of summary table


  F_score = rep(NaN, nrow(group_summary_H_Model_Modified[["coefficients"]]))
  group_p_value = rep(NaN, nrow(group_summary_H_Model_Modified[["coefficients"]]))
  group_R_squared = rep(NaN, nrow(group_summary_H_Model_Modified[["coefficients"]]))
  group_R_squared_change = rep(NaN, nrow(group_summary_H_Model_Modified[["coefficients"]]))

  k = 1
  l = 1
  for (j in seq(length(groups_input))) {

    l=k

    n_row = ifelse(j==1,nrow(group_summary_H_Model[[j]][["coefficients"]])-1,
                   nrow(group_summary_H_Model[[j]][["coefficients"]])-
                     nrow(group_summary_H_Model[[j-1]][["coefficients"]]))

    for (i in seq(n_row)) {

      group_summary_H_Model_Modified[["coefficients"]][k+1,]=
        group_summary_H_Model[[j]][["coefficients"]][k+1,]

      group_R_squared[k+1] = group_summary_H_Model[[j]][["r.squared"]]

      group_R_squared_change[k+1] = ifelse(l == 1, group_R_squared[l+1],
                                           group_R_squared[l+1]-group_R_squared[l])

      F_score[k+1] = group_R_squared_change[k+1]/(1-group_R_squared[k+1])*
        group_summary_H_Model[[j]][["fstatistic"]][["dendf"]]/n_row
      # n_row is the number of predictors
      # F = DELTA(R^2)/(1-R^2_CURRENT)*dendf/numdf
      # num_df: number of predictors in each level

      group_p_value[k+1] = pf(F_score[k+1],n_row,
                              group_summary_H_Model[[j]][["fstatistic"]][["dendf"]],
                              lower.tail = FALSE)

      k = k+1
    }

  }

  # for standard coefficients
  k = 1
  for (j in seq(length(std_groups_input))) {


    std_n_row = ifelse(j==1,nrow(std_group_summary_H_Model[[j]][["coefficients"]])-1,
                       nrow(std_group_summary_H_Model[[j]][["coefficients"]])-
                         nrow(std_group_summary_H_Model[[j-1]][["coefficients"]]))

    for (i in seq(std_n_row)) {

      std_group_summary_H_Model_Modified[["coefficients"]][k+1,]=
        std_group_summary_H_Model[[j]][["coefficients"]][k+1,]

      k = k+1
    }

  }


  group_summary_H_Model_Modified[["coefficients"]]=
    cbind(group_summary_H_Model_Modified[["coefficients"]],
          Group_R_squared = group_R_squared, Group_R_squared_change = group_R_squared_change,
          Group_F_score = F_score, Group_p_value = group_p_value)

  group_summary_table = as.data.frame(group_summary_H_Model_Modified[["coefficients"]])

  group_beta = as.numeric(std_group_summary_H_Model_Modified[["coefficients"]][,"Estimate"])
  group_summary_table = add_column(group_summary_table, Beta = group_beta, .before = 1)
  #group_summary_table = select(group_summary_table,-c("t value", "Std. Error", "Pr(>|t|)"))
  names(group_summary_table)[names(group_summary_table) == "Estimate"] = "Coefficients"
  #print(group_summary_table)

  #View(group_summary_table)
return(group_summary_table)
}
