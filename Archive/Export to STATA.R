library(data.table)
library(ggplot2)
library(data.table)
library(lme4)
library(lmerTest)
library(ordinal)
library(xlsx)

#0) Set directory
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")

#1) Read data
    df <- data.table(readRDS("8 - df.2005.rds"))

#2) Model prep
    #a) set variables
        vars <- c("h_general",
                  "h_conditions",
                  "h_lim_work",
                  "h_disabled",
                  "h_distress",
                  "h_BMI")
    #b) set models
        l.mod <- list()
        l.mod[[1]] <-  function(form, df) clmm(formula = form, data = df, Hess = T)
        l.mod[[2]] <-  function(form, df) glmer(formula = form, data = df, family = poisson, nAGQ = 0)
        l.mod[[3]] <-  function(form, df) clmm(formula = form, data = df, Hess = T)
        l.mod[[4]] <-  function(form, df) glmer(formula = form, data = df, family = binomial, nAGQ = 0)
        l.mod[[5]] <-  function(form, df) glmer(formula = form, data = df, family = poisson, nAGQ = 0)
        l.mod[[6]] <-  function(form, df) lmer(formula = form, data = df)
    #c) set formulas
        form1 <- y ~ ego_black2 + ego_age + ego_age2 + ego_female + ego_black2 * (ego_age + ego_age2) + fam_region + year2 + died  + (1 | fam_id_68 / ind_id)
        form2 <- update(form1, ~ . +
            ihs_income  + ego_dg_highschool + ego_dg_somecollege + ego_dg_bachelors + ego_dg_advanced)
        form2b <- update(form2, ~ . +
            ego_black2 * (ihs_income  + ego_dg_highschool + ego_dg_somecollege + ego_dg_bachelors + ego_dg_advanced) )
        form3 <- update(form2b, ~ . +
            ihs_wealth)
        form3b <- update(form3, ~ . +
            ihs_wealth:ego_black2)
        form4 <- update(form3b, ~ . +
            eq_home_d + ihs_home + ihs_debt + ihs_stock + ihs_savings)
        form4b <- update(form4, ~ . +
            ego_black2:(eq_home_d + ihs_home + ihs_debt + ihs_stock + ihs_savings))
        l.forms <- list(form1, form2, form2b, form3, form3b, form4, form4b)
    #d) remove unecessary variables from df
        #i) Create function to identify variables in formula
            text.form <- function(formula){
              text <- paste(formula, collapse = "+")
              text <- gsub("\\~|\\+|\\|", ",", text)
              text <- gsub(" ", "", text)
              text <- gsub("^,,", "", text)
              text <- gsub("Surv\\(", "", text)
              text <- gsub("\\)", "", text)
              text <- gsub(",,", ",", text)
              text <- gsub("\\(1\\,","", text)
              text <- gsub("\\/",",", text)
              text <- gsub("\\*",",", text)
              text <- gsub("\\(","", text)
              text <- gsub("\\)","", text)
              text <- gsub("\\:",",", text)
              text <- gsub("\\\n","", text)
              unlist(strsplit(text, ","))
            }
        #ii) identify unique variables across all formulas
            keeps <- unique(unlist(sapply(l.forms, text.form)))
        #iii) remove y and append vars
            keeps <- keeps[-1]
            keeps <- c(vars, keeps, "year", "already_died")
        #iv) subset data
            df <- df[, c(keeps), with = F]

#3) convert to SAS
    PSID.Sample <- copy(df)
    setnames(PSID.Sample,
             old = c(
               "h_general",
               "h_conditions",
               "h_lim_work",
               "h_disabled",
               "h_distress",
               "h_BMI",
               "ego_black2",
               "ego_age",
               "ego_age2",
               "ego_female",
               "fam_region",
               "year2",
               "died",
               "fam_id_68",
               "ind_id",
               "ihs_income",
               "ego_dg_highschool",
               "ego_dg_somecollege",
               "ego_dg_bachelors",
               "ego_dg_advanced",
               "ihs_wealth",
               "eq_home_d",
               "ihs_home",
               "ihs_debt",
               "ihs_stock",
               "ihs_savings",
               "year",
               "already_died"
             ),
             new = c(
               "health_self_rated",
               "health_number_conditions",
               "health_work_limitations",
               "health_disability",
               "health_distress",
               "health_BMI",
               "race_black",
               "age_centered",
               "age_centered_squared",
               "female",
               "region_south",
               "year_centered",
               "died",
               "fam_id_68",
               "ind_id",
               "fin_income",
               "educ_highschool",
               "educ_somecollege",
               "educ_bachelors",
               "educ_advanced",
               "fin_wealth",
               "fin_own_home",
               "fin_home_equity",
               "fin_debt",
               "fin_stocks",
               "fin_savings",
               "year",
               "previously_died"
             )
      )
    
    PSID.Sample <- PSID.Sample[, order(names(PSID.Sample)), with = F]
    write.dta(PSID.Sample, "C:/Users/admin/Desktop/PSID.Sample.dta")
    
            
#4) stata code      
    do.code <- "
*Notes:
*a) This is my best attempt at converting the final R models to STATA code, but I could be wrong.
*b) The data contains NAs on some DVs. 
*c) The syntax below assumes that STATA listwise deletes them. 
*d) To get convergence in the more complex poisson and binomial models, I had set the number of adaptive Gauss-Hermite quadrature points to 0. The results are identical in simpler models without this option, but this might be worth noting if you run into convergence issues. STATA might have a similar option.

*read data
  use PSID.Sample.dta

*cumulative link mixed model for self-rated health.
  meologit health_self_rated race_black age_centered age_centered_squared female region_south year_centered died fin_income educ_highschool educ_somecollege educ_bachelors educ_advanced fin_wealth fin_own_home fin_home_equity fin_debt fin_stocks fin_savings race_black#age_centered race_black#age_centered_squared race_black#fin_income race_black#educ_highschool race_black#educ_somecollege race_black#educ_bachelors race_black#educ_advanced race_black#fin_wealth race_black#fin_own_home race_black#fin_home_equity race_black#fin_debt race_black#fin_stocks race_black#fin_savings || fam_id_68: || ind_id:
  
*poisson mixed model for number conditions.
  mepoisson health_number_conditions race_black age_centered age_centered_squared female region_south year_centered died fin_income educ_highschool educ_somecollege educ_bachelors educ_advanced fin_wealth fin_own_home fin_home_equity fin_debt fin_stocks fin_savings race_black#age_centered race_black#age_centered_squared race_black#fin_income race_black#educ_highschool race_black#educ_somecollege race_black#educ_bachelors race_black#educ_advanced race_black#fin_wealth race_black#fin_own_home race_black#fin_home_equity race_black#fin_debt race_black#fin_stocks race_black#fin_savings || fam_id_68: || ind_id:
  
*cumulative link mixed model for work limitations.
  meologit health_work_limitations race_black age_centered age_centered_squared female region_south year_centered died fin_income educ_highschool educ_somecollege educ_bachelors educ_advanced fin_wealth fin_own_home fin_home_equity fin_debt fin_stocks fin_savings race_black#age_centered race_black#age_centered_squared race_black#fin_income race_black#educ_highschool race_black#educ_somecollege race_black#educ_bachelors race_black#educ_advanced race_black#fin_wealth race_black#fin_own_home race_black#fin_home_equity race_black#fin_debt race_black#fin_stocks race_black#fin_savings || fam_id_68: || ind_id:
  
*binomial mixed model for disability.
  melogit health_disability race_black age_centered age_centered_squared female region_south year_centered died fin_income educ_highschool educ_somecollege educ_bachelors educ_advanced fin_wealth fin_own_home fin_home_equity fin_debt fin_stocks fin_savings race_black#age_centered race_black#age_centered_squared race_black#fin_income race_black#educ_highschool race_black#educ_somecollege race_black#educ_bachelors race_black#educ_advanced race_black#fin_wealth race_black#fin_own_home race_black#fin_home_equity race_black#fin_debt race_black#fin_stocks race_black#fin_savings || fam_id_68: || ind_id:
  
*poisson mixed model for distress.
  mepoisson health_distress race_black age_centered age_centered_squared female region_south year_centered died fin_income educ_highschool educ_somecollege educ_bachelors educ_advanced fin_wealth fin_own_home fin_home_equity fin_debt fin_stocks fin_savings race_black#age_centered race_black#age_centered_squared race_black#fin_income race_black#educ_highschool race_black#educ_somecollege race_black#educ_bachelors race_black#educ_advanced race_black#fin_wealth race_black#fin_own_home race_black#fin_home_equity race_black#fin_debt race_black#fin_stocks race_black#fin_savings || fam_id_68: || ind_id:
  
*Linear mixed model for BMI.
  mixed health_BMI race_black age_centered age_centered_squared female region_south year_centered died fin_income educ_highschool educ_somecollege educ_bachelors educ_advanced fin_wealth fin_own_home fin_home_equity fin_debt fin_stocks fin_savings race_black#age_centered race_black#age_centered_squared race_black#fin_income race_black#educ_highschool race_black#educ_somecollege race_black#educ_bachelors race_black#educ_advanced race_black#fin_wealth race_black#fin_own_home race_black#fin_home_equity race_black#fin_debt race_black#fin_stocks race_black#fin_savings || fam_id_68: || ind_id:
      "

            