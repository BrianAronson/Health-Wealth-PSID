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
    df <- data.table(readRDS("8 - df.1984.rds"))

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


#3) Create function to run models
    mod.fun <- function(data, dv, mod, form, year = 2005){
      ##set y to dv and remove missing rows of dv
          data[ , y := .SD, .SDcols = dv]
          data <- data[!is.na(y), ]
      #remove unique ids with less than 2 or 3 occurrences
          if(year == 2005){
            data[, two.obs.since.2005 := ind_id %in% df[year >= 2005 & year <= 2011 & !already_died, .N , by = "ind_id"][N >= 2, ind_id]]
            data <- data[two.obs.since.2005 == T, ]
          }else{
            data[, three.obs.since.1984 := ind_id %in% df[year %in% c(1984, 1989, 1994, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015) & !already_died, .N , by = "ind_id"][N >= 3, ind_id]]
            data <- data[three.obs.since.1984 == T, ]
          }
      #run model
          mod(form, df = data)
    }


#4) Run and save models
    l.results <- list()
    for(i in 1:length(vars)){
      l.init.results <- list()
      for(j in 1: length(l.forms)){
        l.init.results[[j]] <- mod.fun(df, vars[i], l.mod[[i]], l.forms[[j]], year = 1984)
        print(paste(i,"-",j))
      }
      l.results[[i]] <- l.init.results
    }
    saveRDS(l.results, "l.results.1984.rds")


#5) Reduce to var name, estimate, and stars
    #a) Create stars function
      prstars<-function(x){
        ifelse(x<.001,"***",ifelse(x<.01,"**",ifelse(x<.05,"*","")))
    }
    #b) Create pretty model function
      pretty.mod.fun<-function(model){
        sum.mod <- summary(model)
        sum.mod <- as.data.frame(sum.mod$coefficients)
        sum.mod$Variable <- row.names(sum.mod)
        sum.mod <- data.table(sum.mod)
        setnames(sum.mod,
                 "Pr(>|t|)",
                 "Pr(>|z|)",
                 skip_absent=TRUE)
        model.format <- sum.mod[,.(
          var = Variable,
          coef = sapply(Estimate,function(x) sprintf("%.3f", x)),
          # se = sapply(`Std. Error`,function(x) paste("(",sprintf("%.2f", x),")",sep="")),
          pval = prstars(as.numeric(gsub("<","",`Pr(>|z|)` )))
        ) ]
        return(model.format)
      }
    #c) Run functions for each table
      for(i in 1:length(l.results)){
          table <- list()
          table <- lapply(l.results[[i]], pretty.mod.fun)
        #d) append results to one table
          for(j in 1:length(table)){
            names(table[[j]])[2:3] <- paste(names(table[[j]])[2:3], j, sep=".")
          }
          table <- Reduce(function(x, y) merge(x, y, all=TRUE, by = "var"), table)
        #e) remove NAs
          table <- table[, lapply(.SD, function(x) ifelse(is.na(x), "", x))]
        #f) order vars
          #i) create empty row to append to row orders
              tmp <- data.frame(table[1,])
              tmp[1, ] <- ""
              table <- rbind(table, tmp)
          #ii) create variable name order
              ords <- c(
                "ego_black2",
                "ego_age",
                "ego_age2",
                "ego_female",
                "ego_black2:ego_age",
                "ego_black2:ego_age2",
                "fam_region",
                "year2",
                "died",
                "ihs_income",
                "ego_black2:ihs_income",
                "",
                "ego_dg_highschool",
                "ego_dg_somecollege",
                "ego_dg_bachelors",
                "ego_dg_advanced",
                "",
                "ego_black2:ego_dg_highschool",
                "ego_black2:ego_dg_somecollege",
                "ego_black2:ego_dg_bachelors",
                "ego_black2:ego_dg_advanced",
                "ihs_wealth",
                "ego_black2:ihs_wealth",
                "",
                "eq_home_d",
                "ihs_home",
                "ihs_savings",
                "ihs_stock",
                "ihs_debt",
                "",
                "ego_black2:eq_home_d",
                "ego_black2:ihs_home",
                "ego_black2:ihs_savings",
                "ego_black2:ihs_stock",
                "ego_black2:ihs_debt",
                ""
              )
          #iii) append any variables not included above
              ords <- c(ords, table$var[!table$var %in% ords])
          #iv) create row order index
              ind <- match(ords, table$var)
              ind <- ind[!is.na(ind)]
          #v) reorder table
              table <- table[ind, ]
        #g) Force variables to be read by csv as characters
          num.cols <- names(table)
          table[,(num.cols) := lapply(.SD, function(x) paste("'", x, sep = "")), .SDcols = num.cols]
        #h) Save results
          if(i == 1){
            write.xlsx(table, "C:/Users/admin/Desktop/Table 2 - Models 1984.xlsx", sheetName = vars[i], row.names=FALSE)
          }else{
            write.xlsx(table, "C:/Users/admin/Desktop/Table 2 - Models 1984.xlsx", sheetName = vars[i], row.names=FALSE, append=TRUE)
          }
      }

