library(data.table)
library(ggplot2)
library(data.table)
library(lme4)
library(lmerTest)
library(ordinal)
library(splines)
library(xlsx)

#0) Set directory
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")

#1) Read data
    df <- data.table(readRDS("8 - df.2005.rds"))

#2) Model prep
    #a) set variables
        vars <- c("h_general")
    #b) set models
        l.mod <- list()
        l.mod[[1]] <-  function(form, df) clmm(formula=form, data=df, Hess=T)
    #c) set formulas
        f.raceeduc <- y ~ ego_black2 + ego_age + ego_age2 + ego_female + fam_region + 
          year2 + died + (1 | fam_id_68/ind_id) + log_income + ego_dg_highschool + 
          ego_dg_somecollege + ego_dg_bachelors + ego_dg_advanced + 
          log_wealth + eq_home_d + peq_home + peq_debt + peq_stock + 
          peq_savings + ego_black2:ego_age + ego_black2:ego_age2 + 
          ego_black2:log_income + ego_black2:ego_dg_highschool + ego_black2:ego_dg_somecollege + 
          ego_black2:ego_dg_bachelors + ego_black2:ego_dg_advanced + 
          ego_black2:log_wealth + ego_black2:eq_home_d + ego_black2:peq_home + 
          ego_black2:peq_debt + ego_black2:peq_stock + ego_black2:peq_savings
        
        f.ihs.wealth <- y ~ ego_black2 + ego_age + ego_age2 + ego_female + fam_region + 
          year2 + died + (1 | fam_id_68/ind_id) + log_income + ego_dg_highschool + 
          ego_dg_somecollege + ego_dg_bachelors + ego_dg_advanced + 
          ihs_wealth + eq_home_d + peq_home + peq_debt + peq_stock + 
          peq_savings + ego_black2:ego_age + ego_black2:ego_age2 + 
          ego_black2:log_income + ego_black2:ego_dg_highschool + ego_black2:ego_dg_somecollege + 
          ego_black2:ego_dg_bachelors + ego_black2:ego_dg_advanced + 
          ego_black2:ihs_wealth + ego_black2:eq_home_d + ego_black2:peq_home + 
          ego_black2:peq_debt + ego_black2:peq_stock + ego_black2:peq_savings
            
        f.ihs.components <- y ~ ego_black2 + ego_age + ego_age2 + ego_female + fam_region + 
          year2 + died + (1 | fam_id_68/ind_id) + log_income + ego_dg_highschool + 
          ego_dg_somecollege + ego_dg_bachelors + ego_dg_advanced + 
          ihs_wealth + eq_home_d + ihs_home + ihs_debt + ihs_stock + 
          ihs_savings + ego_black2:ego_age + ego_black2:ego_age2 + 
          ego_black2:log_income + ego_black2:ego_dg_highschool + ego_black2:ego_dg_somecollege + 
          ego_black2:ego_dg_bachelors + ego_black2:ego_dg_advanced + 
          ego_black2:ihs_wealth + ego_black2:eq_home_d + ego_black2:ihs_home + 
          ego_black2:ihs_debt + ego_black2:ihs_stock + ego_black2:ihs_savings
        
        f.interaction.components <- y ~ ego_black2 + ego_age + ego_age2 + ego_female + fam_region + 
          year2 + died + (1 | fam_id_68/ind_id) + log_income + ego_dg_highschool + 
          ego_dg_somecollege + ego_dg_bachelors + ego_dg_advanced + 
          ihs_wealth + eq_home_d + ihs_home + ihs_debt + ihs_stock + 
          ihs_savings + ego_black2:ego_age + ego_black2:ego_age2 + 
          ego_black2:log_income + ego_black2:ego_dg_highschool + ego_black2:ego_dg_somecollege + 
          ego_black2:ego_dg_bachelors + ego_black2:ego_dg_advanced + 
          ego_black2:ihs_wealth + ego_black2:eq_home_d + ego_black2:ihs_home + 
          ego_black2:ihs_debt + ego_black2:ihs_stock + ego_black2:ihs_savings +
          ihs_wealth * ego_black2 * (eq_home_d + ihs_home + ihs_debt + ihs_stock + ihs_savings)
        
        knot.vals <- quantile(df$ihs_wealth, c(.25,.5,.75))  
        f.ihs.wealth.splines <- y ~ ego_black2 + ego_age + ego_age2 + ego_female + fam_region + 
          year2 + died + (1 | fam_id_68/ind_id) + log_income + ego_dg_highschool + 
          ego_dg_somecollege + ego_dg_bachelors + ego_dg_advanced + 
          ns(ihs_wealth, knots = knot.vals) + eq_home_d + peq_home + peq_debt + peq_stock + 
          peq_savings + ego_black2:ego_age + ego_black2:ego_age2 + 
          ego_black2:log_income + ego_black2:ego_dg_highschool + ego_black2:ego_dg_somecollege + 
          ego_black2:ego_dg_bachelors + ego_black2:ego_dg_advanced + 
          ego_black2:ns(ihs_wealth, knots = knot.vals) + ego_black2:eq_home_d + ego_black2:peq_home + 
          ego_black2:peq_debt + ego_black2:peq_stock + ego_black2:peq_savings
        
        f.ihs.wealth.interaction.splines <- y ~ ego_black2 + ego_age + ego_age2 + ego_female + fam_region + 
          year2 + died + (1 | fam_id_68/ind_id) + log_income + ego_dg_highschool + 
          ego_dg_somecollege + ego_dg_bachelors + ego_dg_advanced + 
          ns(ihs_wealth, knots = knot.vals) + eq_home_d + ihs_home + ihs_debt + ihs_stock + 
          ihs_savings + ego_black2:ego_age + ego_black2:ego_age2 + 
          ego_black2:log_income + ego_black2:ego_dg_highschool + ego_black2:ego_dg_somecollege + 
          ego_black2:ego_dg_bachelors + ego_black2:ego_dg_advanced + 
          ego_black2:ns(ihs_wealth, knots = knot.vals) + ego_black2:eq_home_d + ego_black2:ihs_home + 
          ego_black2:ihs_debt + ego_black2:ihs_stock + ego_black2:ihs_savings +
          ns(ihs_wealth, knots = knot.vals) * ego_black2 * (eq_home_d + ihs_home + ihs_debt + ihs_stock + ihs_savings)
        
        l.forms <- list(f.raceeduc, f.ihs.wealth, f.ihs.components, f.interaction.components, f.ihs.wealth.splines, f.ihs.wealth.interaction.splines)
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
            keeps <- c(vars, keeps, "year", "already_died", "h_general")
            keeps <- setdiff(keeps, c("nsihs_wealth","knots=knot.vals"))
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
            data[, three.obs.since.1984 := ind_id %in% df.1984[years.after.1984 == T & !already_died, .N , by = "ind_id"][N >= 3, ind_id]]
            data <- data[three.obs.since.1984 == T, ]
          }
      #run model
          mod(form, df = data)
    }

        
#4) Run and save models
    l.results <- list()
      for(j in 1: length(l.forms)){
        l.results[[j]] <- mod.fun(df, vars[1], l.mod[[1]], l.forms[[j]], year = 2005)
        print(j)
      }
    saveRDS(l.results, "ap.4.results.rds")

        
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
          table <- list()
          table <- lapply(l.results, pretty.mod.fun)
        #d) append results to one table
          for(i in 1:length(table)){
            names(table[[i]])[2:3] <- paste(names(table[[i]])[2:3], i, sep=".")
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
                "log_income",
                "ego_black2:log_income",
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
                "log_wealth",
                "ihs_wealth",
                "ego_black2:log_wealth",
                "ego_black2:ihs_wealth",
                "",
                "eq_home_d",
                "peq_home",
                "peq_savings",
                "peq_stock",
                "peq_debt",
                "ihs_home",
                "ihs_savings",
                "ihs_stock",
                "ihs_debt",
                "ihs_wealth:eq_home_d",
                "ihs_wealth:ihs_home",
                "ihs_wealth:ihs_savings",
                "ihs_wealth:ihs_stock",
                "ihs_wealth:ihs_debt",
                "",
                "ego_black2:eq_home_d",
                "ego_black2:peq_home",
                "ego_black2:peq_savings",
                "ego_black2:peq_stock",
                "ego_black2:peq_debt",
                "ego_black2:ihs_home",
                "ego_black2:ihs_savings",
                "ego_black2:ihs_stock",
                "ego_black2:ihs_debt",
                "ego_black2:ihs_wealth:eq_home_d",
                "ego_black2:ihs_wealth:ihs_home",
                "ego_black2:ihs_wealth:ihs_savings",
                "ego_black2:ihs_wealth:ihs_stock",
                "ego_black2:ihs_wealth:ihs_debt",
                
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
            write.xlsx(table, "C:/Users/admin/Desktop/Ap.4 - Wealth model variants.xlsx", row.names=FALSE)
          
