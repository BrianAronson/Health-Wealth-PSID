#For each model, predict health by race and proportion assets in savings. Unfortunately, the clmm predict function is bugged, so have to do this manually.

library(ggplot2)
library(data.table)
library(plm)
library(pglm)
library(lme4)
library(nlme)
library(lmerTest)
library(cowplot)
library(car)
library(xlsx)
library(ordinal)
library(R2admb) #zero-infalted
library(glmmADMB) #zero-inflated models
library(ordinal)
library(merTools)
library(ggthemes)

#0) Set directory
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")

    
#1) read model and raw data
    df.modll <- readRDS("l.results.2005.rds")
    df <- data.table(readRDS("8 - df.2005.rds"))
    
    
#2) Just keep model 7
    df.modll <- list(lapply(df.modll, function(x) x[[7]]))
    df.modll <- unlist(df.modll, recursive = F)
    
    # vars <- names(df)[names(df) %in% c(df.mod[[1]]$var)]
    # df[1:42, (vars) := df.manip[, vars, with = F]]
    # df[1:42, ind_id := df$ind_id[1]]
    # df[1:42, fam_id_68 := df$fam_id_68[1]]
    # df2 <- df[1:42, ]
    # predict(df.modll[[2]], df2, re.form = NA)
    # predictInterval(df.modll[[2]], df2, n.sims = 100)
    
    
#3) put all model results into a data.frame
    #i) identify variables
        vars <- c("Self-Rated Health",
                  "# of Conditions",
                  "Work Limitations",
                  "Disability",
                  "Distress",
                  "BMI"
                )
    #ii) for each dv and for each model, pull out info
        df.mod <- list()
          for(i in 1:length(df.modll)) {
            tdf <- data.frame(summary(df.modll[[i]])$coefficient)
            tdf <- tdf[,c(1:2, ncol(tdf))]
            tdf$var <- row.names(tdf)
            tdf <- data.table(tdf)
            names(tdf) <- c("est", "se", "pr", "var")
            tdf[, ':='(dv = vars[i])]
            df.mod[[i]] <- tdf
          }

        
#4) identify bounds and significance
    for(i in 1:length(df.mod)){
      df.mod[[i]][, ':='(
        CIlower = est - se * 1.96,
        CIupper = est + se * 1.96,
        significant = ifelse(abs(pr) < .05, "black", "grey")
      )]
    }
        
        
#5) make a dataset to base predictions on 
    #a) reduce df to just outcomes of interest
        df <- df[, names(df) %in% c(df.mod[[1]]$var), with = F]
    #b) find the median of everything.
        df.manip <- df[, lapply(.SD, function(x) ifelse(class(x)=="factor", x[1], median(x)))]
    #c) add a row where ego_black2 == 1
        df.manip <- df.manip[rep(1, 2), ]
        df.manip[2, ego_black2 := 1]
    #d) add rows for 20 unique values of ihs_savings
        df.manip <- df.manip[rep(1:2, each = 21), ]
        savings.range <- max(df$ihs_savings) - min(df$ihs_savings)
        ihs_savings.vals <- min(df$ihs_savings) + 0:20 * savings.range/20
        df.manip[, ihs_savings := rep(ihs_savings.vals, 2)]
    #e) add variables for interactions
        df.manip[, ':='(
          `ego_black2:ego_age` = ego_black2 * ego_age,
          `ego_black2:ego_age2` = ego_black2 * ego_age2,
          `ego_black2:ego_dg_highschool` = ego_black2 * ego_dg_highschool,
          `ego_black2:ego_dg_somecollege` = ego_black2 * ego_dg_somecollege,
          `ego_black2:ego_dg_bachelors` = ego_black2 * ego_dg_bachelors,
          `ego_black2:ego_dg_advanced` = ego_black2 * ego_dg_advanced,
          `ego_black2:ihs_income` = ego_black2 * ihs_income,
          `ego_black2:ihs_wealth` = ego_black2 * ihs_wealth,
          `ego_black2:eq_home_d` = ego_black2 * eq_home_d,
          `ego_black2:ihs_home` = ego_black2 * ihs_home,
          `ego_black2:ihs_debt` = ego_black2 * ihs_debt,
          `ego_black2:ihs_stock` = ego_black2 * ihs_stock,
          `ego_black2:ihs_savings` = ego_black2 * ihs_savings
        )]
        
        
#6) make predicted estimates for each manipulation and each model + CIs
    l.df.pred <- list()
    for(j in 1: length(df.mod)){
      #a) grab model
          x <- df.mod[[j]]
      #b) cop structure of manipulations
          df.pred <- copy(df.manip)
      #c) pull variable names
          keyvars <- c("ihs_savings")#, "ego_black2:ihs_savings")
          pred.vars <- setdiff(names(df.pred), keyvars)
      #d) for each variable, multiply estimates by manipulations to get variable-specific predicted values
          for(i in 1:length(pred.vars)){
              df.pred[, (pred.vars[i]) := x$est[x$var == pred.vars[i]] * .SD, .SDcols = pred.vars[i]]
          }
      #e) get EST and confidence estimates for savings
          df.pred[, UCI := x$CIupper[x$var == "ihs_savings"] * ihs_savings]
          df.pred[, EST := x$est[x$var == "ihs_savings"] * ihs_savings]
          df.pred[, LCI := x$CIlower[x$var == "ihs_savings"] * ihs_savings]
      #f) add intercept
          df.pred$intercept <- x$est[x$var == "(Intercept)" | x$var == "2|3"]
      #g) sum variable based predictions
          df.pred$preds <- rowSums(df.pred[, setdiff(names(df.pred), c("UCI", "EST", "LCI", "ihs_savings")), with = F])
          df.pred[, EST := preds + EST]
          df.pred[, UCI := preds + UCI]
          df.pred[, LCI := preds + LCI]
      #h) append to list
          l.df.pred[[j]] <- df.pred
    }

  
    
#7) format predictions
    tempdfl <- list()
    for(i in 1:length(l.df.pred)){
      tempdfl[[i]] <- data.table(
                        Estimate.x = df.manip$ihs_savings, 
                        Estimate.y = l.df.pred[[i]]$EST, 
                        Black = df.manip$ego_black2,
                        DV = df.mod[[i]]$dv[1],
                        UCI = l.df.pred[[i]]$UCI,
                        LCI = l.df.pred[[i]]$LCI
                      )
    }


#8) Graph
    plotfun4 <- function(xr){
        p <- ggplot(tempdfl[[xr]], aes(x=Estimate.x, y=Estimate.y, group=Black, fill = as.factor(Black) ,color=as.factor(Black)))+
        # geom_line(aes(y = UCI), size=1, linetype = "dotted") +
        # geom_line(aes(y = LCI), size=1, linetype = "dotted") +
        #   
        geom_ribbon(aes(ymin=LCI, ymax=UCI), alpha = .2, color = NA) +
          
        geom_line(size=2) +
          
          scale_color_manual(values=c("grey", "black"))+
          scale_fill_manual(values=c("grey", "black"))+
          theme_pander()+
                theme(
                      axis.line.x = element_line(colour = "black"),
                      axis.line.y = element_line(colour = "black"),
                      plot.title = element_text(size=15, hjust=0.5),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank(),
                      strip.text.x = element_blank(),
                      legend.position="none",
                      panel.spacing = unit(2.5, "lines"),
                      axis.ticks= element_blank(),
                      text=element_text(family="serif"),
                      plot.margin = unit(c(l=2, t=.2, b=.2, 2), "lines")
                      )
          if(xr==1){
            p + scale_y_continuous(breaks=c(-4.5, -5.0, -5.5, -6.0)) +
            coord_cartesian(ylim = c(-6.0, -4.4)) +
            ggtitle("Self-Rated Health (Ordinal)")
          }else if(xr==2){
            p + scale_y_continuous(breaks=c(-.5, -.6, -.7, -.8, -.9)) +
            coord_cartesian(ylim = c(-.9, -.5)) +
            ggtitle("# of Conditions (Poisson)")
          }else if(xr==3){
            p + scale_y_continuous(breaks=c(-1.5, -2, -2.5, -3.0)) +
            coord_cartesian(ylim = c(-3, -1.45)) +
            ggtitle("Work Limitations (Poisson)")
          }else if(xr==4){
            p + scale_y_continuous(breaks=c(-3.5, -4.0, -4.5, -5.0, -5.5)) +
            coord_cartesian(ylim = c(-5.5, -3.45)) +
            ggtitle("Disability (Logit)")
          }else if(xr==5){
            p + scale_y_continuous(breaks=c(-.75, -1.0, -1.25, -1.5)) + 
            coord_cartesian(ylim = c(-1.5, -.7)) +
            ggtitle("Distress (Poisson)")
          }else if(xr==6){
            p + scale_y_continuous(breaks=c(28.0, 28.5, 29.0, 29.5, 30.0)) +
            coord_cartesian(ylim = c(28, 30)) +
            ggtitle("BMI (OLS)")
          }
      }
    m1 <- plotfun4(1)
    m2 <- plotfun4(2)
    m3 <- plotfun4(3)
    m4 <- plotfun4(4)
    m5 <- plotfun4(5)
    m6 <- plotfun4(6)
    p <- plot_grid(m1, m2, m3, m4, m5, m6, ncol=3)
    title  <-  ggdraw() + draw_label("Figure 3 - Predicted Values of Health Characteristic by Proportion Wealth in Savings", fontface='bold', size=20, fontfamily="serif")
    p <- plot_grid(title, p, nrow=2, rel_heights=c(0.07, 1)) #put title above graph
    p <- plot_grid(p, scale = .98)
    save_plot(fname <- "C:/Users/bda13/Desktop/Figure 3 - Predicted Values of Health Characteristic by Proportion Wealth in Savings.png", p,
              base_width = 11,
              base_height = 7)
    browseURL(fname)
    
