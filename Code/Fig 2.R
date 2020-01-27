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
library(ggthemes)

#0) Set directory
    setwd("C:/Users/admin/Desktop/Sociology/PSID Data")

    
#1) read models
    dfll <- readRDS("l.results.2005.rds")
    
    
#2) put all model results into a data.frame
    #i) identify variables
        vars <- c("Self-Rated Health",
                  "# of Conditions",
                  "Work Limitations",
                  "Disability",
                  "Distress",
                  "BMI"
                )

    #ii) for each dv and for each model, pull out info and append to one df
        df <- list()
        for(j in 1:length(dfll)) {
          dfl <- dfll[[j]]
          for(i in 1:length(dfl)) {
            tdf <- data.frame(summary(dfl[[i]])$coefficient)
            tdf <- tdf[,c(1:2, ncol(tdf))]
            tdf$var <- row.names(tdf)
            tdf <- data.table(tdf)
            names(tdf) <- c("est", "se", "pr", "var")
            tdf[, ':='(dv = vars[j], model = i)]
            dfl[[i]] <- tdf
          }
          dfl <- rbindlist(dfl)
          df[[j]] <- dfl
        }
        df <- rbindlist(df)

        
#3) format dataframe
    #a) remove control variables
        ivs <- c("eq_home_d", "ihs_home", "ihs_debt", "ihs_stock", "ihs_savings", "ego_black2:eq_home_d", "ego_black2:ihs_home", "ego_black2:ihs_debt", "ego_black2:ihs_stock", "ego_black2:ihs_savings")
        df <- df[var %in% ivs, ]
    #b) identify bounds and significance
        df[, ':='(
          CIlower = est - se * 1.96,
          CIupper = est + se * 1.96,
          significant = ifelse(abs(pr) < .05, "black", "grey")
        )]
    #c) relabel var names
        df[, var := 
            ifelse(var == "eq_home_d", "Own Home",
            ifelse(var == "ihs_home", "Home Equity",
            ifelse(var == "ihs_debt", "Debt",
            ifelse(var == "ihs_stock", "Stocks",
            ifelse(var == "ihs_savings", "Savings",
            ifelse(var == "ego_black2:eq_home_d", "Black * Own Home",
            ifelse(var == "ego_black2:ihs_home", "Black * Home Equity",
            ifelse(var == "ego_black2:ihs_debt", "Black * Debt",
            ifelse(var == "ego_black2:ihs_stock", "Black * Stocks",
            ifelse(var == "ego_black2:ihs_savings", "Black * Savings", ""             
        ))))))))))]
    #d) order IVs
        df[, var := factor(var, levels = rev(c("Own Home", "Home Equity", "Savings", "Stocks", "Debt", "Black * Own Home", "Black * Home Equity", "Black * Savings", "Black * Stocks", "Black * Debt")))]
    #e) order DVs
        df[, dv := factor(dv, levels = vars)]
    

#6) dynamically alter tick marks (breaks)
    #a) function for rounding based on base to round by
        mround  <-  function(x, base){
          base*round(x/base)
        }
    #b) function to set ggplot axis breaks based on input data
        five_breaks  <-  function(...){
            function(x){
              #get real limits
                  lim <- round(c(min(x)/1.1, max(x)/1.1)*100)/100
              #set limit to be simpler number if limits not equal
                  if(lim[1]!=lim[2]){
                    if(nchar(abs(lim[1]))<nchar(lim[2])){
                      lim[2] <- abs(lim[1])
                    }else{
                      lim[1] <- (-lim[2])
                    }
                  }
              #determine 5 breaks with one break at 0
                  br <- sort(c(lim, lim/2, 0))
                  br
              #if can't be broken into nice units, break into 7
                  if(!br[4] %in% c(.01, .025, .05, .1, .25, .5, 1, 1.25, 2.5))
                  br <- c(lim[1], lim[1]-((lim[1]/3)*1:6))
                  round(br, 3)
            }
        }
    #c) function to label axis breaks with correct number of digits
        labsfun <- function(x){
            if(nchar(x[2])>5 | nchar(x[1])>5){
              paste0(sprintf("%.3f", x))
            }else if(nchar(x[2])>4 | nchar(x[1])>4){
              paste0(sprintf("%.2f", x))
            }else{
              paste0(sprintf("%.1f", x))
            }
          }

        
#7) set limits for each y axis
    dft <- copy(df)
    dft[, y_min := -max(c(abs(CIupper), abs(CIlower))), by = dv]
    dft$y_min <- ceiling(dft$y_min*22)/20
    dft$y_max <- (-dft$y_min)
    #manually adjust limits
        # dft$y_max[1:5+(5*0)] <- .2
        # dft$y_max[1:5+(5*1)] <- .1
        # dft$y_max[1:5+(5*2)] <- .15
        # dft$y_max[1:5+(5*3)] <- .5
        # dft$y_max[1:5+(5*4)] <- .05
        # # dft$y_max[1:5+(5*5)] <- .5
        # dft$y_min <- (-dft$y_max)
    
    
#8) reduce to just key DVs and final model
    dft <- dft[var %in% c("Own Home", "Home Equity", "Savings", "Stocks", "Debt"), ]
    dft <- dft[model == 7, ]

    
#9) plot    
    ggplot(dft, aes(y=est, x=var))+
      coord_flip()+
      geom_linerange(aes(ymin=CIlower, ymax=CIupper), size=.7, color=dft$significant) +
      theme_bw() +
      geom_hline(yintercept = 0)+
      geom_point(size=2.5, color=dft$significant) +
      labs(x="")+
      ggtitle("Figure 2: Parameter Estimates of Key Independent Variables by Dependent Variable")+
      theme(panel.background = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"), 
            text=element_text(family="serif"), 
            title = element_text(size=10, face="bold"), 
            strip.text.x = element_text(size = 9.5, margin = margin(0.05, 0.05, 0.05, 0.05, "cm")), 
            axis.text.x = element_text(size=8.5), 
            axis.text.y = element_text(size=9), 
            panel.spacing = unit(.75, "lines"))+
            facet_wrap(~dv, scales = "free_x")+
            geom_blank(aes(y = y_min))+
            geom_blank(aes(y = y_max))+
            scale_y_continuous(breaks = five_breaks(), labels = labsfun)+
            labs(y="Parameter Estimate")
    
    png(file="C:/Users/briarons/Desktop/Figure 2 - Parameter Estimates of key IVs.png", height=4, width=7.5, units = "in", res = 300)
    dev.off()



# 
# #10) Figure 3
#     #a - create a temporary df of variables I care about
#         dft <- df[df$Var %in% c("ego_black2:eq_home_d", "ego_black2:peq_home", "ego_black2:peq_debt", "ego_black2:peq_stock", "ego_black2:peq_savings"), ]
#         # dft <- dft[1:10, ]
#         #set order of variables
#             dft$Var <- factor(rep(c("Black*Own Home", "Black*Home Equity", "Black*Debt", "Black*Stocks", "Black*Savings"), 8), levels=c("Black*Debt", "Black*Stocks", "Black*Savings", "Black*Home Equity", "Black*Own Home"), ordered = T)
#             dft$DV <- factor(dft$DV, levels=c("Self-Rated Health", "BMI", "# of Conditions", "Heart Attack", "Stroke", "Work Limitations", "Disability", "Distress"), ordered = T)
#     
#     #b - set limits for each y axis
#         dft  <-  data.table(dft)
#         dft[, y_min := -max(c(abs(CIupper), abs(CIlower))), by = DV]
#         dft$y_min <- ceiling(dft$y_min*22)/20
#         dft$y_max <- (-dft$y_min)
#         #manually adjust
#             dft$y_max[1:5+(5*0)] <- .75
#             dft$y_max[1:5+(5*1)] <- 1
#         # dft$y_max[1:5+(5*2)] <- .5
#         dft$y_max[1:5+(5*3)] <- 5
#         dft$y_max[1:5+(5*4)] <- 5
#         dft$y_max[1:5+(5*5)] <- 1
#         dft$y_max[1:5+(5*6)] <- 3
#         # dft$y_max[1:5+(5*7)] <- .15
#         dft$y_min <- (-dft$y_max)
#     
#     #c - kill heart attack and stroke
#         dft <- dft[!(dft$DV %in% c("Heart Attack", "Stroke")), ]
#         
#     #d - plot
#         png(file="C:/Users/bda13/Desktop/Figure 3 - Parameter Estimates of Interaction IVs.png", height=4, width=7.75, units = "in", res = 300)
#         ggplot(dft, aes(y=Estimate, x=Var))+
#           coord_flip()+
#           geom_linerange(aes(ymin=CIlower, ymax=CIupper), size=.7, color=dft$significant)+
#           theme_bw()+
#           geom_hline(yintercept = 0)+
#           geom_point(size=2.5, color=dft$significant)+
#           labs(x="")+
#           ggtitle("Figure 3: Parameter Estimates of Interaction Variables by Dependent Variable")+
#           theme(panel.background = element_blank(), 
#                 panel.grid.major = element_blank(), 
#                 panel.grid.minor = element_blank(), 
#                 axis.line = element_line(colour = "black"), 
#                 text=element_text(family="serif"), 
#                 title = element_text(size=10, face="bold"), 
#                 axis.text.x = element_text(size=8.5), 
#                 axis.text.y = element_text(size=9), 
#                 strip.text.x = element_text(size = 9.5, margin = margin(0.05, 0.05, 0.05, 0.05, "cm")), 
#                 panel.spacing = unit(.75, "lines"))+
#                 # legend.text=element_text(size=12), 
#                 # legend.title=element_text(size=12, face="bold"), 
#                 # panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#                 # axis.text.x = element_text(size=12, angle = 30, hjust = 1), 
#                 # legend.position="bottom", 
#                 # axis.text.y = element_blank(), 
#                 # axis.text.y = element_text(size=11.5), 
#                 # axis.title.y= element_text(size=13.5, face="bold", margin = margin(t = 0, r = 15, b = 0, l = 0)), 
#                 # axis.title.x= element_text(size=12, face="bold", margin = margin(t = 10, r = 0, b = 0, l = 0)), 
#                 # axis.title.y = element_blank(), 
#                 # axis.title.x = element_blank(), 
#               facet_wrap(~DV, scales = "free_x")+
#               geom_blank(aes(y = y_min))+
#               geom_blank(aes(y = y_max))+
#               scale_y_continuous(breaks = five_breaks(), labels = labsfun)+
#               labs(y="Parameter Estimate")
#         
#             dev.off()
#         
            

#11) Figure 4 - Predict health by race and wealth component
  #11a) prepare data
    #a - create a data frame composed of mean/mode values of all IVs
        #i - identify unique variables
            vars <- unique(df$Var)
        #ii - pull dataset used for all models
            library(foreign)
            I2 <- read.dta("PSID.Sample.dta")
        #iii - create mean, mode, and median functions
            b.mean <- function(x) mean(x, na.rm=T)
            b.mode <- function(x) names(sort(table(x), decreasing = T))[1]
            b.median <- function(x) median(x, na.rm=T)
        #iv - kill variables that don't matter
            I2 <- I2[, c(names(I2) %in% df$Var)]
        #v - find the median value of everything
            a <- (sapply(I2, b.median))
            #remember that ego_age = age - min_age
        #vi - merge medians into df2
            a <- data.frame(Var=names(a), Median=a, row.names = NULL)
        #vii - transpose a
            b <- as.data.frame(matrix(0, nrow=1, ncol=nrow(a)))
            names(b) <- a$Var
            b[1, ] <- a$Median
        #viii - add vars for intercept and interactions
            b$`ego_black2:ego_age` <- b$ego_black2*b$ego_age
            b$`ego_black2:ego_age2` <- b$ego_black2*b$ego_age2
            b$`ego_black2:eq_home_d` <- b$ego_black2*b$eq_home_d
            b$`ego_black2:peq_home` <- b$ego_black2*b$peq_home
            b$`ego_black2:peq_debt` <- b$ego_black2*b$peq_debt
            b$`ego_black2:peq_stock` <- b$ego_black2*b$peq_stock
            b$`ego_black2:peq_savings` <- b$ego_black2*b$peq_savings
            refdf <- b
            refdf$`(Intercept)` <- 1
    #b - create a dataframe of all coefficients by DV
        tdf <- df
        tdf <- tdf[!tdf$DV %in% c("Heart Attack", "Stroke"), ]
        tdf$Var[3] <- "(Intercept)"
        tdf <- tdf[-c(1, 2, 4), ]
        coefdf <- as.data.frame(matrix(tdf$Estimate, nrow=length(unique(tdf$DV)), ncol=ncol(refdf), byrow = T))
        lowerdf <- as.data.frame(matrix(tdf$CIlower, nrow=length(unique(tdf$DV)), ncol=ncol(refdf), byrow = T))
        upperdf <- as.data.frame(matrix(tdf$CIupper, nrow=length(unique(tdf$DV)), ncol=ncol(refdf), byrow = T))
        names(coefdf) <- tdf$Var[1:ncol(coefdf)]
        names(lowerdf) <- tdf$Var[1:ncol(lowerdf)]
        names(upperdf) <- tdf$Var[1:ncol(upperdf)]
    #c - expand both columns to hold all facets I will examine
        #i - order both dfs to be the same
            refdf <- refdf[match(names(coefdf), names(refdf))]
        #ii - save number of columns that will be manipulated
            cols <- ncol(refdf)
        #iii - 6 lines in refdf and add DV indicator for both
            refdf <- refdf[rep(1, nrow(coefdf)), ]
            refdf$DV <- unique(tdf$DV)
            coefdf$DV <- unique(tdf$DV)
            lowerdf$DV <- unique(tdf$DV)
            upperdf$DV <- unique(tdf$DV)
        #iv - add lines for manipulation of each variable 20 times
            interv <- (0:20)/20
            ntimes=length(interv)
            nvars=5
            nprod=ntimes*nvars
            refdf <- refdf[rep(1:nrow(refdf), each=nprod), ]
            coefdf <- coefdf[rep(1:nrow(coefdf), each=nprod), ]
            lowerdf <- lowerdf[rep(1:nrow(lowerdf), each=nprod), ]
            upperdf <- upperdf[rep(1:nrow(upperdf), each=nprod), ]
            Estimate.x <- vector()  #for graphing
            for(i in 1:length(unique(refdf$DV))){ #i=1:6
              refdf$eq_home_d[(i-1)*nprod+(1:ntimes)] <- c(rep(0, ceiling(ntimes/2)), rep(1, floor(ntimes/2)))
              refdf$peq_home[(i-1)*nprod+((ntimes+1):(2*ntimes))] <- interv
              refdf$peq_savings[(i-1)*nprod+((ntimes*2+1):(3*ntimes))] <- interv
              refdf$peq_stock[(i-1)*nprod+((ntimes*3+1):(4*ntimes))] <- interv
              refdf$peq_debt[(i-1)*nprod+((ntimes*4+1):(5*ntimes))] <- interv
              Estimate.x[(i-1)*nprod+(1:ntimes)] <- c(rep(0, ceiling(ntimes/2)), rep(1, floor(ntimes/2)))
              Estimate.x[(i-1)*nprod+((ntimes+1):(nvars*ntimes))] <- rep(interv, nvars-1)
            }
        #v - indicate which var you are manipulating
            refdf$IV <- rep(rep(c("Own Home", "Home Equity", "Savings", "Stocks", "Debt"), each=ntimes), 6)
        #vi - add lines for each race
            refdf <- refdf[rep(1:nrow(refdf), 2), ]
            #alter coefdf to reflect different races
                refdf$ego_black2[1:nrow(coefdf)] <- 1
                refdf$`ego_black2:ego_age` <- refdf$ego_black2*refdf$ego_age
                refdf$`ego_black2:ego_age2` <- refdf$ego_black2*refdf$ego_age2
                refdf$`ego_black2:eq_home_d` <- refdf$ego_black2*refdf$eq_home_d
                refdf$`ego_black2:peq_home` <- refdf$ego_black2*refdf$peq_home
                refdf$`ego_black2:peq_debt` <- refdf$ego_black2*refdf$peq_debt
                refdf$`ego_black2:peq_stock` <- refdf$ego_black2*refdf$peq_stock
                refdf$`ego_black2:peq_savings` <- refdf$ego_black2*refdf$peq_savings
            #add matching lines for coefdf
                coefdf <- coefdf[rep(1:nrow(coefdf), 2), ]
                lowerdf <- lowerdf[rep(1:nrow(lowerdf), 2), ]
                upperdf <- upperdf[rep(1:nrow(upperdf), 2), ]
    #d - multiply columns and sum to predict health for each individual 
        preds <- refdf[1:cols]*coefdf[1:cols]
        lopreds <- refdf[1:cols]*lowerdf[1:cols]
        uppreds <- refdf[1:cols]*upperdf[1:cols]
        a <- rowSums(preds)
        a1 <- rowSums(lopreds)
        a2 <- rowSums(uppreds)
    #e - use this info to create a data frame to graph based on.
        gdf <- data.frame(DV=refdf$DV, IV=refdf$IV, Black=refdf$ego_black2, Estimate.y=a, Estimate.x=Estimate.x, loestimate = a1, upestimate = a2)
    #f - create graph
        gdf2 <- gdf[gdf$IV=="Debt" & gdf$DV=="Self-Rated Health", ]
        ggplot(gdf2, aes(x=Estimate.x, y=Estimate.y, group=Black))+
          geom_line(color=ifelse(gdf2$Black==1, "Black", "Grey"))
    #g - relevel variables to be in right order
        gdf$DV <- factor(gdf$DV, levels=c("Self-Rated Health", "BMI", "# of Conditions", "Work Limitations", "Disability", "Distress"), ordered = T)
        gdf$IV <- factor(gdf$IV, levels=c(as.character(unique(gdf$IV))), ordered = T)
    #h - split data by DV
        plotdf <- gdf
        tempdfl <- split(gdf, gdf$DV)
  #11b) plot data
    #i - create plot functions
        plotfun <- function(xr){
          p <- ggplot(tempdfl[[xr]], aes(x=Estimate.x, y=Estimate.y, group=Black, color=as.factor(Black)))
          p+ geom_line(size=2)+
            scale_color_manual(values=c("grey", "black"))+
            theme_pander()+
                  theme(
                        axis.line.x = element_line(colour = "black"), 
                        axis.line.y = element_line(colour = "black"), 
                        plot.title = element_text(size=20, hjust=0), 
                        axis.title.x=element_blank(), 
                        axis.text.x=element_blank(), 
                        axis.title.y= element_text(size=13.5, face="bold", margin = margin(t = 0, r = 15, b = 0, l = 0)), 
                        strip.text.x = element_blank(), 
                        legend.position="none", 
                        panel.spacing = unit(2.5, "lines"), 
                        axis.ticks= element_blank(), 
                        text=element_text(family="serif"), 
                        plot.margin = unit(c(.5, t=0, b=.2, 0.5), "lines"))+
              facet_wrap(~IV, nrow=1)+
                    if(xr==1){
                      scale_y_continuous(name=expression(atop("Self-Rated Health", "(Ordinal)")), breaks=c(-5.0, -5.5, -6.0, -6.5), labels=c("-5.0", "-5.5", "-6.0", "-6.5"), limits=c(-6.5, -5.0))
                    }else if(xr==2){
                      scale_y_continuous(name=expression(atop("BMI", "(OLS)")), breaks=c(27.0, 28.0, 29.0, 30.0, 31.0), labels=c("27.0", "28.0", "29.0", "30.0", "31.0"), limits=c(27, 31))
                    }else if(xr==3){
                      scale_y_continuous(name=expression(atop("# of Conditions", "(Poisson)")), breaks=c(0, -.25, -.50, -.75, -1.00), labels=c("0.00", "-0.25", "-0.50", "-0.75", "-1.00"), limits=c(-1, 0))
                    }else if(xr==4){
                      scale_y_continuous(name=expression(atop("Work Limitations", "(Poisson)")), breaks=c(-2.5, -2.6, -2.7, -2.8, -2.9, -3.0), labels=c("-2.5", "-2.6", "-2.7", "-2.8", "-2.9", "-3.0"), limits=c(-3, -2.5))
                    }else if(xr==5){
                      scale_y_continuous(name=expression(atop("Disability", "(Logit)")), breaks=c(-3.5, -4.0, -4.5, -5.0, -5.5), labels=c("-3.5", "-4.0", "-4.5", "-5.0", "-5.5"), limits=c(-5.5, -3.5))
                    }else if(xr==6){
                      scale_y_continuous(name=expression(atop("Distress", "(Poisson)"))) #, breaks=c(28.0, 29.0, 30.0), labels=c("28.0", "29.0", "30.0")
                    }
       }
       plotfun2 <- function(xr){
          p <- ggplot(tempdfl[[xr]], aes(x=Estimate.x, y=Estimate.y, group=Black, color=as.factor(Black)))
          p+ geom_line(size=2)+
            scale_color_manual(values=c("grey", "black"))+
            theme_pander()+
                  theme(
                        axis.line.x = element_line(colour = "black"), 
                        axis.line.y = element_line(colour = "black"), 
                        plot.title = element_text(size=20, hjust=0), 
                        axis.title.x=element_blank(), 
                        axis.text.x=element_blank(), 
                        axis.title.y=element_blank(), 
                        axis.text.y=element_blank(), 
                        strip.text.x = element_blank(), 
                        legend.position="none", 
                        panel.spacing = unit(2.5, "lines"), 
                        axis.ticks= element_blank(), 
                        text=element_text(family="serif"), 
                        plot.margin = unit(c(.5, t=0, b=.2, 0.5), "lines"))+
              facet_wrap(~IV, nrow=1)+
                    if(xr==1){
                      scale_y_continuous(name=expression(atop("Self-Rated Health", "(Ordinal)")), breaks=c(-5.0, -5.5, -6.0, -6.5), labels=c("-5.0", "-5.5", "-6.0", "-6.5"), limits=c(-6.5, -5.0))
                    }else if(xr==2){
                      scale_y_continuous(name=expression(atop("BMI", "(OLS)")), breaks=c(27.0, 28.0, 29.0, 30.0, 31.0), labels=c("27.0", "28.0", "29.0", "30.0", "31.0"), limits=c(27, 31))
                    }else if(xr==3){
                      scale_y_continuous(name=expression(atop("# of Conditions", "(Poisson)")), breaks=c(0, -.25, -.50, -.75, -1.00), labels=c("0.00", "-0.25", "-0.50", "-0.75", "-1.00"), limits=c(-1, 0))
                    }else if(xr==4){
                      scale_y_continuous(name=expression(atop("Work Limitations", "(Poisson)")), breaks=c(-2.5, -2.6, -2.7, -2.8, -2.9, -3.0), labels=c("-2.5", "-2.6", "-2.7", "-2.8", "-2.9", "-3.0"), limits=c(-3, -2.5))
                    }else if(xr==5){
                      scale_y_continuous(name=expression(atop("Disability", "(Logit)")), breaks=c(-3.5, -4.0, -4.5, -5.0, -5.5), labels=c("-3.5", "-4.0", "-4.5", "-5.0", "-5.5"), limits=c(-5.5, -3.5))
                    }else if(xr==6){
                      scale_y_continuous(name=expression(atop("Distress", "(Poisson)"))) #, breaks=c(28.0, 29.0, 30.0), labels=c("28.0", "29.0", "30.0")
                    }
       }
    #j - create empty plot
       m02 <- ggplot(tempdfl[[5]], aes(x=Estimate.x, y=Estimate.y, group=Black, color=as.factor(Black)))+
          scale_color_manual(values=c("black", "grey"))+
          theme_pander()+
          theme(
            axis.line.x = element_line(colour = "black"),
            axis.line.y = element_line(colour = "black"),
            # plot.title = element_text(size=20, hjust=0),
            axis.title.x=element_text(size=13.5, face="bold"),
            legend.position="none",
            panel.spacing = unit(2.5, "lines"),
            axis.ticks= element_blank(),
            text=element_text(family="serif"),
            legend.title=element_text(size=12, face="bold"),
            title = element_text(size=12, face="bold"),
            axis.title.y= element_text(size=11.75, face="bold", margin = margin(t = 0, r = 15, b = 0, l = 0)),
            axis.text.x= element_text(size=11),
            strip.text.x=element_text(size=13, face="bold", margin = margin(t = 0, r = 5, b = 15, l = 0)),
            plot.margin = unit(c(.5, t=0, b=.2, 0.5), "lines"))+
            scale_x_continuous(breaks=c(0, .5, 1))+
          facet_wrap(~IV, nrow=1)+
          geom_blank()+
          labs(x="Proportion Wealth in Asset", y=" \n \n")
    #k - plot all
        m1 <- plotfun(1)
        # m2 <- plotfun(2) #bmi
        m3 <- plotfun(3)
        m4 <- plotfun(4)
        m5 <- plotfun(5)
        m6 <- plotfun(6)
        p <- plot_grid(m1, m3, m4, m5, m6, ncol=1)
        p <- plot_grid(p, m02, ncol=1, rel_heights=c(1, 0.15))
        title  <-  ggdraw() + draw_label("Figure 3 - Predicted Values of Health Characteristic by Wealth Component", fontface='bold', size=20, fontfamily="serif")
        p <- plot_grid(title, p, nrow=2, rel_heights=c(0.07, 1)) #put title above graph
        p <- plot_grid(p, scale = .98)
    #l - save plot
        save_plot("C:/Users/bda13/Desktop/Fig 3 - Predicted Values of Health Characteristic by Wealth Component1.png", p, 
                  base_height=10, 
                  base_aspect_ratio = .93)
        
        
#12) create version with standardized widths              
      m1 <- plotfun2(1)
      # m2 <- plotfun2(2) #BMI
      m3 <- plotfun2(3)
      m4 <- plotfun2(4)
      m5 <- plotfun2(5)
      m6 <- plotfun2(6)
      p <- plot_grid(m1, m3, m4, m5, m6, ncol=1)
      p <- plot_grid(p, m02, ncol=1, rel_heights=c(1, 0.15))
      title  <-  ggdraw() + draw_label("Figure 3 - Predicted Values of Health Characteristic by Wealth Component", fontface='bold', size=20, fontfamily="serif")
      p <- plot_grid(title, p, nrow=2, rel_heights=c(0.07, 1)) #put title above graph
      p <- plot_grid(p, scale = .98)
      save_plot("C:/Users/bda13/Desktop/Fig 3 - Predicted Values of Health Characteristic by Wealth Component2.png", p, 
                base_height=10, 
                base_aspect_ratio = .93)
              

#13) create version with dots and point estimates
        plotfun3 <- function(xr){
            p <- ggplot(tempdfl[[xr]], aes(x=Estimate.x, y=Estimate.y, group=Black, color=as.factor(Black)))
            p+ geom_line(size=.1, linetype="dashed")+
            geom_errorbar(aes(ymin=loestimate, ymax=upestimate), width=.1)+ 
            geom_point(size=3)+
            facet_wrap(~IV, nrow=1)+
              scale_color_manual(values=c("grey", "black"))+
              theme_pander()+
                    theme(
                          axis.line.x = element_line(colour = "black"), 
                          axis.line.y = element_line(colour = "black"), 
                          plot.title = element_text(size=20, hjust=0), 
                          axis.title.x=element_blank(), 
                          axis.text.x=element_blank(), 
                          axis.title.y=element_blank(), 
                          axis.text.y=element_blank(), 
                          strip.text.x = element_blank(), 
                          legend.position="none", 
                          panel.spacing = unit(2.5, "lines"), 
                          axis.ticks= element_blank(), 
                          text=element_text(family="serif"), 
                          plot.margin = unit(c(.5, t=0, b=.2, 0.5), "lines"))+
              if(xr==1){
                scale_y_continuous(name=expression(atop("Self-Rated Health", "(Ordinal)")), breaks=c(-5.0, -5.5, -6.0, -6.5), labels=c("-5.0", "-5.5", "-6.0", "-6.5"), limits=c(-6.5, -5.0))
              }else if(xr==2){
                scale_y_continuous(name=expression(atop("BMI", "(OLS)")), breaks=c(27.0, 28.0, 29.0, 30.0, 31.0), labels=c("27.0", "28.0", "29.0", "30.0", "31.0"), limits=c(27, 31))
              }else if(xr==3){
                scale_y_continuous(name=expression(atop("# of Conditions", "(Poisson)")), breaks=c(0, -.25, -.50, -.75, -1.00), labels=c("0.00", "-0.25", "-0.50", "-0.75", "-1.00"), limits=c(-1, 0))
              }else if(xr==4){
                scale_y_continuous(name=expression(atop("Work Limitations", "(Poisson)")), breaks=c(-2.5, -2.6, -2.7, -2.8, -2.9, -3.0), labels=c("-2.5", "-2.6", "-2.7", "-2.8", "-2.9", "-3.0"), limits=c(-3, -2.5))
              }else if(xr==5){
                scale_y_continuous(name=expression(atop("Disability", "(Logit)")), breaks=c(-3.5, -4.0, -4.5, -5.0, -5.5), labels=c("-3.5", "-4.0", "-4.5", "-5.0", "-5.5"), limits=c(-5.5, -3.5))
              }else if(xr==6){
                scale_y_continuous(name=expression(atop("Distress", "(Poisson)"))) #, breaks=c(28.0, 29.0, 30.0), labels=c("28.0", "29.0", "30.0")
              }
        }
        m1 <- plotfun3(1)
        m3 <- plotfun3(3)
        m4 <- plotfun3(4)
        m5 <- plotfun3(5)
        m6 <- plotfun3(6)
        p <- plot_grid(m1, m3, m4, m5, m6, ncol=1)
        p <- plot_grid(p, m02, ncol=1, rel_heights=c(1, 0.15))
        title  <-  ggdraw() + draw_label("Figure 3 - Predicted Values of Health Characteristic by Wealth Component", fontface='bold', size=20, fontfamily="serif")
        p <- plot_grid(title, p, nrow=2, rel_heights=c(0.07, 1)) #put title above graph
        p <- plot_grid(p, scale = .98)
        save_plot("C:/Users/bda13/Desktop/Fig 3 - Predicted Values of Health Characteristic by Wealth Component3.png", p, 
                  base_height=10, 
                  base_aspect_ratio = .93)

#14) get Ns for each model
    sumfun <- function(var, var.sample, ind){
      print(paste(
        length(var[var.sample & !is.na(var)]), 
        length(unique(ind[var.sample]))
      )
      )
    }
    sumfun(I3$h_general, I3$h_general.sample, I3$ind_id)
    sumfun(I3$h_conditions, I3$h_conditions.sample, I3$ind_id)
    sumfun(I3$h_lim_work, I3$h_lim_work.sample, I3$ind_id)
    sumfun(I3$h_disabled, I3$h_disabled.sample, I3$ind_id)
    sumfun(I3$h_distress, I3$h_distress.sample, I3$ind_id)

    
#15) NEW FIGURE 3 10-17-19; Just predict health by saveings*race
    newdf <- data.table(gdf)
    newdf <- newdf[IV == "Savings"]
    l.newdf <- split(newdf, by = "DV")
    
    plotfun4 <- function(xr){
        p <- ggplot(l.newdf[[xr]], aes(x=Estimate.x, y=Estimate.y, group=Black, color=as.factor(Black)))+
        geom_line(size=2)+
          scale_color_manual(values=c("grey", "black"))+
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
            p + scale_y_continuous(breaks=c(-5.0, -5.5, -6.0, -6.5), labels=c("-5.0", "-5.5", "-6.0", "-6.5"), limits=c(-6.5, -5.0)) +
            ggtitle("Self-Rated Health (Ordinal)")
          }else if(xr==2){
            p + scale_y_continuous(breaks=c(27.0, 28.0, 29.0, 30.0, 31.0), labels=c("27.0", "28.0", "29.0", "30.0", "31.0"), limits=c(27, 31)) +
            ggtitle("BMI (OLS)")
          }else if(xr==3){
            p + scale_y_continuous(breaks=c(0, -.25, -.50, -.75, -1.00), labels=c("0.00", "-0.25", "-0.50", "-0.75", "-1.00"), limits=c(-1, 0)) +
            ggtitle("# of Conditions (Poisson)")
          }else if(xr==4){
            p + scale_y_continuous(breaks=c(-2.5, -2.6, -2.7, -2.8, -2.9, -3.0), labels=c("-2.5", "-2.6", "-2.7", "-2.8", "-2.9", "-3.0"), limits=c(-3, -2.5)) +
            ggtitle("Work Limitations (Poisson)")
          }else if(xr==5){
            p + scale_y_continuous(breaks=c(-3.5, -4.0, -4.5, -5.0, -5.5), labels=c("-3.5", "-4.0", "-4.5", "-5.0", "-5.5"), limits=c(-5.5, -3.5)) +
            ggtitle("Disability (Logit)")
          }else if(xr==6){
            p + ggtitle("Distress (Poisson)")
          }
      }
    m1 <- plotfun4(1)
    m3 <- plotfun4(3)
    m4 <- plotfun4(4)
    m5 <- plotfun4(5)
    m6 <- plotfun4(6)
    p <- plot_grid(m1, m3, m4, m5, m6, ncol=3)
    title  <-  ggdraw() + draw_label("Figure 3 - Predicted Values of Health Characteristic by Proportion Wealth in Savings", fontface='bold', size=20, fontfamily="serif")
    p <- plot_grid(title, p, nrow=2, rel_heights=c(0.07, 1)) #put title above graph
    p <- plot_grid(p, scale = .98)
    save_plot(fname <- "C:/Users/bda13/Desktop/Figure 3 - Predicted Values of Health Characteristic by Proportion Wealth in Savings.png", p,
              base_width = 11,
              base_height = 7)

    browseURL(fname)
    
    
    
    # dft  <-  data.table(gdf)
    # dft[, y_max := max(Estimate.y), by = list(DV)]
    # dft[, y_min := min(Estimate.y), by = list(DV)]
    # dft$y_min <- dft$y_min*ifelse(sign(dft$y_min)==-1, 21/20, 20/21)
    # dft$y_max <- dft$y_max*ifelse(sign(dft$y_max)==-1, 20/21, 21/20)
    # ggplot(gdf2, aes(x=Estimate.x, y=Estimate.y, group=Black))+
    #   geom_line(color=ifelse(gdf2$Black==1, "Black", "Grey"), size=2)+
    #   theme_bw()+
    #   labs(x="", y="")+
    #   theme(panel.background = element_blank(), 
    #         panel.grid.major = element_blank(), 
    #         panel.grid.minor = element_blank(), 
    #         axis.line = element_line(colour = "black"), 
    #         text=element_text(family="serif"), 
    #         title = element_text(size=10, face="bold"), 
    #         strip.text.x = element_text(size = 9.5, margin = margin(0.05, 0.05, 0.05, 0.05, "cm")), 
    #         axis.text.x = element_text(size=8.5), 
    #         axis.text.y = element_text(size=9), 
    #         panel.spacing = unit(.75, "lines"))+
    # scale_x_continuous(breaks = (0:5)/5, limits = c(0, 1))
    # dev.off()