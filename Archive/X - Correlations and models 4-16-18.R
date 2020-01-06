{
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
options(scipen = 999)

#0 - Set directory
    setwd("C:/Users/bda13/Desktop/Sociology/Statistics/PSID/")

#1 - read data
    I<-readRDS("PreppedData2.rds")

#a<-round(cor(I[I$eq_wealth>0,(c("inc_total","eq_wealth","eq_home","eq_savings","eq_stock","eq_debt"))],use="complete"),2)
    #round(chol(a),2)    
#    a<-round(cor(I[I$eq_wealth>0,(c("inc_total","eq_wealth","eq_home","eq_savings","eq_stock","eq_debt"))],use="complete"),2)
#    round(chol(a),2)
#    ?chol
#2 - Create a few other variables
    #Reduce dataset to variables of interest
        I<-I[,c("h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","ego_age","ego_age2","ego_black2","ego_female","ego_dg_advanced","ego_dg_bachelors","ego_dg_highschool","ego_dg_somecollege","eq_home","eq_bus","eq_savings","eq_otr_estate","eq_stock","eq_debt","inc_total","eq_wealth","ind_id", "year","fam_id_68","fam_id","eq_home_d","eq_assets")] #"eq_vehicle","eq_otr_assets"
    #remove any rows with nas
        #I <- I[rowSums(is.na(I))== 0,]
    #Asset to debt ratios - Cap these at 1 and 0, except for debt which can be capped at 2; convert NAS to 0, convert Infs to 1.
        propfun<-function(x,debt=F){
          x<-x/I$eq_assets
          x<-ifelse(x<0,0,x)
          x<-ifelse(is.na(x),0,x)
          x<-ifelse(is.infinite(x),1,x)
          if(debt==T){
            x<-ifelse(x>2,0,x)
          }else{
            x<-ifelse(x>1,0,x)
          }
          return(x)
        }
        I$peq_home<-propfun(I$eq_home)
        I$peq_debt<-propfun(I$eq_debt,debt=T)
        I$peq_stock<-propfun(I$eq_stock)
        I$peq_savings<-propfun(I$eq_savings)
    #Divide all eqity vars income by 1000
        I$inc_total<-I$inc_total/1000
        I$eq_wealth<-I$eq_wealth/1000
        I$eq_home<-I$eq_home/1000
        I$eq_debt<-I$eq_debt/1000
        I$eq_stock<-I$eq_stock/1000
        I$eq_savings<-I$eq_savings/1000
        
    #create squared terms
        I$eq_wealth2<-I$eq_wealth^2
        I$inc_total2<-I$inc_total^2
    # #Divide age by 10
    #     I$ego_age<-I$ego_age/10
    #     I$ego_age2<-I$ego_age2/10
}

#degree variables in 1984...
#Ira, especially prior to 1994.
#medical expenses: need to recode 2013 and 2015
#debt variables: none exist  prior to 2011

#3 - Models
  #a - Across all households
      #i - create base model
          y<-"h_general"
          form1<-formula(paste(y,"~ (ego_age+ego_age2)*ego_black2 + ego_age + ego_female + ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced + (1|ind_id)+ (1|fam_id_68)"))
          model<-function(x){suppressWarnings(round(as.data.frame(summary(lmer(formula=x,data =I))$coefficients),2))[,-3]}
      #ii - create different formulas by increasing sophistication
          form2<-update(form1,~.+eq_home_d)
          form3<-update(form2,~.+inc_total+inc_total2)
          form4<-update(form3,~.+eq_wealth+eq_wealth2)
          form5<-update(form4,~.+eq_wealth+(eq_home+eq_debt+eq_stock+eq_savings)+eq_wealth2+(eq_home+eq_debt+eq_stock+eq_savings))
      #iii - Run the model based on different formulas
          (m1.1<-model(form1))
          (m1.2<-model(form2))
          (m1.3<-model(form3))
          (m1.4<-model(form4))
          (m1.5<-model(form5))
  #b - Across households at different points in the wealth distribution (since less skewed, assume linear income and wealth effects)
      #i - create base model
          y<-"h_general"
          form1<-formula(paste(y,"~ (ego_age)+ego_black2 +ego_age2 + ego_age + ego_female + ego_dg_highschool+ego_dg_somecollege+ego_dg_bachelors+ego_dg_advanced + eq_home_d + inc_total +(peq_home+peq_debt+peq_stock+peq_savings) + (1|ind_id)+ (1|fam_id_68)"))
          prstars<-function(x){
            x[,2]<-ifelse(x[,2]<.001,"***",ifelse(x[,2]<.01,"**",ifelse(x[,2]<.05,"*","")))
            return(x)
          }
          model<-function(x,df){suppressWarnings(prstars(round(as.data.frame(summary(lmer(formula=x,data=df))$coefficients),10)[c("Estimate","Pr(>|t|)")]))}
          
      #Run models by different sections of the wealth distribution
          partition<-data.frame(lower=c(25>I$eq_wealth & I$eq_wealth>0),
                                lmiddle=c(50>I$eq_wealth & I$eq_wealth>25),
                                mmiddle=c(100>I$eq_wealth & I$eq_wealth>50),
                                umiddle=c(250>I$eq_wealth & I$eq_wealth>100),
                                uumiddle=c(500>I$eq_wealth & I$eq_wealth>250),
                                upper=c(1000>I$eq_wealth &I$eq_wealth>500),
                                uupper=c(I$eq_wealth>1000))
          (m2.1<-model(form1,df=I[partition$lower,]))
          (m2.2<-model(form1,df=I[partition$lmiddle,]))
          (m2.3<-model(form1,df=I[partition$mmiddle,]))
          (m2.4<-model(form1,df=I[partition$umiddle,]))
          (m2.5<-model(form1,df=I[partition$uumiddle,]))
          (m2.6<-model(form1,df=I[partition$upper,]))
          (m2.7<-model(form1,df=I[partition$uupper,]))
          results<-cbind(m2.1,m2.2,m2.3,m2.4,m2.5,m2.6,m2.7)
          #reorder results
          corder<-c("inc_total","peq_debt","peq_savings","peq_stock","peq_home","eq_home_d","ego_age","ego_age2","ego_black2","ego_female","ego_dg_highschool","ego_dg_somecollege","ego_dg_bachelors","ego_dg_advanced","(Intercept)")
          results<-results[match(corder,row.names(results)),]
          #insert empty rows for easy formatting
          empty<-which(row.names(results)=="ego_female")
          emptydf<-as.data.frame(matrix("",nrow=1,ncol=ncol(results)))
          names(emptydf)<-names(results)
          results<-rbind(results[1:empty,],emptydf,results[(empty+1):nrow(results),])
          #save
          write.xlsx(results, file="C:/Users/bda13/Desktop/results.xlsx", sheetName="sheet1", row.names=T)

      #get correlation matrices
          rcors<-function(x) round(cor(x[,(c("inc_total","eq_wealth","eq_debt","eq_savings","eq_stock","eq_home","eq_home_d"))],use="complete"),2)
          c1<-rcors(I[partition$lower,])
          c2<-rcors(I[partition$lmiddle,])
          c3<-rcors(I[partition$mmiddle,])
          c4<-rcors(I[partition$umiddle,])
          c5<-rcors(I[partition$uumiddle,])
          c6<-rcors(I[partition$upper,])
          c7<-rcors(I[partition$uupper,])
          write.xlsx(c1, file="C:/Users/bda13/Desktop/cors.xlsx", sheetName="0-25k", row.names=T)
          write.xlsx(c2, file="C:/Users/bda13/Desktop/cors.xlsx", sheetName="25k-50k", append=TRUE, row.names=T)
          write.xlsx(c3, file="C:/Users/bda13/Desktop/cors.xlsx", sheetName="50k-100k", append=TRUE, row.names=T)
          write.xlsx(c4, file="C:/Users/bda13/Desktop/cors.xlsx", sheetName="100k-250k", append=TRUE, row.names=T)
          write.xlsx(c5, file="C:/Users/bda13/Desktop/cors.xlsx", sheetName="250k-500k", append=TRUE, row.names=T)
          write.xlsx(c6, file="C:/Users/bda13/Desktop/cors.xlsx", sheetName="500k-1000k", append=TRUE, row.names=T)
          write.xlsx(c7, file="C:/Users/bda13/Desktop/cors.xlsx", sheetName="1000k+", append=TRUE, row.names=T)
        
    #Produce heatmap with correlations 
        t<-rcors(I[partition$lower| partition$lmiddle | partition$mmiddle | partition$umiddle,])
        cormat<-t[1:6,1:6]
        cormat<-cormat[order(row.names(cormat)),order(row.names(cormat))]
        
        row.names(cormat)<-c("Debt","Home Equity","Savings","Stock Value","Net Worth","Income")
        colnames(cormat)<-c("Debt","Home Equity","Savings","Stock Value","Net Worth","Income")
        
        get_upper_tri <- function(cormat){
          cormat[lower.tri(cormat)]<- NA
          return(cormat)
        }
        upper_tri <- get_upper_tri(cormat)
        library(reshape2)
        melted_cormat <- melt(upper_tri, na.rm = TRUE)
        
        melted_cormat[melted_cormat$Var1==melted_cormat$Var2,]$value<-0
        melted_cormat$val2<-melted_cormat$value
        melted_cormat$val2[melted_cormat$val2==0]<-""
        
        (ggheatmap<-ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
          geom_tile(color = "white")+
          scale_fill_gradient2(low = "blue4", high = "red4", 
                               midpoint = 0, limit = c(-1,1), space = "Lab", 
                               name="Pearson\nCorrelation") +
          theme_minimal()+ 
          theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                           size = 20, hjust = 1),
                axis.text.y = element_text(size = 20))+
          coord_fixed())
        
        png("C:/Users/bda13/Desktop/Fig 1 - Correlations Among Components of Wealth (0k-250k).png", width=1050, height=600)
        ggheatmap + 
          geom_text(aes(Var2, Var1, label = val2), color = "black", size = 10) +
          theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            legend.text = element_text(size = 24),
            legend.title = element_text(size = 24),
            plot.title = element_text(size=42, hjust=.38),
            plot.margin = margin(0, 4, 0, 0, "cm"))+
          guides(fill = guide_colorbar(barwidth = 4, barheight = 15))+
          ggtitle("Correlations Among Components of Wealth (0k-250k)")+
          geom_vline(xintercept = 5.54,size=3)
        dev.off()
        
        
          

          
    # #get correlations with scf
    #     scf<-read.csv("C:/Users/bda13/Desktop/Sociology/Statistics/SCF/SCF Full Data Files/rscffull.csv")
    #     partition2<-data.frame(lower=c(25>scf$NETWORTH & scf$NETWORTH>0),
    #                           lmiddle=c(50>scf$NETWORTH & scf$NETWORTH>25),
    #                           mmiddle=c(100>scf$NETWORTH & scf$NETWORTH>50),
    #                           umiddle=c(250>scf$NETWORTH & scf$NETWORTH>100),
    #                           uumiddle=c(500>scf$NETWORTH & scf$NETWORTH>250),
    #                           upper=c(1000>scf$NETWORTH &scf$NETWORTH>500),
    #                           uupper=c(scf$NETWORTH>1000))
    #     
    #     library("wCorr")
    #     w.cor.mat<-function(df){
    #         vars<-c("INCOME","NETWORTH","DEBT","SavingsPlanValue","StockValue","HomeEquity","HomeOwner")
    #         tempdf<-df
    #         tmat<-matrix(0,length(vars),length(vars))
    #         for(i in 1:length(vars)){
    #           var1n<-vars[i]
    #           for(j in 1:length(vars)){
    #             var2n<-vars[j]
    #             tmat[i,j]<-weightedCorr(tempdf[,var1n],tempdf[,var2n],method="pearson",weights=tempdf$WGT)
    #           }
    #         }
    #         tmat<-round(tmat,2)
    #         return(tmat)
    #     }
    #     cw1<-w.cor.mat(scf[partition2$lower,])
    #     cw2<-w.cor.mat(scf[partition2$lmiddle,])
    #     cw3<-w.cor.mat(scf[partition2$mmiddle,])
    #     cw4<-w.cor.mat(scf[partition2$umiddle,])
    #     cw5<-w.cor.mat(scf[partition2$uumiddle,])
    #     cw6<-w.cor.mat(scf[partition2$upper,])
    #     cw7<-w.cor.mat(scf[partition2$uupper,])
    #     write.xlsx(cw1, file="C:/Users/bda13/Desktop/wcors.xlsx", sheetName="0-25k", row.names=T)
    #     write.xlsx(cw2, file="C:/Users/bda13/Desktop/wcors.xlsx", sheetName="25k-50k", append=TRUE, row.names=T)
    #     write.xlsx(cw3, file="C:/Users/bda13/Desktop/wcors.xlsx", sheetName="50k-100k", append=TRUE, row.names=T)
    #     write.xlsx(cw4, file="C:/Users/bda13/Desktop/wcors.xlsx", sheetName="100k-250k", append=TRUE, row.names=T)
    #     write.xlsx(cw5, file="C:/Users/bda13/Desktop/wcors.xlsx", sheetName="250k-500k", append=TRUE, row.names=T)
    #     write.xlsx(cw6, file="C:/Users/bda13/Desktop/wcors.xlsx", sheetName="500k-1000k", append=TRUE, row.names=T)
    #     write.xlsx(cw7, file="C:/Users/bda13/Desktop/wcors.xlsx", sheetName="1000k+", append=TRUE, row.names=T)
    #     
        
        
      #determine VIF of above models without random effects
          model2<-function(df){lm(form1,data=df)}
          a1<-model2(I[partition$lower,])
          a2<-model2(I[partition$lmiddle,])
          a3<-model2(I[partition$mmiddle,])
          a4<-model2(I[partition$umiddle,])
          a5<-model2(I[partition$uumiddle,])
          a6<-model2(I[partition$upper,])
          a7<-model2(I[partition$uupper,])
          vifdf[1,]<-vif(a1)[grepl("eq|inc",names(vif(a1)))]
          vifdf[2,]<-vif(a2)[grepl("eq|inc",names(vif(a2)))]
          vifdf[3,]<-vif(a3)[grepl("eq|inc",names(vif(a3)))]
          vifdf[4,]<-vif(a4)[grepl("eq|inc",names(vif(a4)))]
          vifdf[5,]<-vif(a5)[grepl("eq|inc",names(vif(a5)))]
          vifdf[6,]<-vif(a6)[grepl("eq|inc",names(vif(a6)))]
          vifdf[7,]<-vif(a7)[grepl("eq|inc",names(vif(a7)))]
          names(vifdf)<-names(vif(a7))[grepl("eq|inc",names(vif(a7)))]
          vifdf$eq_wealth2<-c("lower","lmiddle","mmiddle","umiddle","uumiddle","upper","uupper")
          write.xlsx(vifdf, file="C:/Users/bda13/Desktop/vifdf.xlsx", sheetName="sheet1", row.names=FALSE)
          

    #wealth
        m1.1
        m2.1<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_wealth2 +(1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m3.1<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_wealth2 + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home_d + inc_total + inc_total2 + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
    #home
        m1.2<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m2.2<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_home2 +(1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m3.2<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_home2 + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home_d + inc_total + inc_total2 + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
    #debt
        m1.3<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m2.3<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_debt2 +(1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m3.3<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_debt2 + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home_d + inc_total + inc_total2 + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
    #stocks
        m1.4<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m2.4<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_stock2 +(1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m3.4<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_stock2 + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home_d + inc_total + inc_total2 + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
    #savings
        m1.5<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m2.5<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_savings2 +(1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        m3.5<-summary(lmer(h_general ~ (ego_age+ego_age2)*ego_black2+ego_age+ego_female + eq_savings2 + ego_dg_advanced+ego_dg_bachelors+ego_dg_highschool+ego_dg_somecollege + eq_home_d + inc_total + inc_total2 + (1|ind_id)+ (1|fam_id_68),  data=I))$coefficients
        
  #5 - print to excel
        write.xlsx(m1.1, file="Models.xlsx", sheetName="sheet1", row.names=FALSE)
        write.xlsx(m2.1, file="Models.xlsx", sheetName="sheet2", append=TRUE, row.names=FALSE)
        write.xlsx(m3.1, file="Models.xlsx", sheetName="sheet3", append=TRUE, row.names=FALSE)
        write.xlsx(m1.2, file="Models.xlsx", sheetName="sheet4", append=TRUE, row.names=FALSE)
        write.xlsx(m2.2, file="Models.xlsx", sheetName="sheet5", append=TRUE, row.names=FALSE)
        write.xlsx(m3.2, file="Models.xlsx", sheetName="sheet6", append=TRUE, row.names=FALSE)
        write.xlsx(m1.3, file="Models.xlsx", sheetName="sheet7", append=TRUE, row.names=FALSE)
        write.xlsx(m2.3, file="Models.xlsx", sheetName="sheet8", append=TRUE, row.names=FALSE)
        write.xlsx(m3.3, file="Models.xlsx", sheetName="sheet9", append=TRUE, row.names=FALSE)
        write.xlsx(m1.4, file="Models.xlsx", sheetName="sheet10", append=TRUE, row.names=FALSE)
        write.xlsx(m2.4, file="Models.xlsx", sheetName="sheet11", append=TRUE, row.names=FALSE)
        write.xlsx(m3.4, file="Models.xlsx", sheetName="sheet12", append=TRUE, row.names=FALSE)
        write.xlsx(m1.5, file="Models.xlsx", sheetName="sheet13", append=TRUE, row.names=FALSE)
        write.xlsx(m2.5, file="Models.xlsx", sheetName="sheet14", append=TRUE, row.names=FALSE)
        write.xlsx(m3.5, file="Models.xlsx", sheetName="sheet15", append=TRUE, row.names=FALSE)

        
        a
        