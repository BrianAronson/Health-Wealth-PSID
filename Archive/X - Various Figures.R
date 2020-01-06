library(ggplot2)
library(data.table)
library(xlsx)

head(I[,c("ind_id","year","fam_id_68","fam_id","eq_wealth")])

head(I[,c("eq_wealth","eq_assets","eq_debt","eq_stock","eq_savings","eq_home")])

mean(I$eq_home[I$eq_wealth<1000000])
mean(I$eq_wealth[I$eq_wealth<1000000])

#0 - Set directory
    setwd("C:/Users/bda13/Desktop/Sociology/Statistics/PSID/")

#1 - read data
    I<-readRDS("PreppedData2.rds")
    I<-I[I$year>2004 & I$year<2012,]

#2 - Create dataframe with most recent info
    I<-I[order(I$year,decreasing = T),]
    I<-I[!(duplicated(I$ind_id)),]
    I$ind_id<-NULL


#3 - Table of all variables:
        sm<-as.data.frame(matrix(0,ncol(I),6))
        names(sm)<-c("variable","mean","median","max","min","sd")
        for(i in 1:length(I)){
          try({
            sm$variable[i]<-names(I)[i]
            sm$mean[i]<-mean(I[,i],na.rm = T)
            sm$median[i]<-median(I[,i],na.rm = T)
            sm$max[i]<-max(I[,i],na.rm = T)
            sm$min[i]<-min(I[,i],na.rm = T)
            sm$sd[i]<-sd(I[,i],na.rm = T)
          })
        }

        write.xlsx(sm, "C:/Users/bda13/Desktop/Sociology/Papers in Progress/Health - Race - Wealth/Figures/Descriptives - All.xlsx")
        
#4 - Tables of all variables by race:
        #White df
          W<-I[I$ego_black==0,]
        #black df
          B<-I[I$ego_black==1,]
        #Prep output
          sm<-as.data.frame(matrix(0,ncol(I),6))
          names(sm)<-c("variable","mean","median","max","min","sd")
          for(i in 1:length(W)){
            try({
              sm$variable[i]<-names(W)[i]
              sm$mean[i]<-mean(W[,i],na.rm = T)
              sm$median[i]<-median(W[,i],na.rm = T)
              sm$max[i]<-max(W[,i],na.rm = T)
              sm$min[i]<-min(W[,i],na.rm = T)
              sm$sd[i]<-sd(W[,i],na.rm = T)
            })
          }
          sm2<-sm
        # write.xlsx(sm, "C:/Users/bda13/Desktop/Sociology/Papers in Progress/Health - Race - Wealth/Figures/Descriptives - Whites.xlsx")
          write.xlsx(sm, "C:/Users/bda13/Desktop/tempw.xlsx")
          
        #Blacks
          B<-I[I$ego_black==1,]
          sm<-as.data.frame(matrix(0,ncol(I),6))
          names(sm)<-c("variable","mean","median","max","min","sd")
        for(i in 1:length(B)){
          try({
            sm$variable[i]<-names(B)[i]
            sm$mean[i]<-mean(B[,i],na.rm = T)
            sm$median[i]<-median(B[,i],na.rm = T)
            sm$max[i]<-max(B[,i],na.rm = T)
            sm$min[i]<-min(B[,i],na.rm = T)
            sm$sd[i]<-sd(B[,i],na.rm = T)
          })
        }

        # write.xlsx(sm, "C:/Users/bda13/Desktop/Sociology/Papers in Progress/Health - Race - Wealth/Figures/Descriptives - Blacks.xlsx")
          write.xlsx(sm, "C:/Users/bda13/Desktop/tempb.xlsx")
          
          
#5 - wealth distributions
    #read data again
        I<-readRDS("PreppedData2.rds")
        options(scipen = 999)
        
    #estimate distributions
        W<-I[!is.na(I$eq_wealth) & I$year==2015,]
        W$eq_wealth<-W$eq_wealth/1000
        library(ggplot2)
        library(scales)
        W$color=as.numeric(2)
        ggplot(W[W$eq_wealth<2000&W$eq_wealth>-500,], aes(x = eq_wealth,fill=color))+
        geom_density(alpha = 0.35,fill="blue")+
        scale_x_continuous(label=dollar_format(),breaks = (-1:4)*500)+
          ggtitle("Distribution of Net Worth in PSID")+
          labs(x="Net Worth (Thousands)",y="Density")+
        guides(fill=FALSE)+
        theme(axis.text = element_text(size=20),
              axis.title = element_text(size=26),
              plot.title=element_text(size=32,hjust=.38))
        
        plot(density(sort(W$eq_wealth)))
        plot(density(sort(W$eq_wealth)),main="PSID Wealth Distribution")
        plot(density(sort(W$eq_wealth[W$eq_wealth<2000000 & W$eq_wealth>-500000])),main="PSID Wealth Distribution")
        plot(density(sort(W$eq_wealth[W$eq_wealth<0])),main="PSID Wealth Distribution")
        plot(density(sort(W$eq_wealth[W$eq_wealth>500000])),main="PSID Wealth Distribution")
        
#6 - Wealth health correlations
    #read data again
        I<-readRDS("PreppedData2.rds")
    #wealth and general health
        I$eq_wealth<-I$eq_wealth/1000
        #make separate dataframes with health and wealth variables of interest
            temp<-I[I$eq_wealth<1000 & I$eq_wealth>0,]
            temp2<-temp[,c("eq_wealth","h_activities")]
            temp3<-temp[,c("eq_wealth","h_conditions")]
            temp4<-temp[,c("eq_wealth","h_general")]
            temp5<-temp[,c("eq_wealth","h_BMI")]
            temp6<-temp[,c("eq_wealth","h_disabled")]
            temp7<-temp[,c("eq_wealth","h_lim_work")]
            temp8<-temp[,c("eq_wealth","h_heart_attack")]
            temp9<-temp[,c("eq_wealth","h_stroke")]
            temp10<-temp[,c("eq_wealth","h_hospital")]
        #rename health variable to description of health variable
            temp2$Health="Activities Limitation"
            temp3$Health="Number of Health Conditions"
            temp4$Health="General Health"
            temp5$Health="BMI"
            temp6$Health="Rate of Disability"
            temp7$Health="Health Limits Work"
            temp8$Health="Rate of Heart Attacks"
            temp9$Health="Rate of Strokes"
            temp10$Health="Average Days in Hospital"
        #rename health variables to value
            names(temp2)[2]<-"value"
            names(temp3)[2]<-"value"
            names(temp4)[2]<-"value"
            names(temp5)[2]<-"value"
            names(temp6)[2]<-"value"
            names(temp7)[2]<-"value"
            names(temp8)[2]<-"value"
            names(temp9)[2]<-"value"
            names(temp10)[2]<-"value"
        #combine the data
            temp11<-rbind(temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10)
        #plot
            p<-ggplot(temp11,aes(x=eq_wealth,y=value))
            p+geom_smooth(size=2)+
              scale_colour_gradient(low = "white", high = "Dark Blue")+
              ggtitle("Correlation between Health and Wealth in 2015")+
              labs(x="Wealth")+
              facet_wrap(~Health, scales="free")
            
            p<-ggplot(temp4,aes(x=eq_wealth,y=value))
            p+geom_smooth(size=2)+
              scale_colour_gradient(low = "white", high = "Dark Blue")+
              ggtitle("Correlation between Health and Wealth in 2015")+
              labs(x="Wealth")
            
            p+geom_smooth(size=2)+
              scale_colour_gradient(low = "white", high = "Dark Blue")+
              ggtitle("Association between General Health and Net Worth")+
              scale_x_continuous(label=dollar_format(),breaks = (-1:8)*250)+
              labs(x="Net Worth (Thousands)",y="General Health (higher=worse)")+
              guides(fill=FALSE)+
              theme(axis.text = element_text(size=20),
                    axis.title = element_text(size=26),
                    plot.title=element_text(size=32,hjust=.38))
          
    #debt and general health
        #make separate dataframes with health and wealth variables of interest
            temp<-I[I$year==2015 & I$eq_debt<500000 & I$eq_debt>-200000,]
            temp2<-temp[,c("eq_debt","h_activities")]
            temp3<-temp[,c("eq_debt","h_conditions")]
            temp4<-temp[,c("eq_debt","h_general")]
            temp5<-temp[,c("eq_debt","h_BMI")]
            temp6<-temp[,c("eq_debt","h_disabled")]
            temp7<-temp[,c("eq_debt","h_lim_work")]
            temp8<-temp[,c("eq_debt","h_heart_attack")]
            temp9<-temp[,c("eq_debt","h_stroke")]
            temp10<-temp[,c("eq_debt","h_hospital")]
        #rename health variable to description of health variable
            temp2$Health="Activities Limitation (higher = more disabled)"
            temp3$Health="Number of Health Conditions (higher = more disabled)"
            temp4$Health="General Health (higher = healthier)"
            temp5$Health="BMI"
            temp6$Health="Rate of Disability"
            temp7$Health="Health Limits Work (higher = more disabled)"
            temp8$Health="Rate of Heart Attacks"
            temp9$Health="Rate of Strokes"
            temp10$Health="Average Days in Hospital"
        #rename health variables to value
            names(temp2)[2]<-"value"
            names(temp3)[2]<-"value"
            names(temp4)[2]<-"value"
            names(temp5)[2]<-"value"
            names(temp6)[2]<-"value"
            names(temp7)[2]<-"value"
            names(temp8)[2]<-"value"
            names(temp9)[2]<-"value"
            names(temp10)[2]<-"value"
        #combine the data
            temp11<-rbind(temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10)
        #plot
            p<-ggplot(temp11,aes(x=eq_debt,y=value))
            p+geom_smooth(size=2)+
              scale_colour_gradient(low = "white", high = "Dark Blue")+
              ggtitle("Correlation between Health and Debt in 2015")+
              labs(x="Debt")+
              facet_wrap(~Health, scales="free")

    #wealth and general health and age
            I$ego_agecuts<-ifelse(I$ego_age<40,"25-40",ifelse(I$ego_age<55,"40-55","55+"))
        #make separate dataframes with health and wealth variables of interest
            temp<-I[I$year==2015 & I$eq_wealth<500000 & I$eq_wealth>-200000,]
            temp2<-temp[,c("eq_wealth","h_activities","ego_agecuts")]
            temp3<-temp[,c("eq_wealth","h_conditions","ego_agecuts")]
            temp4<-temp[,c("eq_wealth","h_general","ego_agecuts")]
            temp5<-temp[,c("eq_wealth","h_BMI","ego_agecuts")]
            temp6<-temp[,c("eq_wealth","h_disabled","ego_agecuts")]
            temp7<-temp[,c("eq_wealth","h_lim_work","ego_agecuts")]
            temp8<-temp[,c("eq_wealth","h_heart_attack","ego_agecuts")]
            temp9<-temp[,c("eq_wealth","h_stroke","ego_agecuts")]
            temp10<-temp[,c("eq_wealth","h_hospital","ego_agecuts")]
        #rename health variable to description of health variable
            temp2$Health="Activities Limitation (higher = more disabled)"
            temp3$Health="Number of Health Conditions (higher = more disabled)"
            temp4$Health="General Health (higher = healthier)"
            temp5$Health="BMI"
            temp6$Health="Rate of Disability"
            temp7$Health="Health Limits Work (higher = more disabled)"
            temp8$Health="Rate of Heart Attacks"
            temp9$Health="Rate of Strokes"
            temp10$Health="Average Days in Hospital"
        #rename health variables to value
            names(temp2)[2]<-"value"
            names(temp3)[2]<-"value"
            names(temp4)[2]<-"value"
            names(temp5)[2]<-"value"
            names(temp6)[2]<-"value"
            names(temp7)[2]<-"value"
            names(temp8)[2]<-"value"
            names(temp9)[2]<-"value"
            names(temp10)[2]<-"value"
        #combine the data
            temp11<-rbind(temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10)
            temp11$ego_agecuts<-as.factor(temp11$ego_agecuts)
        #plot
            p<-ggplot(temp11,aes(x=eq_wealth,y=value,color=ego_agecuts))
            p+geom_smooth(size=2)+
              ggtitle("Correlation between Health and Wealth in 2015")+
              labs(x="Wealth")+
              facet_wrap(~Health, scales="free")
                       
    #debt and general health and age
            I$ego_agecuts<-ifelse(I$ego_age<40,"25-40",ifelse(I$ego_age<55,"40-55","55+"))
        #make separate dataframes with health and wealth variables of interest
            temp<-I[I$year==2015 & I$eq_debt<300000 & I$eq_debt>-200000,]
            temp2<-temp[,c("eq_debt","h_activities","ego_agecuts")]
            temp3<-temp[,c("eq_debt","h_conditions","ego_agecuts")]
            temp4<-temp[,c("eq_debt","h_general","ego_agecuts")]
            temp5<-temp[,c("eq_debt","h_BMI","ego_agecuts")]
            temp6<-temp[,c("eq_debt","h_disabled","ego_agecuts")]
            temp7<-temp[,c("eq_debt","h_lim_work","ego_agecuts")]
            temp8<-temp[,c("eq_debt","h_heart_attack","ego_agecuts")]
            temp9<-temp[,c("eq_debt","h_stroke","ego_agecuts")]
            temp10<-temp[,c("eq_debt","h_hospital","ego_agecuts")]
        #rename health variable to description of health variable
            temp2$Health="Activities Limitation (higher = more disabled)"
            temp3$Health="Number of Health Conditions (higher = more disabled)"
            temp4$Health="General Health (higher = healthier)"
            temp5$Health="BMI"
            temp6$Health="Rate of Disability"
            temp7$Health="Health Limits Work (higher = more disabled)"
            temp8$Health="Rate of Heart Attacks"
            temp9$Health="Rate of Strokes"
            temp10$Health="Average Days in Hospital"
        #rename health variables to value
            names(temp2)[2]<-"value"
            names(temp3)[2]<-"value"
            names(temp4)[2]<-"value"
            names(temp5)[2]<-"value"
            names(temp6)[2]<-"value"
            names(temp7)[2]<-"value"
            names(temp8)[2]<-"value"
            names(temp9)[2]<-"value"
            names(temp10)[2]<-"value"
        #combine the data
            temp11<-rbind(temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10)
            temp11$ego_agecuts<-as.factor(temp11$ego_agecuts)
        #plot
            p<-ggplot(temp11,aes(x=eq_debt,y=value,color=ego_agecuts))
            p+geom_smooth(size=2)+
              ggtitle("Correlation between Health and Wealth in 2015")+
              labs(x="debt")+
              facet_wrap(~Health, scales="free")

    #wealth and general health and race
        #make separate dataframes with health and wealth variables of interest
            temp<-I[I$year==2015 & I$eq_wealth<500000 & I$eq_wealth>-200000,]
            temp2<-temp[,c("eq_wealth","h_activities","ego_black")]
            temp3<-temp[,c("eq_wealth","h_conditions","ego_black")]
            temp4<-temp[,c("eq_wealth","h_general","ego_black")]
            temp5<-temp[,c("eq_wealth","h_BMI","ego_black")]
            temp6<-temp[,c("eq_wealth","h_disabled","ego_black")]
            temp7<-temp[,c("eq_wealth","h_lim_work","ego_black")]
            temp8<-temp[,c("eq_wealth","h_heart_attack","ego_black")]
            temp9<-temp[,c("eq_wealth","h_stroke","ego_black")]
            temp10<-temp[,c("eq_wealth","h_hospital","ego_black")]
        #rename health variable to description of health variable
            temp2$Health="Activities Limitation (higher = more disabled)"
            temp3$Health="Number of Health Conditions (higher = more disabled)"
            temp4$Health="General Health (higher = healthier)"
            temp5$Health="BMI"
            temp6$Health="Rate of Disability"
            temp7$Health="Health Limits Work (higher = more disabled)"
            temp8$Health="Rate of Heart Attacks"
            temp9$Health="Rate of Strokes"
            temp10$Health="Average Days in Hospital"
        #rename health variables to value
            names(temp2)[2]<-"value"
            names(temp3)[2]<-"value"
            names(temp4)[2]<-"value"
            names(temp5)[2]<-"value"
            names(temp6)[2]<-"value"
            names(temp7)[2]<-"value"
            names(temp8)[2]<-"value"
            names(temp9)[2]<-"value"
            names(temp10)[2]<-"value"
        #combine the data
            temp11<-rbind(temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10)
            temp11$ego_black<-as.factor(temp11$ego_black)
        #plot
            p<-ggplot(temp11,aes(x=eq_wealth,y=value,color=ego_black))
            p+geom_smooth(size=2)+
              ggtitle("Correlation between Health and Wealth in 2015")+
              labs(x="Wealth")+
              facet_wrap(~Health, scales="free")

    #debt, health, and race
        #make separate dataframes with health and wealth variables of interest
            temp<-I[I$year==2015 & I$debt_student<50000,]
            temp2<-temp[,c("debt_student","h_activities","ego_black")]
            temp3<-temp[,c("debt_student","h_conditions","ego_black")]
            temp4<-temp[,c("debt_student","h_general","ego_black")]
            temp5<-temp[,c("debt_student","h_BMI","ego_black")]
            temp6<-temp[,c("debt_student","h_disabled","ego_black")]
            temp7<-temp[,c("debt_student","h_lim_work","ego_black")]
            temp8<-temp[,c("debt_student","h_heart_attack","ego_black")]
            temp9<-temp[,c("debt_student","h_stroke","ego_black")]
            temp10<-temp[,c("debt_student","h_hospital","ego_black")]
        #rename health variable to description of health variable
            temp2$Health="Activities Limitation (higher = more disabled)"
            temp3$Health="Number of Health Conditions (higher = more disabled)"
            temp4$Health="General Health (higher = healthier)"
            temp5$Health="BMI"
            temp6$Health="Rate of Disability"
            temp7$Health="Health Limits Work (higher = more disabled)"
            temp8$Health="Rate of Heart Attacks"
            temp9$Health="Rate of Strokes"
            temp10$Health="Average Days in Hospital"
        #rename health variables to value
            names(temp2)[2]<-"value"
            names(temp3)[2]<-"value"
            names(temp4)[2]<-"value"
            names(temp5)[2]<-"value"
            names(temp6)[2]<-"value"
            names(temp7)[2]<-"value"
            names(temp8)[2]<-"value"
            names(temp9)[2]<-"value"
            names(temp10)[2]<-"value"
        #combine the data
            temp11<-rbind(temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10)
            temp11$ego_black<-as.factor(temp11$ego_black)
        #plot
            p<-ggplot(temp11,aes(x=debt_student,y=value,color=ego_black))
            p+geom_smooth(size=2)+
              ggtitle("Correlation between Health and debt in 2015")+
              labs(x="debt_student")+
              facet_wrap(~Health, scales="free")

#7 - Wealth over time
    library(ggplot2)
    library(scales)
    I<-readRDS("PreppedData2.rds")
    I<-data.table(I)
    df<-I[,.("Mean Wealth (thousands)"=mean(eq_wealth,na.rm=T)/1000,"Median Wealth (thousands)"=median(eq_wealth,na.rm=T)/1000),by=year]
    df2<-data.frame(year=rep(c(df$year),3),value=c(df$mean/10,df$median,df$sd/10),type=rep(c("Mean","Median","Standard Deviation"),each=nrow(df)))
  
    p<-ggplot(df,aes(x=year))
    p <- p + geom_line(aes(y = `Median Wealth (thousands)`, colour = "Median Wealth"),size=3)
    p <- p + geom_line(aes(y = `Mean Wealth (thousands)`/3, colour = "Mean Wealth"),size=3)
    p <- p + scale_y_continuous(label=dollar_format(), sec.axis = sec_axis(~.*3,name="Mean Wealth (thousands)",label=dollar_format()))
    p <- p + scale_x_continuous(breaks = c(1985, 1990, 1995,2000,2005,2010,2015),name="Year")
    p <- p + ggtitle("Mean and Median Wealth by Year") 
    p+ scale_color_manual(values=c("red", "blue")) + theme(legend.title=element_blank(),text = element_text(size=18),axis.title.y = element_text(margin = margin(r = 20),color="blue"),axis.title.y.right = (element_text(margin = margin(l = 20),color="red"))) 

    
#8 - Components of wealth by quintile
    #a - prep data
        I<-readRDS("PreppedData2.rds")
        #Reduce dataset to variables of interest
            I<-I[,c("h_general","h_BMI","h_activities","h_conditions","h_heart_attack","h_stroke","h_hospital","h_lim_work","h_disabled","ego_age","ego_age2","ego_black2","ego_female","ego_dg_advanced","ego_dg_bachelors","ego_dg_highschool","ego_dg_somecollege","eq_home","eq_bus","eq_savings","eq_otr_estate","eq_stock","eq_debt","inc_total","eq_wealth","ind_id", "year","fam_id_68","fam_id","eq_home_d")] #"eq_vehicle","eq_otr_assets"
        #remove any rows with nas
            I <- I[rowSums(is.na(I))== 0,]
    #b - cut data by wealth
            x=I$eq_wealth
            cuts<-c(min(x)-1,-1,1,10000,50000,100000,300000,max(x)+1)
            I$eq_wealth2<-relevel(cut(x, cuts,right=F), ref="[-1,1)")
            levels(I$eq_wealth2)<-c("0","<0","0-10k","10k-50k","50k-100k","100k-300k","300k+")
            qs<-split(I,I$eq_wealth2)
    #c - estimate the following for each wealth group:
        a<-as.data.frame(matrix(nrow=7,ncol=10))
        for (i in 1:length(qs)){
          temp<-qs[[i]][,c("eq_home","eq_debt","eq_stock","eq_savings","inc_total")]
          for(j in 1:ncol(temp)){
            a[i,(j*2)-1]<-mean(temp[,j])
            a[i,j*2]<-sd(temp[,j])
          }
        }
        a<-a/1000
        a<-round(a,2)
        a[]<-lapply(a,as.character)
        for(i in 1:5){
          a[,2*i]<-paste("(",a[,2*i],")",sep="")
        }
        names(a)<-c("eq_home","","eq_debt","","eq_stock","","eq_savings","","inc_total","")
        row.names(a)<-levels(I$eq_wealth2)
        write.xlsx(a, "C:/Users/bda13/Desktop/Sociology/Papers in Progress/Health - Race - Wealth/Figures/Components of Wealth by Quintile1.xlsx")
    #c2 - produce box plot
        #prep data
            #choose variables
                vars<-c("eq_home","eq_debt","eq_stock","eq_savings","inc_total")
            #create shell of dataframe with three columns (x, y, and facet)
                df<-as.data.frame(matrix(nrow=(nrow(I)*length(vars)),ncol=3))
                names(df)<-c("Variable","Value","Quantile")
            #insert data into dataframe (basically changing wide to long)
                df$Variable<-as.factor(rep(vars,each=nrow(I)))
                df$Quantile<-as.factor(rep(I$eq_wealth2,length(vars)))
                value<-vector()
                temp<-I[,vars[1]]
                for(i in 2:length(vars)){
                  temp<-c(temp,I[,vars[i]])
                }
                df$Value<-temp/1000
                df$Quantile<-relevel(df$Quantile, ref="<0")
                levels(df$Quantile)<-c("Net Worth < 0","Net Worth = 0","Net Worth = 0-10k","Net Worth = 10k-50k","Net Worth = 50k-100k","Net Worth = 100k-300k","Net Worth = 300k+")
                levels(df$Variable)<-c("Debt","Home Equity","Savings","Stocks","Income")
                df<-df[df$Quantile!="Net Worth < 0",]
        #plot
            p <- ggplot(df, aes(x=Variable, y=Value))
            p + geom_boxplot() +
              scale_y_continuous(name="Thousands USD",labels = scales::comma) +
              facet_wrap(~Quantile, scales="free")
            
    #d - create variables measuring proportion of wealth; repeat above
        I$eq_home2<-I$eq_home/I$eq_wealth
        I$eq_debt2<-I$eq_debt/I$eq_wealth
        I$eq_stock2<-I$eq_stock/I$eq_wealth
        I$eq_savings2<-I$eq_savings/I$eq_wealth
        I$inc_total2<-I$inc_total/I$eq_wealth
        
        qs<-split(I,I$eq_wealth2)
        a<-as.data.frame(matrix(nrow=7,ncol=10))
        for (i in 1:length(qs)){
          temp<-qs[[i]][,c("eq_home2","eq_debt2","eq_stock2","eq_savings2","inc_total2")]
          for(j in 1:ncol(temp)){
            a[i,(j*2)-1]<-mean(temp[,j][temp[,j]>0],na.rm=T)
            a[i,j*2]<-sd(temp[,j])
          }
        }
        row.names(a)<-levels(I$eq_wealth2)
        a<-round(a,2)
        a<-a[-1,]
        a<-a[-1,]
        a[]<-lapply(a,as.character)
        for(i in 1:5){
          a[,2*i]<-paste("(",a[,2*i],")",sep="")
        }
        names(a)<-c("eq_home","","eq_debt","","eq_stock","","eq_savings","","inc_total","")
        write.xlsx(a, "C:/Users/bda13/Desktop/Sociology/Papers in Progress/Health - Race - Wealth/Figures/Components of Wealth by Quintile2.xlsx")
    #d2 - produce box plot
        J<-I[I$year==2015,]

        #prep data
            #choose variables
                vars<-c("eq_home2","eq_debt2","eq_stock2","eq_savings2","inc_total2")
            #create shell of dataframe with three columns (x, y, and facet)
                df<-as.data.frame(matrix(nrow=(nrow(J)*length(vars)),ncol=3))
                names(df)<-c("Variable","Value","Quantile")
            #insert data into dataframe (basically changing wide to long)
                df$Variable<-as.factor(rep(vars,each=nrow(J)))
                df$Quantile<-as.factor(rep(J$eq_wealth2,length(vars)))
                value<-vector()
                temp<-J[,vars[1]]
                for(i in 2:length(vars)){
                  temp<-c(temp,J[,vars[i]])
                }
                df$Value<-temp
                df$Quantile<-relevel(df$Quantile, ref="<0")
                levels(df$Quantile)<-c("Net Worth < 0","Net Worth = 0","Net Worth = 0-10k","Net Worth = 10k-50k","Net Worth = 50k-100k","Net Worth = 100k-300k","Net Worth = 300k+")
                levels(df$Variable)<-c("Debt","Home","Savings","Stocks","Income")
                df<-df[df$Quantile!="Net Worth < 0" & df$Quantile!="Net Worth = 0",]

                df$Value[df$Value>1]<-1
                df$Value[df$Value<0]<-0

        #plot
            p <- ggplot(df, aes(x=Variable, y=Value))
            p + geom_boxplot() +
              scale_y_continuous(limits=c(0, 1),name="Proportion (Variable / Net Worth)",labels = scales::comma) +
              scale_x_discrete(name ="")+
              theme(legend.title=element_blank(),text = element_text(size=12),axis.title.y = element_text(size=16),title=element_text(size=20, face='bold'))+
              ggtitle('Proportion of Financial Asset to Net Worth')+
              facet_wrap(~Quantile, scales="free")
              

              prop.table(table(I$eq_stock==0,I$eq_wealth2),2)  
              #Here is a table indicating the proportion of households with more than $0 of net worth in stocks.
