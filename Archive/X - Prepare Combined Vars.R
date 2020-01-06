#Since I am studying health/wealth from a household level,  

D<-readRDS("PreppedData.rds")
D<-D[D$Year>1980,]
D<-D[,order(names(D))]
names(D)

summary(D$head_sex)
summary(D[D$head_sex==2,]$any_wife)

table(D$Year[D$fam_wealth>0])
a<-head(D[D$head_sex==2 &D$any_wife==1,],5)

#For each individual, note whether they were the household head when they first appeared in the data
    #sort data by ind_id and year
        DI<-DI[order(DI$ind_id,DI$Year),]
    #subset data to just first occurrence of each ind_id
        dups<-duplicated(DI$ind_id)
        tempDI<-DI[!dups,]
    #identify relationship to head
        tempDI$rel_to_head<-tempDI$ER30003
    #just keep rel to head and id
        tempDI<-tempDI[,c("rel_to_head","ind_id")]
    #merge info back into DI
        DI<-merge(DI,tempDI,by="ind_id",all=T)
        head(DI)
    #identify whether relationship to head differs from originally
        DI$rel_diff<-DI$rel_to_head!=DI$ER30003

#If the relationship to head changed, I'll want to swap them somehow. The goal is to make this more of an individual level study. 

        sum(D$wife_ind_id=="4173")
        