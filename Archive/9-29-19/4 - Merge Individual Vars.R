#Merge individual and family

#0 - Set directory
    setwd("C:/Users/bda13/Desktop/Sociology/Statistics/PSID/")
    
#1 - load data
    I <- readRDS("IndividualData.rds")
    D <- readRDS("PanelData.rds")

#2 - Match by year and id in that year
    DI <- merge(D,I,by=c("fam_id","Year"))
    
#3 - Order by fam id and year
    DI <- DI[order(DI$ind_id,DI$ind_fam_id_68,DI$Year),]
    
#4 - Save
    saveRDS(DI,"PreppedData.rds")
    
 