#1 - Load package
    library(easyPSID)

#2 - Set directory
    setwd("C:/Users/bda13/Desktop/Sociology/Statistics/PSID/")
    
#3 - Unzip and import all data
    PSID.Unzip(Type = c("Family","Individual","Wealth"))
    PSID.Import(Type = c("Family","Individual","Wealth"))
    
#4 - Rename variables
    PSID.Rename.Fam(replace=T)
    PSID.Rename.Ind(custvars=c("ER30001", "ER30003", "ER30004", "ER30006", "ER30010", "ER30011", "ER30012", "ER30013", "ER30014", "ER30019", "ER30021", "ER30029", "ER30030", "ER30031", "ER30039", "ER30151", "ER30171", "ER30173", "ER30175", "ER30293", "ER30294", "ER30526", "ER30527", "ER30002", "ER32000", "ER32001", "ER32004", "ER32009", "ER32010", "ER32016", "ER32017", "ER32050", "id_68", "Year", "ER33518", "ER34129","ER33536Q"))
    PSID.Merge.Aux(Replace = T)
