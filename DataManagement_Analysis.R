library(dplyr)
library(ggplot2)
library(stringr)
library(readxl)
library(tidyverse)
library(reshape2)
library(psych)
library(Rcpp) 
library(lme4)
library(performance)
library(papaja) 
library(RColorBrewer)
library(sjPlot)
library(jtools)
library(viridis)
library(gridExtra)
library(corrplot)
library(moments)

##################################################################################
##importing the datasets
##################################################################################

#psychological traits
moba8 <- read.csv("N:/durable/data/moba/Original files/csv/PDB2601_Q8yrs_v12.csv", sep=",",na.strings = c("NA"," "))
moba <- moba8 |> dplyr::select(c( 'PREG_ID_2601',
                                  'BARN_NR',
                                  'VERSJON_SKJEMA_8AAR_TBL1',
                                  
                                  'NN68':'NN80',                        #SMFQ depression
                                  
                                  'NN81':'NN110','NN368':'NN372',       #NHiPIC-30 personality
                                  
                                  'NN111':'NN144',                      #RSDBD ADHD
                                  
                                  'NN145':'NN149',                      #SCARED anxiety
                                  
                                  'NN150':'NN189',                      #SCQ autism
                                  
                                  'NN190':'NN207' ,                     #CEBQ eating behavior
                                  
                                  'NN211':'NN226',                      #CCC2 communication
                                  
                                  'NN227':'NN233','NN374'               #sprek20 communication
))


#different versions
table(moba$VERSJON_SKJEMA_8AAR_TBL1)
moba[moba$VERSJON_SKJEMA_8AAR_TBL1=='SKJEMA_8AAR',]
moba[moba$VERSJON_SKJEMA_8AAR_TBL1=='SKJEMA_8AARB',]
moba[moba$VERSJON_SKJEMA_8AAR_TBL1=='SKJEMA_8AARC',]
moba[moba$VERSJON_SKJEMA_8AAR_TBL1=='SKJEMA_8AARKORT',]


#barn number
table(moba$BARN_NR)

#duplicated id
length(unique(moba$PREG_ID_2601))
table(table(moba$PREG_ID_2601))

# dupmoba <- moba %>% group_by(PREG_ID_2601)%>% filter(n()==1)

#creating unique identifiers for moba dataset
moba$unique_id <- paste0(moba$PREG_ID_2601,'-',moba$BARN_NR)
moba <- moba[,c(173,1:172)]
table(table(moba$unique_id)) #no duplicates


######################################################################################
#school performance
gpa <- read.csv("N:/durable/data/registers/SSB/01_data/data_v4.0/EDUCATION_ADVANCED/csv/EDUCATION_F_UTD_KURS_GR.csv", sep=",",na.strings = c("NA"," "))
gpa <- na.omit(gpa)
str(gpa)
range(gpa$REGDATO)
# summarytools::dfSummary(gpa)
sd(gpa$GRUNNSKOLEPOENG)
#remove the GPA with scores less than 10 and those larger than 60
gpa <- gpa[!gpa$GRUNNSKOLEPOENG>60,]
gpa <- gpa[!gpa$GRUNNSKOLEPOENG<10,]

#any duplicated gpa for individuals?
table(duplicated(gpa$w19_0634_lnr))
# dupgpa <- gpa %>% group_by(w19_0634_lnr)%>% filter(n()>1)
# table(table(dupgpa$w19_0634_lnr))

#keeping the latest gpa for these duplicates
# table(dupgpa$REGDATO)
gpa <- gpa %>% group_by(w19_0634_lnr)%>% filter(REGDATO==max(REGDATO))

#removing the duplicates
ugpa <- gpa[!duplicated(gpa$w19_0634_lnr), ]

#time range when they receive the gpa
range(ugpa$REGDATO)

mean(ugpa$GRUNNSKOLEPOENG)
sd(ugpa$GRUNNSKOLEPOENG)

#creating standardized gpa score with the population mean and sd 
ugpa$z_gpa <- (ugpa$GRUNNSKOLEPOENG - mean(ugpa$GRUNNSKOLEPOENG)) / sd(ugpa$GRUNNSKOLEPOENG)

range(ugpa$z_gpa)
mean(ugpa$z_gpa)
sd(ugpa$z_gpa)

######################################################################################
#linkage data
linakage <- read.csv("N:/durable/data/moba/linkage/PDB2601_kobling_SSB_v12.csv",na.strings = c("NA"," "))
#unique identifier for each child
linakage$unique_id <- paste0(linakage$PREG_ID_2601,'-',linakage$barn_nr)
linakage <- linakage |>filter(grepl("CHILD", rolle))
colnames(moba)
joinmoba <- inner_join(linakage[,4:5],moba,by = "unique_id")

#check any duplicates in the project
table(table(joinmoba$w19_0634_lnr))
joinmoba[duplicated(joinmoba$w19_0634_lnr),]
table(table(joinmoba$unique_id))
table(table(joinmoba$PREG_ID_2601))
umoba <- joinmoba[!joinmoba$w19_0634_lnr=='P007036657',]

#joining the moba and gpa dataset 
mobagpa <- inner_join(umoba,ugpa)

#year ranges in moba 
range(mobagpa$AVGDATO)
colnames(mobagpa)

####################################################################################

#school id
schoolid <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/EDUCATION_VGS_GRS/csv/EDUCATION_NASJONALE_PROVER.csv", sep=",",na.strings = c("NA"," "))
str(schoolid)

primary <- schoolid |> filter(grepl("05", PROVE))|> dplyr::select(w19_0634_lnr, primaryid = w19_0634_lopenr_orgnr)
str(primary)
primary <- na.omit(primary)
lowersec <- schoolid |>filter(grepl("09", PROVE))|> dplyr::select(w19_0634_lnr, lowsecid = w19_0634_lopenr_orgnr)
lowersec <- na.omit(lowersec)

#unique school ids
uprimary <- primary[!duplicated(primary$w19_0634_lnr), ]
ulowersec <- lowersec[!duplicated(lowersec$w19_0634_lnr), ]

#adding the school id to the sample
registrylist <- list(mobagpa,uprimary,ulowersec) #,ugeo,ufam
regismobamerge <- registrylist %>% reduce(inner_join,by='w19_0634_lnr')


#age 8 - age 16
# identify the year when students receive their gpa and when they were at 8
range(regismobamerge$REGDATO)
regismobamerge <- transform(regismobamerge, gpa_dato = substr(REGDATO, 1, 4))
colnames(regismobamerge)
regismobamerge$moba_dato <- as.numeric(regismobamerge$gpa_dato) - 8
range(regismobamerge$moba_dato)
range(regismobamerge$gpa_dato)
colnames(regismobamerge)
# write.csv(regismobamerge,"preprepared_df.csv",row.names=FALSE)
preprepared_df <- read.csv("N:/durable/projects/Qi/Rscript/new/preprepared_df.csv", sep=",",na.strings = c("NA"," "))


####################################################################################

#geographical codes
geo08 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_RESIDENCY/csv/POPULATION_GRUNNKRETS_2008.csv",col.names = c("w19_0634_lnr", "Kommune_2008", "Grunnkretser_2008", "Delomraade_2008"),na.strings = c("NA",""))
geo09 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_RESIDENCY/csv/POPULATION_GRUNNKRETS_2009.csv",col.names = c("w19_0634_lnr", "Kommune_2009", "Grunnkretser_2009", "Delomraade_2009"),na.strings = c("NA",""))
geo10 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_RESIDENCY/csv/POPULATION_GRUNNKRETS_2010.csv",col.names = c("w19_0634_lnr", "Kommune_2010", "Grunnkretser_2010", "Delomraade_2010"),na.strings = c("NA",""))
geo11 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_RESIDENCY/csv/POPULATION_GRUNNKRETS_2011.csv",col.names = c("w19_0634_lnr", "Kommune_2011", "Grunnkretser_2011", "Delomraade_2011"),na.strings = c("NA",""))
geo12 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_RESIDENCY/csv/POPULATION_GRUNNKRETS_2012.csv",col.names = c("w19_0634_lnr", "Kommune_2012", "Grunnkretser_2012", "Delomraade_2012"),na.strings = c("NA",""))
geo13 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_RESIDENCY/csv/POPULATION_GRUNNKRETS_2013.csv",col.names = c("w19_0634_lnr", "Kommune_2013", "Grunnkretser_2013", "Delomraade_2013"),na.strings = c("NA",""))
geo14 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_RESIDENCY/csv/POPULATION_GRUNNKRETS_2014.csv",col.names = c("w19_0634_lnr", "Kommune_2014", "Grunnkretser_2014", "Delomraade_2014"),na.strings = c("NA",""))
geo15 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_RESIDENCY/csv/POPULATION_GRUNNKRETS_2015.csv",col.names = c("w19_0634_lnr", "Kommune_2015", "Grunnkretser_2015", "Delomraade_2015"),na.strings = c("NA",""))
geo16 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_RESIDENCY/csv/POPULATION_GRUNNKRETS_2016.csv",col.names = c("w19_0634_lnr", "Kommune_2016", "Grunnkretser_2016", "Delomraade_2016"),na.strings = c("NA",""))
geo17 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_RESIDENCY/csv/POPULATION_GRUNNKRETS_2017.csv",col.names = c("w19_0634_lnr", "Kommune_2017", "Grunnkretser_2017", "Delomraade_2017"),na.strings = c("NA",""))
geo18 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_RESIDENCY/csv/POPULATION_GRUNNKRETS_2018.csv",col.names = c("w19_0634_lnr", "Kommune_2018", "Grunnkretser_2018", "Delomraade_2018"),na.strings = c("NA",""))
geo19 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_RESIDENCY/csv/POPULATION_GRUNNKRETS_2019.csv",col.names = c("w19_0634_lnr", "Kommune_2019", "Grunnkretser_2019", "Delomraade_2019"),na.strings = c("NA",""))
geo20 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_RESIDENCY/csv/POPULATION_GRUNNKRETS_2020.csv",col.names = c("w19_0634_lnr", "Kommune_2020", "Grunnkretser_2020", "Delomraade_2020"),na.strings = c("NA",""))
geo21 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_RESIDENCY/csv/POPULATION_GRUNNKRETS_2021.csv",col.names = c("w19_0634_lnr", "Kommune_2021", "Grunnkretser_2021", "Delomraade_2021"),na.strings = c("NA",""))
geo22 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_RESIDENCY/csv/POPULATION_GRUNNKRETS_2022.csv",col.names = c("w19_0634_lnr", "Kommune_2022", "Grunnkretser_2022", "Delomraade_2022"),na.strings = c("NA",""))


geo08c <- na.omit(geo08) |> select(w19_0634_lnr, Kommune_2008,Delomraade_2008, Grunnkretser_2008)
geo09c <- na.omit(geo09) |> select(w19_0634_lnr, Kommune_2009,Delomraade_2009, Grunnkretser_2009)
geo10c <- na.omit(geo10) |> select(w19_0634_lnr, Kommune_2010,Delomraade_2010, Grunnkretser_2010)
geo11c <- na.omit(geo11) |> select(w19_0634_lnr, Kommune_2011,Delomraade_2011, Grunnkretser_2011)
geo12c <- na.omit(geo12) |> select(w19_0634_lnr, Kommune_2012,Delomraade_2012, Grunnkretser_2012)
geo13c <- na.omit(geo13) |> select(w19_0634_lnr, Kommune_2013,Delomraade_2013, Grunnkretser_2013)
geo14c <- na.omit(geo14) |> select(w19_0634_lnr, Kommune_2014,Delomraade_2014, Grunnkretser_2014)
geo15c <- na.omit(geo15) |> select(w19_0634_lnr, Kommune_2015,Delomraade_2015, Grunnkretser_2015)
geo16c <- na.omit(geo16) |> select(w19_0634_lnr, Kommune_2016,Delomraade_2016, Grunnkretser_2016)
geo17c <- na.omit(geo17) |> select(w19_0634_lnr, Kommune_2017,Delomraade_2017, Grunnkretser_2017)
geo18c <- na.omit(geo18) |> select(w19_0634_lnr, Kommune_2018,Delomraade_2018, Grunnkretser_2018)
geo19c <- na.omit(geo19) |> select(w19_0634_lnr, Kommune_2019,Delomraade_2019, Grunnkretser_2019)
geo20c <- na.omit(geo20) |> select(w19_0634_lnr, Kommune_2020,Delomraade_2020, Grunnkretser_2020)
geo21c <- na.omit(geo21) |> select(w19_0634_lnr, Kommune_2021,Delomraade_2021, Grunnkretser_2021)
geo22c <- na.omit(geo22) |> select(w19_0634_lnr, Kommune_2022,Delomraade_2022, Grunnkretser_2022)

geolist <- list(geo08c,geo09c,geo10c,geo11c,geo12c,geo13c,geo14c,geo15c,geo16c,geo17c,geo18c,geo19c,geo20c,geo21c,geo22c) 
geomerge <- geolist %>% reduce(inner_join,by='w19_0634_lnr')
geomerge[duplicated(geomerge$w19_0634_lnr), ]

####################################################################################

#household id
fam08 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_SOCIAL/csv/POPULATION_HUSH_FAM_2008.csv",na.strings = c("NA"," "))
fam09 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_SOCIAL/csv/POPULATION_HUSH_FAM_2009.csv",na.strings = c("NA"," "))
fam10 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_SOCIAL/csv/POPULATION_HUSH_FAM_2010.csv",na.strings = c("NA"," "))
fam11 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_SOCIAL/csv/POPULATION_HUSH_FAM_2011.csv",na.strings = c("NA"," "))
fam12 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_SOCIAL/csv/POPULATION_HUSH_FAM_2012.csv",na.strings = c("NA"," "))
fam13 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_SOCIAL/csv/POPULATION_HUSH_FAM_2013.csv",na.strings = c("NA"," "))
fam14 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_SOCIAL/csv/POPULATION_HUSH_FAM_2014.csv",na.strings = c("NA"," "))
fam15 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_SOCIAL/csv/POPULATION_HUSH_FAM_2015.csv",na.strings = c("NA"," "))
fam16 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_SOCIAL/csv/POPULATION_HUSH_FAM_2016.csv",na.strings = c("NA"," "))
fam17 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_SOCIAL/csv/POPULATION_HUSH_FAM_2017.csv",na.strings = c("NA"," "))
fam18 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_SOCIAL/csv/POPULATION_HUSH_FAM_2018.csv",na.strings = c("NA"," "))
fam19 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_SOCIAL/csv/POPULATION_HUSH_FAM_2019.csv",na.strings = c("NA"," "))
fam20 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_SOCIAL/csv/POPULATION_HUSH_FAM_2020.csv",na.strings = c("NA"," "))
fam21 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_SOCIAL/csv/POPULATION_HUSH_FAM_2021.csv",na.strings = c("NA"," "))
fam22 <- read.csv("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_SOCIAL/csv/POPULATION_HUSH_FAM_2022.csv",na.strings = c("NA"," "))


fam08c <- na.omit(fam08) |> select(w19_0634_lnr,fam_2008 = lopenr_familie)
fam09c <- na.omit(fam09) |> select(w19_0634_lnr,fam_2009 = lopenr_familie)
fam10c <- na.omit(fam10) |> select(w19_0634_lnr,fam_2010 = lopenr_familie)
fam11c <- na.omit(fam11) |> select(w19_0634_lnr,fam_2011 = lopenr_familie)
fam12c <- na.omit(fam12) |> select(w19_0634_lnr,fam_2012 = lopenr_familie)
fam13c <- na.omit(fam13) |> select(w19_0634_lnr,fam_2013 = lopenr_familie)
fam14c <- na.omit(fam14) |> select(w19_0634_lnr,fam_2014 = lopenr_familie)
fam15c <- na.omit(fam15) |> select(w19_0634_lnr,fam_2015 = lopenr_familie)
fam16c <- na.omit(fam16) |> select(w19_0634_lnr,fam_2016 = lopenr_familie)
fam17c <- na.omit(fam17) |> select(w19_0634_lnr,fam_2017 = lopenr_familie)
fam18c <- na.omit(fam18) |> select(w19_0634_lnr,fam_2018 = lopenr_familie)
fam19c <- na.omit(fam19) |> select(w19_0634_lnr,fam_2019 = lopenr_familie)
fam20c <- na.omit(fam20) |> select(w19_0634_lnr,fam_2020 = lopenr_familie)
fam21c <- na.omit(fam21) |> select(w19_0634_lnr,fam_2021 = lopenr_familie)
fam22c <- na.omit(fam22) |> select(w19_0634_lnr,fam_2022 = lopenr_familie)

famlist <- list(fam08c,fam09c,fam10c,fam11c,fam12c,fam13c,fam14c,fam15c,fam16c,fam17c,fam18c,fam19c,fam20c,fam21c,fam22c) 
fammerge <- famlist %>% reduce(inner_join,by='w19_0634_lnr')
ufam <- fammerge[!duplicated(fammerge$w19_0634_lnr), ]

#merge the geo and fam code to the regisdata
geofamlist <- list(regismobamerge,geomerge,ufam)
geofammerge <- geofamlist %>% reduce(inner_join,by='w19_0634_lnr')
df <- geofammerge[!(is.na(geofammerge$primaryid) | geofammerge$primaryid=="" |geofammerge$lowsecid==""), ]

#save the dataset
# write.csv(df,"regismobamerge.csv",row.names=FALSE)


###############################################################################################################
#------------------------------------------managing the geo codes and family codes -------------------------------
###############################################################################################################
library(dplyr)
library(ggplot2)
library(summarytools)
library(stringr)
library(readxl)
library(tidyverse)
library(reshape2)
library(psych)
library(Rcpp) 
library(mirt)
library(misty)
library(janitor)

df <- read.csv("N:/durable/projects/Qi/Rscript/new/regismobamerge.csv", sep=",",na.strings = c("NA"," "))
str(df)
colnames(df)
##creating new contacted geo variables
df_geo <- df |> dplyr::select(w19_0634_lnr,gpa_dato,moba_dato,Kommune_2008:Grunnkretser_2022)

#padding the 0s
df_geo <- df_geo |>
  mutate_at(vars(contains("Delomraade")), ~ str_pad(., width = 2, side = "left", pad = "0"))|>
  mutate_at(vars(contains("Kommune")), ~ str_pad(., width = 4, side = "left", pad = "0"))

#pasting the codes together
df_geo$KomDel_2008 <- paste0(df_geo$Kommune_2008,df_geo$Delomraade_2008)
df_geo$KomDel_2009 <- paste0(df_geo$Kommune_2009,df_geo$Delomraade_2009)
df_geo$KomDel_2010 <- paste0(df_geo$Kommune_2010,df_geo$Delomraade_2010)
df_geo$KomDel_2011 <- paste0(df_geo$Kommune_2011,df_geo$Delomraade_2011)
df_geo$KomDel_2012 <- paste0(df_geo$Kommune_2012,df_geo$Delomraade_2012)
df_geo$KomDel_2013 <- paste0(df_geo$Kommune_2013,df_geo$Delomraade_2013)
df_geo$KomDel_2014 <- paste0(df_geo$Kommune_2014,df_geo$Delomraade_2014)
df_geo$KomDel_2015 <- paste0(df_geo$Kommune_2015,df_geo$Delomraade_2015)
df_geo$KomDel_2016 <- paste0(df_geo$Kommune_2016,df_geo$Delomraade_2016)
df_geo$KomDel_2017 <- paste0(df_geo$Kommune_2017,df_geo$Delomraade_2017)
df_geo$KomDel_2018 <- paste0(df_geo$Kommune_2018,df_geo$Delomraade_2018)
df_geo$KomDel_2019 <- paste0(df_geo$Kommune_2019,df_geo$Delomraade_2019)
df_geo$KomDel_2020 <- paste0(df_geo$Kommune_2020,df_geo$Delomraade_2020)
df_geo$KomDel_2021 <- paste0(df_geo$Kommune_2021,df_geo$Delomraade_2021)
df_geo$KomDel_2022 <- paste0(df_geo$Kommune_2022,df_geo$Delomraade_2022)

df_geo$KomDelGrunn_2008 <- paste0(df_geo$Kommune_2008,df_geo$Delomraade_2008,df_geo$Grunnkretser_2008)
df_geo$KomDelGrunn_2009 <- paste0(df_geo$Kommune_2009,df_geo$Delomraade_2009,df_geo$Grunnkretser_2009)
df_geo$KomDelGrunn_2010 <- paste0(df_geo$Kommune_2010,df_geo$Delomraade_2010,df_geo$Grunnkretser_2010)
df_geo$KomDelGrunn_2011 <- paste0(df_geo$Kommune_2011,df_geo$Delomraade_2011,df_geo$Grunnkretser_2011)
df_geo$KomDelGrunn_2012 <- paste0(df_geo$Kommune_2012,df_geo$Delomraade_2012,df_geo$Grunnkretser_2012)
df_geo$KomDelGrunn_2013 <- paste0(df_geo$Kommune_2013,df_geo$Delomraade_2013,df_geo$Grunnkretser_2013)
df_geo$KomDelGrunn_2014 <- paste0(df_geo$Kommune_2014,df_geo$Delomraade_2014,df_geo$Grunnkretser_2014)
df_geo$KomDelGrunn_2015 <- paste0(df_geo$Kommune_2015,df_geo$Delomraade_2015,df_geo$Grunnkretser_2015)
df_geo$KomDelGrunn_2016 <- paste0(df_geo$Kommune_2016,df_geo$Delomraade_2016,df_geo$Grunnkretser_2016)
df_geo$KomDelGrunn_2017 <- paste0(df_geo$Kommune_2017,df_geo$Delomraade_2017,df_geo$Grunnkretser_2017)
df_geo$KomDelGrunn_2018 <- paste0(df_geo$Kommune_2018,df_geo$Delomraade_2018,df_geo$Grunnkretser_2018)
df_geo$KomDelGrunn_2019 <- paste0(df_geo$Kommune_2019,df_geo$Delomraade_2019,df_geo$Grunnkretser_2019)
df_geo$KomDelGrunn_2020 <- paste0(df_geo$Kommune_2020,df_geo$Delomraade_2020,df_geo$Grunnkretser_2020)
df_geo$KomDelGrunn_2021 <- paste0(df_geo$Kommune_2021,df_geo$Delomraade_2021,df_geo$Grunnkretser_2021)
df_geo$KomDelGrunn_2022 <- paste0(df_geo$Kommune_2022,df_geo$Delomraade_2022,df_geo$Grunnkretser_2022)


colnames(df_geo)

#----------------------------------------------------------------------------------
#read the changes
change <- read_excel("eventualchanges_norgeo.xlsx")
change_na <- change |> drop_na()
change_clean <- filter(change_na, oldName != "Nothing from API")

#separate the code for kommune, delomrede, and grunnkrets
change_sep <- transform(change_clean, oldKommune = substr(oldCode, 1, 4), oldDelomrede = substr(oldCode, 1, 6),oldGrunnkrets = substr(oldCode, 1, 8))
change_sep <- transform(change_sep, newKommune = substr(currentCode, 1, 4), newDelomrede = substr(currentCode, 1, 6),newGrunnkrets = substr(currentCode, 1, 8))

change_kom <- change_sep |> select(oldKommune,newKommune) |> unique() |> filter(oldKommune!=newKommune)
change_komdel <- change_sep |> select(oldDelomrede,newDelomrede) |> unique() |> filter(oldDelomrede!=newDelomrede)
change_komdelgrunn <- change_sep |> select(oldGrunnkrets,newGrunnkrets)|> unique() |> filter(oldGrunnkrets!=newGrunnkrets)

#----------------------------------------------------------------------------------
# harmonize the geo codes
geo_kom_wide <- df_geo |> select(w19_0634_lnr,gpa_dato,moba_dato,contains("Kommune"))
geo_komdel_wide <- df_geo |> select(w19_0634_lnr,gpa_dato,moba_dato,contains("KomDel"))
geo_komdelgrunn_wide <- df_geo |> select(w19_0634_lnr,gpa_dato,moba_dato,contains("KomDelGrunn"))

source("N:/durable/projects/Qi/Rscript/new/municipality_district_harmonization.R")
updategeo_kom <- geocodeupdate(geo_kom_wide[,4:18],change_kom,'oldKommune','newKommune')
updategeo_komdel <- geocodeupdate(geo_komdel_wide[,4:18],change_komdel,'oldDelomrede','newDelomrede')
View(df_geo_updategeo_kom)
View(df_geo_updategeo_komdel)

# write.csv(updategeo_kom,"updategeo_kom.csv",row.names=FALSE)
# write.csv(updategeo_komdel,"updategeo_komdel.csv",row.names=FALSE)
updategeo_kom <- read.csv("N:/durable/projects/Qi/Rscript/new/updategeo_kom.csv", sep=",",na.strings = c("NA"," "))
updategeo_komdel <- read.csv("N:/durable/projects/Qi/Rscript/new/updategeo_kom.csv", sep=",",na.strings = c("NA"," "))

df_geo_updategeo_kom <- cbind(df_geo |> select(w19_0634_lnr,gpa_dato,moba_dato),updategeo_kom)
df_geo_updategeo_komdel <- cbind(df_geo |> select(w19_0634_lnr,gpa_dato,moba_dato),updategeo_komdel)

#----------------------------------------------------------------------------------
#exposure time - stayed the longest and the latest year they have stayed 
str(df_geo_updategeo_komdel)
long_df_geo_kom <- gather(df_geo_updategeo_kom |> select(c(w19_0634_lnr,gpa_dato,moba_dato,Kommune_2008:Kommune_2022)), geoyear, geocode_kom, Kommune_2008:Kommune_2022, factor_key=TRUE)
long_df_geo_kom <- transform(long_df_geo_kom, year = gsub(".*?(\\d{4})$", "\\1", long_df_geo_kom$geoyear))

long_df_geo_komdel <- gather(df_geo_updategeo_komdel |> select(c(w19_0634_lnr,gpa_dato,moba_dato,KomDel_2008:KomDel_2022)), geoyear, geocode_komdel, KomDel_2008:KomDel_2022, factor_key=TRUE)
long_df_geo_komdel <- transform(long_df_geo_komdel, year = gsub(".*?(\\d{4})$", "\\1", long_df_geo_komdel$geoyear))

long_df_geo_grunn <- gather(geo_komdelgrunn_wide |> select(c(w19_0634_lnr,gpa_dato,moba_dato,KomDelGrunn_2008:KomDelGrunn_2022)), geoyear, geocode_komdelgrunn, KomDelGrunn_2008:KomDelGrunn_2022, factor_key=TRUE)
long_df_geo_grunn <- transform(long_df_geo_grunn, year = gsub(".*?(\\d{4})$", "\\1", long_df_geo_kom$geoyear))


long_df_geo_kom_dur <- long_df_geo_kom |> group_by(w19_0634_lnr)|> arrange(w19_0634_lnr,year) |> filter(year >= moba_dato-1 & year <= gpa_dato) 
long_df_geo_komdel_dur <- long_df_geo_komdel |> group_by(w19_0634_lnr)|> arrange(w19_0634_lnr,year) |> filter(year >= moba_dato-1 & year <= gpa_dato) 
long_df_geo_grunn_dur <- long_df_geo_grunn |> group_by(w19_0634_lnr)|> arrange(w19_0634_lnr,year) |> filter(year >= moba_dato-1 & year <= gpa_dato) 


summary_data_kom <- long_df_geo_kom_dur |>
  group_by(w19_0634_lnr,geocode_kom) |> #count the number of each places one participant has lived 
  reframe(count = n(),year) |>
  arrange(w19_0634_lnr, desc(count), desc(year)) |>
  group_by(w19_0634_lnr,geocode_kom) |>
  slice(1) |> 
  group_by(w19_0634_lnr,count) |> #same count take the most recent
  slice(which.max(year)) |>
  group_by(w19_0634_lnr) |> #same participant take the most lived 
  slice(which.max(count)) |>
  ungroup() 


summary_data_komdel <- long_df_geo_komdel_dur |>
  group_by(w19_0634_lnr,geocode_komdel) |>
  reframe(count = n(),year) %>%
  arrange(w19_0634_lnr, desc(count), desc(year)) |>
  group_by(w19_0634_lnr,geocode_komdel) |>
  slice(1) |> 
  group_by(w19_0634_lnr,count) |>
  slice(which.max(year)) |>
  group_by(w19_0634_lnr) |>
  slice(which.max(count)) |>
  ungroup() 

summary_data_komdelgrunn <- long_df_geo_grunn_dur |>
  group_by(w19_0634_lnr,geocode_komdelgrunn) |>
  reframe(count = n(),year) %>%
  arrange(w19_0634_lnr, desc(count), desc(year)) |>
  group_by(w19_0634_lnr,geocode_komdelgrunn) |>
  slice(1) |> 
  group_by(w19_0634_lnr,count) |>
  slice(which.max(year)) |>
  group_by(w19_0634_lnr) |>
  slice(which.max(count)) |>
  ungroup() 

#how many times have they moved 
#number of places they have lived in 

summary_data2_kom <- long_df_geo_kom_dur |>
  arrange(w19_0634_lnr, year) |>  
  group_by(w19_0634_lnr) |>
  mutate(movings = geocode_kom != lag(geocode_kom)) |>  
  reframe(
    num_movings = sum(movings, na.rm = TRUE),  
    num_places_stayed = n_distinct(geocode_kom)
  )

summary_data2_komdel <- long_df_geo_komdel_dur |>
  arrange(w19_0634_lnr, year) |>  
  group_by(w19_0634_lnr) |>
  mutate(movings = geocode_komdel != lag(geocode_komdel)) |>  
  reframe(
    num_movings = sum(movings, na.rm = TRUE),  
    num_places_stayed = n_distinct(geocode_komdel)
  )

geo_id <- inner_join(summary_data_kom[,c('w19_0634_lnr','geocode_kom')],summary_data_komdel[,c('w19_0634_lnr','geocode_komdel')])
geo_id <- inner_join(geo_id,summary_data_komdelgrunn[,c('w19_0634_lnr','geocode_komdelgrunn')])

# ----------------------------------------------------------------------------------
# family id 
#changed or not
df_fam <- df|> select(w19_0634_lnr,contains('fam'))

#keep the year matches mobadato and gpa dato 
long_df_fam <- gather(df%>%select(c(w19_0634_lnr,gpa_dato,moba_dato,fam_2009:fam_2022)), famyear, famid, fam_2009:fam_2022, factor_key=TRUE)
long_df_fam <- transform(long_df_fam, year = gsub(".*?(\\d{4})$", "\\1", long_df_fam$famyear))

long_df_geo_fam_dur <- long_df_fam |> group_by(w19_0634_lnr)|> arrange(w19_0634_lnr,year) |> filter(year >= moba_dato-1 & year <= gpa_dato) 
summary_data_fam <- long_df_geo_fam_dur |>
  group_by(w19_0634_lnr,famid) |> 
  reframe(count = n(),year) |>
  arrange(w19_0634_lnr, desc(count), desc(year)) |>
  group_by(w19_0634_lnr,famid) |>
  slice(1) |> 
  group_by(w19_0634_lnr,count) |> 
  slice(which.max(year)) |>
  group_by(w19_0634_lnr) |> 
  slice(which.max(count)) |>
  ungroup() 

fam_id <- summary_data_fam[,1:2]
colnames(geo_id)
colnames(preprepared_df)

preprepared_df <- select(preprepared_df,w19_0634_lnr,NN68:GRUNNSKOLEPOENG,z_gpa:moba_dato)
prepared_df <- inner_join(preprepared_df,fam_id)
prepared_df <- inner_join(prepared_df,geo_id)
colnames(prepared_df)
str(daf[,171:180])

# write.csv(prepared_df,"prepared_df_with_id.csv",row.names=FALSE)
# saveRDS(prepared_df,"prepared_df_with_id.rds")

###########################################################################################
# ------------sum scores----------------------
###########################################################################################
daf <- readRDS("prepared_df_with_id.rds")


#check if any reversed coding-----------------------------------------------------
corPlot(daf%>%select(NN68:NN80),pair.wise.complete=T) #depression #no need to change
corPlot(daf%>%select(NN145:NN149),pair.wise.complete=T) #anxiety SCARED #no need to change
corPlot(daf%>%select(NN211:NN226),pair.wise.complete=T) #ccc2 #NN220-NN226
corPlot(daf%>%select(NN227:NN233,NN374),pair.wise.complete=T) #sprek20 #no need to change
corPlot(daf%>%select(NN150:NN189),pair.wise.complete=T) #Autism SCQ #recode 2 to 0 #then reverse
corPlot(daf%>%select(NN190:NN207),pair.wise.complete=T) #CEBQ #NN190 198 199 


#---personality scales----

corPlot(daf[,c('NN370','NN371','NN98','NN99','NN103','NN105')],pair.wise.complete=T)#extraversionA
corPlot(daf[,c('NN84', 'NN85','NN91','NN96','NN101','NN108')],pair.wise.complete=T)#agreeableness #84 108 should be recoded
corPlot(daf[,c('NN83','NN87','NN89','NN93','NN95','NN106')],pair.wise.complete=T)#conscientiousness #87 89 93
corPlot(daf[,c('NN368','NN86','NN94','NN97','NN102','NN372')],pair.wise.complete=T)#neuroticismA NN372
corPlot(daf[,c('NN369','NN92','NN100','NN104','NN109','NN110')],pair.wise.complete=T)#opennessA

corPlot(daf[,c('NN88','NN90','NN98','NN99','NN103','NN105')],pair.wise.complete=T)#extraversion
corPlot(daf[,c('NN84', 'NN85','NN91','NN96','NN101','NN108')],pair.wise.complete=T)#agreeableness #84 108 should be recoded
corPlot(daf[,c('NN83','NN87','NN89','NN93','NN95','NN106')],pair.wise.complete=T)#conscientiousness #87 89 93
corPlot(daf[,c('NN81','NN86','NN94','NN97','NN102','NN107')],pair.wise.complete=T)#neuroticism NN107
corPlot(daf[,c('NN82','NN92','NN100','NN104','NN109','NN110')],pair.wise.complete=T)#openness

#---RS-DBD---
corPlot(daf%>%select(NN111:NN144),pair.wise.complete=T) #CD #no need to change
corPlot(daf%>%select(NN111:NN118),pair.wise.complete=T) #CD #no need to change
corPlot(daf%>%select(NN119:NN136),pair.wise.complete=T) #ADHD #no need to change
corPlot(daf%>%select(NN137:NN144),pair.wise.complete=T) #OD #no need to change


# #separating AD and HD
# #PCA
# adhdpca <- prcomp(daf%>%select(NN119:NN136)%>%na.omit(), scale = TRUE)
# dim(adhdpca$x)
# my_pca.var <- adhdpca$sdev ^ 2
# propve <- my_pca.var / sum(my_pca.var)
# plot(propve, xlab = "principal component",
#      ylab = "Proportion of Variance Explained",
#      ylim = c(0, 1), type = "b",
#      main = "Scree Plot")
# 
# library('ggfortify')
# autoplot(adhdpca)
# 
# #CFA
# #uncorrelated two factor solution, var std method
# mcfa1 <- 'f1 =~ NN119+NN120+NN121+NN122+NN123+NN124+NN125+NN126+NN127+NN128+NN129+NN130+NN131+NN132+NN133+NN134+NN135+NN136' 
# cfa1 <- cfa(mcfa1, data=daf%>%select(NN119:NN136)%>%na.omit(),std.lv=TRUE) 
# summary(cfa1) 
# 
# mcfa2 <- 'f1 =~ NN119+NN120+NN121+NN122+NN123+NN124+NN125+NN126+NN127
#           f2 =~ NN128+NN129+NN130+NN131+NN132+NN133+NN134+NN135+NN136' 
# cfa2 <- cfa(mcfa2, data=daf%>%select(NN119:NN136)%>%na.omit(),std.lv=TRUE) 
# summary(cfa2,fit.measures=TRUE,standardized=TRUE)

######################################################################################
#Recoding the reversed variables--------------------------------------------------
#ccc2
daf <- daf %>%
  mutate_at(vars(NN220:NN226),
            recode, '1'=4,'2'=3,'3'=2,'4'=1)
#autism
daf <- daf %>%
  mutate_at(vars(NN152:NN157,NN159:NN167),
            recode, '1'=2,'2'=1)

#eating disorder
daf <- daf %>%
  mutate_at(vars(NN190,NN198,NN199),
            recode, '1'=5,'2'=4,'4'=2,'5'=1)

#personality
daf <- daf %>%
  mutate_at(vars(NN85,NN91,NN96,NN101,NN87,NN89,NN93,NN372,NN107),
            recode, '1'=5,'2'=4,'4'=2,'5'=1)

# Computing the sum scores for each scale-----------------------------------------------
daf$CCC2 <- rowSums(daf%>%select(NN211:NN226),na.rm = T)
daf$Sprek20 <- rowSums(daf%>%select(NN227:NN233,NN374),na.rm = T)
daf$Autism <- rowSums(daf%>%select(NN150:NN189),na.rm = T)
daf$Depression <- rowSums(daf%>%select(NN68:NN80),na.rm = T)
daf$Anxiety <- rowSums(daf%>%select(NN145:NN149),na.rm = T)
daf$EatingDisorder <- rowSums(daf%>%select(NN190:NN207),na.rm = T)


#CD
daf$CD <- rowSums(daf%>%select(NN111:NN118),na.rm = T)

#OD
daf$OD <- rowSums(daf%>%select(NN137:NN144),na.rm = T)

#ADHD
daf$ADHD <- rowSums(daf%>%select(NN119:NN136),na.rm = T)

#AD
daf$AD <- rowSums(daf%>%select(NN119:NN127),na.rm = T)

#HD
daf$HD <- rowSums(daf%>%select(NN128:NN136),na.rm = T)

#personality 
daf1 <- daf[!rowSums(is.na(daf[,c('NN370','NN371','NN88','NN90','NN98','NN99','NN103','NN105')]))==ncol(daf[,c('NN370','NN371','NN88','NN90','NN98','NN99','NN103','NN105')]),]
daf1 <- daf1[!rowSums(is.na(daf1[,c('NN84', 'NN85','NN91','NN96','NN101','NN108')]))==ncol(daf1[,c('NN84', 'NN85','NN91','NN96','NN101','NN108')]),]
daf1 <- daf1[!rowSums(is.na(daf1[,c('NN83','NN87','NN89','NN93','NN95','NN106')]))==ncol(daf1[,c('NN83','NN87','NN89','NN93','NN95','NN106')]),]
daf1 <- daf1[!rowSums(is.na(daf1[,c('NN81','NN86','NN94','NN97','NN102','NN107','NN368','NN372')]))==ncol(daf1[,c('NN81','NN86','NN94','NN97','NN102','NN107','NN368','NN372')]),]
daf1 <- daf1[!rowSums(is.na(daf1[,c('NN82','NN369','NN92','NN100','NN104','NN109','NN110')]))==ncol(daf1[,c('NN82','NN369','NN92','NN100','NN104','NN109','NN110')]),]


E.grm <- mirt(daf1[,c('NN370','NN371','NN88','NN90','NN98','NN99','NN103','NN105')], 1, rep("graded", 8),verbose = T)
A.grm <- mirt(daf1[,c('NN84', 'NN85','NN91','NN96','NN101','NN108')], 1, rep("graded", 6),verbose = FALSE)
C.grm <- mirt(daf1[,c('NN83','NN87','NN89','NN93','NN95','NN106')], 1, rep("graded", 6),verbose = FALSE)
N.grm <- mirt(daf1[,c('NN81','NN86','NN94','NN97','NN102','NN107','NN368','NN372')], 1, rep("graded", 8),verbose = FALSE)
O.grm <- mirt(daf1[,c('NN82','NN369','NN92','NN100','NN104','NN109','NN110')], 1, rep("graded", 7),verbose = FALSE)

summary(E.grm)
E.grm.fs <- fscores(E.grm, full.scores.SE = F)
A.grm.fs <- fscores(A.grm, full.scores.SE = F)
C.grm.fs <- fscores(C.grm, full.scores.SE = F)
N.grm.fs <- fscores(N.grm, full.scores.SE = F)
O.grm.fs <- fscores(O.grm, full.scores.SE = F)

head(E.grm.fs)
head(A.grm.fs)
head(C.grm.fs)
head(N.grm.fs)
head(O.grm.fs)


irtlist <- list(daf1,E.grm.fs,A.grm.fs,C.grm.fs,N.grm.fs,O.grm.fs)
daf <- irtlist  %>% reduce(cbind)

colnames(daf)[192:196] <- c('E_grm_fs','A_grm_fs','C_grm_fs','N_grm_fs','O_grm_fs')

# prepare the dataset
daf_ready <- select(daf,w19_0634_lnr,GRUNNSKOLEPOENG:lowsecid,famid:O_grm_fs)
# saveRDS(daf_ready,"daf_ready.rds")

###################################################################################
#centering
###################################################################################
setwd("N:/durable/projects/Qi/Rscript/new")
daf_ready <- readRDS("daf_ready.rds")
colnames(daf_ready)
str(daf_ready)

#centering and scaling
centerscaledaf <-daf_ready |>
  mutate(Communication_centered = scale(CCC2, center = TRUE, scale = TRUE),
         Communication_nor_centered = scale(Sprek20, center = TRUE, scale = TRUE),
         Autism_centered = scale(Autism, center = TRUE, scale = TRUE),
         Depression_centered = scale(Depression, center = TRUE, scale = TRUE),
         Anxiety_centered = scale(Anxiety, center = TRUE, scale = TRUE),
         EatingDisorder_centered = scale(EatingDisorder, center = TRUE, scale = TRUE),
         CD_centered = scale(CD, center = TRUE, scale = TRUE),
         OD_centered = scale(OD, center = TRUE, scale = TRUE),
         ADHD_centered = scale(ADHD, center = TRUE, scale = TRUE),
         AD_centered = scale(AD, center = TRUE, scale = TRUE),
         HD_centered = scale(HD, center = TRUE, scale = TRUE),
         Extraversion_centered = scale(E_grm_fs, center = TRUE, scale = TRUE),
         Agreeableness_centered = scale(A_grm_fs, center = TRUE, scale = TRUE),
         Conscientiousness_centered = scale(C_grm_fs, center = TRUE, scale = TRUE),
         Neuroticism_centered = scale(N_grm_fs, center = TRUE, scale = TRUE),
         Openness_centered = scale(O_grm_fs, center = TRUE, scale = TRUE)) |>
  ungroup()



# saveRDS(centerscaledaf,"daf_centered_scaled.rds")
###################################################################################
#Descriptive statistics
###################################################################################

df <- readRDS("daf_centered_scaled.rds")
colnames(df)[c(2,10:25)] <- c("GPA","Communication","Communication_nor",
                              "Autism","Depression","Anxiety"  ,        
                              "EatingDisorder","CD","OD"   ,            
                              "ADHD","AD","HD"        ,       
                              "Extraversion","Agreeableness","Conscientiousness",
                              "Neuroticism","Openness")
psych::describe(df)

#correlation between gpa and each trait
#plot
corre_matrix <- round(cor(df[c(2,10:25)],df[c(2,10:25)],use = "complete.obs"),2)
custom_col <- colorRampPalette(c("#6D9EC1", "white", "#E46726"))
ind <- corrplot(corre_matrix, type="lower", tl.col="black", col = custom_col(200),tl.srt=45,
                # addCoef.col = "black",number.cex = .6,
                method="color")$corrPos
text(ind$x, ind$y, round(ind$corr, 2),cex = 0.7)

#the range of the number of kids per area
#in our sample

ggplot(df, aes(x = GPA)) +
  geom_histogram(aes(y = ..density..),binwidth = 5, fill = "grey", color = "black") +
  geom_density(size = 1,color = "black")+
  labs(x = "Scores", y = "Frequency", title = "Distribution of Scores") +
  theme_classic() +
  theme(
    text = element_text(family = "Arial"),
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.position = "none"
  )

mean(df$GPA)
sd(df$GPA)

#skewness and kurtosis
skewness(df$GPA)
kurtosis(df$GPA)

#number of sample, primary school, middle school, district, municipality
length(unique(df$w19_0634_lnr))
length(unique(df$primaryid))
length(unique(df$lowsecid))
length(unique(df$geocode_kom))
length(unique(df$geocode_komdel))
length(unique(df$geocode_komdelgrunn))
length(unique(df$famid))

# summary of number of students per area
# in each municipality - number of students, family, school, district, neighborhood
municipalitypopiulation <- df |>
  group_by(geocode_kom) |>
  summarise(num_students = n(),
            num_schools = n_distinct(lowsecid),
            num_primary = n_distinct(primaryid),
            num_family = n_distinct(famid),
            num_del = n_distinct(geocode_komdel),
            num_grunn = n_distinct(geocode_komdelgrunn))


municipalitypopiulation_long <- melt(municipalitypopiulation[,1:2])
municipalitypopiulation_long$Level <- "Municiaplity"
colnames(municipalitypopiulation_long)[1] <- "id"


districtpopulation <- df |>
  group_by(geocode_komdel) |>
  summarise(num_students = n(),
            num_schools = n_distinct(lowsecid),
            num_primary = n_distinct(primaryid),
            num_family = n_distinct(famid),
            num_grunn = n_distinct(geocode_komdelgrunn))

districtpopulation_long <- melt(districtpopulation[,1:2])
districtpopulation_long$Level <- "District"
colnames(districtpopulation_long)[1] <- "id"

#percentage of students going to different schools
nrow(districtpopulation[districtpopulation$num_schools>1,])/nrow(districtpopulation)
nrow(districtpopulation[districtpopulation$num_primary>1,])/nrow(districtpopulation)



neighborhoodpopulation <- df |>
  group_by(geocode_komdelgrunn) |>
  summarise(num_students = n(),
            num_schools = n_distinct(lowsecid),
            num_primary = n_distinct(primaryid),
            num_family = n_distinct(famid))

neighborhoodpopulation_long <- melt(neighborhoodpopulation[,1:2])
neighborhoodpopulation_long$Level <- "Neighborhood"
colnames(neighborhoodpopulation_long)[1] <- "id"

lowersecpopulation <- df |>
  group_by(lowsecid) |>
  summarise(num_students = n(),
            num_primary = n_distinct(primaryid),
            num_family = n_distinct(famid),
            num_kom = n_distinct(geocode_kom),
            num_del = n_distinct(geocode_komdel),
            num_grunn = n_distinct(geocode_komdelgrunn))

lowersecpopulation_long <- melt(lowersecpopulation[,1:2])
lowersecpopulation_long$Level <- "Middle_School"
colnames(lowersecpopulation_long)[1] <- "id"


primpopulation <- df |>
  group_by(primaryid) |>
  summarise(num_students = n(),
            num_schools = n_distinct(lowsecid),
            num_family = n_distinct(famid),
            num_kom = n_distinct(geocode_kom),
            num_del = n_distinct(geocode_komdel),
            num_grunn = n_distinct(geocode_komdelgrunn))

primpopulation_long <- melt(primpopulation[,1:2])
primpopulation_long$Level <- "Primary_School"
colnames(primpopulation_long)[1] <- "id"

fampopulation <- df |>
  group_by(famid) |>
  summarise(num_students = n(),
            num_primary = n_distinct(primaryid),
            num_schools = n_distinct(lowsecid),
            num_family = n_distinct(famid),
            num_kom = n_distinct(geocode_kom),
            num_del = n_distinct(geocode_komdel),
            num_grunn = n_distinct(geocode_komdelgrunn))
fampopulation_long <- melt(fampopulation[,1:2])
fampopulation_long$Level <- "Family"
colnames(fampopulation_long)[1] <- "id"

n_student_long <- rbind(fampopulation_long,
                        primpopulation_long,lowersecpopulation_long,
                        neighborhoodpopulation_long,districtpopulation_long,municipalitypopiulation_long)

ggplot(n_student_long, aes(x = factor(Level), y = value)) + 
  geom_boxplot() +
  labs(title = "Number of Students per School and Geographical Area",
       y = "Number") + 
  theme_classic()

ggplot(n_student_long, aes(x = factor(Level), y = value)) + 
  geom_boxplot() +
  labs(title = "Number of Students per School and Geographical Area",
       y = "Number") + 
  theme_classic() + 
  ylim(0,100)

#mean and sd of the students per area and school
mean(fampopulation$num_students)
mean(primpopulation$num_students)
mean(lowersecpopulation$num_students)
mean(neighborhoodpopulation$num_students)
mean(districtpopulation$num_students)
mean(municipalitypopiulation$num_students)

sd(fampopulation$num_students)
sd(primpopulation$num_students)
sd(lowersecpopulation$num_students)
sd(neighborhoodpopulation$num_students)
sd(districtpopulation$num_students)
sd(municipalitypopiulation$num_students)

min(fampopulation$num_students)
min(primpopulation$num_students)
min(lowersecpopulation$num_students)
min(neighborhoodpopulation$num_students)
min(districtpopulation$num_students)
min(municipalitypopiulation$num_students)

max(fampopulation$num_students)
max(primpopulation$num_students)
max(lowersecpopulation$num_students)
max(neighborhoodpopulation$num_students)
max(districtpopulation$num_students)
max(municipalitypopiulation$num_students)

#number of municipalities/districts/neighborhoods/lowersecondary schools/primary schools/families
length(unique(df$geocode_kom))
length(unique(df$geocode_komdel))
length(unique(df$geocode_komdelgrunn))
length(unique(df$primaryid))
length(unique(df$lowsecid))
length(unique(df$famid))


#response distributions
colnames(df)
par(mfrow = c(4, 4))
hist(df$Communication_centered)
hist(df$Communication_nor_centered)
hist(df$Autism_centered)
hist(df$Depression_centered)
hist(df$Anxiety_centered)
hist(df$EatingDisorder_centered)
hist(df$CD_centered)
hist(df$OD_centered)
hist(df$ADHD_centered)
hist(df$AD_centered)
hist(df$HD_centered)
hist(df$Extraversion_centered)
hist(df$Agreeableness_centered)
hist(df$Conscientiousness_centered)
hist(df$Neuroticism_centered)
hist(df$Openness_centered)



###################################################################################
##-----------------------MLM------------------
###################################################################################
#no predictor null model - 6 levels
m0 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|primaryid)+(1|geocode_kom)+(1|geocode_komdel)+(1|geocode_komdelgrunn)+(1|famid),data=df,REML=F)
summary(m0)

#ICC(m0)
#performance::icc(m0)
compute.icc <- function(x){
  VarCorr(x) %>% as_tibble() %>% mutate(icc=vcov/sum(vcov)) %>% select(grp, icc)
  
}
compute.icc(m0)

#-----------------------------whether to include the intercepts-----------------------------------------
#5 levels
m01 <- lmer(z_gpa ~1 +(1|primaryid)+(1|geocode_kom)+(1|geocode_komdel)+(1|geocode_komdelgrunn)+(1|famid),data=df,REML=F)
m02 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|geocode_kom)+(1|geocode_komdel)+(1|geocode_komdelgrunn)+(1|famid),data=df,REML=F)
m03 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|primaryid)+(1|geocode_komdel)+(1|geocode_komdelgrunn)+(1|famid),data=df,REML=F)
m04 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|primaryid)+(1|geocode_kom)+(1|geocode_komdelgrunn)+(1|famid),data=df,REML=F)
m05 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|primaryid)+(1|geocode_kom)+(1|geocode_komdel)+(1|famid),data=df,REML=F)
m06 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|primaryid)+(1|geocode_kom)+(1|geocode_komdel)+(1|geocode_komdelgrunn),data=df,REML=F)

#4 levels
m07 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|primaryid)+(1|geocode_kom)+(1|geocode_komdel),data=df,REML=F)
m08 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|primaryid)+(1|geocode_kom)+(1|geocode_komdelgrunn),data=df,REML=F)
m09 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|primaryid)+(1|geocode_kom)+(1|famid),data=df,REML=F)

m10 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|primaryid)+(1|geocode_komdel)+(1|geocode_komdelgrunn),data=df,REML=F)
m11 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|primaryid)+(1|geocode_komdel)+(1|famid),data=df,REML=F)

m12 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|primaryid)+(1|geocode_komdelgrunn)+(1|famid),data=df,REML=F)

m13 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|geocode_kom)+(1|geocode_komdel)+(1|geocode_komdelgrunn),data=df,REML=F)
m14 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|geocode_kom)+(1|geocode_komdel)+(1|famid),data=df,REML=F)

m15 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|geocode_kom)+(1|geocode_komdelgrunn)+(1|famid),data=df,REML=F)

m16 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|geocode_komdel)+(1|geocode_komdelgrunn)+(1|famid),data=df,REML=F)

m17 <- lmer(z_gpa ~1 +(1|primaryid)+(1|geocode_kom)+(1|geocode_komdel)+(1|geocode_komdelgrunn),data=df,REML=F)
m18 <- lmer(z_gpa ~1 +(1|primaryid)+(1|geocode_kom)+(1|geocode_komdel)+(1|famid),data=df,REML=F)
m19 <- lmer(z_gpa ~1 +(1|primaryid)+(1|geocode_kom)+(1|geocode_komdelgrunn)+(1|famid),data=df,REML=F)
m20 <- lmer(z_gpa ~1 +(1|primaryid)+(1|geocode_komdel)+(1|geocode_komdelgrunn)+(1|famid),data=df,REML=F)

m21 <- lmer(z_gpa ~1 +(1|geocode_kom)+(1|geocode_komdel)+(1|geocode_komdelgrunn)+(1|famid),data=df,REML=F)

#3 levels

m22 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|primaryid)+(1|geocode_kom),data=df,REML=F)
m23 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|primaryid)+(1|geocode_komdel),data=df,REML=F)
m24 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|primaryid)+(1|geocode_komdelgrunn),data=df,REML=F)
m25 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|primaryid)+(1|famid),data=df,REML=F)

m26 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|geocode_kom)+(1|geocode_komdel),data=df,REML=F)
m27 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|geocode_kom)+(1|geocode_komdelgrunn),data=df,REML=F)
m28 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|geocode_kom)+(1|famid),data=df,REML=F)

m29 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|geocode_komdel)+(1|geocode_komdelgrunn),data=df,REML=F)
m30 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|geocode_komdel)+(1|famid),data=df,REML=F)

m31 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|geocode_komdelgrunn)+(1|famid),data=df,REML=F)

m32 <- lmer(z_gpa ~1 +(1|primaryid)+(1|geocode_kom)+(1|geocode_komdel),data=df,REML=F)
m33 <- lmer(z_gpa ~1 +(1|primaryid)+(1|geocode_kom)+(1|geocode_komdelgrunn),data=df,REML=F)
m34 <- lmer(z_gpa ~1 +(1|primaryid)+(1|geocode_kom)+(1|famid),data=df,REML=F)

m35 <- lmer(z_gpa ~1 +(1|primaryid)+(1|geocode_komdel)+(1|geocode_komdelgrunn),data=df,REML=F)
m36 <- lmer(z_gpa ~1 +(1|primaryid)+(1|geocode_komdel)+(1|famid),data=df,REML=F)

m37 <- lmer(z_gpa ~1 +(1|primaryid)+(1|geocode_komdelgrunn)+(1|famid),data=df,REML=F)

m38 <- lmer(z_gpa ~1 +(1|geocode_kom)+(1|geocode_komdel)+(1|geocode_komdelgrunn),data=df,REML=F)
m39 <- lmer(z_gpa ~1 +(1|geocode_kom)+(1|geocode_komdel)+(1|famid),data=df,REML=F)

m40 <- lmer(z_gpa ~1 +(1|geocode_kom)+(1|geocode_komdelgrunn)+(1|famid),data=df,REML=F)

m41 <- lmer(z_gpa ~1 +(1|geocode_komdel)+(1|geocode_komdelgrunn)+(1|famid),data=df,REML=F)

#2 levels
m42 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|primaryid),data=df,REML=F)
m43 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|geocode_kom),data=df,REML=F)
m44 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|geocode_komdel),data=df,REML=F)
m45 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|geocode_komdelgrunn),data=df,REML=F)
m46 <- lmer(z_gpa ~1 +(1|lowsecid)+(1|famid),data=df,REML=F)

m47 <- lmer(z_gpa ~1 +(1|primaryid)+(1|geocode_kom),data=df,REML=F)
m48 <- lmer(z_gpa ~1 +(1|primaryid)+(1|geocode_komdel),data=df,REML=F)
m49 <- lmer(z_gpa ~1 +(1|primaryid)+(1|geocode_komdelgrunn)+(1|famid),data=df,REML=F)
m50 <- lmer(z_gpa ~1 +(1|primaryid)+(1|famid),data=df,REML=F)

m51 <- lmer(z_gpa ~1 +(1|geocode_kom)+(1|geocode_komdel),data=df,REML=F)
m52 <- lmer(z_gpa ~1 +(1|geocode_kom)+(1|geocode_komdelgrunn),data=df,REML=F)
m53 <- lmer(z_gpa ~1 +(1|geocode_kom)+(1|famid),data=df,REML=F)

m54 <- lmer(z_gpa ~1 +(1|geocode_komdel)+(1|geocode_komdelgrunn),data=df,REML=F)
m55 <- lmer(z_gpa ~1 +(1|geocode_komdel)+(1|famid),data=df,REML=F)

m56 <- lmer(z_gpa ~1 +(1|geocode_komdelgrunn)+(1|famid),data=df,REML=F)

#1 levels
m57 <- lmer(z_gpa ~1 +(1|lowsecid),data=df,REML=F)
m58 <- lmer(z_gpa ~1 +(1|primaryid),data=df,REML=F)
m59 <- lmer(z_gpa ~1 +(1|geocode_kom),data=df,REML=F)
m60 <- lmer(z_gpa ~1 +(1|geocode_komdel),data=df,REML=F)
m61 <- lmer(z_gpa ~1 +(1|geocode_komdelgrunn),data=df,REML=F)
m62 <- lmer(z_gpa ~1 +(1|famid),data=df,REML=F)



model_name <- paste0("m",sprintf("%02d",1:62))
anova_result0 <- list()
anova_summary0 <- data.frame(AIC_m0 = rep(NA,62),AIC_m = rep(NA,62),Pvalue=rep(NA,62))

for (i in 1:62) {
  # cat("#######################################\n")
  # cat("########Results for m", i, ":#############\n")
  # cat("#######################################\n")
  anova_result0[[i]] <- anova(m0, get(model_name[i]))
  anova_summary0[i,1] <- anova_result0[[i]]["AIC"][2,1]
  anova_summary0[i,2] <- anova_result0[[i]]["AIC"][1,1]
  anova_summary0[i,3] <- round(anova_result0[[i]][["Pr(>Chisq)"]][2],5)
  anova_summary0$P_less_than_.05[i] <- round(anova_summary0[i,3],4) < 0.05
}


#-----------------------------test the random slopes------------------------------------
colnames(df)
scales <- c('Communication','Communication_nor','Autism', "Depression","Anxiety","EatingDisorder",
            "CD","OD","ADHD",
            "AD","HD","Extraversion",
            "Agreeableness","Conscientiousness","Neuroticism" ,     
            "Openness")
formula_list <- list()

#specify the models
for (j in scales ) {
  formula_list[[j]][["municipality, district, primary, middle"]] <- formula(paste0("z_gpa ~", j,"_centered+(1+",j,"_centered|geocode_kom)+(1+",j,"_centered|geocode_komdel)+(1+",j,"_centered|primaryid)+(1+",j,"_centered|lowsecid)+(1|famid)"))
  # 3
  formula_list[[j]][["district, primary, middle"]] <- formula(paste0("z_gpa ~", j,"_centered+(1|geocode_kom)+(1+",j,"_centered|geocode_komdel)+(1+",j,"_centered|primaryid)+(1+",j,"_centered|lowsecid)+(1|famid)"))
  formula_list[[j]][["municipality, primary, middle"]] <- formula(paste0("z_gpa ~", j,"_centered+(1+",j,"_centered|geocode_kom)+(1|geocode_komdel)+(1+",j,"_centered|primaryid)+(1+",j,"_centered|lowsecid)+(1|famid)"))
  formula_list[[j]][["municipality, district, middle"]] <- formula(paste0("z_gpa ~", j,"_centered", " +(1+",j,"_centered|geocode_kom)+(1+",j,"_centered|geocode_komdel)+(1|primaryid)+(1+",j,"_centered|lowsecid)+(1|famid)"))
  formula_list[[j]][["municipality, district, primary"]] <- formula(paste0("z_gpa ~", j,"_centered+(1+",j,"_centered|geocode_kom)+(1+",j,"_centered|geocode_komdel)+(1+",j,"_centered|primaryid)+(1|lowsecid)+(1|famid)"))
  # 2
  formula_list[[j]][["municipality, district"]] <- formula(paste0("z_gpa ~", j,"_centered+(1+",j,"_centered|geocode_kom)+(1+",j,"_centered|geocode_komdel)+(1|primaryid)+(1|lowsecid)+(1|famid)"))
  formula_list[[j]][["municipality, primary"]] <- formula(paste0("z_gpa ~", j,"_centered+(1+",j,"_centered|geocode_kom)+(1|geocode_komdel)+(1+",j,"_centered|primaryid)+(1|lowsecid)+(1|famid)"))
  formula_list[[j]][["municipality, middle"]] <- formula(paste0("z_gpa ~", j,"_centered+(1+",j,"_centered|geocode_kom)+(1|geocode_komdel)+(1|primaryid)+(1+",j,"_centered|lowsecid)+(1|famid)"))
  formula_list[[j]][["district, primary"]] <- formula(paste0("z_gpa ~", j,"_centered+(1|geocode_kom)+(1+",j,"_centered|geocode_komdel)+(1+",j,"_centered|primaryid)+(1|lowsecid)+(1|famid)"))
  formula_list[[j]][["district, middle"]] <- formula(paste0("z_gpa ~", j,"_centered+(1|geocode_kom)+(1+",j,"_centered|geocode_komdel)+(1|primaryid)+(1+",j,"_centered|lowsecid)+(1|famid)"))
  formula_list[[j]][["primary, middle"]] <- formula(paste0("z_gpa ~", j,"_centered+(1|geocode_kom)+(1|geocode_komdel)+(1+",j,"_centered|primaryid)+(1+",j,"_centered|lowsecid)+(1|famid)"))
  # 1
  formula_list[[j]][["middle"]] <- formula(paste0("z_gpa ~", j,"_centered+(1|geocode_kom)+(1|geocode_komdel)+(1|primaryid)+(1+",j,"_centered|lowsecid)+(1|famid)"))
  formula_list[[j]][["primary"]] <- formula(paste0("z_gpa ~", j,"_centered+(1|geocode_kom)+(1|geocode_komdel)+(1+",j,"_centered|primaryid)+(1|lowsecid)+(1|famid)"))
  formula_list[[j]][["district"]] <- formula(paste0("z_gpa ~", j,"_centered+(1|geocode_kom)+(1+",j,"_centered|geocode_komdel)+(1|primaryid)+(1|lowsecid)+(1|famid)"))
  formula_list[[j]][["municipality"]] <- formula(paste0("z_gpa ~", j,"_centered+(1+",j,"_centered|geocode_kom)+(1|geocode_komdel)+(1|primaryid)+(1|lowsecid)+(1|famid)"))
  # 0
  formula_list[[j]][["none"]] <- formula(paste0("z_gpa ~", j,"_centered+(1|geocode_kom)+(1|geocode_komdel)+(1|primaryid)+(1|lowsecid)+(1|famid)"))
}




#fitting the models
model_list <- formula_list
for (j in scales) {
  for (i in 1:16) {
    model_list[[j]][[i]] <- lmer(formula_list[[j]][[i]], data=df,REML=F)
  }
}

# saveRDS(model_list, "model_list.rds")

# model_list <- readRDS("model_list.rds")

#comparing the AICs and warning messages
summary_list <- list()
for (i in 1:16) {
  summary_list[[i]] <- data.frame(matrix(ncol = 2, nrow = 16))
  colnames(summary_list[[i]]) <- c('AIC','message')
  summary_list[[i]]$levels <- c("municipality, district, primary, middle",
                                "district, primary, middle",
                                "municipality, primary, middle",
                                "municipality, district, middle",
                                "municipality, district, primary",
                                "municipality, district",
                                "municipality, primary",
                                "municipality, middle",
                                "district, primary",
                                "district, middle",
                                "primary, middle",
                                "middle",
                                "primary",
                                "district",
                                "municipality",
                                "none")
  
}


#save the AIC and warning message of each model
for (j in 1:1:length(scales)) {
  for (i in 1:16) {
    summary_list[[j]][i,'AIC'] <- AIC(model_list[[j]][[i]])
    summary_list[[j]][i,'message'] <- ifelse(is.null(summary(model_list[[j]][[i]])$optinfo$conv$lme4$messages),
                                             'No warning',
                                             summary(model_list[[j]][[i]])$optinfo$conv$lme4$messages)
  }
}
names(summary_list) <- names(model_list)

#filter the model with the lowest AIC and no warning
results <- lapply(summary_list, function(df) {
  df |>
    arrange(AIC) |> 
    filter(message == "No warning") |>  
    slice_head(n = 1)
})

#final best fitted model for each trait
results <- bind_rows(results) |>
  mutate(Trait = names(summary_list)) |>
  select(Trait,levels,AIC,message)

#best fitted models with lowest AIC
bestmodel_list <- list()
for (i in 1:16) {
  bestmodel_list[[i]] <-  model_list[[i]][which(summary_list[[i]]$levels==results$levels[i])]
}


##################################################################################
# best fitted models 
##################################################################################

setwd("N:/durable/projects/Qi/Rscript/new")
df <- readRDS("daf_centered_scaled.rds")
#primary
fit_Communication <- lmer(z_gpa ~ Communication_centered + (1 | geocode_kom) + (1 | geocode_komdel) +  
                            (1 + Communication_centered | primaryid) + (1 | lowsecid) +  
                            (1 | famid),data = df,REML = F)
#middle
fit_Communication_nor <- lmer(z_gpa ~ Communication_nor_centered + (1 | geocode_kom) + (1 | geocode_komdel) +
                                (1 | primaryid) + (1 + Communication_nor_centered |lowsecid) + (1 | famid),data = df,REML = F)
#none
fit_Autism <- lmer( z_gpa ~ Autism_centered + (1 | geocode_kom) + (1 | geocode_komdel) +  
                      (1 | primaryid) + (1 | lowsecid) + (1 | famid),data = df,REML = F)

#district,middle
fit_Depression <- lmer(z_gpa ~ Depression_centered + (1 | geocode_kom) + 
                         (1 + Depression_centered |geocode_komdel) + (1 | primaryid) + (1 + Depression_centered |lowsecid) + (1 | famid),data = df,REML = F)
#none
fit_Anxiety <- lmer(z_gpa ~ Anxiety_centered + (1 | geocode_kom) + (1 | geocode_komdel) +  
                      (1 | primaryid) + (1 | lowsecid) + (1 | famid),data = df,REML = F)
#none
fit_EatingDisorder <- lmer(z_gpa ~ EatingDisorder_centered + (1 | geocode_kom) + (1 | geocode_komdel) +  
                             (1 | primaryid) + (1 | lowsecid) + (1 | famid),data = df,REML = F)
#none
fit_CD <- lmer(z_gpa ~ CD_centered + (1 | geocode_kom) + (1 | geocode_komdel) +  
                 (1 | primaryid) + (1 | lowsecid) + (1 | famid),data = df,REML = F)
#middle
fit_OD <- lmer(z_gpa ~ OD_centered + (1 | geocode_kom) + (1 | geocode_komdel) +  
                 (1 | primaryid) + (1 + OD_centered | lowsecid) + (1 | famid),data = df,REML = F)
#primary
fit_ADHD <- lmer( z_gpa ~ ADHD_centered + (1 | geocode_kom) + (1 | geocode_komdel) + (1 + ADHD_centered | primaryid) + (1 | lowsecid) + (1 | famid),data = df,REML = F)
#municipality
fit_AD <- lmer(z_gpa ~ AD_centered + (1 + AD_centered | geocode_kom) + (1 |geocode_komdel) + (1 | primaryid) + (1 | lowsecid) + (1 | famid),data = df,REML = F)

#middle
fit_HD <- lmer(z_gpa ~ HD_centered + (1 | geocode_kom) + (1 | geocode_komdel) +  
                 (1 | primaryid) + (1 + HD_centered | lowsecid) + (1 | famid),data = df,REML = F)
#middle
fit_Extraversion <- lmer(z_gpa ~ Extraversion_centered + (1 | geocode_kom) + (1 | geocode_komdel) +  
                           (1 | primaryid) + (1 + Extraversion_centered | lowsecid) +  
                           (1 | famid),data = df,REML = F)
#district
fit_Agreeableness <- lmer( z_gpa ~ Agreeableness_centered + (1 | geocode_kom) + (1 + Agreeableness_centered | geocode_komdel) + (1 | primaryid) + (1 | lowsecid) + (1 |  famid),data = df,REML = F)

#district
fit_Conscientiousness <- lmer( z_gpa ~ Conscientiousness_centered + (1 | geocode_kom) + (1 + Conscientiousness_centered | geocode_komdel) + (1 | primaryid) +  
                                 (1 | lowsecid) + (1 | famid),data = df,REML = F)
#middle
fit_Neuroticism <- lmer( z_gpa ~ Neuroticism_centered + (1 | geocode_kom) + (1 | geocode_komdel) +  
                           (1 | primaryid) + (1 + Neuroticism_centered | lowsecid) +  
                           (1 | famid) ,data = df,REML = F)
#none
fit_Openness <- lmer( z_gpa ~ Openness_centered + (1 | geocode_kom) + (1 | geocode_komdel) +  
                        (1 | primaryid) + (1 | lowsecid) + (1 | famid),data = df,REML = F)

summary(fit_Communication)
summary(fit_Communication_nor)
summary(fit_Autism)
summary(fit_Depression)
summary(fit_Anxiety)
summary(fit_EatingDisorder)
summary(fit_CD)
summary(fit_OD)
summary(fit_ADHD)
summary(fit_AD)
summary(fit_HD)
summary(fit_Extraversion)
summary(fit_Agreeableness)
summary(fit_Conscientiousness)
summary(fit_Neuroticism)
summary(fit_Openness)


#95% of adhd slopes
-0.252310+(1.96*0.08969)
-0.252310-(1.96*0.08969)

#95% of extraversion slopes
0.024380+(1.96*0.02226)
0.024380-(1.96*0.02226)

#communication
-0.193226+(1.96*0.07036)
-0.193226-(1.96*0.07036)

#depression
-0.128436+(1.96*0.02684)
-0.128436-(1.96*0.02684)

###############################################################################
#plot random effects 
###############################################################################
merged_scaled <-readRDS("daf_centered_scaled.rds")

# Neurodevelopment#
# Communication
randoms<-ranef(fit_Communication, condVar = TRUE)
m_int_slp<-as.data.frame(randoms$primaryid)

colnames(m_int_slp)<-c("Intercept","Slope")
head(m_int_slp)

df <- tibble::rownames_to_column(m_int_slp, "primaryid")
df2<-merge(merged_scaled, df, by = 'primaryid')

fe<-fixef(fit_Communication)
df2$pred<-df2$Intercept+df2$Slope * df2$Communication_centered+fe[1]+fe[2]*df2$Communication_centered

theme_set(theme_classic()) 
plot_communication <- ggplot(data = df2, aes(x = Communication_centered, y=pred,group=primaryid,color=Slope))+
  geom_line(alpha=.4)+
  xlab("Communication Skill Problems")+
  ylab("Predicted GPA")+
  xlim(-3,3)+
  ylim(-1,1)+
  theme(plot.title = element_text(color="black", size=16, face="bold",hjust = 0.5),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        strip.text = element_text(size = 16))+
  geom_vline(xintercept=0,linetype="dashed")+
  geom_abline(slope = fe[2], intercept = fe[1], color = "black",linewidth = .5,linetype="dashed") +
  scale_color_viridis(discrete=F)+
  theme(legend.position = "none")

#compensation
test_communication <- df2 %>% group_by(primaryid) %>% summarize(gpa=mean(z_gpa,na.rm=T),slope=mean(Slope,na.rm=T))
test_communication$slope <- abs(test_communication$slope)

# Q1 <- quantile(test_communication$gpa, 0.25)
# Q3 <- quantile(test_communication$gpa, 0.75)
# IQR <- Q3 - Q1
# 
# lower_bound <- Q1 - 1.5 * IQR
# upper_bound <- Q3 + 1.5 * IQR

df_clean_communication <- test_communication 

# %>%
#   filter(gpa >= lower_bound & gpa <= upper_bound)
# 

#Communication Norwegian version
randoms<-ranef(fit_Communication_nor, condVar = TRUE)
m_int_slp<-as.data.frame(randoms$lowsecid)

colnames(m_int_slp)<-c("Intercept","Slope")
head(m_int_slp)

df <- tibble::rownames_to_column(m_int_slp, "lowsecid")
df2<-merge(merged_scaled, df, by = 'lowsecid')

fe<-fixef(fit_Communication_nor)
colnames(df2)
df2$pred<-df2$Intercept+df2$Slope*df2$Communication_nor_centered + fe[1] + fe[2]*df2$Communication_nor_centered


theme_set(theme_classic())
plot_communication_nor <- ggplot(data = df2, aes(x = Communication_nor_centered, y=pred,group=lowsecid,color=Slope))+
  geom_line(alpha=.4)+
  xlab("Language Difficulties")+
  ylab("Predicted GPA")+
  xlim(-3,3)+
  ylim(-1,1)+
  theme(plot.title = element_text(color="black", size=16, face="bold",hjust = 0.5),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        strip.text = element_text(size = 16))+
  geom_vline(xintercept=0,linetype="dashed")+
  geom_abline(slope = fe[2], intercept = fe[1], color = "black",size = .5,linetype="dashed") +
  scale_color_viridis(discrete=F)+
  theme(legend.position = "none")


#compensation
test_communication_nor <- df2 %>% group_by(lowsecid) %>% summarize(gpa=mean(z_gpa,na.rm=T),slope=mean(Slope,na.rm=T))
test_communication_nor$slope <- abs(test_communication_nor$slope)

df_clean_communication_nor <- test_communication_nor 

# Hyperactivity Disorder
randoms<-ranef(fit_HD, condVar = TRUE)
m_int_slp<-as.data.frame(randoms$lowsecid)

colnames(m_int_slp)<-c("Intercept","Slope")
head(m_int_slp)

df <- tibble::rownames_to_column(m_int_slp, "lowsecid")
df2<-merge(merged_scaled, df, by = 'lowsecid')

fe<-fixef(fit_HD)
colnames(df2)
df2$pred<-df2$Intercept+df2$Slope * df2$HD_centered+fe[1]+fe[2]*df2$HD_centered

theme_set(theme_classic()) 
plot_HD <- ggplot(data = df2, aes(x = HD_centered, y=pred,group=lowsecid,color=Slope))+
  geom_line(alpha=.4)+
  xlab("Hyperactivity Disorder")+
  ylab("Predicted GPA")+
  xlim(-3,3)+
  ylim(-1,1)+
  theme(plot.title = element_text(color="black", size=16, face="bold",hjust = 0.5),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        strip.text = element_text(size = 16))+
  geom_vline(xintercept=0,linetype="dashed")+
  geom_abline(slope = fe[2], intercept = fe[1], color = "black",size = .5,linetype="dashed") +
  scale_color_viridis(discrete=F)+
  theme(legend.position = "none")



#compensation
test_HD <- df2 %>% group_by(lowsecid) %>% summarize(gpa=mean(z_gpa,na.rm=T),slope=mean(Slope,na.rm=T))
test_HD$slope <- abs(test_HD$slope)

df_clean_HD <- test_HD 


# Attention Deficit
randoms<-ranef(fit_AD, condVar = TRUE)
m_int_slp<-as.data.frame(randoms$geocode_kom)

colnames(m_int_slp)<-c("Intercept","Slope")
head(m_int_slp)

df <- tibble::rownames_to_column(m_int_slp, "geocode_kom")
df2<-merge(merged_scaled, df, by = 'geocode_kom')

fe<-fixef(fit_AD)
colnames(df2)
df2$pred<-df2$Intercept+df2$Slope * df2$AD_centered+fe[1]+fe[2]*df2$AD_centered

theme_set(theme_classic()) 
plot_AD <- ggplot(data = df2, aes(x = AD_centered, y=pred,group=geocode_kom,color=Slope))+
  geom_line(alpha=.4)+
  xlab("Attention Deficit")+
  ylab("Predicted GPA")+
  xlim(-3,3)+
  ylim(-1,1)+
  theme(plot.title = element_text(color="black", size=16, face="bold"),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        strip.text = element_text(size = 16))+
  geom_vline(xintercept=0,linetype="dashed")+
  geom_abline(slope = fe[2], intercept = fe[1], color = "black",size = .5,linetype="dashed") +
  scale_color_viridis(discrete=F)+
  theme(legend.position = "none")


#compensation
test_AD <- df2 %>% group_by(geocode_kom) %>% summarize(gpa=mean(z_gpa,na.rm=T),slope=mean(Slope,na.rm=T))
test_AD$slope <- abs(test_AD$slope)

df_clean_AD <- test_AD 

# ADHD
randoms<-ranef(fit_ADHD, condVar = TRUE)
m_int_slp<-as.data.frame(randoms$primaryid)

colnames(m_int_slp)<-c("Intercept","Slope")
head(m_int_slp)

df <- tibble::rownames_to_column(m_int_slp, "primaryid")
df2<-merge(merged_scaled, df, by = 'primaryid')

fe<-fixef(fit_ADHD)
colnames(df2)
df2$pred<-df2$Intercept+df2$Slope * df2$ADHD_centered+fe[1]+fe[2]*df2$ADHD_centered

theme_set(theme_classic()) 
plot_ADHD <- ggplot(data = df2, aes(x = ADHD_centered, y=pred,group=primaryid,color=Slope))+
  geom_line(alpha=.4)+
  xlab("ADHD")+
  ylab("Predicted GPA")+
  xlim(-3,3)+
  ylim(-1,1)+
  theme(plot.title = element_text(color="black", size=16, face="bold"),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        strip.text = element_text(size = 16))+
  geom_vline(xintercept=0,linetype="dashed")+
  geom_abline(slope = fe[2], intercept = fe[1], color = "black",size = .5,linetype="dashed") +
  scale_color_viridis(discrete=F)+
  theme(legend.position = "none")



#compensation
test_ADHD <- df2 %>% group_by(primaryid) %>% summarize(gpa=mean(z_gpa,na.rm=T),slope=mean(Slope,na.rm=T))
test_ADHD$slope <- abs(test_ADHD$slope)
df_clean_ADHD <- test_ADHD 


# Personality Traits #
# Neuroticism
randoms<-ranef(fit_Neuroticism, condVar = TRUE)
m_int_slp<-as.data.frame(randoms$lowsecid)

colnames(m_int_slp)<-c("Intercept","Slope")
head(m_int_slp)

df <- tibble::rownames_to_column(m_int_slp, "lowsecid")
df2<-merge(merged_scaled, df, by = 'lowsecid')

fe<-fixef(fit_Neuroticism)
colnames(df2)
df2$pred<-df2$Intercept+df2$Slope * df2$Neuroticism_centered+fe[1]+fe[2]*df2$Neuroticism_centered

theme_set(theme_classic()) 
plot_Neuroticism <- ggplot(data = df2, aes(x = Neuroticism_centered, y=pred,group=lowsecid,color=Slope))+
  geom_line(alpha=.4)+
  xlab("Neuroticism")+
  ylab("Predicted GPA")+
  xlim(-3,3)+
  ylim(-1,1)+
  theme(plot.title = element_text(color="black", size=16, face="bold",hjust = 0.5),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        strip.text = element_text(size = 16))+
  geom_vline(xintercept=0,linetype="dashed")+
  geom_abline(slope = fe[2], intercept = fe[1], color = "black",size = .5,linetype="dashed") +
  scale_color_viridis(discrete=F,)+
  theme(legend.position = "none")



#compensation
test_Neuroticism <- df2 %>% group_by(lowsecid) %>% summarize(gpa=mean(z_gpa,na.rm=T),slope=mean(Slope,na.rm=T))
test_Neuroticism$slope <- abs(test_Neuroticism$slope)

df_clean_Neuroticism <- test_Neuroticism



# Extraversion
randoms<-ranef(fit_Extraversion, condVar = TRUE)
m_int_slp<-as.data.frame(randoms$lowsecid)

colnames(m_int_slp)<-c("Intercept","Slope")
head(m_int_slp)

df <- tibble::rownames_to_column(m_int_slp, "lowsecid")
df2<-merge(merged_scaled, df, by = 'lowsecid')

fe<-fixef(fit_Extraversion)
colnames(df2)
df2$pred<-df2$Intercept+df2$Slope * df2$Extraversion_centered+fe[1]+fe[2]*df2$Extraversion_centered

theme_set(theme_classic()) 
plot_Extraversion <- ggplot(data = df2, aes(x = Extraversion_centered, y=pred,group=lowsecid,color=Slope))+
  geom_line(alpha=.4)+
  xlab("Extraversion")+
  ylab("Predicted GPA")+
  xlim(-3,3)+
  ylim(-1,1)+
  theme(plot.title = element_text(color="black", size=16, face="bold",hjust = 0.5),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        strip.text = element_text(size = 16))+
  geom_vline(xintercept=0,linetype="dashed")+
  geom_abline(slope = fe[2], intercept = fe[1], color = "black",size = .5,linetype="dashed") +
  scale_color_viridis(discrete=F,direction = -1)+
  theme(legend.position = "none")

#compensation
test_Extraversion <- df2 %>% group_by(lowsecid) %>% summarize(gpa=mean(z_gpa,na.rm=T),slope=mean(Slope,na.rm=T))
test_Extraversion$slope <- abs(test_Extraversion$slope)

df_clean_Extraversion <- test_Extraversion 



# Agreeableness
randoms<-ranef(fit_Agreeableness, condVar = TRUE)
m_int_slp<-as.data.frame(randoms$geocode_komdel)

colnames(m_int_slp)<-c("Intercept","Slope")
head(m_int_slp)

df <- tibble::rownames_to_column(m_int_slp, "geocode_komdel")
df2<-merge(merged_scaled, df, by = 'geocode_komdel')

fe<-fixef(fit_Agreeableness)
colnames(df2)
df2$pred<-df2$Intercept+df2$Slope * df2$Agreeableness_centered+fe[1]+fe[2]*df2$Agreeableness_centered

theme_set(theme_classic()) 
plot_Agreeableness <- ggplot(data = df2, aes(x = Agreeableness_centered, y=pred,group=geocode_komdel,color=Slope))+
  geom_line(alpha=.4)+
  xlab("Agreeableness")+
  ylab("Predicted GPA")+
  xlim(-3,3)+
  ylim(-1,1)+
  theme(plot.title = element_text(color="black", size=16, face="bold",hjust = 0.5),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        strip.text = element_text(size = 16))+
  geom_vline(xintercept=0,linetype="dashed")+
  geom_abline(slope = fe[2], intercept = fe[1], color = "black",size = .5,linetype="dashed") +
  scale_color_viridis(discrete=F,direction = -1)+
  theme(legend.position = "none")

#compensation
test_Agreeableness <- df2 %>% group_by(geocode_komdel) %>% summarize(gpa=mean(z_gpa,na.rm=T),slope=mean(Slope,na.rm=T))
test_Agreeableness$slope <- abs(test_Agreeableness$slope)

df_clean_Agreeableness <- test_Agreeableness 



# Conscientiousness
randoms<-ranef(fit_Conscientiousness, condVar = TRUE)
m_int_slp<-as.data.frame(randoms$geocode_komdel)

colnames(m_int_slp)<-c("Intercept","Slope")
head(m_int_slp)

df <- tibble::rownames_to_column(m_int_slp, "geocode_komdel")
df2<-merge(merged_scaled, df, by = 'geocode_komdel')

fe<-fixef(fit_Conscientiousness)
colnames(df2)
df2$pred<-df2$Intercept+df2$Slope * df2$Conscientiousness_centered+fe[1]+fe[2]*df2$Conscientiousness_centered

theme_set(theme_classic()) 
plot_Conscientiousness <- ggplot(data = df2, aes(x = Conscientiousness_centered, y=pred,group=geocode_komdel,color=Slope))+
  geom_line(alpha=.4)+
  xlab("Conscientiousness")+
  ylab("Predicted GPA")+
  xlim(-3,3)+
  ylim(-1,1)+
  theme(plot.title = element_text(color="black", size=16, face="bold",hjust = 0.5),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        strip.text = element_text(size = 16))+
  geom_vline(xintercept=0,linetype="dashed")+
  geom_abline(slope = fe[2], intercept = fe[1], color = "black",size = .5,linetype="dashed") +
  scale_color_viridis(discrete=F,direction = -1)+
  theme(legend.position = "none")

#compensation
test_Conscientiousness <- df2 %>% group_by(geocode_komdel) %>% summarize(gpa=mean(z_gpa,na.rm=T),slope=mean(Slope,na.rm=T))
test_Conscientiousness$slope <- abs(test_Conscientiousness$slope)

df_clean_Conscientiousness <- test_Conscientiousness




# Mental health traits #
# Depression
##districts
randoms<-ranef(fit_Depression, condVar = TRUE)
m_int_slp<-as.data.frame(randoms$geocode_komdel)

colnames(m_int_slp)<-c("Intercept","Slope")
head(m_int_slp)

df <- tibble::rownames_to_column(m_int_slp, "geocode_komdel")
df2<-merge(merged_scaled, df, by = 'geocode_komdel')

fe<-fixef(fit_Depression)
colnames(df2)
df2$pred<-df2$Intercept+df2$Slope * df2$Depression_centered+fe[1]+fe[2]*df2$Depression_centered

theme_set(theme_classic()) 
plot_Depression_district <- ggplot(data = df2, aes(x = Depression_centered, y=pred,group=geocode_komdel,color=Slope))+
  geom_line(alpha=.4)+
  xlim(-3,3)+
  ylim(-1,1)+
  xlab("Depression")+
  ylab("Predicted GPA")+
  theme(plot.title = element_text(color="black", size=16, face="bold"),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        strip.text = element_text(size = 16))+
  geom_vline(xintercept=0,linetype="dashed")+
  geom_abline(slope = fe[2], intercept = fe[1], color = "black",size = .5,linetype="dashed") +
  scale_color_viridis(discrete=F)+
  theme(legend.position = "none")


#compensation
test_Depression_district <- df2 %>% group_by(geocode_komdel) %>% summarize(gpa=mean(z_gpa,na.rm=T),slope=mean(Slope,na.rm=T))
test_Depression_district$slope <- abs(test_Depression_district$slope)

df_clean_Depression_district <- test_Depression_district


##middle school
randoms<-ranef(fit_Depression, condVar = TRUE)
m_int_slp<-as.data.frame(randoms$lowsecid)

colnames(m_int_slp)<-c("Intercept","Slope")
head(m_int_slp)

df <- tibble::rownames_to_column(m_int_slp, "lowsecid")
df2<-merge(merged_scaled, df, by = 'lowsecid')

fe<-fixef(fit_Depression)
colnames(df2)
df2$pred<-df2$Intercept+df2$Slope * df2$Depression_centered+fe[1]+fe[2]*df2$Depression_centered

theme_set(theme_classic()) 
plot_Depression_middle <- ggplot(data = df2, aes(x = Depression_centered, y=pred,group=lowsecid,color=Slope))+
  geom_line(alpha=.4)+
  xlab("Depression")+
  ylab("Predicted GPA")+
  xlim(-3,3)+
  ylim(-1,1)+
  theme(plot.title = element_text(color="black", size=16, face="bold",hjust = 0.5),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        strip.text = element_text(size = 16))+
  geom_vline(xintercept=0,linetype="dashed")+
  geom_abline(slope = fe[2], intercept = fe[1], color = "black",size = .5,linetype="dashed") +
  scale_color_viridis(discrete=F)+
  theme(legend.position = "none")

#compensation
test_Depression_lowsecid <- df2 %>% group_by(lowsecid) %>% summarize(gpa=mean(z_gpa,na.rm=T),slope=mean(Slope,na.rm=T))
test_Depression_lowsecid$slope <- abs(test_Depression_lowsecid$slope)

df_clean_Depression_lowsecid <- test_Depression_lowsecid 

#Oppositional Defiant Disorder
randoms<-ranef(fit_OD, condVar = TRUE)
m_int_slp<-as.data.frame(randoms$lowsecid)

colnames(m_int_slp)<-c("Intercept","Slope")
head(m_int_slp)

df <- tibble::rownames_to_column(m_int_slp, "lowsecid")
df2<-merge(merged_scaled, df, by = 'lowsecid')

fe<-fixef(fit_OD)
colnames(df2)
df2$pred<-df2$Intercept+df2$Slope * df2$OD_centered+fe[1]+fe[2]*df2$OD_centered

theme_set(theme_classic()) 
plot_OD_middle <- ggplot(data = df2, aes(x = OD_centered, y=pred,group=lowsecid,color=Slope))+
  geom_line(alpha=.4)+
  xlab("Oppositional Defiant Disorder")+
  ylab("Predicted GPA")+
  xlim(-3,3)+
  ylim(-1,1)+
  theme(plot.title = element_text(color="black", size=16, face="bold"),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        strip.text = element_text(size = 16))+
  geom_vline(xintercept=0,linetype="dashed")+
  geom_abline(slope = fe[2], intercept = fe[1], color = "black",size = .5,linetype="dashed") +
  scale_color_viridis(discrete=F)+
  theme(legend.position = "none")


#compensation
test_ODD <- df2 %>% group_by(lowsecid) %>% summarize(gpa=mean(z_gpa,na.rm=T),slope=mean(Slope,na.rm=T))
test_ODD$slope <- abs(test_ODD$slope)

df_clean_ODD <- test_ODD

#plot them together
plot_OD_middle
plot_Depression_middle
plot_Depression_district
plot_Conscientiousness
plot_Extraversion
plot_Agreeableness
plot_Neuroticism
plot_communication
plot_communication_nor
plot_AD
plot_HD
plot_ADHD

library(gridExtra)
library(grid)

#depression
grid.arrange(plot_Depression_middle,
             plot_Depression_district,
             nrow = 1, ncol = 2
             # ,
             # top = textGrob("Trait Effects on GPA Between Primary Schools",gp = gpar(fontsize = 16,fontface="bold"))
)



#primary school
grid.arrange(plot_ADHD,
             plot_communication,
             nrow = 1, ncol = 2
             # ,
             # top = textGrob("Trait Effects on GPA Between Primary Schools",gp = gpar(fontsize = 16,fontface="bold"))
)


#middle school
grid.arrange(plot_OD_middle,
             plot_Depression_middle,
             plot_communication_nor,
             plot_HD,
             plot_Neuroticism,
             plot_Extraversion,
             nrow = 2, ncol = 3
             # ,
             # top = textGrob("Trait Effects on GPA Between Middle Schools",gp = gpar(fontsize = 16,fontface="bold"))
)

#district
grid.arrange(plot_Depression_district,
             plot_Conscientiousness,
             plot_Agreeableness,
             nrow = 1, ncol = 3
             # ,
             # top = textGrob("Trait Effects on GPA Between Districts",gp = gpar(fontsize = 16,fontface="bold"))
)

#municipality
plot_AD

###############################################################################
#gpa*slope
###############################################################################
#primary
df_clean_communication$level <- "Primary_School"
df_clean_communication$cate <- "Neurodevelopmemtal_Traits"
df_clean_communication$trait <- "Communication_Primary"

df_clean_ADHD$level <- "Primary_School"
df_clean_ADHD$cate <- "Neurodevelopmemtal_Traits"
df_clean_ADHD$trait <- "ADHD_Primary"

#middle
df_clean_ODD$level <- "Middle_School"
df_clean_Depression_lowsecid$level <- "Middle_School"
df_clean_communication_nor$level <- "Middle_School"
df_clean_HD$level <- "Middle_School"
df_clean_Neuroticism$level <- "Middle_School"
df_clean_Extraversion$level <- "Middle_School"

df_clean_ODD$cate <- "MentalHealth_Traits"
df_clean_Depression_lowsecid$cate <- "MentalHealth_Traits"
df_clean_communication_nor$cate <- "Neurodevelopmemtal_Traits"
df_clean_HD$cate <- "Neurodevelopmemtal_Traits"
df_clean_Neuroticism$cate <- "Personality_Traits"
df_clean_Extraversion$cate <- "Personality_Traits"

df_clean_ODD$trait <- "ODD_Middle"
df_clean_Depression_lowsecid$trait <- "Depression_Middle"
df_clean_communication_nor$trait <- "Language_Middle"
df_clean_HD$trait <- "HD_Middle"
df_clean_Neuroticism$trait <- "Neuroticism_Middle"
df_clean_Extraversion$trait <- "Extraversion_Middle"

#district
df_clean_Depression_district$level <- "District"
df_clean_Conscientiousness$level <- "District"
df_clean_Agreeableness$level <- "District"

df_clean_Depression_district$cate <- "MentalHealth_Traits"
df_clean_Conscientiousness$cate <- "Personality_Traits"
df_clean_Agreeableness$cate <- "Personality_Traits"

df_clean_Depression_district$trait <- "Depression_District"
df_clean_Conscientiousness$trait <- "Conscientiousness_District"
df_clean_Agreeableness$trait <- "Agreeableness_District"

#municipality
df_clean_AD$level <- "Municipality"
df_clean_AD$cate <- "Neurodevelopmemtal_Traits"
df_clean_AD$trait <- "AD_Municipality"


binddf <- rbind(df_clean_communication[,-1],df_clean_ADHD[,-1],df_clean_ODD[,-1],df_clean_Depression_lowsecid[,-1],df_clean_communication_nor[,-1],df_clean_HD[,-1],df_clean_Neuroticism[,-1],
                df_clean_Extraversion[,-1],df_clean_Depression_district[,-1],df_clean_Conscientiousness[,-1],df_clean_Agreeableness[,-1],df_clean_AD[,-1])

binddf$level <- factor(binddf$level,levels = c("Primary_School","Middle_School","District","Municipality"))


ggplot(binddf, aes(x = gpa, y = slope, fill = trait,color=trait)) +                       
  geom_smooth(method = "lm") + 
  labs(x = "Average GPA per Environmental Context", y = "Absolute Slope Coefficient") + 
  theme(plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 16)) +
  facet_wrap(~ cate,scales = "fixed")+
  scale_color_manual(values = c("Communication_Primary" = "#f94144", "ODD_Middle" = "#5b8e7d",
                                "Language_Middle" = "#d81159", "Neuroticism_Middle" = "#1e88e5",
                                "Depression_Middle" = "#aad576", "Depression_District" = "#57cc99","Extraversion_Middle" = "#00509d",
                                "ADHD_Primary" = "#ffbd00","Agreeableness_District"="#62b6cb",
                                "AD_Municipality"="#8f2d56",  "Conscientiousness_District" = "#507dbc",
                                "HD_Middle"="#fe9ebc"))+
  scale_fill_manual(values = c("Communication_Primary" = "#f94144", "ODD_Middle" = "#5b8e7d",
                               "Language_Middle" = "#d81159", "Neuroticism_Middle" = "#1e88e5",
                               "Depression_Middle" = "#aad576", "Depression_District" = "#57cc99","Extraversion_Middle" = "#00509d",
                               "ADHD_Primary" = "#ffbd00","Agreeableness_District"="#62b6cb",
                               "AD_Municipality"="#8f2d56",  "Conscientiousness_District" = "#507dbc",
                               "HD_Middle"="#fe9ebc"))+
  labs(fill = "Trait",
       color = "Trait")+
  theme_bw()



#coefficient
# Fit linear models for each group
grouped_models <- binddf %>%
  group_by(trait) %>% 
  do(tidy(lm(slope ~ gpa, data = .)))

# Extract coefficients and standard errors for each group
coefficients_se <- grouped_models %>%
  select(trait, term, estimate, std.error)

###############################################################################
#density plot
###############################################################################

#distributions of slopes
slopeindices <- data.frame(Traits =  c("Depression_district","Depression_middle",
                                       "Agreeableness","Conscientiousness",
                                       "Extraversion","Neuroticism",
                                       "Communication","Language","ODD","ADHD","AD","HD"
                                       
),
mean_slope = c(fixef(fit_Depression)['Depression_centered'],
               fixef(fit_Depression)['Depression_centered'],
               fixef(fit_Agreeableness)['Agreeableness_centered'],
               fixef(fit_Conscientiousness)['Conscientiousness_centered'],
               fixef(fit_Extraversion)['Extraversion_centered'],
               fixef(fit_Neuroticism)['Neuroticism_centered'],
               fixef(fit_Communication)['Communication_centered'],
               fixef(fit_Communication_nor)['Communication_nor_centered'],
               fixef(fit_OD)['OD_centered'],
               fixef(fit_ADHD)['ADHD_centered'],
               fixef(fit_AD)['AD_centered'],
               fixef(fit_HD)['HD_centered']
               
),
sd_slope = c(attr(VarCorr(fit_Depression)[["geocode_komdel"]],"stddev")["Depression_centered"],
             attr(VarCorr(fit_Depression)[["lowsecid"]],"stddev")["Depression_centered"],
             attr(VarCorr(fit_Agreeableness)[["geocode_komdel"]],"stddev")["Agreeableness_centered"],
             attr(VarCorr(fit_Conscientiousness)[["geocode_komdel"]],"stddev")["Conscientiousness_centered"],
             attr(VarCorr(fit_Extraversion)[["lowsecid"]],"stddev")["Extraversion_centered"],
             attr(VarCorr(fit_Neuroticism)[["lowsecid"]],"stddev")["Neuroticism_centered"],
             attr(VarCorr(fit_Communication)[["primaryid"]],"stddev")["Communication_centered"],
             attr(VarCorr(fit_Communication_nor)[["lowsecid"]],"stddev")["Communication_nor_centered"],
             attr(VarCorr(fit_OD)[["lowsecid"]],"stddev")["OD_centered"],
             attr(VarCorr(fit_ADHD)[["primaryid"]],"stddev")["ADHD_centered"],
             attr(VarCorr(fit_AD)[["geocode_kom"]],"stddev")["AD_centered"],
             attr(VarCorr(fit_HD)[["lowsecid"]],"stddev")["HD_centered"]
             
             
))

set.seed(1)
slopes_densities <- as.data.frame(apply(slopeindices[, -1], 1, function(x) rnorm(n = 2500, mean = x[1], sd = x[2])))
colnames(slopes_densities) <- c( "Depression_district", "Depression_middle" ,  "Agreeableness_district",   
                                 "Conscientiousness_district"  , "Extraversion_middle"   , "Neuroticism_middle"   ,     
                                 "Communication_primary"   ,    "Language_middle" ,  "ODD_middle"   ,              
                                 "ADHD_primary"         ,       "AD_municipality"               ,   "HD_middle"
                                 
)
densities_long <- melt(slopes_densities)
densities_long$variable=factor(densities_long$variable)
densities_mean <- aggregate(value ~ variable, densities_long, mean)
colnames(densities_mean)[2] <- "Mean_eff"

no_slopes <- data.frame(c( "Openness","EatingDisorder","Autism","CD","Anxiety"),c(fixef(fit_Openness)['Openness_centered'],
                                                                                  fixef(fit_EatingDisorder)['EatingDisorder_centered'],
                                                                                  fixef(fit_Autism)['Autism_centered'],
                                                                                  fixef(fit_CD)['CD_centered'],
                                                                                  fixef(fit_Anxiety)['Anxiety_centered']))
colnames(no_slopes) <- colnames(densities_mean)
densities_long<-merge(densities_mean, densities_long,by='variable')
densities_long$variable=factor(densities_long$variable)
densities_long$Mean_eff=as.numeric(densities_long$Mean_eff)
densities_long$value=as.numeric(densities_long$value)

densities_long <- densities_long %>%
  mutate(group = case_when(
    str_detect(variable, "district") ~ "District",
    str_detect(variable, "middle") ~ "Middle_School",
    str_detect(variable, "municipality") ~ "Municipality",
    str_detect(variable, "primary") ~ "Primary_School",
    TRUE ~ NA_character_  # Handle any cases that don't match
  ))


str(densities_long)
densities_long <- densities_long %>%
  mutate(categroup = case_when(
    str_detect(variable, "Depression") ~ "Mental_Health_Traits",
    str_detect(variable, "OD") ~ "Mental_Health_Traits",
    str_detect(variable, "Conscientiousness") ~ "Personality_Traits",
    str_detect(variable, "Agreeableness") ~ "Personality_Traits",
    str_detect(variable, "Extraversion") ~ "Personality_Traits",
    str_detect(variable, "Neuroticism") ~ "Personality_Traits",
    str_detect(variable, "Communication") ~ "Neurodevelopment",
    str_detect(variable, "AD") ~ "Neurodevelopment",
    str_detect(variable, "HD") ~ "Neurodevelopment",
    str_detect(variable, "Language") ~ "Neurodevelopment",
    TRUE ~ NA_character_  # Handle any cases that don't match
  ))


densities_long$group <- factor(densities_long$group, levels = c("Primary_School", "Middle_School", "District","Municipality"))
densities_long <- densities_long %>%
  mutate(
    Neurodevelopmental_Traits = ifelse(categroup == "Neurodevelopment", as.character(variable), NA),
    MentalHealth_Traits = ifelse(categroup == "Mental_Health_Traits", as.character(variable), NA),
    Personality_Traits = ifelse(categroup == "Personality_Traits", as.character(variable), NA)
  )


ggplot(densities_long, aes(x = value, fill = variable,color=variable)) +                       
  geom_density(position = "identity", alpha = 0.5) +
  ggtitle("Between-group variation in the Effect of Traits on GPA") +
  xlab("Effect on GPA outcome") +
  xlim(-.5, .25) +
  theme(plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(-0.5, 1, .1)) +
  geom_vline(data = densities_long, aes(xintercept = Mean_eff, color = variable),
             linetype = "dashed", show.legend = FALSE) +
  # geom_vline(data = no_slopes, aes(xintercept = Mean_eff),
  #            linetype = "dashed") +
  geom_vline(xintercept = 0) +
  facet_wrap(~ group,scales = "fixed")+
  scale_color_manual(values = c("Communication_primary" = "#f94144", "ODD_middle" = "#5b8e7d", "Openness" = "grey",
                                "Language_middle" = "#d81159", "EatingDisorder" = "grey", "Neuroticism_middle" = "#1e88e5",
                                "Autism" = "grey", "Depression_middle" = "#aad576", "Depression_district" = "#57cc99","Extraversion_middle" = "#00509d",
                                "ADHD_primary" = "#ffbd00","CD" = "grey", "Agreeableness_district"="#62b6cb",
                                "AD_municipality"="#8f2d56", "Anxiety"="grey", "Conscientiousness_district" = "#507dbc",
                                "HD_middle"="#fe9ebc"))+
  scale_fill_manual(values = c("Communication_primary" = "#f94144", "ODD_middle" = "#5b8e7d", "Openness" = "grey",
                               "Language_middle" = "#d81159", "EatingDisorder" = "grey", "Neuroticism_middle" = "#1e88e5",
                               "Autism" = "grey", "Depression_middle" = "#aad576", "Depression_district" = "#57cc99","Extraversion_middle" = "#00509d",
                               "ADHD_primary" = "#ffbd00","CD" = "grey", "Agreeableness_district"="#62b6cb",
                               "AD_municipality"="#8f2d56", "Anxiety"="grey", "Conscientiousness_district" = "#507dbc",
                               "HD_middle"="#fe9ebc"))+
  labs(fill = "Trait",
       color = "Trait")+
  theme_bw()


ggplot(subset(densities_long,variable %in% "AD_municipality"), aes(x = value, fill = Neurodevelopmental_Traits,color=Neurodevelopmental_Traits)) +                       
  geom_density(position = "identity", alpha = 0.5)+
  scale_color_manual(values = c("Communication_primary" = "#f94144", "ODD_middle" = "#5b8e7d", "Openness_middle" = "grey",
                                "Language_middle" = "#d81159", "EatingDisorder_middle" = "grey", "Neuroticism_middle" = "#1e88e5",
                                "Autism_middle" = "grey", "Depression_middle" = "#aad576", "Depression_district" = "#57cc99","Extraversion_middle" = "#00509d",
                                "ADHD_primary" = "#ffbd00","CD_middle" = "grey", "Agreeableness_district"="#62b6cb",
                                "AD_municipality"="#8f2d56", "Anxiety_middle"="grey", "Conscientiousness_district" = "#507dbc",
                                "HD_middle"="#fe9ebc"))+
  scale_fill_manual(values = c("Communication_primary" = "#f94144", "ODD_middle" = "#5b8e7d", "Openness_middle" = "grey",
                               "Language_middle" = "#d81159", "EatingDisorder_middle" = "grey", "Neuroticism_middle" = "#1e88e5",
                               "Autism_middle" = "grey", "Depression_middle" = "#aad576", "Depression_district" = "#57cc99","Extraversion_middle" = "#00509d",
                               "ADHD_primary" = "#ffbd00","CD_middle" = "grey", "Agreeableness_district"="#62b6cb",
                               "AD_municipality"="#8f2d56", "Anxiety_middle"="grey", "Conscientiousness_district" = "#507dbc",
                               "HD_middle"="#fe9ebc"))

ggplot(subset(densities_long,variable %in% c("ADHD_primary","Communication_primary")), aes(x = value, fill = Neurodevelopmental_Traits,color=Neurodevelopmental_Traits)) +                       
  geom_density(position = "identity", alpha = 0.5)+
  scale_color_manual(values = c("Communication_primary" = "#f94144", "ODD_middle" = "#5b8e7d", "Openness_middle" = "grey",
                                "Language_middle" = "#d81159", "EatingDisorder_middle" = "grey", "Neuroticism_middle" = "#1e88e5",
                                "Autism_middle" = "grey", "Depression_middle" = "#aad576", "Depression_district" = "#57cc99","Extraversion_middle" = "#00509d",
                                "ADHD_primary" = "#ffbd00","CD_middle" = "grey", "Agreeableness_district"="#62b6cb",
                                "AD_municipality"="#8f2d56", "Anxiety_middle"="grey", "Conscientiousness_district" = "#507dbc",
                                "HD_middle"="#fe9ebc"))+
  scale_fill_manual(values = c("Communication_primary" = "#f94144", "ODD_middle" = "#5b8e7d", "Openness_middle" = "grey",
                               "Language_middle" = "#d81159", "EatingDisorder_middle" = "grey", "Neuroticism_middle" = "#1e88e5",
                               "Autism_middle" = "grey", "Depression_middle" = "#aad576", "Depression_district" = "#57cc99","Extraversion_middle" = "#00509d",
                               "ADHD_primary" = "#ffbd00","CD_middle" = "grey", "Agreeableness_district"="#62b6cb",
                               "AD_municipality"="#8f2d56", "Anxiety_middle"="grey", "Conscientiousness_district" = "#507dbc",
                               "HD_middle"="#fe9ebc"))

# subset(p_data_district, variable %in% c("Depression_district", "Conscientiousness_district","Agreeableness_district"))
# subset(p_data_middle, Trait %in% c("ODD_middle", "Language_middle","Neuroticism_middle","Depression_middle","Extraversion_middle","HD_middle"))) +

#depression
ggplot(subset(densities_long,variable %in% c("Depression_middle","Depression_district")), aes(x = value, fill = variable,color=variable)) +                       
  geom_density(position = "identity", alpha = 0.5) +
  ggtitle("Between-group variation in the Effect of Traits on GPA") +
  xlab("Effect on GPA outcome") +
  xlim(-.5, .25) +
  theme(plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(-0.5, 1, .1)) +
  geom_vline(data = subset(densities_long,variable %in% c("Depression_middle","Depression_district")), aes(xintercept = Mean_eff, color = variable),
             linetype = "dashed", show.legend = FALSE) +
  geom_vline(xintercept = 0) +
  scale_color_manual(values = c("Communication_primary" = "#f94144", "ODD_middle" = "#5b8e7d", "Openness" = "grey",
                                "Language_middle" = "#d81159", "EatingDisorder" = "grey", "Neuroticism_middle" = "#1e88e5",
                                "Autism" = "grey", "Depression_middle" = "#aad576", "Depression_district" = "#57cc99","Extraversion_middle" = "#00509d",
                                "ADHD_primary" = "#ffbd00","CD" = "grey", "Agreeableness_district"="#62b6cb",
                                "AD_municipality"="#8f2d56", "Anxiety"="grey", "Conscientiousness_district" = "#507dbc",
                                "HD_middle"="#fe9ebc"))+
  scale_fill_manual(values = c("Communication_primary" = "#f94144", "ODD_middle" = "#5b8e7d", "Openness" = "grey",
                               "Language_middle" = "#d81159", "EatingDisorder" = "grey", "Neuroticism_middle" = "#1e88e5",
                               "Autism" = "grey", "Depression_middle" = "#aad576", "Depression_district" = "#57cc99","Extraversion_middle" = "#00509d",
                               "ADHD_primary" = "#ffbd00","CD" = "grey", "Agreeableness_district"="#62b6cb",
                               "AD_municipality"="#8f2d56", "Anxiety"="grey", "Conscientiousness_district" = "#507dbc",
                               "HD_middle"="#fe9ebc"))+
  labs(fill = "Trait",
       color = "Trait")+
  theme_bw()



head(densities_long)
theme_set(theme_bw()) 
ggplot(densities_long, aes(x = value, fill = variable)) +                       
  geom_density(position = "identity", alpha = 0.2) +
  ggtitle("Between-group variation in the Effect of Traits on GPA") +
  xlab("Effect on GPA outcome") +
  xlim(-.5, .25) +
  theme(plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(-0.5, 1, .1)) +
  guides(fill = guide_legend(title = "Trait")) +
  geom_vline(data = densities_long, aes(xintercept = Mean_eff, color = variable),
             linetype = "dashed", show.legend = FALSE) +
  geom_vline(xintercept = 0) +
  facet_wrap(~ categroup)  # Facet by the new grouping variable


##################################################################
#proportion of schools and institution Effect(slope variance) explained by intercepts 
##################################################################
summary(fit_Communication)
summary(fit_Communication_nor)
summary(fit_Autism)
summary(fit_Depression)
summary(fit_Anxiety)
summary(fit_EatingDisorder)
summary(fit_CD)
summary(fit_OD)
summary(fit_ADHD)
summary(fit_AD)
summary(fit_HD)
summary(fit_Extraversion)
summary(fit_Agreeableness)
summary(fit_Conscientiousness)
summary(fit_Neuroticism)
summary(fit_Openness)

setwd("N:/durable/projects/Qi/Rscript/new")
df <- readRDS("daf_centered_scaled.rds")

#variance function at given x
variance_function <- function(x_value) {
  # Total variance for given x_value
  var_intercept + 2 * cov_intercept_slope * x_value + var_slope * x_value^2
}

#intercept slope correlation function at given x
correlation_function <- function(x_value) {
  (cov_intercept_slope + x_value * var_slope) / sqrt(variance_function(x_value) * var_slope)
}

colnames(df)
#autism
var_components <- as.data.frame(VarCorr(fit_Autism))
var_intercept <- var_components[var_components$grp == "primaryid","vcov"][1]
x_values <- seq(min(df$Autism_centered), max(df$Autism_centered), length.out = 1000)
p_data_autism <- data.frame(x = x_values, Autism_primary = var_intercept)
p_data_autism$Autism_district <- var_components[var_components$grp == "geocode_komdel","vcov"][1]
p_data_autism$Autism_middle <- var_components[var_components$grp == "lowsecid","vcov"][1]
p_data_autism$Autism_municipality <- var_components[var_components$grp == "geocode_kom","vcov"][1]
p_data_autism <- melt(p_data_autism,id="x")
p_data_autism$cor_ir <- 0
colnames(p_data_autism) <- c( "x", "Trait", "p_explained", "cor_ir")
p_data_autism$Category <- "Neurodevelopmental Traits"

#anxiety
var_components <- as.data.frame(VarCorr(fit_Anxiety))
var_intercept <- var_components[var_components$grp == "primaryid","vcov"][1]
x_values <- seq(min(df$Anxiety_centered), max(df$Anxiety_centered), length.out = 1000)
p_data_Anxiety <- data.frame(x = x_values, Anxiety_primary = var_intercept)
p_data_Anxiety$Anxiety_district <- var_components[var_components$grp == "geocode_komdel","vcov"][1]
p_data_Anxiety$Anxiety_middle <- var_components[var_components$grp == "lowsecid","vcov"][1]
p_data_Anxiety$Anxiety_municipality <- var_components[var_components$grp == "geocode_kom","vcov"][1]
p_data_Anxiety <- melt(p_data_Anxiety,id="x")
p_data_Anxiety$cor_ir <- 0
colnames(p_data_Anxiety)<- c( "x", "Trait", "p_explained", "cor_ir")
p_data_Anxiety$Category <- "MentalHealth Traits"

#CD
var_components <- as.data.frame(VarCorr(fit_CD))
var_intercept <- var_components[var_components$grp == "primaryid","vcov"][1]
x_values <- seq(min(df$CD_centered), max(df$CD_centered), length.out = 1000)
p_data_CD <- data.frame(x = x_values, CD_primary = var_intercept)
p_data_CD$CD_district <- var_components[var_components$grp == "geocode_komdel","vcov"][1]
p_data_CD$CD_middle <- var_components[var_components$grp == "lowsecid","vcov"][1]
p_data_CD$CD_municipality <- var_components[var_components$grp == "geocode_kom","vcov"][1]
p_data_CD <- melt(p_data_CD,id="x")
p_data_CD$cor_ir <- 0
colnames(p_data_CD)<- c( "x", "Trait", "p_explained", "cor_ir")
p_data_CD$Category <- "MentalHealth Traits"



#ED
var_components <- as.data.frame(VarCorr(fit_EatingDisorder))
var_intercept <- var_components[var_components$grp == "primaryid","vcov"][1]
x_values <- seq(min(df$EatingDisorder_centered), max(df$EatingDisorder_centered), length.out = 1000)
p_data_EatingDisorder <- data.frame(x = x_values, EatingDisorder_primary = var_intercept)
p_data_EatingDisorder$EatingDisorder_district <- var_components[var_components$grp == "geocode_komdel","vcov"][1]
p_data_EatingDisorder$EatingDisorder_middle <- var_components[var_components$grp == "lowsecid","vcov"][1]
p_data_EatingDisorder$EatingDisorder_municipality <- var_components[var_components$grp == "geocode_kom","vcov"][1]
p_data_EatingDisorder <- melt(p_data_EatingDisorder,id="x")
p_data_EatingDisorder$cor_ir <- 0
colnames(p_data_EatingDisorder)<- c( "x", "Trait", "p_explained", "cor_ir")
p_data_EatingDisorder$Category <- "MentalHealth Traits"


#openness
var_components <- as.data.frame(VarCorr(fit_Openness))
var_intercept <- var_components[var_components$grp == "primaryid","vcov"][1]
x_values <- seq(min(df$Openness_centered), max(df$Openness_centered), length.out = 1000)
p_data_Openness <- data.frame(x = x_values, Openness_primary = var_intercept)
p_data_Openness$Openness_district <- var_components[var_components$grp == "geocode_komdel","vcov"][1]
p_data_Openness$Openness_middle <- var_components[var_components$grp == "lowsecid","vcov"][1]
p_data_Openness$Openness_municipality <- var_components[var_components$grp == "geocode_kom","vcov"][1]
p_data_Openness <- melt(p_data_Openness,id="x")
p_data_Openness$cor_ir <- 0
colnames(p_data_Openness)<- c( "x", "Trait", "p_explained", "cor_ir")
p_data_Openness$Category <- "Personality Traits"


#adhd
var_components <- as.data.frame(VarCorr(fit_ADHD))
var_intercept <- var_components[var_components$grp == "primaryid","vcov"][1]
var_slope <-  var_components[var_components$grp == "primaryid","vcov"][2]
cov_intercept_slope <- var_components[var_components$grp == "primaryid","vcov"][3]
x_values <- seq(min(df$ADHD_centered), max(df$ADHD_centered), length.out = 1000)
portion_values <- sapply(x_values, variance_function)
p_data_adhd <- data.frame(x = x_values, ADHD_primary = portion_values)
p_data_adhd$ADHD_district <- var_components[var_components$grp == "geocode_komdel","vcov"][1]
p_data_adhd$ADHD_middle <- var_components[var_components$grp == "lowsecid","vcov"][1]
p_data_adhd$ADHD_municipality <- var_components[var_components$grp == "geocode_kom","vcov"][1]
p_data_adhd <- melt(p_data_adhd,id="x")
p_data_adhd$cor_ir <- sapply(x_values, correlation_function)
colnames(p_data_adhd)<- c( "x", "Trait", "p_explained", "cor_ir")
p_data_adhd$Category <- "Neurodevelopmental Traits"



#communication
var_components <- as.data.frame(VarCorr(fit_Communication))
var_intercept <- var_components[var_components$grp == "primaryid","vcov"][1]
var_slope <-  var_components[var_components$grp == "primaryid","vcov"][2]
cov_intercept_slope <- var_components[var_components$grp == "primaryid","vcov"][3]
x_values <- seq(min(df$Communication_centered), max(df$Communication_centered), length.out = 1000)
portion_values <- sapply(x_values, variance_function)
p_data_communication <- data.frame(x = x_values, Communication_primary = portion_values)
p_data_communication$Communication_district <- var_components[var_components$grp == "geocode_komdel","vcov"][1]
p_data_communication$Communication_middle <- var_components[var_components$grp == "lowsecid","vcov"][1]
p_data_communication$Communication_municipality <- var_components[var_components$grp == "geocode_kom","vcov"][1]
p_data_communication <- melt(p_data_communication,id="x")
p_data_communication$cor_ir <- sapply(x_values, correlation_function)
colnames(p_data_communication)<- c( "x", "Trait", "p_explained", "cor_ir")
p_data_communication$Category <- "Neurodevelopmental Traits"


#HD
var_components <- as.data.frame(VarCorr(fit_HD))
var_intercept <- var_components[var_components$grp == "lowsecid","vcov"][1]
var_slope <-  var_components[var_components$grp == "lowsecid","vcov"][2]
cov_intercept_slope <- var_components[var_components$grp == "lowsecid","vcov"][3]
x_values <- seq(min(df$HD_centered), max(df$HD_centered), length.out = 1000)
portion_values <- sapply(x_values, variance_function)
p_data_HD <- data.frame(x = x_values, HD_middle = portion_values)
p_data_HD$HD_district <- var_components[var_components$grp == "geocode_komdel","vcov"][1]
p_data_HD$HD_primary<- var_components[var_components$grp == "primaryid","vcov"][1]
p_data_HD$HD_municipality <- var_components[var_components$grp == "geocode_kom","vcov"][1]
p_data_HD <- melt(p_data_HD,id="x")
p_data_HD$cor_ir <- sapply(x_values, correlation_function)
colnames(p_data_HD)<- c( "x", "Trait", "p_explained", "cor_ir")
p_data_HD$Category <- "Neurodevelopmental Traits"


#Communication_nor
var_components <- as.data.frame(VarCorr(fit_Communication_nor))
var_intercept <- var_components[var_components$grp == "lowsecid","vcov"][1]
var_slope <-  var_components[var_components$grp == "lowsecid","vcov"][2]
cov_intercept_slope <- var_components[var_components$grp == "lowsecid","vcov"][3]
x_values <- seq(min(df$Communication_nor_centered), max(df$Communication_nor_centered), length.out = 1000)
portion_values <- sapply(x_values, variance_function)
p_data_Communication_nor <- data.frame(x = x_values, Language_middle = portion_values)
p_data_Communication_nor$Language_district <- var_components[var_components$grp == "geocode_komdel","vcov"][1]
p_data_Communication_nor$Language_primary<- var_components[var_components$grp == "primaryid","vcov"][1]
p_data_Communication_nor$Language_municipality <- var_components[var_components$grp == "geocode_kom","vcov"][1]
p_data_Communication_nor <- melt(p_data_Communication_nor,id="x")
p_data_Communication_nor$cor_ir <- sapply(x_values, correlation_function)
colnames(p_data_Communication_nor)<- c( "x", "Trait", "p_explained", "cor_ir")
p_data_Communication_nor$Category <- "Neurodevelopmental Traits"


#Extraversion
var_components <- as.data.frame(VarCorr(fit_Extraversion))
var_intercept <- var_components[var_components$grp == "lowsecid","vcov"][1]
var_slope <-  var_components[var_components$grp == "lowsecid","vcov"][2]
cov_intercept_slope <- var_components[var_components$grp == "lowsecid","vcov"][3]
x_values <- seq(min(df$Extraversion_centered), max(df$Extraversion_centered), length.out = 1000)
portion_values <- sapply(x_values, variance_function)
p_data_Extraversion <- data.frame(x = x_values, Extraversion_middle = portion_values)
p_data_Extraversion$Extraversion_district <- var_components[var_components$grp == "geocode_komdel","vcov"][1]
p_data_Extraversion$Extraversion_primary<- var_components[var_components$grp == "primaryid","vcov"][1]
p_data_Extraversion$Extraversion_municipality <- var_components[var_components$grp == "geocode_kom","vcov"][1]
p_data_Extraversion <- melt(p_data_Extraversion,id="x")
p_data_Extraversion$cor_ir <- sapply(x_values, correlation_function)
colnames(p_data_Extraversion)<- c( "x", "Trait", "p_explained", "cor_ir")
p_data_Extraversion$Category <- "Personality Traits"


#Neuroticism
var_components <- as.data.frame(VarCorr(fit_Neuroticism))
var_intercept <- var_components[var_components$grp == "lowsecid","vcov"][1]
var_slope <-  var_components[var_components$grp == "lowsecid","vcov"][2]
cov_intercept_slope <- var_components[var_components$grp == "lowsecid","vcov"][3]
x_values <- seq(min(df$Neuroticism_centered), max(df$Neuroticism_centered), length.out = 1000)
portion_values <- sapply(x_values, variance_function)
p_data_Neuroticism <- data.frame(x = x_values, Neuroticism_middle = portion_values)
p_data_Neuroticism$Neuroticism_district <- var_components[var_components$grp == "geocode_komdel","vcov"][1]
p_data_Neuroticism$Neuroticism_primary<- var_components[var_components$grp == "primaryid","vcov"][1]
p_data_Neuroticism$Neuroticism_municipality <- var_components[var_components$grp == "geocode_kom","vcov"][1]
p_data_Neuroticism <- melt(p_data_Neuroticism,id="x")
p_data_Neuroticism$cor_ir <- sapply(x_values, correlation_function)
colnames(p_data_Neuroticism)<- c( "x", "Trait", "p_explained", "cor_ir")
p_data_Neuroticism$Category <- "Personality Traits"


#Depression
var_components <- as.data.frame(VarCorr(fit_Depression))
var_intercept <- var_components[var_components$grp == "lowsecid","vcov"][1]
var_slope <-  var_components[var_components$grp == "lowsecid","vcov"][2]
cov_intercept_slope <- var_components[var_components$grp == "lowsecid","vcov"][3]
x_values <- seq(min(df$Depression_centered), max(df$Depression_centered), length.out = 1000)
portion_values <- sapply(x_values, variance_function)
p_data_Depression <- data.frame(x = x_values, Depression_middle = portion_values)
var_intercept <- var_components[var_components$grp == "geocode_komdel","vcov"][1]
var_slope <-  var_components[var_components$grp == "geocode_komdel","vcov"][2]
cov_intercept_slope <- var_components[var_components$grp == "geocode_komdel","vcov"][3]
p_data_Depression$Depression_district <- sapply(x_values, variance_function)
p_data_Depression$Depression_primary<- var_components[var_components$grp == "primaryid","vcov"][1]
p_data_Depression$Depression_municipality <- var_components[var_components$grp == "geocode_kom","vcov"][1]
p_data_Depression <- melt(p_data_Depression,id="x")
p_data_Depression$cor_ir <- sapply(x_values, correlation_function)
colnames(p_data_Depression)<- c( "x", "Trait", "p_explained", "cor_ir")
p_data_Depression$Category <- "MentalHealth Traits"


#OD
var_components <- as.data.frame(VarCorr(fit_OD))
var_intercept <- var_components[var_components$grp == "lowsecid","vcov"][1]
var_slope <-  var_components[var_components$grp == "lowsecid","vcov"][2]
cov_intercept_slope <- var_components[var_components$grp == "lowsecid","vcov"][3]
x_values <- seq(min(df$OD_centered), max(df$OD_centered), length.out = 1000)
portion_values <- sapply(x_values, variance_function)
p_data_ODD <- data.frame(x = x_values, ODD_middle = portion_values)
p_data_ODD$ODD_district <- var_components[var_components$grp == "geocode_komdel","vcov"][1]
p_data_ODD$ODD_primary<- var_components[var_components$grp == "primaryid","vcov"][1]
p_data_ODD$ODD_municipality <- var_components[var_components$grp == "geocode_kom","vcov"][1]
p_data_ODD <- melt(p_data_ODD,id="x")
p_data_ODD$cor_ir <- sapply(x_values, correlation_function)
colnames(p_data_ODD)<- c( "x", "Trait", "p_explained", "cor_ir")
p_data_ODD$Category <- "MentalHealth Traits"


#Conscientiousness
var_components <- as.data.frame(VarCorr(fit_Conscientiousness))
var_intercept <- var_components[var_components$grp == "geocode_komdel","vcov"][1]
var_slope <-  var_components[var_components$grp == "geocode_komdel","vcov"][2]
cov_intercept_slope <- var_components[var_components$grp == "geocode_komdel","vcov"][3]
x_values <- seq(min(df$Conscientiousness_centered), max(df$Conscientiousness_centered), length.out = 1000)
portion_values <- sapply(x_values, variance_function)
p_data_Conscientiousness <- data.frame(x = x_values, Conscientiousness_district = portion_values)
p_data_Conscientiousness$Conscientiousness_middle<- var_components[var_components$grp == "lowsecid","vcov"][1]
p_data_Conscientiousness$Conscientiousness_primary<- var_components[var_components$grp == "primaryid","vcov"][1]
p_data_Conscientiousness$Conscientiousness_municipality <- var_components[var_components$grp == "geocode_kom","vcov"][1]
p_data_Conscientiousness <- melt(p_data_Conscientiousness,id="x")
p_data_Conscientiousness$cor_ir <- sapply(x_values, correlation_function)
colnames(p_data_Conscientiousness)<- c( "x", "Trait", "p_explained", "cor_ir")
p_data_Conscientiousness$Category <- "Personality Traits"


#Agreeableness
var_components <- as.data.frame(VarCorr(fit_Agreeableness))
var_intercept <- var_components[var_components$grp == "geocode_komdel","vcov"][1]
var_slope <-  var_components[var_components$grp == "geocode_komdel","vcov"][2]
cov_intercept_slope <- var_components[var_components$grp == "geocode_komdel","vcov"][3]
x_values <- seq(min(df$Agreeableness_centered), max(df$Agreeableness_centered), length.out = 1000)
portion_values <- sapply(x_values, variance_function)
p_data_Agreeableness <- data.frame(x = x_values, Agreeableness_district = portion_values)
p_data_Agreeableness$Agreeableness_middle<- var_components[var_components$grp == "lowsecid","vcov"][1]
p_data_Agreeableness$Agreeableness_primary<- var_components[var_components$grp == "primaryid","vcov"][1]
p_data_Agreeableness$Agreeableness_municipality <- var_components[var_components$grp == "geocode_kom","vcov"][1]
p_data_Agreeableness <- melt(p_data_Agreeableness,id="x")
p_data_Agreeableness$cor_ir <- sapply(x_values, correlation_function)
colnames(p_data_Agreeableness)<- c( "x", "Trait", "p_explained", "cor_ir")
p_data_Agreeableness$Category <- "Personality Traits"


#ad
var_components <- as.data.frame(VarCorr(fit_AD))
var_intercept <- var_components[var_components$grp == "geocode_kom","vcov"][1]
var_slope <-  var_components[var_components$grp == "geocode_kom","vcov"][2]
cov_intercept_slope <- var_components[var_components$grp == "geocode_kom","vcov"][3]
x_values <- seq(min(df$AD_centered), max(df$AD_centered), length.out = 1000)
portion_values <- sapply(x_values, variance_function)
p_data_ad <- data.frame(x = x_values, AD_municipality = portion_values)
p_data_ad$AD_middle<- var_components[var_components$grp == "lowsecid","vcov"][1]
p_data_ad$AD_primary<- var_components[var_components$grp == "primaryid","vcov"][1]
p_data_ad$AD_district <- var_components[var_components$grp == "geocode_komdel","vcov"][1]
p_data_ad <- melt(p_data_ad,id="x")
p_data_ad$cor_ir <- sapply(x_values, correlation_function)
colnames(p_data_ad)<- c( "x", "Trait", "p_explained", "cor_ir")
p_data_ad$Category <- "Neurodevelopmental Traits"



p_data <- rbind(p_data_adhd,p_data_communication,
                p_data_HD,
                p_data_Communication_nor,
                p_data_Extraversion,
                p_data_Neuroticism,
                p_data_Depression,
                p_data_ODD,
                p_data_ad,
                p_data_autism,p_data_Anxiety,p_data_CD,p_data_EatingDisorder,p_data_Openness,p_data_Conscientiousness,p_data_Agreeableness)
str(p_data)
p_data_primary <- p_data[grep("primary", p_data$Trait), ]
p_data_middle <- p_data[grep("middle", p_data$Trait), ]
p_data_district <- p_data[grep("district", p_data$Trait), ]
p_data_municipality <- p_data[grep("municipality", p_data$Trait), ]


#####plotting the variance curves#####
p_data$Environment <- NA

p_data[grep("primary", p_data$Trait), "Environment"] <- "Primary_School"
p_data[grep("middle", p_data$Trait), "Environment"] <- "Middle_School"
p_data[grep("district", p_data$Trait),"Environment" ] <- "District"
p_data[grep("municipality", p_data$Trait), "Environment"] <- "Municipality"

plot_by_category <- ggplot(p_data, aes(x = x, y = p_explained,linetype = Environment)) +
  geom_line(aes(color = Trait), alpha=0.4,linewidth = 1.4, data = subset(p_data, !Trait %in% c("Communication_primary","ADHD_primary",
                                                                                               "ODD_middle","Language_middle" ,  "Neuroticism_middle" ,
                                                                                               "Depression_middle" , "Extraversion_middle","HD_middle",
                                                                                               "Depression_district" ,"Conscientiousness_district" ,
                                                                                               "Agreeableness_district",
                                                                                               "AD_municipality"
  ))) +
  geom_line(aes(color = Trait), linewidth = 1.4,data = subset(p_data, Trait %in% c("Communication_primary","ADHD_primary",
                                                                                   "ODD_middle","Language_middle" ,  "Neuroticism_middle" ,
                                                                                   "Depression_middle" , "Extraversion_middle","HD_middle",
                                                                                   "Depression_district" ,"Conscientiousness_district" ,
                                                                                   "Agreeableness_district",
                                                                                   "AD_municipality"
  ))) +
  labs(title = "Variation in the Effect of Environmental Context on GPA by Trait Levels",
       x = "Trait Level",
       y = "Variance in GPA Explained by Environment") +
  xlim(-3,3)+
  ylim(0,0.1)+
  theme_classic()+
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16),
        strip.text = element_text(size = 15))+
  scale_color_manual(values = c("Communication_primary" = "#f94144", "ODD_primary" = "grey", "Openness_primary" = "grey",
                                "Language_primary" = "grey", "EatingDisorder_primary" = "grey", "Neuroticism_primary" = "grey",
                                "Autism_primary" = "grey", "Depression_primary" = "grey", "Extraversion_primary" = "grey",
                                "ADHD_primary" = "#ffbd00","CD_primary" = "grey","Conscientiousness_primary" = "grey",
                                "AD_primary"="grey",  "Anxiety_primary"="grey",  "Agreeableness_primary"="grey",
                                "HD_primary"="grey",
                                "Communication_middle" = "grey", "ODD_middle" = "#5b8e7d", "Openness_middle" = "grey",
                                "Language_middle" = "#d81159", "EatingDisorder_middle" = "grey", "Neuroticism_middle" = "#1e88e5",
                                "Autism_middle" = "grey", "Depression_middle" = "#aad576", "Extraversion_middle" = "#00509d",
                                "ADHD_middle" = "grey","CD_middle" = "grey","Conscientiousness_middle" = "grey",
                                "AD_middle"="grey",  "Anxiety_middle"="grey",  "Agreeableness_middle"="grey",
                                "HD_middle"="#fe9ebc",
                                "Communication_district" = "grey", "ODD_district" = "grey", "Openness_district" = "grey",
                                "Language_district" = "grey", "EatingDisorder_district" = "grey", "Neuroticism_district" = "grey",
                                "Autism_district" = "grey", "Depression_district" = "#57cc99", "Extraversion_district" = "grey",
                                "ADHD_district" = "grey","CD_district" = "grey","Conscientiousness_district" = "#507dbc",
                                "AD_district"="grey",  "Anxiety_district"="grey",  "Agreeableness_district"="#62b6cb",
                                "HD_district"="grey",
                                "Communication_municipality" = "grey", "ODD_municipality" = "grey", "Openness_municipality" = "grey",
                                "Language_municipality" = "grey", "EatingDisorder_municipality" = "grey", "Neuroticism_municipality" = "grey",
                                "Autism_municipality" = "grey", "Depression_municipality" = "grey", "Extraversion_municipality" = "grey",
                                "ADHD_municipality" = "grey","CD_municipality" = "grey","Conscientiousness_municipality" = "grey",
                                "AD_municipality"="#8f2d56",  "Anxiety_municipality"="grey",  "Agreeableness_municipality"="grey",
                                "HD_municipality"="grey"
  ))+
  scale_linetype_manual(values = c("Primary_School" = "solid", "Middle_School" = "dashed", "District" = "dotdash", "Municipality"="dotted"))+
  facet_wrap(~ Category)



#intercept slope correlation curves
plot_cor_all <- ggplot(data=subset(p_data, Trait %in% c("Communication_primary","ADHD_primary",
                                                        "ODD_middle","Language_middle" ,  "Neuroticism_middle" ,
                                                        "Depression_middle" , "Extraversion_middle","HD_middle",
                                                        "Depression_district" ,"Conscientiousness_district" ,
                                                        "Agreeableness_district",
                                                        "AD_municipality")),aes(x = x, y = cor_ir,color = Trait,linetype=Environment)) +
  geom_line(size = 1.4) +
  labs(title = "Intercept-Slope Correlations by Trait Level",
       x = "Trait Level",
       y = "Intercept-Slope Correlation") +
  xlim(-3,3)+
  geom_hline(yintercept=0,linetype="dashed")+
  theme_classic()+
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16),
        strip.text = element_text(size = 15))+
  scale_color_manual(values = c("Communication_primary" = "#f94144", "ODD_primary" = "grey", "Openness_primary" = "grey",
                                "Language_primary" = "grey", "EatingDisorder_primary" = "grey", "Neuroticism_primary" = "grey",
                                "Autism_primary" = "grey", "Depression_primary" = "grey", "Extraversion_primary" = "grey",
                                "ADHD_primary" = "#ffbd00","CD_primary" = "grey","Conscientiousness_primary" = "grey",
                                "AD_primary"="grey",  "Anxiety_primary"="grey",  "Agreeableness_primary"="grey",
                                "HD_primary"="grey",
                                "Communication_middle" = "grey", "ODD_middle" = "#5b8e7d", "Openness_middle" = "grey",
                                "Language_middle" = "#d81159", "EatingDisorder_middle" = "grey", "Neuroticism_middle" = "#1e88e5",
                                "Autism_middle" = "grey", "Depression_middle" = "#aad576", "Extraversion_middle" = "#00509d",
                                "ADHD_middle" = "grey","CD_middle" = "grey","Conscientiousness_middle" = "grey",
                                "AD_middle"="grey",  "Anxiety_middle"="grey",  "Agreeableness_middle"="grey",
                                "HD_middle"="#fe9ebc",
                                "Communication_district" = "grey", "ODD_district" = "grey", "Openness_district" = "grey",
                                "Language_district" = "grey", "EatingDisorder_district" = "grey", "Neuroticism_district" = "grey",
                                "Autism_district" = "grey", "Depression_district" = "#57cc99", "Extraversion_district" = "grey",
                                "ADHD_district" = "grey","CD_district" = "grey","Conscientiousness_district" = "#507dbc",
                                "AD_district"="grey",  "Anxiety_district"="grey",  "Agreeableness_district"="#62b6cb",
                                "HD_district"="grey",
                                "Communication_municipality" = "grey", "ODD_municipality" = "grey", "Openness_municipality" = "grey",
                                "Language_municipality" = "grey", "EatingDisorder_municipality" = "grey", "Neuroticism_municipality" = "grey",
                                "Autism_municipality" = "grey", "Depression_municipality" = "grey", "Extraversion_municipality" = "grey",
                                "ADHD_municipality" = "grey","CD_municipality" = "grey","Conscientiousness_municipality" = "grey",
                                "AD_municipality"="#8f2d56",  "Anxiety_municipality"="grey",  "Agreeableness_municipality"="grey",
                                "HD_municipality"="grey"))+
  scale_linetype_manual(values = c("Primary_School" = "solid", "Middle_School" = "dashed", "District" = "dotdash", "Municipality"="dotted"))+
  facet_wrap(~ Category)


#generating legends separately
library(cowplot)
p_data <- p_data %>%
  mutate(
    Primary_School = ifelse(Environment == "Primary_School", as.character(sub("_.*", "", p_data$Trait)), NA),
    Middle_School = ifelse(Environment == "Middle_School", as.character(sub("_.*", "", p_data$Trait)), NA),
    District = ifelse(Environment == "District", as.character(sub("_.*", "", p_data$Trait)), NA),
    Municipality = ifelse(Environment == "Municipality", as.character(sub("_.*", "", p_data$Trait)), NA)
  )
p_data$Trait <- sub("_.*", "", p_data$Trait)


ggplot(p_data, aes(x = x, y = p_explained,color = Primary_School)) +
  geom_line(linewidth = 1.4) +
  labs(title = "Variation in the Effect of Primary Schools on GPA by Trait Levels",
       x = "Trait Level",
       y = "Environmental Effect") +
  xlim(-3,3)+
  ylim(0,0.2)+
  theme_classic()+
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16),
    strip.text = element_text(size = 15),
    legend.position = "bottom",        
    legend.direction = "horizontal"    
  ) +
  guides(color = guide_legend(nrow = 2))+
  scale_color_manual(values = c("Communication" = "#f94144", "ODD" = "grey", "Openness" = "grey",
                                "Language" = "grey", "EatingDisorder" = "grey", "Neuroticism" = "grey",
                                "Autism" = "grey", "Depression" = "grey", "Extraversion" = "grey",
                                "ADHD" = "#ffbd00","CD" = "grey","Conscientiousness" = "grey",
                                "AD"="grey",  "Anxiety"="grey",  "Agreeableness"="grey",
                                "HD"="grey"
                                
  ))+
  scale_linetype_manual(values = c("Primary_School" = "solid", "Middle_School" = "dashed", "District" = "dotdash", "Municipality"="dotted"))

ggplot(p_data, aes(x = x, y = p_explained,color = Middle_School)) +
  geom_line(linewidth = 1.4,linetype="dashed") +
  xlim(-3,3)+
  ylim(0,0.2)+
  theme_classic()+
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16),
    strip.text = element_text(size = 15),
    legend.position = "bottom",        
    legend.direction = "horizontal"    
  ) +
  guides(color = guide_legend(nrow = 2))+
  scale_color_manual(values = c( "Communication" = "grey", "ODD" = "#5b8e7d", "Openness" = "grey",
                                 "Language" = "#d81159", "EatingDisorder" = "grey", "Neuroticism" = "#1e88e5",
                                 "Autism" = "grey", "Depression" = "#aad576", "Extraversion" = "#00509d",
                                 "ADHD" = "grey","CD" = "grey","Conscientiousness" = "grey",
                                 "AD"="grey",  "Anxiety"="grey",  "Agreeableness"="grey",
                                 "HD"="#fe9ebc"
                                 
                                 
  ))+
  scale_linetype_manual(values = c("Primary_School" = "solid", "Middle_School" = "dashed", "District" = "dotdash", "Municipality"="dotted"))

ggplot(p_data, aes(x = x, y = p_explained,color = Primary_School)) +
  geom_line(linewidth = 1.4,linetype="dotdash") +
  labs(title = "Variation in the Effect of Primary Schools on GPA by Trait Levels",
       x = "Trait Level",
       y = "Environmental Effect") +
  xlim(-3,3)+
  ylim(0,0.2)+
  theme_classic()+
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16),
    strip.text = element_text(size = 15),
    legend.position = "bottom",        
    legend.direction = "horizontal"    
  ) +
  guides(color = guide_legend(nrow = 2))+
  scale_color_manual(values = c( 
    "Communication" = "grey", "ODD" = "grey", "Openness" = "grey",
    "Language" = "grey", "EatingDisorder" = "grey", "Neuroticism" = "grey",
    "Autism" = "grey", "Depression" = "#57cc99", "Extraversion" = "grey",
    "ADHD" = "grey","CD" = "grey","Conscientiousness" = "#507dbc",
    "AD"="grey",  "Anxiety"="grey",  "Agreeableness"="#62b6cb",
    "HD"="grey"
    
  ))+
  scale_linetype_manual(values = c("Primary_School" = "solid", "Middle_School" = "dashed", "District" = "dotdash", "Municipality"="dotted"))

ggplot(p_data, aes(x = x, y = p_explained,color = Municipality)) +
  geom_line(linewidth = 1.4,linetype="dotted") +
  labs(title = "Variation in the Effect of Primary Schools on GPA by Trait Levels",
       x = "Trait Level",
       y = "Environmental Effect") +
  xlim(-3,3)+
  ylim(0,0.2)+
  theme_classic()+
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16),
    strip.text = element_text(size = 15),
    legend.position = "bottom",        
    legend.direction = "horizontal"    
  ) +
  guides(color = guide_legend(nrow = 2))+
  scale_color_manual(values = c( 
    "Communication" = "grey", "ODD" = "grey", "Openness" = "grey",
    "Language" = "grey", "EatingDisorder" = "grey", "Neuroticism" = "grey",
    "Autism" = "grey", "Depression" = "grey", "Extraversion" = "grey",
    "ADHD" = "grey","CD" = "grey","Conscientiousness" = "grey",
    "AD"="#8f2d56",  "Anxiety"="grey",  "Agreeableness"="grey",
    "HD"="grey"
    
  ))+
  scale_linetype_manual(values = c("Primary_School" = "solid", "Middle_School" = "dashed", "District" = "dotdash", "Municipality"="dotted"))

####
ggplot(p_data, aes(x = x, y = p_explained,color = Primary_School)) +
  geom_line(linewidth = 1.4) +
  labs(title = "Variation in the Effect of Primary Schools on GPA by Trait Levels",
       x = "Trait Level",
       y = "Environmental Effect") +
  xlim(-3,3)+
  ylim(0,0.2)+
  theme_classic()+
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16),
    strip.text = element_text(size = 15)
  ) +
  guides(color = guide_legend(ncol = 2))+
  scale_color_manual(values =c("ODD" = "#5b8e7d","Depression" = "#aad576",   "Language" = "#d81159", "Neuroticism" = "#1e88e5",
                               "Extraversion" = "#00509d",
                               "HD"="#fe9ebc","Depression" = "#57cc99", "Conscientiousness" = "#507dbc",
                               "Agreeableness"="#62b6cb","AD"="#8f2d56","Communication" = "#f94144", 
                               "ADHD" = "#ffbd00" ))

p_data$Middle_School <- factor(p_data$Middle_School, levels = c("Depression","ODD", "HD", "Language", "Neuroticism","Extraversion"))
p_data$District <- factor(p_data$District, levels = c("Depression", "Agreeableness", "Conscientiousness"))

ggplot(data=subset(p_data),aes(x = x, y = cor_ir,color = Middle_School)) +
  geom_line(linewidth = 1.4,linetype="dashed") +
  xlim(-3,3)+
  ylim(0,0.2)+
  theme_classic()+
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16),
    strip.text = element_text(size = 15)
  ) +
  guides(color = guide_legend(ncol = 2)) +
  scale_color_manual(values =c("ODD" = "#5b8e7d","Depression" = "#aad576"))



#generating legends separately
library(cowplot)
p_data <- binddf %>%
  mutate(
    MentalHealth_Traits = ifelse(cate == "MentalHealth_Traits", as.character(binddf$trait), NA),
    Neurodevelopmemtal_Traits  = ifelse(cate == "Neurodevelopmemtal_Traits", as.character(binddf$trait), NA),
    Personality_Traits = ifelse(cate == "Personality_Traits", as.character(binddf$trait), NA)
  )


ggplot(p_data %>% drop_na(MentalHealth_Traits), aes(x = gpa, y = slope)) +
  geom_smooth(method = "lm",aes(color = MentalHealth_Traits, fill = MentalHealth_Traits)) + 
  scale_color_manual(values = c("Communication_Primary" = "#f94144", "ODD_Middle" = "#5b8e7d",
                                "Language_Middle" = "#d81159", "Neuroticism_Middle" = "#1e88e5",
                                "Depression_Middle" = "#aad576", "Depression_District" = "#57cc99","Extraversion_Middle" = "#00509d",
                                "ADHD_Primary" = "#ffbd00","Agreeableness_District"="#62b6cb",
                                "AD_Municipality"="#8f2d56",  "Conscientiousness_District" = "#507dbc",
                                "HD_Middle"="#fe9ebc"))+
  scale_fill_manual(values = c("Communication_Primary" = "#f94144", "ODD_Middle" = "#5b8e7d",
                               "Language_Middle" = "#d81159", "Neuroticism_Middle" = "#1e88e5",
                               "Depression_Middle" = "#aad576", "Depression_District" = "#57cc99","Extraversion_Middle" = "#00509d",
                               "ADHD_Primary" = "#ffbd00","Agreeableness_District"="#62b6cb",
                               "AD_Municipality"="#8f2d56",  "Conscientiousness_District" = "#507dbc",
                               "HD_Middle"="#fe9ebc"))


ggplot(p_data %>% drop_na(Neurodevelopmemtal_Traits), aes(x = gpa, y = slope)) +
  geom_smooth(method = "lm",aes(color = Neurodevelopmemtal_Traits, fill = Neurodevelopmemtal_Traits)) + 
  scale_color_manual(values = c("Communication_Primary" = "#f94144", "ODD_Middle" = "#5b8e7d",
                                "Language_Middle" = "#d81159", "Neuroticism_Middle" = "#1e88e5",
                                "Depression_Middle" = "#aad576", "Depression_District" = "#57cc99","Extraversion_Middle" = "#00509d",
                                "ADHD_Primary" = "#ffbd00","Agreeableness_District"="#62b6cb",
                                "AD_Municipality"="#8f2d56",  "Conscientiousness_District" = "#507dbc",
                                "HD_Middle"="#fe9ebc"))+
  scale_fill_manual(values = c("Communication_Primary" = "#f94144", "ODD_Middle" = "#5b8e7d",
                               "Language_Middle" = "#d81159", "Neuroticism_Middle" = "#1e88e5",
                               "Depression_Middle" = "#aad576", "Depression_District" = "#57cc99","Extraversion_Middle" = "#00509d",
                               "ADHD_Primary" = "#ffbd00","Agreeableness_District"="#62b6cb",
                               "AD_Municipality"="#8f2d56",  "Conscientiousness_District" = "#507dbc",
                               "HD_Middle"="#fe9ebc"))


ggplot(p_data %>% drop_na(Personality_Traits), aes(x = gpa, y = slope)) +
  geom_smooth(method = "lm",aes(color = Personality_Traits, fill = Personality_Traits)) + 
  scale_color_manual(values = c("Communication_Primary" = "#f94144", "ODD_Middle" = "#5b8e7d",
                                "Language_Middle" = "#d81159", "Neuroticism_Middle" = "#1e88e5",
                                "Depression_Middle" = "#aad576", "Depression_District" = "#57cc99","Extraversion_Middle" = "#00509d",
                                "ADHD_Primary" = "#ffbd00","Agreeableness_District"="#62b6cb",
                                "AD_Municipality"="#8f2d56",  "Conscientiousness_District" = "#507dbc",
                                "HD_Middle"="#fe9ebc"))+
  scale_fill_manual(values = c("Communication_Primary" = "#f94144", "ODD_Middle" = "#5b8e7d",
                               "Language_Middle" = "#d81159", "Neuroticism_Middle" = "#1e88e5",
                               "Depression_Middle" = "#aad576", "Depression_District" = "#57cc99","Extraversion_Middle" = "#00509d",
                               "ADHD_Primary" = "#ffbd00","Agreeableness_District"="#62b6cb",
                               "AD_Municipality"="#8f2d56",  "Conscientiousness_District" = "#507dbc",
                               "HD_Middle"="#fe9ebc"))




