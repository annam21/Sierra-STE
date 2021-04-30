

install.packages("devtools")
require(devtools)
library(devtools)
install.packages("dplyr")
library(dplyr)
load("dplyr")

devtools::install_github("annam21/spaceNtime", build_opts = c("--no-resave-data", "--no-manual"), force = T)

browseVignettes("spaceNtime")
library(spaceNtime)

#Ungulates:

d<-read.csv("/Users/sierramcmurry/Documents/Moose/Moose R Code/MOOSE AND DEER DATA FINAL.csv", header=T, sep=",")
colnames(d) <- c("CAM","ROT","DATE","TIME","TEMP","SP","N_AD","N_AD_M","N_AD_F","N_JV","GRP_SZ","CLR_YN")
ref <- read.csv("/Users/sierramcmurry/Documents/Moose/Moose R Code/Game Cam Data.csv", header = TRUE, sep = ",")
ref <- ref[, c(1,2,7,9,10)]
sapply(ref[, -c(4,5)], function(x) table(x))
colnames(ref) <- c("CAM", "ROT", "AREA", "LAT", "LONG")
ref$ID <- paste(ref$ROT, ref$CAM, sep = "-")
sapply(ref, function(x) sum(is.na(x)))
head(ref)
str(ref)
prey_dat <- merge(d, ref[, c(3:6)], by = "ID", all = TRUE)
prey_dat <- prey_dat[complete.cases(prey_dat), ]
sapply(prey_dat[, -c(4,5,9,10)], function(x) table(x))
sapply(prey_dat, function(x) sum(is.na(x)))
head(prey_dat)
str(prey_dat)
data<-data.frame(prey_dat$ID, prey_dat$SP, prey_dat$DT, prey_dat$GRP_SZ, prey_dat$AREA)
df #data frame with cam, dt, size for all three species
names(data)<- c("cam","species", "datetime", "count", "site")
data$area <- 78.9
head(data)
head(prey_dat)
d$SP <- as.character(d$SP)
toreplace <- list(" " = "", "M" = "m", "D" = "d", "E" = "e", "ELK" = "elk") #fix any spacing errors so we only have three characters
d$SP <- gsubfn(paste(names(toreplace), collapse = "|"), toreplace, d$SP)
d_prey <- subset(d, SP == "deer" | SP == "elk" | SP == "moose")
unique(d_prey$SP) #run and see we only have three
d_prey
a<- 78.9

moose<-subset(data, data$species =="moose")
moose #subset all data for moose
moosedf<- data.frame(moose$cam, moose$datetime, moose$count, moose$site, moose$area)
names(moosedf)<- c("cam", "datetime", "count", "site", "area")
moosedf #data frame for cam, dt, group size for moose
unique(moosedf$cam)
tz(moosedf$datetime)
moose_N<- subset(moosedf, moosedf$site == "N")
moose_S<- subset(moosedf, moosedf$site == "S")
moosedf.ID<- data.frame(moose$ID, moose$DT, moose$GRP_SZ, moose$AREA)
names(moosedf.ID)<- c("cam", "datetime", "count", "site")
moosedf.ID
moose_N_ID<- subset(moosedf.ID, moosedf.ID$site == "N")
moose_N_ID
moose_S_ID <- subset(moosedf.ID, moosedf.ID$site == "S")
moose_S_ID
moose<-subset(d_prey, d_prey$SP =="moose")
moose #subset all data for moose
moosedf<- data.frame(moose$CAM, moose$DT, moose$GRP_SZ)
names(moosedf)<- c("cam", "datetime", "count")
moosedf #data frame for cam, dt, group size for moose
moosedf$area <- 78.9
deerdf$area <- 78.9
deer<-subset(d_prey, d_prey$SP =="deer")
deer #subset all data for deer
deerdf<- data.frame(deer$CAM, deer$DT, deer$GRP_SZ)
names(deerdf)<- c("cam", "datetime", "count")
deerdf #data frame for cam, dt, group size for moose



deer_N<- subset(deerdf, deerdf$site == "N")
deer_N
deer_S<- subset(deerdf, deerdf$site == "S")
deer_S
str(deer_N)
deer<-subset(data, data$species =="deer")
deer #subset all data for deer
deerdf<- data.frame(deer$cam, deer$datetime, deer$count, deer$site, deer$area)
names(deerdf)<- c("cam", "datetime", "count", "site", "area")
deerdf #data frame for cam, dt, group size for moose
tail(deerdf)
unique(deerdf$cam)
deerdf.ID<- data.frame(deer$ID, deer$DT, deer$GRP_SZ, deer$AREA)
names(deerdf.ID)<- c("cam", "datetime", "count", "site")
deerdf.ID
deer_N_ID<- subset(deerdf, deerdf$site == "N")
deer_N_ID
deer_S_ID <- subset(deerdf, deerdf$site == "S")

df<-data.frame(d_prey$CAM, d_prey$DT, d_prey$GRP_SZ)
df #data frame with cam, dt, size for all three species
names(df)<- c("cam", "datetime", "count")
df
d_prey


#Deploy:

getwd()
setwd("/Users/sierramcmurry/Documents/Moose/Moose R Code/")
deploy<- read.csv("/Users/sierramcmurry/Documents/Moose/Moose R Code/deploy.csv", header = T, sep=",")
head(deploy)
str(deploy)
deploy$Start <- deploy$Start + 1
deploy$End <- deploy$End + 1
deploy$Start <- as.POSIXct(deploy$Start, format = "%m/%d/%y", tz= "GMT")
deploy$End<- as.POSIXct(deploy$End, format = "%m/%d/%y", tz = "GMT" ) 
?as.POSIXct
colnames(deploy)<- c("cam", "area", "start", "end")
head(deploy)


getwd()
setwd("/Users/sierramcmurry/Documents/Moose/Moose R Code/")
deployareas<- read.csv("/Users/sierramcmurry/Documents/Moose/Moose R Code/deploywithstudyarea.csv", header = T, sep=",")
deployfix<- read.csv("/Users/sierramcmurry/Documents/Moose/Moose R Code/deployfix.csv", header = T, sep=",")
View(deployfix)
View(deployareas)
deployfix$Start <- as.POSIXct(deployfix$Start, format = "%m/%d/%y", tz= "GMT")
deployfix$End<- as.POSIXct(deployfix$End, format = "%m/%d/%y", tz = "GMT" )
deployfix$Start <- deployfix$Start + 1
deployfix$End <- deployfix$End + 1
head(deployfix)
tz(deployfix$Start)
head(deployareas)
deployfixdf
deployfix$cam <- paste(deployfix$Rotation, deployfix$Camera, sep = "-")
deployareas
deploynorth$cam
deployareas$Start <- as.POSIXct(deployareas$Start, format = "%m/%d/%y", tz= "GMT")
deployareas$End<- as.POSIXct(deployareas$End, format = "%m/%d/%y", tz = "GMT" )
deployareas$Start <- deployareas$Start + 1
deployareas$End <- deployareas$End + 1
head(deployfix)
tz(deployfix$Start)
deployareas$cam <- paste(deployareas$Rotation, deployareas$Camera, sep = "-")

lubridate::tz(moosedf)<-"GMT"
lubridate::tz(deployfix$Start)<- "GMT"
lubridate::tz(moosedf.ID)<- "GMT"
lubridate::tz(deployfix$End) <-"GMT"
deployfixdf<- data.frame(deployfix$cam, deployfix$Area, deployfix$Start, deployfix$End)
deployareadf<- data.frame(deployareas$cam, deployareas$Area, deployareas$Start, deployareas$End)
colnames(deployareadf) <- c("cam", "area", "start", "end")
deploynorthdf <- data.frame(deploynorthdf$cam, deploynorthdf$Area, deploynorthdf$Start, deploynorthdf$End, deploynorthdf$study_area)
colnames(deploynorthdf) <- c("cam", "area", "start", "end", "study_area")
deploynorthdf
deploysouthdf <- subset(deployareas,deployareas$study_area == "S")
deploysouthdf <- data.frame(deploysouthdf$cam, deploysouthdf$Area, deploysouthdf$Start, deploysouthdf$End, deploysouthdf$study_area)
colnames(deploysouthdf) <- c("cam", "area", "start", "end", "study_area")

head(deployfixdf)
colnames(deployfixdf) <- c("cam", "area", "start", "end")

lubridate::tz(deployfixdf$end)<- "GMT"
lubridate::tz(deployfixdf$start)<- "GMT"
read.table(deployfixdf)
str(deployfixdf)

unique(deployfixdf$cam)
identical(deployfixdf$cam, moosedf$cam)
identical(deploysouthdf$cam, mldf_S)

identical(deployfixdf$cam[1], moosedf$cam[1])
(deployfixdf$cam[1] == moosedf$cam[1])

identical(deployfixdf$cam == "1-1", moosedf$cam == "1-1")
setdiff(levels(deployfixdf$cam), levels(moosedf$cam))
setdiff(levels(deployfixdf$cam), levels(moosedf$cam))
deployfixdf$cam <- as.factor(deployfixdf$cam)
moosedf$cam <- as.factor(moosedf$cam)
deerdf$cam <- as.factor(deerdf$cam)


str(deerdf)
str(deployfixdf)
str(moosedf)
str(deployfixdf)

#Predators:
predatordata<-read.csv("/Users/sierramcmurry/Documents/Moose/Moose R Code/threepredators.csv", header=T, sep=",")
head(predatordata)


predators<- data.frame(predatordata$DATE, predatordata$CAM_ID, 
            predatordata$ROT_N, predatordata$AREA, 
            predatordata$DET_TYPE, 
            predatordata$TIME..HH.MM.SS., predatordata$count)
colnames(predators)<- c("date","cam","rot", "study area", "species", "time", "count")
head(predators)
predators$datetime<-as.POSIXct(paste(predators$date, predators$time), format = "%m/%d/%y %H:%M:%S", tz = "GMT")
predators$ID <- paste(predators$rot, predators$cam, sep = "-")
moose<-subset(data, data$species =="moose")
bb <- subset(predators, predators$species == "BB")
ml <- subset(predators, predators$species == "ML")
w <- subset(predators, predators$species == "W")
bb
ml
w
deployareadf
moose
bb$area <- 78.9
ml$area <- 78.9
w$area <- 78.9

bbdf <- data.frame(bb$ID, bb$datetime, bb$area, bb$species, bb$count) #all sites
colnames(bbdf) <- c("cam", "datetime", "area", "species", "count")
View(bbdf_areas)

bbdf_areas <- data.frame(bb$ID, bb$datetime, bb$area, bb$species, bb$count, bb$`study area`)
colnames(bbdf_areas) <-c("cam","datetime", "area", "species", "count", "study" )
bbdf_N <- subset (bbdf_areas, bbdf_areas$study == "N")
bbdf_S <- subset(bbdf_areas, bbdf_areas$study == "S")
mldf <- data.frame(ml$ID, ml$datetime, ml$area, ml$species, ml$count)
colnames(mldf) <- c("cam", "datetime", "area", "species", "count")
mldf_areas <- data.frame(ml$ID, ml$datetime, ml$area, ml$species, ml$count, ml$`study area`)
colnames(mldf_areas) <-c("cam","datetime", "area", "species", "count", "study" )
mldf_N <- subset (mldf_areas, mldf_areas$study == "N")
mldf_S <- subset (mldf_areas, mldf_areas$study== "S")
wdf_S
wdf <- data.frame(w$ID, w$datetime, w$area, w$species, w$count)
colnames(wdf) <- c("cam", "datetime", "area", "species", "count")
wdf
wdf_N <- data.frame(w$ID, w$datetime, w$area, w$species, w$count, w$`study area`)
colnames(wdf_N) <-c("cam","datetime", "area", "species", "count", "study" )
wdf_NN <- subset (wdf_N, wdf_N$study == "N")
wdf_S <- subset (wdf_N, wdf_N$study== "S")
deployareas
deploynorthdf <- subset(deployareas,deployareas$study_area == "N")
deploynorthdf <- data.frame("cam", "Area", "study_area", "start", "end")
deploynorthdf
deployareadf
colnames(deploynorthdf) <- colnames(wdf) <- c("cam", "datetime", "area", "species", "count")
wdf_S

head(deployfixdf)
study_dates <- as.POSIXct(c("2015-08-27 00:00:01", "2018-02-20 00:00:01"), tz = "GMT")
occ <- build_occ(samp_freq = 3600, # seconds between the start of each sampling occasion
                 samp_length = 10, # duration of each sampling occasion (seconds)
                 study_start = study_dates[1],
                 study_end = study_dates[2])
View(deployareadf)
library(spaceNtime)
ste_eh_bb <- ste_build_eh(bbdf, deployareadf, occ)
total_bears <- ste_estN_fn(ste_eh_bb, study_area = 1.252e9)
total_bears
total_bears_se <- ste_estN_fn(ste_eh_bb, study_area = 100000000)
total_bears_se

ste_eh_bb_N <- ste_build_eh(bbdf_N, deploynorthdf, occ)
bears_north <- ste_estN_fn(ste_eh_bb_N, study_area = 775000000)
bears_north
bears_north_se <- ste_estN_fn(ste_eh_bb_N, study_area = 100000000)
bears_north_se

ste_eh_bb_S <- ste_build_eh(bbdf_S, deploysouthdf, occ)
bears_south <- ste_estN_fn(ste_eh_bb_S, study_area = 477000000)
bears_south
bears_south_se <- ste_estN_fn(ste_eh_bb_S, study_area = 100000000)
bears_south_se

ste_eh_ml <- ste_build_eh(mldf, deployfixdf, occ)
total_lions <- ste_estN_fn(ste_eh_ml, study_area = 1.252e9)
total_lions
total_lions_se <- ste_estN_fn(ste_eh_ml, study_area = 100000000)
total_lions_se
ste_eh_ml_N <- ste_build_eh(mldf_N, deploynorthdf, occ)
lions_north <-ste_estN_fn(ste_eh_ml_N, study_area = 775000000)
lions_north
lions_north_se <- ste_estN_fn(ste_eh_ml_N, study_area = 100000000)
lions_north_se
ste_eh_ml_S <- ste_build_eh(mldf_S, deploysouthdf, occ)
lions_south <- ste_estN_fn(ste_eh_ml_S, study_area = 477000000)
lions_south
lions_south_se <- ste_estN_fn(ste_eh_ml_S, study_area = 100000000)
lions_south_se

ste_eh_w <- ste_build_eh(wdf, deployfixdf, occ) #all wolves EH
totalwolves <- ste_estN_fn(ste_eh_w, study_area = 1.252e9)
totalwolves
ste_eh_w_N <- ste_build_eh(wdf_NN, deploynorthdf, occ) #north wolves EH with north deploy
wolves_north <- ste_estN_fn(ste_eh_w_N, study_area = 775000000)
wolves_north
wolf_total_se <- ste_estN_fn(ste_eh_w, study_area = 100000000)
wolf_total_se
wolf_north_se <- ste_estN_fn(ste_eh_w_N, study_area = 100000000)
wolf_north_se
ste_eh_w_S <- ste_build_eh(wdf_S, deploysouthdf, occ)

wdf
wdf_S
moose_N_ID
moose_S_ID
deer_N_ID
deer_S_ID
moosedf.ID
deerdf.ID
tz(deerdf.ID$datetime) <- "GMT"
tz(moosedf.ID$datetime) <- "GMT"
ste_eh_moose <- ste_build_eh(moosedf.ID, deployareadf, occ)
total_moose <- ste_estN_fn(ste_eh_moose, study_area = 1.252e9)
total_moose_se <- ste_estN_fn(ste_eh_moose, study_area = 100000000)
total_moose
total_moose_se

ste_eh_moose_N <- ste_build_eh(moose_N_ID, deploynorthdf, occ)
moose_north <- ste_estN_fn(ste_eh_moose_N, study_area = 755000000)
moose_north_se <- ste_estN_fn(ste_eh_moose_N, study_area = 100000000)
moose_north
moose_north_se

ste_eh_moose_S <- ste_build_eh(moose_S_ID, deploysouthdf, occ)
moose_south <- ste_estN_fn(ste_eh_moose_S, study_area = 477000000)
moose_south
moose_south_se <- ste_estN_fn(ste_eh_moose_S, study_area = 100000000)
moose_south_se

ste_eh_d <- ste_build_eh(deerdf.ID, deployareadf, occ)
total_deer <- ste_estN_fn(ste_eh_d, study_area = 1.252e9)
total_deer_se <- ste_estN_fn(ste_eh_d, study_area = 100000000)
total_deer
total_deer_se

ste_eh_d_N <- ste_build_eh(deer_N_ID, deploynorthdf, occ)
deer_north <- ste_estN_fn(ste_eh_d_N, study_area = 775000000)
deer_north_se <- ste_estN_fn(ste_eh_d_N,study_area = 100000000)
deer_north
deer_north_se

ste_eh_d_S <- ste_build_eh(deer_S_ID, deploysouthdf, occ)
deer_south <- ste_estN_fn(ste_eh_d_S, study_area = 477000000)
deer_south_se <- ste_estN_fn(ste_eh_d_S, study_area = 100000000)
deer_south
deer_south_se

moose_N_results
#total
ste_eh_moose %>% filter(!is.na(STE)) %>%  nrow() #6
ste_eh_d %>% filter(!is.na(STE)) %>%  nrow() #23
ste_eh_bb %>% filter(!is.na(STE)) %>%  nrow() #2
ste_eh_bb %>% filter(!is.na(STE))
ste_eh_ml %>% filter(!is.na(STE)) %>%  nrow() #2
ste_eh_ml %>% filter(!is.na(STE))
ste_eh_w %>% filter(!is.na(STE)) %>%  nrow() #1
ste_eh_w %>% filter(!is.na(STE))

#north
ste_eh_moose_N %>% filter(!is.na(STE)) %>%  nrow() #4
ste_eh_d_N %>% filter(!is.na(STE)) %>%  nrow() #11
ste_eh_w_N %>% filter(!is.na(STE)) %>%  nrow() #1
ste_eh_ml_N %>% filter(!is.na(STE)) %>%  nrow() #1
ste_eh_bb_N %>% filter(!is.na(STE)) %>%  nrow() #1

#south
ste_eh_moose_S %>% filter(!is.na(STE)) %>%  nrow() #2
ste_eh_d_S %>% filter(!is.na(STE)) %>%  nrow() #12
ste_eh_w_S %>% filter(!is.na(STE)) %>%  nrow() #0
ste_eh_bb_S %>% filter(!is.na(STE)) %>%  nrow() #1
ste_eh_ml_S %>% filter(!is.na(STE)) %>%  nrow() #1


nrow(occ) #27048
nrow(ste_eh_bb_N)

library(tidyverse) 

deployareadf %>%
  ungroup() %>%
  mutate(days = difftime(end, start, units = "days")) %>%
  summarize(d = sum(days))

max(deployareadf$end) 
min(deployareadf$start)

difftime(study_dates[2], study_dates[1],units = "days")
