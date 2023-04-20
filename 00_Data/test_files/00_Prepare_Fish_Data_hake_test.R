rm(list=ls()) # Clear variables in workspace

##### prepare fish data ####

# main = getwd()
MainFile = "/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub"
#MainFile = "/Users/cathleen.vestfals/Desktop/NRC_Hake_Kristin_Marshall/Data/Hake_Analysis"

setwd(paste0(MainFile,'/00_Data'))

fish = data.frame(read.table(paste0(MainFile,'/00_Data/hake.data_raw_test.csv'), header=TRUE, sep=",", stringsAsFactors = FALSE))

head(fish, n=20)
sapply(fish,as.numeric)


#add some columns to lag spawning biomass one year = sp.bio.1
fish$yrminusone = fish$year - 1
fish$sp.bio.1 = fish$sp.bio[match(fish$yrminusone,fish$year)]

### add deviations ####

devs = data.frame(read.table(paste0(MainFile,'/00_Data/HakeRecDevForCathleen_2020_medians.csv'), header=TRUE, sep=","))

devs$Label = as.character(devs$Label)
devs$year = substring(devs$Label,nchar(devs$Label)-3,nchar(devs$Label)) 
colnames(devs) <- c("Label","dev",'devsd','year')


fish.out = merge(fish,devs,'year',all = TRUE)
head(fish.out, n=45)

# problems converting to numeric - must do individually
fish.out$year = as.numeric(fish.out$year)
fish.out$sp.bio = as.numeric(fish.out$sp.bio)
fish.out$t.bio = as.numeric(fish.out$t.bio)
fish.out$Age2.bio = as.numeric(fish.out$Age2.bio)
fish.out$Age0.rec = as.numeric(fish.out$Age0.rec)


head(fish.out, n=45)

# select 1980 - 2016 data
fish.out2 <- fish.out[fish.out$year %in% 1980:2016,]
write.csv(fish.out2,'hake.data_2020_medians_test.csv')


