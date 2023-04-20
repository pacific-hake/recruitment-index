rm(list=ls()) # Clear variables in workspace
library(MuMIn) # load package with dredge 

##### prepare fish data ####

# main = getwd()
MainFile = "/Users/cathleen.vestfals/Desktop/NRC_Hake_Kristin_Marshall/Data/Hake_Analysis"

DredgeFile = paste0(MainFile,"/03_Dredge")
dir.create(DredgeFile)
setwd(DredgeFile)
####################################################################
##### BRING IN HAKE DATA - preconditioning to age-0 ################
####################################################################

fish00 = data.frame(read.table(paste0(MainFile,'/00_Data/hake.data_2020_medians.csv'), header=TRUE, sep=","))
head(fish00)
max(fish00$year)

years = 1980:2010  # only have ROMS data 1980-2010
fish = fish00[fish00$year %in% years,]

roms = data.frame(read.csv(paste0(MainFile,'/01_ROMS_Prelim/Data_ROMS.for.hake.analysis.pre_to_age0.csv'), header=TRUE))
roms = roms[roms$year %in% years,]

##########################################
#### RUN MODEL FITTING ###################
##########################################
# roms = the roms data after processing


#### Build equation ####
for(j in 2:ncol(roms)){# build equation
  x1 = colnames(roms)[j]
  if(j==ncol(roms)){x2 = x1}else{x2 = paste(x1,"+")}
  if(j==2){ROMS = x2}else{ROMS = paste(ROMS,x2)}
}
ROMS

#### Set up Formula #### variables with AIC < 2 (AIC - AICq)
form_dredge = as.formula(paste('dev ~ I(CSTlarv.s^2) + I(CSTlatelarv.s^2) + ', ROMS, sep=''))
# 
form_dredge

saveRDS(form_dredge, "Formula_for_Dredge.hake_pre_to_age0.rds")

#### Prep & combine data ####
# data_1 = # combine roms data and resids
data_1 = merge(fish,roms, 'year')
data_1 = na.omit(data_1)
write.table(data_1, 'Analyzed.data.hake.stage_pre_to_age0.csv',sep=',',col.names = TRUE, row.names = FALSE)



###############################################################################
##### run dredge twice ########################################################
##### once full, once minus the last five years ###############################
##### save output                               ###############################
###############################################################################

fit = lm(form_dredge, data=data_1, na.action = na.fail)

#### Run Dredge and save results ####

mtable = dredge(fit, rank = AICc, m.lim = c(NA,5),
                subset= # block correlated terms from the model > 0.70                  
                  
                  !(ASTyolk && ASTlarv) &&  # 0.83
                                    
                  !(CSTlarv.s && CSTlatelarv.s) &&  # 0.94
                  
              
                  dc(CSTlarv.s, I(CSTlarv.s^2))  &&
                  dc(CSTlatelarv.s, I(CSTlatelarv.s^2)),
                extra = list(R2 = function(x)
                  summary(x)$r.squared[[1]], 
                  F = function(y)
                  summary(y)$fstatistic[[1]]),
                trace=2 )
mtable2 = subset(mtable, delta < 2)
mtable2

print(mtable2, abbrev.names = FALSE)
saveRDS(mtable, file='Table_model.fits.hake_pre_to_age0.rds')
write.csv(mtable2,"R_Table_Delta2.hake_pre_to_age0.csv")



mtable = readRDS('Table_model.fits.hake_pre_to_age0.rds')
mtable2 = subset(mtable, delta < 2)
mtable2

setwd(MainFile)






