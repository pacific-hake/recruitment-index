####################################################
######## OUTPUT, DIAGNOTICS AND TESTS   ############
####################################################
# source(paste0(MainFile,'/00_BestModelInfo.R'))
# setwd(data.loc)

rm(list=ls())

# MainFile = getwd()
MainFile = "/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub"
results.loc = "/03_Dredge"
data.loc = paste0(MainFile,results.loc)
FigLoc = paste0(MainFile,'/Figures')

# variables in the best model, including the dependent variable
# first slot is the dependent variable

best_fit_vars = c("dev",'BIpre','EKEpre.ms.c','PREYpre.her','ASTyolk','STORMB') 

####### bring in model info and set BEST MODEL ################
# mostly just to look at

df0 = data.frame(read.csv(paste0(data.loc,"/Analyzed.data.hake_all_stages_ROMS_PPC.csv")))
# choose model output

mtable = readRDS(paste0(data.loc,"/Table_model.fits.hake_all_stages_ROMS_PPC.rds"))
mtable2 = subset(mtable, delta<2)
mtable2

# set the best model based on mtable2

# create formula 
form0 = paste0(best_fit_vars[1]," ~ ",best_fit_vars[2])
for(i in 3:length(best_fit_vars)){
  form0 = paste0(form0, " + ", best_fit_vars[i])
}
form0
form_best = as.formula(form0)

form_dredge = readRDS(paste0(data.loc,"/Formula_for_Dredge.hake_all_stages_ROMS_PPC.rds"))
form_dredge



parms = c('BIpre','EKEpre.ms.c','PREYpre.her','ASTyolk','STORMB')

# some data manipulation for later
parms = best_fit_vars[-1] # used later
x = grep("^", parms, fixed = TRUE)
if(length(x) > 0){parms2 = parms[-x]}else{parms2 = parms}

library(MuMIn)


m1 = lm(form_best, data=df0)
(s1 = summary(m1))
capture.output(summary(m1),file="R_BestFit_raw_hake_all_stages_ROMS_PPC.txt")
p1 = predict(m1)

###### plot diagnostics ######

graphics.off()
png(paste0(MainFile,'/Figures/ACF_Model_1_hake_all_stages_ROMS_PPC.png'), units='in',res=300, width=5, height=5)
acf(m1$residuals, main = "Model Autocorrelation plot")
dev.off()

graphics.off()
png(paste0(MainFile,"/Figures/Diagnoistic_plots_hake_all_stages_ROMS_PPC.png"), units='in', res=300, width=5, height=5)
par(mfrow=c(2,2))
plot(m1)
dev.off()

#############  get standardized coefficients #########################

std = apply(df0[,parms2],2,scale)
dfstd = df0
dfstd[,parms2] = std

m2 = lm(form_best, data=dfstd)
(s2 = summary(m2))
capture.output(summary(m2),file="R_BestFit_standardized_hake_all_stages_ROMS_PPC.txt")

#############   variance inflation factors for best-fit model ###################
library(car)
(VIF = vif(m1))

capture.output(VIF, file="R_VIF_hake_all_stages_ROMS_PPC.txt")

########### Various refits based on leaving out years ##########################

##### LAST 5 Years, one year at a time ####
retro5 = c()
retro5$year = 2006:2010
retro5 = data.frame(retro5)
retro5$pred = NA
head(retro5)

for(i in 1:nrow(retro5)){
  df1 = df0[df0$year<retro5$year[i],] 
  m1 = lm(form_best, data=df1)
  #summary(m1)
  NewData = df0[df0$year==retro5$year[i], parms2]
  retro5[i,'pred'] = predict(m1, newdata = NewData)
} # end i
retro5
write.table(retro5,'/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub/03_Dredge/D_Retro5.results_hake_all_stages_ROMS_PPC.csv',col.names=TRUE, row.names=FALSE,sep=',')

####### jacknife individual years #########
retro1 = c()
retro1$year = df0$year
retro1 = data.frame(retro1)
retro1$pred = NA

for(i in 1:nrow(retro1)){
  df1 = df0[-i,]
  m1 = lm(form_best, data=df1)
  #summary(m1)
  NewData = df0[df0$year==retro1$year[i],parms2]
  retro1[i,'pred'] = predict(m1, newdata = NewData)
}
retro1
write.table(retro1,'/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub/03_Dredge/D_Retro1.results_hake_all_stages_ROMS_PPC.csv',col.names=TRUE, row.names=FALSE,sep=',')

#### Use 1981-2005 to predict last five years #####
graphics.off()
retro5a = c()
retro5a$year = 2006:2010
retro5a = data.frame(retro5a)
retro5a$pred = NA
head(retro5a)

df1 = df0[df0$year %in% c(1981:2005),]
m1 = lm(form_best, data=df1)
NewData = df0[df0$year %in% c(2006:2010),parms2]
retro5a[,'pred'] = predict(m1, newdata = NewData)
retro5a
write.table(retro5a,'/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub/03_Dredge/D_Retro5a.results_hake_all_stages_ROMS_PPC.csv',col.names=TRUE, row.names=FALSE,sep=',')

######### retro-analysis 5 year blocks ##########

for(i in 1981:2005){
  print(i)
  x = i:(i+4)
  df1 = df0[df0$year != x,]     
  m1 = lm(form_best,data=df1)
  NewData = df0[df0$year %in% x,]
  pred0 = predict(m1, newdata = NewData)
  pred1 = data.frame(cbind(i,x,pred0))
  if(i==1981){PRED = pred1}else{PRED = rbind(PRED,pred1)}
}
PRED
write.table(PRED,'/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub/03_Dredge/D_full.retro_hake_all_stages_ROMS_PPC.csv',col.names=TRUE, row.names=FALSE,sep=',')

setwd(MainFile)
