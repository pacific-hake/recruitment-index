rm(list=ls()) # Clear variables in workspace


MainFile = "/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub"
StatsPrelim = paste0(paste0(MainFile,'/02_Stats_prelim'))

setwd(StatsPrelim)


# load data ####
roms = data.frame(read.table(paste0(MainFile,'/01_ROMS_prelim/Data_ROMS.PPC.for.hake.analysis.mean.master.csv'), header=TRUE, sep=','))
head(roms)



#### big table of correlations ####
df2 = roms[roms$year %in% 1981:2010,]
df3 = df2[,-1] # remove year
df3 = df3[,order(colnames(df3))]
cor1 = cor(df3)
write.csv(cor1,"R_corr.among.roms.hake.csv")

# BRING IN HAKE DATA AND SELECT YEARS ####

fish1 = data.frame(read.table(paste0(MainFile,"/00_Data/hake.data_2020_medians.csv"),header=TRUE, sep=','))
fish = fish1[fish1$year %in% 1981:2010,]

# Quick regressions between hake recrutiment deviations and each predictor ####
for(k in 2:ncol(roms)){
  print(k)
  Y = fish$dev # recruitment deviations from stock assessment
  X = roms[match(fish$year,roms$year),k]
 
  # run quick regressions 
  mod = lm(fish$dev ~ X)
  r2 = summary(mod)$r.squared
  b0 = summary(mod)$coefficients[1,1]
  b0.e = summary(mod)$coefficients[1,2]
  b0.t = summary(mod)$coefficients[1,3]
  b0.p = summary(mod)$coefficients[1,4]
  b1 = summary(mod)$coefficients[2,1]
  b1.e = summary(mod)$coefficients[2,2]
  b1.t = summary(mod)$coefficients[2,3]
  b1.p = summary(mod)$coefficients[2,4]
  aic = AIC(mod)
  colnames(roms)[k]
  Z = data.frame(cbind(colnames(roms)[k], r2,aic,b0, b0.e, b0.t, b0.p,b1, b1.e, b1.t, b1.p))
  colnames(Z)[1]<-"roms.var"
  
  # combine
  if(k==2){results1 = Z}else{results1 = rbind(results1,Z)}
          
  # plot quick figures 
  graphics.off()
  pdf(paste0(colnames(roms)[k],".hake.medians.pdf"),height=4, width=4)
  plot(X,Y, xlab=colnames(roms)[k], ylab="Recruitment devs")
  text(par()$usr[1], par()$usr[3]+ 0.9*(par()$usr[4]-par()$usr[3]), paste("r^2 =",round(r2,3)), pos=4)
  text(par()$usr[1], par()$usr[3]+ 0.8*(par()$usr[4]-par()$usr[3]), paste("p =",round(b1.p,3)), pos=4)
  dev.off()
  }
write.csv(results1,"R_Univariate.Regressions.hake.medians.csv")



# RUN quick POLYNOMIAL regressions between hake recruitment deviations and each predictor ####

# Quick regressions between hake recrutiment deviations and each predictor ####
for(k in 2:ncol(roms)){
  print(k)
  Y = fish$dev # recruitment deviations from stock assessment
  X = roms[match(fish$year,roms$year),k]
  # run quick regressions 
  mod = lm(fish$dev ~ X + I(X^2))
  mod1  = lm(fish$dev ~ X)
  r2 = summary(mod)$r.squared
  b0 = summary(mod)$coefficients[1,1]
  b0.e = summary(mod)$coefficients[1,2]
  b0.t = summary(mod)$coefficients[1,3]
  b0.p = summary(mod)$coefficients[1,4]
  b1 = summary(mod)$coefficients[2,1]
  b1.e = summary(mod)$coefficients[2,2]
  b1.t = summary(mod)$coefficients[2,3]
  b1.p = summary(mod)$coefficients[2,4]
  aicq = AIC(mod)
  colnames(roms)[k]
  Z = data.frame(cbind(colnames(roms)[k], r2,aicq,b0, b0.e, b0.t, b0.p,b1, b1.e, b1.t, b1.p))
  colnames(Z)[1]<-"roms.var"
  # combine
  if(k==2){results2 = Z}else{results2 = rbind(results2,Z)}
  
  # plot quick figures 
  graphics.off()
  pdf(paste0(colnames(roms)[k],".poly.hake.medians.pdf"),height=4, width=4)
  plot(X,Y, xlab=colnames(roms)[k], ylab="Recruitment devs")
  text(par()$usr[1], par()$usr[3]+ 0.9*(par()$usr[4]-par()$usr[3]), paste("r^2 =",round(r2,3)), pos=4)
  text(par()$usr[1], par()$usr[3]+ 0.8*(par()$usr[4]-par()$usr[3]), paste("p =",round(b1.p,3)), pos=4)
  dev.off()
}
write.csv(results2,"R_Polynomial.Regressions.hake.medians.csv")

RESULTS = results1[,c('roms.var','r2', 'aic')]
results.poly = results2[,c('roms.var','r2', 'aicq')] 
colnames(results.poly)[2] <- 'r2.q'
RESULTS = merge(RESULTS,results.poly, by="roms.var")
RESULTS$r2 = as.numeric(as.character(RESULTS$r2))
RESULTS$r2.q = as.numeric(as.character(RESULTS$r2.q))
RESULTS$dif = RESULTS$r2 - RESULTS$r2.q
RESULTS$dAIC = as.numeric(as.character(RESULTS$aic)) - as.numeric(as.character(RESULTS$aicq))
RESULTS
write.csv(RESULTS, 'R_R2.comparison.hake.medians.csv')




setwd(MainFile)
