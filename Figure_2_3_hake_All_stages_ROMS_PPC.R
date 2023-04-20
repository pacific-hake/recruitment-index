rm(list=ls()) # Clear variables in workspace
library(plotrix)
library(tidyverse)


MainFile = "/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub"


setwd(paste0(MainFile,'/00_Data'))

#setwd(paste0(MainFile,"/Figures"))
################################################
##### BRING IN HAKE DATA ####################
################################################
fish00 = data.frame(read.csv('hake.data_2020_medians.csv'), header=TRUE )
head(fish00)
max(fish00$year)
# years = 1981:max(fish00$year)
years = 1981:2010
fish_raw = fish00[fish00$year %in% years,]

fish <- map_df(fish_raw, ~ gsub("[,]", "", .x))

# sapply(fish,class)
fish$year = as.numeric(fish$year)
fish$sp.bio = as.numeric(fish$sp.bio)
fish$t.bio = as.numeric(fish$t.bio)
fish$Age2.bio = as.numeric(fish$Age2.bio)
fish$Age0.rec = as.numeric(fish$Age0.rec)
fish$yrminusone = as.numeric(fish$yrminusone)
fish$sp.bio.1 = as.numeric(fish$sp.bio.1)
fish$dev <- as.numeric(fish$dev)
fish$devsd <- as.numeric(fish$devsd)

fish <-data.frame(fish)

#### Background figures  ####
setwd(paste0(MainFile,"/Figures"))

graphics.off()


####### fish background ########
quartz()
pdf("Figure_2_hake_bio_params_medians.pdf", width = 4, height=12)


par(mfrow=c(3,1))

# spawning biomass
plot(fish$year, fish$sp.bio.1, type='l', pch=19, cex=1.5, xlab = "Year", ylab = "Spawning biomass (mt)")
legend('topleft',legend ='(a)', bty='n', cex = 1.5, inset = -0.03)



# recruitment through time
plot(fish$year, fish$Age0.rec, type='l', pch=19, cex=1.5, xlab = "Year", ylab = "Age-0 recruits (millions)")
legend('topleft',legend ='(b)', bty='n', cex = 1.5, inset = -0.03)




# recuits per spawner
max(fish$sp.bio.1) # 2326
min(fish$sp.bio.1) # 602

plot(fish$sp.bio.1, fish$Age0.rec,type='p', pch=19, cex=1.5,
     xlim=c(0,2300),
     xlab = "Spawning biomass (mt)", ylab = "Age-0 recruits (millions)")


#abline(lm(fish$Age0.rec ~ fish$sp.bio.1))
legend('topleft',legend ='(c)', bty='n', cex = 1.5, inset = -0.03)


# Plot S-R relationship
# h and R0 from the 2020 assessment are in Table 27: h = 0.854 and R0 = 1600
h = 0.854
R0 = 1600
SSB0= 1385

SSB.seq=seq(0,3000, by = 10)

R.plot = (SSB.seq *4*h*R0) / (SSB0*(1-h) + SSB.seq*(5*h-1))

lines(SSB.seq, R.plot)


dev.off()

######## ROMS Variables in final model #########
roms = data.frame(read.csv(paste0(MainFile, '/01_ROMS_Prelim/Data_ROMS.for.hake.analysis_all_stages_ROMS_PPC.csv'), header=TRUE))

roms = roms[roms$year %in% 1981:2010,]

parms = c('BIpre','EKEpre.ms.c','PREYpre.her','ASTyolk','STORMB')

Inset = -0.01

graphics.off()

quartz()
pdf("Figure_3_hake_dev_vert.pdf", width = 6, height=12)


op <- par(mfrow=c(6,1), mar=c(5, 6, 2, 1) + 0.1)

plot(roms$year,roms$BIpre, type='l', cex=1.25, xlab = '', ylab="BI: Preconditioning", bty='l')
#expression("BI: Preconditioning")
legend('topleft', legend='(a)', bty='n', inset=Inset)

plot(roms$year,roms$EKEpre.ms.c, type='l', cex=1.25, xlab = '', ylab="May-Sep EKE: Preconditioning", bty='l')
legend('topleft', legend='(b)', bty='n', inset=Inset)

plot(roms$year,roms$PREYpre.her, type='l', cex=1.25, xlab = '', ylab = "Herring biomass: Preconditioning", bty='l')
legend('topleft', legend='(c)', bty='n', inset=Inset)

plot(roms$year,roms$ASTyolk, type='l', cex=1.25, xlab = '', ylab = "Alongshore transport:\nYolk-sac larvae", bty='l')
legend('topleft', legend='(d)', bty='n', inset=Inset)

plot(roms$year,roms$STORMB, type='l', cex=1.25, xlab = '', ylab = "# of days between storms:\nLarvae", bty='l')
legend('topleft', legend='(e)', bty='n', inset=Inset)

plot(fish$year, fish$dev, type='l', pch=19, cex=1.25, xlab="Year", ylab='Median log deviations', ylim=c(-4.05,4.05), bty='l')
lines(fish$year, fish$dev + fish$devsd, lty='dotted')
lines(fish$year, fish$dev - fish$devsd, lty='dotted')
legend('topleft', legend='(f)', bty='n', inset=Inset)

par(op)

dev.off()


