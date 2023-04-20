# this is actually Figure 4a in the manuscript
#library(MuMIn)
# source(paste0(main,'/00_BestModelInfo.R'))
# setwd(paste0(main,"/Figures"))

MainFile = "/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub"
data.loc = "/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub/03_Dredge"
FigLoc = paste0(MainFile,'/Figures')
setwd(paste0(MainFile,"/Figures"))


retro1 = data.frame(read.table(paste0(data.loc,'/D_Retro1.results_hake_all_stages_ROMS_PPC.csv'),header=TRUE, sep=','))
retro5 = data.frame(read.table(paste0(data.loc,'/D_Retro5.results_hake_all_stages_ROMS_PPC.csv'),header=TRUE, sep=','))
retro5a = data.frame(read.table(paste0(data.loc,'/D_Retro5a.results_hake_all_stages_ROMS_PPC.csv'),header=TRUE, sep=','))
retro.full = data.frame(read.table(paste0(data.loc,'/D_full.retro_hake_all_stages_ROMS_PPC.csv'),header=TRUE, sep=','))

m1 = lm(form_best, data=df0)
(s1 = summary(m1))
p1 = predict(m1, se.fit=TRUE)

#### Figure 3 ########
#### Predicted vs Observed Figure ####

Inset = -0.01

graphics.off()
quartz()
pdf("Figure_3_hake_all_stages_ROMS_PPC_test.pdf", width = 5.45, height=3.25)
#png("Figure_3_hake_all_stages_ROMS_PPC.png",width = 320, height=250)

par(mar=c(4,4,1,1))
pt.cex = 0.6

ymin = min(p1$fit-1.96*p1$se.fit, df0$dev)
ymax = max(p1$fit+1.96*p1$se.fit, df0$dev)
plot(1981:2010, df0$dev, type='p', ylim=c(-4.0, 4.0), 
       xlab="Year", ylab = 'Median log deviations', pch=21, cex=pt.cex)

lines(1981:2010,p1$fit)
lines(1981:2010,p1$fit+1.96*p1$se.fit, lty='dotted')
lines(1981:2010,p1$fit-1.96*p1$se.fit, lty='dotted')
axis(side=1, labels=FALSE, at=1981:2010, tick = TRUE, tck = -0.01)
axis(side=2, seq(-4,4,2),tick = TRUE, tck = -0.01)
points(retro1$year, retro1$pred, pch=8, cex=pt.cex)
points(retro5a$year, retro5a$pred, pch=21, bg='red', cex=pt.cex)

legend('topleft', legend='(a)', bty='n', inset=Inset)
                       
#legend('topleft', legend='', bty='n', inset=Inset)

dev.off()


