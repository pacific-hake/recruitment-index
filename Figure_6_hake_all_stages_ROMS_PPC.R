library(MuMIn)
library(plotrix)
# source(paste0(main,'/00_BestModelInfo.R'))

############## FIGURE ##################
#setwd('/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub/04_Model.Testing/02_Jackknife')
results = data.frame(read.csv('/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub/04_Model.Testing/02_Jackknife/R_jackknife_hake_all_stages_ROMS_PPC_raw.csv'))
results = na.omit(results)
boot.final = data.frame(read.csv('/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub/04_Model.Testing/02_Jackknife/R_jackknife_stats_all_stages_redo_2836_log_master_table_complete_all_stages_best_model_plus_all_BestFit_ROMS_plus_ppc_02_2021_NINO_NPGO_PREYpre_corrected_SC_medians_raw.csv'))

graphics.off()
 pdf("/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub/Figures/Figure_6_hake_all_stages.pdf", width = 4, height = 6)
# png("Figure_6.png", units='in',res=600, width=4, height=6)
# tiff("Figure_6.tif", units='in',res=300, width=4, height=6)
par(mfrow=c(2,1), mar=c(4,4,1,1))
#hist(results$r2, xlab = "R-squared", main=NA)
hist(results$r2, xlab = expression(~R^{2}~""), main=NA, ylim = c(0,20), yaxt='n')
axis(2, at= seq(0,16,by=4), labels = seq(0,16,by=4))
 

xy = par()$usr
text(xy[1]+0.1*(xy[2]-xy[1]), xy[3]+0.95*(xy[4]-xy[3]), "(a)")
plot(1981:2010, results$r2, xlab="Year (removed)", ylab = expression(~R^{2}~""), bty='l', pch=19, cex=0.8, cex.axis=0.8, ylim= c(0,1))
xy = par()$usr
text(xy[1]+0.1*(xy[2]-xy[1]), xy[3]+0.95*(xy[4]-xy[3]), "(b)")
segments(1980,boot.final$r2[1], 2010, boot.final$r2[1], lty='dotted')

dev.off()

setwd(MainFile)