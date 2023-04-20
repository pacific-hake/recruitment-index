# Figure 5

rm(list=ls())
library(MuMIn)

# Load part of 00_Initial_Diagnostic code before running the residual plot code 


MainFile = getwd()
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



# partial residual plots - standardized coefficients

dfstd = apply(df0[,parms2],2,scale)
df2 = data.frame(cbind(df0$year, df0$dev, dfstd))
colnames(df2)[1:2] <- c('year','dev')

#head(df2,n=30)

#sapply(df2,class) # numeric
  
m1 = lm(form_best, data=df2)
(s1 = summary(m1))



##### PARTIAL RESIDUAL PLOTS ####
# Plot each variable individually 

graphics.off()
quartz()
pdf("/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub/Figures/Figure_5_hake_all_stages_ROMS_PPC.pdf",width=6, height=7)
par(mfrow=c(3,2), mar=c(4,4,1,2), pch=19, cex=0.7)


x1 = df2$BIpre
x2 = s1$coefficients[2,1]*x1 + s1$residuals

plot(x1, x2, xlab = "Bifurcation Index: Adult preconditioning", xlim=c(-3.5,3.5), ylim=c(-4.5,4.0), ylab = "Partial residuals", bty="l")
#4)


m2 = lm(x2  ~ x1)
p1 = predict(m2, se.fit=TRUE)
p2 = data.frame(cbind(x1,p1$fit,p1$se.fit))
colnames(p2) = c('x','y','se')
p2 = p2[order(p2$x),]
lines(p2$x, p2$y)
lines(p2$x, p2$y+1.96*p2$se, lty='dotted')
lines(p2$x, p2$y-1.96*p2$se, lty='dotted')
xy = par()$usr
text(x1,x2, df2$year, cex=0.6, pos=2)
legend('topleft',legend ='(a)', bty='n', inset = -0.03)


# May-Sep EKE central region
x1 = df2$EKEpre.ms.c
x2 = s1$coefficients[3,1]*x1 + s1$residuals


plot(x1, x2, xlab = expression("Eddy kinetic energy" ~ (m^{2}/s^{2}):~"Adult preconditioning"), xlim=c(-3.5,3.5), ylim=c(-4.5,4.0), ylab = "Partial residuals", bty="l")


m2 = lm(x2  ~ x1)
p1 = predict(m2, se.fit=TRUE)
p2 = data.frame(cbind(x1,p1$fit,p1$se.fit))
colnames(p2) = c('x','y','se')
p2 = p2[order(p2$x),]
lines(p2$x, p2$y)
lines(p2$x, p2$y+1.96*p2$se, lty='dotted')
lines(p2$x, p2$y-1.96*p2$se, lty='dotted')
xy = par()$usr
text(x1,x2,df2$year, cex=0.6, pos=2)
legend('topleft',legend ='(b)', bty='n', inset = -0.03)


# Herring biomass 
x1 = df2$PREYpre.her
x2 = s1$coefficients[4,1]*x1 + s1$residuals


plot(x1, x2, xlab = 'Herring biomass (mt): Adult preconditioning', xlim=c(-3.5,3.5), ylim=c(-4.5,4.0), ylab = "Partial residuals", bty="l")


m2 = lm(x2  ~ x1)
p1 = predict(m2, se.fit=TRUE)
p2 = data.frame(cbind(x1,p1$fit,p1$se.fit))
colnames(p2) = c('x','y','se')
p2 = p2[order(p2$x),]
lines(p2$x, p2$y)
lines(p2$x, p2$y+1.96*p2$se, lty='dotted')
lines(p2$x, p2$y-1.96*p2$se, lty='dotted')
xy = par()$usr
text(x1,x2,df2$year, cex=0.6, pos=2)
legend('topleft',legend ='(c)', bty='n', inset = -0.03)



# Long-shore transport during yolk-sac larval stage
x1 = df2$ASTyolk
x2 = s1$coefficients[5,1]*x1 + s1$residuals


plot(x1, x2, xlab = 'Alongshore transport (m/s): Yolk-sac larvae', xlim=c(-3.5,3.5), ylim=c(-4.5,4.0), ylab = "Partial residuals", bty="l")


m2 = lm(x2  ~ x1)
p1 = predict(m2, se.fit=TRUE)
p2 = data.frame(cbind(x1,p1$fit,p1$se.fit))
colnames(p2) = c('x','y','se')
p2 = p2[order(p2$x),]
lines(p2$x, p2$y)
lines(p2$x, p2$y+1.96*p2$se, lty='dotted')
lines(p2$x, p2$y-1.96*p2$se, lty='dotted')
xy = par()$usr
text(x1,x2,df2$year, cex=0.6, pos=2)
legend('topleft',legend ='(d)', bty='n', inset = -0.03)


# Days between storms during first-feeding larval stage 
x1 = df2$STORMB 
x2 = s1$coefficients[6,1]*x1 + s1$residuals


plot(x1, x2, xlab = 'Days between storms: First-feeding larvae', xlim=c(-3.5,3.5), ylim=c(-4.5,4.0), ylab = "Partial residuals", bty="l")


m2 = lm(x2  ~ x1)
p1 = predict(m2, se.fit=TRUE)
p2 = data.frame(cbind(x1,p1$fit,p1$se.fit))
colnames(p2) = c('x','y','se')
p2 = p2[order(p2$x),]
lines(p2$x, p2$y)
lines(p2$x, p2$y+1.96*p2$se, lty='dotted')
lines(p2$x, p2$y-1.96*p2$se, lty='dotted')
xy = par()$usr
text(x1,x2,df2$year, cex=0.6, pos=2)
legend('topleft',legend ='(e)', bty='n', inset = -0.03)



dev.off()




# Horizontal figure
graphics.off()
quartz()
pdf("/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub/Figures/Figure_5_hake_all_stages_ROMS_PPC_5panel_standardized_corrected_SC_medians_horizontal.pdf",width=10, height=6)
par(mfrow=c(2,3), mar=c(4,4,1,2), pch=19, cex=0.7)


x1 = df2$BIpre
x2 = s1$coefficients[2,1]*x1 + s1$residuals

plot(x1, x2, xlab = "Bifurcation Index: Adult preconditioning", xlim=c(-3.5,3.5), ylim=c(-4.5,4.0), ylab = "Partial residuals", bty="l")
#4)


m2 = lm(x2  ~ x1)
p1 = predict(m2, se.fit=TRUE)
p2 = data.frame(cbind(x1,p1$fit,p1$se.fit))
colnames(p2) = c('x','y','se')
p2 = p2[order(p2$x),]
lines(p2$x, p2$y)
lines(p2$x, p2$y+1.96*p2$se, lty='dotted')
lines(p2$x, p2$y-1.96*p2$se, lty='dotted')
xy = par()$usr
text(x1,x2, df2$year, cex=0.6, pos=2)
legend('topleft',legend ='(a)', bty='n', inset = -0.03)


# May-Sep EKE central region
x1 = df2$EKEpre.ms.c
x2 = s1$coefficients[3,1]*x1 + s1$residuals


plot(x1, x2, xlab = expression("Eddy kinetic energy" ~ (m^{2}/s^{2}):~"Adult preconditioning"), xlim=c(-3.5,3.5), ylim=c(-4.5,4.0), ylab = "Partial residuals", bty="l")


m2 = lm(x2  ~ x1)
p1 = predict(m2, se.fit=TRUE)
p2 = data.frame(cbind(x1,p1$fit,p1$se.fit))
colnames(p2) = c('x','y','se')
p2 = p2[order(p2$x),]
lines(p2$x, p2$y)
lines(p2$x, p2$y+1.96*p2$se, lty='dotted')
lines(p2$x, p2$y-1.96*p2$se, lty='dotted')
xy = par()$usr
text(x1,x2,df2$year, cex=0.6, pos=2)
legend('topleft',legend ='(b)', bty='n', inset = -0.03)


# Herring biomass 
x1 = df2$PREYpre.her
x2 = s1$coefficients[4,1]*x1 + s1$residuals


plot(x1, x2, xlab = 'Herring biomass (mt): Adult preconditioning', xlim=c(-3.5,3.5), ylim=c(-4.5,4.0), ylab = "Partial residuals", bty="l")


m2 = lm(x2  ~ x1)
p1 = predict(m2, se.fit=TRUE)
p2 = data.frame(cbind(x1,p1$fit,p1$se.fit))
colnames(p2) = c('x','y','se')
p2 = p2[order(p2$x),]
lines(p2$x, p2$y)
lines(p2$x, p2$y+1.96*p2$se, lty='dotted')
lines(p2$x, p2$y-1.96*p2$se, lty='dotted')
xy = par()$usr
text(x1,x2,df2$year, cex=0.6, pos=2)
legend('topleft',legend ='(c)', bty='n', inset = -0.03)



# Long-shore transport during yolk-sac larval stage
x1 = df2$ASTyolk
x2 = s1$coefficients[5,1]*x1 + s1$residuals


plot(x1, x2, xlab = 'Alongshore transport (m/s): Yolk-sac larvae', xlim=c(-3.5,3.5), ylim=c(-4.5,4.0), ylab = "Partial residuals", bty="l")


m2 = lm(x2  ~ x1)
p1 = predict(m2, se.fit=TRUE)
p2 = data.frame(cbind(x1,p1$fit,p1$se.fit))
colnames(p2) = c('x','y','se')
p2 = p2[order(p2$x),]
lines(p2$x, p2$y)
lines(p2$x, p2$y+1.96*p2$se, lty='dotted')
lines(p2$x, p2$y-1.96*p2$se, lty='dotted')
xy = par()$usr
text(x1,x2,df2$year, cex=0.6, pos=2)
legend('topleft',legend ='(d)', bty='n', inset = -0.03)


# Days between storms during first-feeding larval stage 
x1 = df2$STORMB 
x2 = s1$coefficients[6,1]*x1 + s1$residuals


plot(x1, x2, xlab = 'Days between storms: First-feeding larvae', xlim=c(-3.5,3.5), ylim=c(-4.5,4.0), ylab = "Partial residuals", bty="l")


m2 = lm(x2  ~ x1)
p1 = predict(m2, se.fit=TRUE)
p2 = data.frame(cbind(x1,p1$fit,p1$se.fit))
colnames(p2) = c('x','y','se')
p2 = p2[order(p2$x),]
lines(p2$x, p2$y)
lines(p2$x, p2$y+1.96*p2$se, lty='dotted')
lines(p2$x, p2$y-1.96*p2$se, lty='dotted')
xy = par()$usr
text(x1,x2,df2$year, cex=0.6, pos=2)
legend('topleft',legend ='(e)', bty='n', inset = -0.03)


dev.off()
