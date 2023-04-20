# Jackknife and ReDredge to 2005: jackknife resampling but fit only the years 1981–2005, 
# compared the resulting models to the best-fit model (from original Dredge), 
# and used the 1981–2005 model to predict recruitment deviations for 2006–2010.


rm(list=ls())

MainFile = "/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub"
setwd(paste0(MainFile,'/04_Model.Testing/06_Jackknife_and_ReDredge_to_2005'))
#results.loc = "/03_Dredge"
#data.loc = paste0(MainFile,results.loc)


library(MuMIn)


###### LOAD ORIGINAL DATA #####

df0 = data.frame(read.csv('/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub/03_Dredge/Analyzed.data.hake_all_stages_ROMS_PPC.csv'))


######## Now Jackknife and ReDredge to 2005 ########

######## LOAD DATA  ############
data_1 = df0[df0$year %in% 1981:2005,]


# bring up original dredge model
form_dredge = readRDS('/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub/03_Dredge/Formula_for_Dredge.hake_all_stages_ROMS_PPC.rds')
form_dredge

##########################################
#### RUN MODEL FITTING ###################
##########################################



#### Fit base model -- all terms ####
fit = lm(form_dredge, data=data_1, na.action = na.fail)

for(k in 1:nrow(data_1)){
     print(k)
     data_2 = data_1[-k,]
     #### FIT BASE MODEL ####
     fit = lm(form_dredge, data=data_2, na.action = na.fail)

     # RUN DREDGE ####
     mtable = dredge(fit, rank = AICc, m.lim = c(0,5),
                     subset= # block correlated terms from the model > 0.70
                             !(CSTlarv.s && CSTlatelarv.s ) &&  # 0.94 
                             
                             dc(CSTlarv.s, I(CSTlarv.s^2)) &&
                             dc(CSTlatelarv.s, I(CSTlatelarv.s^2)),
                     extra = list(R2 = function(x)
                        summary(x)$r.squared[[1]], 
                        F = function(y)
                        summary(y)$fstatistic[[1]]),
                     trace=2 )
     #mtable = dredge(fit, rank = AICc, m.lim = c(0, max.parms))
     saveRDS(mtable, file=paste('Table_model.fits','_k.',k,"_all_stages_ROMS_PPC.rds", sep="" ))
}  # end K

x = dir()
y = x[grep('Table_model',x)]
y

write.table(y,'Model_order_hake_all_stages_ROMS_PPC.csv',row.names=FALSE, col.names=TRUE, sep=',')

w = strsplit(y, fixed = TRUE, "_")

# get models with AIC < 2 and fewest DF

for(i in 1:length(y)){
     print(i)
     a = readRDS(y[i])
     a_2 = subset(a, delta<2)
     a_2
     a1 = a_2[a_2$df == min(a_2$df),]
     rownames(a1) = paste(rep(y[i], nrow(a1)), 1:nrow(a1), sep='_')
     print(y[i])
     print(a1)
     if(i==1){A = a1}else{A = rbind(A,a1)}
}

A
saveRDS(A, "RESULTS_JACKKNIFE.REFIT_top.models.by.year.to2005.hake_all_stages_ROMS_PPC.rds")
write.table(A, "RESULTS_JACKKNIFE.REFIT_top.models.by.year.to2005.hake_all_stages_ROMS_PPC.csv",col.names = TRUE,sep = ',',row.names = FALSE)

A = readRDS('RESULTS_JACKKNIFE.REFIT_top.models.by.year.to2005.hake_all_stages_ROMS_PPC.rds')

A = data.frame(A)
colnames(A)
A[is.na(A)] <- 0
A[A!=0] <- 1
A

B = data.frame(colSums(A))
B
write.csv(B,'Table_7_hake_all_stages_ROMS_PPC.csv')
write.csv(B,paste0(MainFile,"/Figures/Table_7_hake_all_stages_ROMS_PPC.csv"))


########### bring in form_best from 00_Initial_Diagnostic code ###########
best_fit_vars = c("dev",'BIpre','EKEpre.ms.c','PREYpre.her','LSTyolk','STORMB') 

# create formula 
form0 = paste0(best_fit_vars[1]," ~ ",best_fit_vars[2])
for(i in 3:length(best_fit_vars)){
        form0 = paste0(form0, " + ", best_fit_vars[i])
}
form0
form_best = as.formula(form0)


########### predict last 1981-2010 ###################
m3 = lm(form_best, data = data_1 )
(s3 = summary(m3))
capture.output(s3, file = "Model_1981_2005_hake_all_stages_ROMS_PPC.txt")
new.data = df0[df0$year %in% 1981:2010,]
new.data$color = ifelse(new.data$year < 2006, 'black','red')
new.data$year<- as.numeric(new.data$year)

p1 = predict(m3, newdata = new.data, se.fit = TRUE)

ymin = min(new.data$dev, (p1$fit-p1$se.fit))*1.1
ymax = max(new.data$dev, (p1$fit+p1$se.fit))*1.1
p1 = data.frame(p1)
data.out = cbind(new.data,p1)
as.numeric(data.out$year)
d05 <- data.out[data.out$year < 2006,]
d06 <- data.out[data.out$year >= 2006,]

write.csv(data.out, "D_jackknife_to_2005_hake_all_stages_ROMS_PPC.csv", row.names = FALSE)


Inset = -0.01

graphics.off()
quartz()
#png("F_jackknife_2005_hake_test3.png", units='in',res=600, width = 5, height = 3)
pdf("/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub/Figures/F_jackknife_2005_hake_all_stages_ROMS_PPC.pdf", width = 5.45, height= 3.27)

par(mar=c(4,4,1,1))
pt.cex = 0.6



par(mar=c(4,4,1,1))
plot(data.out$year, data.out$dev, ylim=c(-4.0, 4.0), col = ifelse(data.out$year < 2006, 'black','red'), pch=21, xlab = "Year", ylab = "Median log deviations", cex=pt.cex)
lines(d05$year, d05$fit,col = 'black')
lines(d06$year, d06$fit,col = 'red')
lines(d05$year, d05$fit + 1.96*d05$se.fit, col = 'black', lty='dotted')
lines(d05$year, d05$fit - 1.96*d05$se.fit, col = 'black', lty='dotted')

lines(d06$year, d06$fit + 1.96*d06$se.fit, col = 'red', lty='dotted')
lines(d06$year, d06$fit - 1.96*d06$se.fit, col = 'red', lty='dotted')
axis(side=1, at=1981:2010, labels = NA, tck = -0.02)

legend('topleft', legend='(b)', bty='n', inset=Inset)

dev.off()



# return to best model info settings

setwd(MainFile)





########### bring in best-fit model from 1981 - 2005 jackknife refit ###########
best_fit_vars = c("dev",'BIpre','CSTlarv.s','I(CSTlarv.s^2)','logPRED.pjuv.age0.age1.hake','NINOpre') 

# create formula 
form0 = paste0(best_fit_vars[1]," ~ ",best_fit_vars[2])
for(i in 3:length(best_fit_vars)){
        form0 = paste0(form0, " + ", best_fit_vars[i])
}
form0
form_best = as.formula(form0)




########### predict last 1981-2010 ###################
m3 = lm(form_best, data = data_1 )
(s3 = summary(m3))
capture.output(s3, file = "Model_1981_2005_hake_all_stages_ROMS_PPC_best_fit_1981_2005.txt")
new.data = df0[df0$year %in% 1981:2010,]
new.data$color = ifelse(new.data$year < 2006, 'black','red')
new.data$year<- as.numeric(new.data$year)

p1 = predict(m3, newdata = new.data, se.fit = TRUE)

ymin = min(new.data$dev, (p1$fit-p1$se.fit))*1.1
ymax = max(new.data$dev, (p1$fit+p1$se.fit))*1.1
p1 = data.frame(p1)
data.out = cbind(new.data,p1)
as.numeric(data.out$year)
d05 <- data.out[data.out$year < 2006,]
d06 <- data.out[data.out$year >= 2006,]

write.csv(data.out, "D_jackknife_to_2005_hake_all_stages_ROMS_PPC_best_fit_1981_2005.csv", row.names = FALSE)


Inset = -0.01

graphics.off()
quartz()

pdf("/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub/Figures/F_jackknife_2005_hake_all_stages_ROMS_PPC_best_fit_1981_2005.pdf", width = 5, height = 3)

par(mar=c(4,4,1,1))
pt.cex = 0.6



par(mar=c(4,4,1,1))
plot(data.out$year, data.out$dev, ylim=c(-4.0, 4.0), col = ifelse(data.out$year < 2006, 'black','red'), pch=21, xlab = "Year", ylab = "Median log deviations", cex=pt.cex)
lines(d05$year, d05$fit,col = 'black')
lines(d06$year, d06$fit,col = 'red')
lines(d05$year, d05$fit + 1.96*d05$se.fit, col = 'black', lty='dotted')
lines(d05$year, d05$fit - 1.96*d05$se.fit, col = 'black', lty='dotted')

lines(d06$year, d06$fit + 1.96*d06$se.fit, col = 'red', lty='dotted')
lines(d06$year, d06$fit - 1.96*d06$se.fit, col = 'red', lty='dotted')
axis(side=1, at=1981:2010, labels = NA, tck = -0.02)

legend('topleft', legend='(b)', bty='n', inset=Inset)

dev.off()

