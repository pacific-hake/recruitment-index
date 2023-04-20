# setwd(paste0(main,'/04_Model.Testing/04_Jackknife & Refit'))

MainFile = "/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub"
setwd(paste0(MainFile,'/04_Model.Testing/04_Jackknife_and_ReDredge'))

library(MuMIn)

######## LOAD DATA  ############
data_1 = df0


# bring up original dredge model
form_dredge = readRDS("/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub/03_Dredge/Formula_for_Dredge.hake_all_stages_ROMS_PPC.rds")
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
     saveRDS(mtable, file=paste('Table_model.fits','_k.',k,"_2010.rds", sep="" ))
}  # end K

year = 1981:2010


x = dir()
y = x[grep('Table',x)]
y

write.table(y,'Model_order_hake_all_stages_ROMS_PPC.csv',row.names=FALSE, col.names=TRUE, sep=',')

w = strsplit(y, fixed = TRUE, "_")

# get models with AIC < 2 and fewest DF

for(i in 1:length(y)){
     print(i)
     a = readRDS(y[i])
     a_2 = subset(a, delta<2)
     a1 = a_2[a_2$df == min(a_2$df),]
     rownames(a1) = paste(rep(y[i], nrow(a1)), 1:nrow(a1), sep='_')
     print(y[i])
     print(a1)
     if(i==1){A = a1}else{A = rbind(A,a1)}
}

A
saveRDS(A, "RESULTS_JACKKNIFE.REFIT_top.models.by.year_hake_all_stages_ROMS_PPC.rds")
write.table(A, "RESULTS_JACKKNIFE.REFIT_top.models.by.year_hake_all_stages_ROMS_PPC.csv",col.names = TRUE,sep = ',',row.names = FALSE)

A = readRDS("RESULTS_JACKKNIFE.REFIT_top.models.by.year_hake_all_stages_ROMS_PPC.rds")

B = data.frame(A)
B[!is.na(B)] <- 1

cs = colSums(B, na.rm = TRUE)
cs
Table5 = data.frame(cs)
Table5

write.csv(Table5, paste0(MainFile,"/Figures/Table_5_hake_all_stages_ROMS_PPC.csv"), row.names = TRUE)

# return to best model info settings
setwd(MainFile)



# check to see top models in a year 1982
x1= readRDS("/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub/04_Model.Testing/04_Jackknife_and_ReDredge/Table_model.fits_k.1_2010_medians.rds")
head(x1,n=1)


