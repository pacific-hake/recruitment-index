# source(paste0(main,'/00_BestModelInfo.R'))
# setwd(paste0(main,'/04_Model.Testing/05_dredge.refit.w.error'))

MainFile = "/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub"
results.loc = "/03_Dredge"
data.loc = paste0(MainFile,results.loc)
setwd(paste0(MainFile,'/04_Model.Testing/05_Resample_Recuitment_and_ReDredge'))

library(MuMIn)


####### Bring in Data ################

df0 = data.frame(read.csv(paste0(data.loc,"/Analyzed.data.hake_all_stages_ROMS_PPC.csv")))

data_1 = df0

# bring up original dredge model
form_dredge = readRDS(paste0(data.loc,"/Formula_for_Dredge.hake_all_stages_ROMS_PPC.rds"))
form_dredge


##########################################
#### RUN MODEL FITTING ###################
##########################################

#### Fit base model -- all terms ####
fit = lm(form_dredge, data=data_1, na.action = na.fail)

#### get predicted recruitment from stock assessment parameters ####
n = nrow(data_1) 
data_2 = data_1
  
for(i in 1:100){
     print(i)
  logrec = rec = 1:n
  
  
    for(z in 1:n){
      data_2$dev[z] = rnorm(1, data_1$dev[z], data_1$devsd[z])
    }

  
     #### FIT BASE MODEL ####
     fit = lm(form_dredge, data=data_2, na.action = na.fail)
     # RUN DREDGE ####
  mtable = dredge(fit, rank = AICc, m.lim = c(NA,5),
                  subset= # block correlated terms from the model > 0.70
                    
                    !(CSTlarv.s && CSTlatelarv.s ) &&  # 0.94 
                    
                    dc(CSTlarv.s, I(CSTlarv.s^2)) &&
                    dc(CSTlatelarv.s, I(CSTlatelarv.s^2)),
                     extra = list(R2 = function(x)
                       summary(x)$r.squared[[1]], 
                       F = function(y)
                         summary(y)$fstatistic[[1]]),
                     trace=2 )
     # mtable = dredge(fit, rank = AICc, m.lim = c(0, max.parms))
     print(paste('dredge',i))
     saveRDS(mtable, file=paste('Table_model.fits_i.',i,"_2010.rds", sep="" ))

} ####  end i


#############

#######
library(MuMIn)
#  load all the table and take best fit model from each
year = 1981:2010
x = dir()
y = x[grep('Table',x)]


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

A
saveRDS(A, "RESULTS_RESAMPLE.w.ERROR_top.models.by.year.NEW.hake_all_stages_ROMS_PPC.rds")
write.table(A, "RESULTS_RESAMPLE.w.ERROR_top.models.by.year.NEW.hake_all_stages_ROMS_PPC.csv",col.names = TRUE,sep = ',',row.names = FALSE)

A = readRDS('RESULTS_RESAMPLE.w.ERROR_top.models.by.year.NEW.hake_all_stages_ROMS_PPC.rds')

A = data.frame(A)
colnames(A)
A[is.na(A)] <- 0
A[A!=0] <- 1

B = data.frame(colSums(A))
B
# get number of jacknife iterations that included terms



x = grep('R2',colnames(A))-1
AA = data.frame(A[,2:x])
model.name = as.character(rownames(AA))
AA$name = substring(model.name,1,nchar(model.name)-6)
C = aggregate(. ~ name, data=AA, FUN = sum)
C[C > 0] <- 1
rownames(C) <- C$name
CC = C[,-1]
D = data.frame(colSums(CC))
D

B$ROMS = rownames(B)
D$ROMS = rownames(D)

Table6 = merge(B,D,'ROMS')
colnames(Table6) <- c("ROMS","Number of models", "Number of jackknifes")

write.csv(Table6,"Table_6_terms.in.models.hake_all_stages_ROMS_PPC.csv")
write.csv(Table6,paste0(MainFile,"/Figures/Table_6_terms.in.models.hake_all_stages_ROMS_PPC.csv"))


