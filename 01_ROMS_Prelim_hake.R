rm(list=ls()) # Clear variables in workspace

MainFile = "/Users/cdvestfals/Desktop/Hake_Analysis_for_GitHub"
ROMSPrelim = paste0(MainFile,'/01_ROMS_Prelim')

setwd(ROMSPrelim)

ROMS = data.frame(read.table(paste0(MainFile,'/00_Data/ROMS_vars_hake_south_for_analysis.csv'),sep=',',header=TRUE))

colnames(ROMS)

head(ROMS)
tail(ROMS)

# make new var for winter values that cross years
ROMS$y2 = ROMS$Year
ROMS$y2 = ifelse(ROMS$Month %in% c(11,12),ROMS$Year+1,ROMS$Year)
head(ROMS)

#### calcuate periods specific values for each variable ####
fun = c('sum','mean','sd','max','min','median')
for(i in 1:length(fun)){
  print(i)
  roms <- ROMS  # reset dd calculations
  roms_2 = data.frame(1980:2010)
  colnames(roms_2)<-'year'
  
  # PRECONDITIONING STAGE (April - Sep/Oct)
  # H1 NA. Prey - euphausiids or Bifurcation Index. No data. 
  
  # H2a: Temperature
  H2a  = aggregate(H2a ~ y2, data=roms[roms$Month %in% c(4:10),], FUN=fun[i])               
  roms_2$H2a = H2a[match(roms_2$year, H2a$y2+1),2] # match previous year
  
  # H2b: Degree days preconditioning 
  temp.ref = 5.0
  roms$H2b = roms$H2b-temp.ref
  H2b = aggregate(H2b ~ Year, data=roms[roms$Month %in% 4:10,], FUN=fun[i]) # mean fall transition date 292/Oct 22         
  roms_2$H2b = H2b[match(roms_2$year, H2b$Year+1),2] # match previous year
  
  # H3a: Upwelling CUTI (42 - 47 °N)
  H3a  = aggregate(H3a ~ y2, data=roms[roms$Month %in% c(4:10),], FUN=fun[i])               
  roms_2$H3a = H3a[match(roms_2$year, H3a$y2+1),2] # match previous year
  
  # H3b: Upwelling BEUTI (42 - 47 °N)
  H3b  = aggregate(H3b ~ y2, data=roms[roms$Month %in% c(4:10),], FUN=fun[i])               
  roms_2$H3b = H3b[match(roms_2$year, H3b$y2+1),2] # match previous year
  
  
  
  # SPAWNING STAGE (Jan - Mar)
  
  # H4: Temperature
  H4  = aggregate(H4 ~ y2, data=roms[roms$Month %in% c(1,2,3),], FUN=fun[i])               
  roms_2$H4 = H4[match(roms_2$year, H4$y2),2]
  
  # H5 Degree days
  roms$H5 = roms$H5-temp.ref
  H5 = aggregate(H5 ~ y2, data=roms[roms$Month %in% c(1,2,3),], FUN=fun[i])        
  roms_2$H5 = H5[match(roms_2$year, H5$y2),2]
  
  
  
  # EGG STAGE (Jan - Mar)
  # H6: MLD
  H6  = aggregate(H6 ~ y2, data=roms[roms$Month %in% c(1,2,3),], FUN=fun[i])             
  roms_2$H6 = H6[match(roms_2$year, H6$y2),2]
  
  # H7a: Cross shelf south/shallow (31 - 34.5 °N; 40 - 60 m)
  H7a  = aggregate(H7a ~ y2, data=roms[roms$Month %in% c(1,2,3),], FUN=fun[i])             
  roms_2$H7a = H7a[match(roms_2$year, H7a$y2),2]
  
  # H7b: Cross shelf south/deep (31 - 34.5 °N; 40 - 200 m)
  H7b  = aggregate(H7b ~ y2, data=roms[roms$Month %in% c(1,2,3),], FUN=fun[i])             
  roms_2$H7b = H7b[match(roms_2$year, H7b$y2),2]
  
  # H7c: Cross shelf north/shallow  (34.5 - 36 °N; 40 - 60 m)
  H7c  = aggregate(H7c ~ y2, data=roms[roms$Month %in% c(1,2,3),], FUN=fun[i])             
  roms_2$H7c = H7c[match(roms_2$year, H7c$y2),2]
  
  # H7d: Cross shelf north/deep (34.5 - 36 °N; 40 - 200 m)
  H7d  = aggregate(H7d ~ y2, data=roms[roms$Month %in% c(1,2,3),], FUN=fun[i])             
  roms_2$H7d = H7d[match(roms_2$year, H7d$y2),2]
  
  # H8a: Long shore shallow (40 - 60 m)
  H8a = aggregate(H8a ~ y2, data=roms[roms$Month %in% c(1,2,3),], FUN=fun[i])  
  roms_2$H8a = H8a[match(roms_2$year, H8a$y2),2]
  
  # H8b: Long shore deep (40 - 200 m)
  H8b = aggregate(H8b ~ y2, data=roms[roms$Month %in% c(1,2,3),], FUN=fun[i])  
  roms_2$H8b = H8b[match(roms_2$year, H8b$y2),2]
  
  # H9: Poleward Undercurrent
  H9  = aggregate(H9 ~ y2, data=roms[roms$Month %in% c(1,2,3),], FUN=fun[i])             
  roms_2$H9 = H9[match(roms_2$year, H9$y2),2]
  
  # H10a: Degree days shallow (40 - 60 m)
  roms$H10a = roms$H10a-temp.ref
  H10a  = aggregate(H10a ~ y2, data=roms[roms$Month %in% c(1,2,3),], FUN=fun[i])           
  roms_2$H10a = H10a[match(roms_2$year, H10a$y2),2]
  
  # H10b: Degree days deep (40 - 200 m)
  roms$H10b = roms$H10b-temp.ref
  H10b  = aggregate(H10b ~ y2, data=roms[roms$Month %in% c(1,2,3),], FUN=fun[i])           
  roms_2$H10b = H10b[match(roms_2$year, H10b$y2),2]
  
  
  # YOLK-SAC STAGE (Jan - Apr) 
  # H11: MLD
  H11 = aggregate(H11 ~ y2, data=roms[roms$Month %in% c(1,2,3,4),], FUN=fun[i])        
  roms_2$H11 = H11[match(roms_2$year, H11$y2),2]
  
  # H12a Cross shelf - south (31 - 34.5 °N)
  H12a = aggregate(H12a ~ y2, data=roms[roms$Month %in% c(1,2,3,4),], FUN=fun[i])        
  roms_2$H12a = H12a[match(roms_2$year, H12a$y2),2]
  
  # H12b Cross shelf - north (34.5 - 36 °N)
  H12b = aggregate(H12b ~ y2, data=roms[roms$Month %in% c(1,2,3,4),], FUN=fun[i])        
  roms_2$H12b = H12b[match(roms_2$year, H12b$y2),2]
  
  # H13 Alongshore
  H13 = aggregate(H13 ~ y2, data=roms[roms$Month %in% c(1,2,3,4),], FUN=fun[i])        
  roms_2$H13 = H13[match(roms_2$year, H13$y2),2]
  
  # H14 Poleward Undercurrent
  H14 = aggregate(H14 ~ y2, data=roms[roms$Month %in% c(1,2,3,4),], FUN=fun[i])        
  roms_2$H14 = H14[match(roms_2$year, H14$y2),2]
  
  # H15 Degree days
  roms$H15 = roms$H15-temp.ref
  H15 = aggregate(H15 ~ y2, data=roms[roms$Month %in% c(1,2,3,4),], FUN=fun[i])        
  roms_2$H15 = H15[match(roms_2$year, H15$y2),2]
  
  
  # FIRST-FEEDING LARVAE (Feb - May)
  # H16 Zooplankton transported south. No data. Transport same as H18 
  H16 = aggregate(H16 ~ y2, data=roms[roms$Month %in% c(2,3,4,5),], FUN=fun[i])        
  roms_2$H16 = H16[match(roms_2$year, H16$y2),2]     
  
  # H17 MLD 
  H17 = aggregate(H17 ~ y2, data=roms[roms$Month %in% c(2,3,4,5),], FUN=fun[i])        
  roms_2$H17 = H17[match(roms_2$year, H17$y2),2]
  
  # H18 Alongshore 
  H18 = aggregate(H18 ~ y2, data=roms[roms$Month %in% c(2,3,4,5),], FUN=fun[i])        
  roms_2$H18 = H18[match(roms_2$year, H18$y2),2]
  
  # H19a Cross shelf - south (31 - 34.5 °N)
  H19a = aggregate(H19a ~ y2, data=roms[roms$Month %in% c(2,3,4,5),], FUN=fun[i])        
  roms_2$H19a = H19a[match(roms_2$year, H19a$y2),2]
  
  # H19b Cross shelf - north (34.5 - 36 °N)
  H19b = aggregate(H19b ~ y2, data=roms[roms$Month %in% c(2,3,4,5),], FUN=fun[i])        
  roms_2$H19b = H19b[match(roms_2$year, H19b$y2),2]
  
  # H20 Poleward Undercurrent
  H20 = aggregate(H20 ~ y2, data=roms[roms$Month %in% c(2,3,4,5),], FUN=fun[i])        
  roms_2$H20 = H20[match(roms_2$year, H20$y2),2]
  
  # H21a: Upwelling CUTI - south (31 - 34°N)
  H21a  = aggregate(H21a ~ y2, data=roms[roms$Month %in% c(2,3,4,5),], FUN=fun[i])               
  roms_2$H21a = H21a[match(roms_2$year, H21a$y2),2]
  
  # H21b: Upwelling CUTI - north (35 - 36°N)
  H21b  = aggregate(H21b ~ y2, data=roms[roms$Month %in% c(2,3,4,5),], FUN=fun[i])               
  roms_2$H21b = H21b[match(roms_2$year, H21b$y2),2]
  
  # H21c: Upwelling BEUTI - south (31 - 34°N)
  H21c  = aggregate(H21c ~ y2, data=roms[roms$Month %in% c(2,3,4,5),], FUN=fun[i])               
  roms_2$H21c = H21c[match(roms_2$year, H21c$y2),2]
  
  # H21d: Upwelling BEUTI - north (35 - 36°N)
  H21d  = aggregate(H21d ~ y2, data=roms[roms$Month %in% c(2,3,4,5),], FUN=fun[i])               
  roms_2$H21d = H21d[match(roms_2$year, H21d$y2),2]
  
  # H22 Degree days
  roms$H22 = roms$H22-temp.ref
  H22 = aggregate(H22 ~ y2, data=roms[roms$Month %in% c(2:5),], FUN=fun[i])        
  roms_2$H22 = H22[match(roms_2$year, H22$y2),2]
  
  # H23 NA. Prey - CalCOFI Zooplankton. 
  
  
  # LATE LARVAE (Mar - Jun)
  # H24 Zooplankton transported south. No data. Transport same as H26.
  H24 = aggregate(H24 ~ y2, data=roms[roms$Month %in% c(3,4,5,6),], FUN=fun[i])        
  roms_2$H24 = H24[match(roms_2$year, H24$y2),2]
  
  # H25 MLD 
  H25 = aggregate(H25 ~ y2, data=roms[roms$Month %in% c(3,4,5,6),], FUN=fun[i])        
  roms_2$H25 = H25[match(roms_2$year, H25$y2),2]
  
  # H26 Alongshore 
  H26 = aggregate(H26 ~ y2, data=roms[roms$Month %in% c(3,4,5,6),], FUN=fun[i])        
  roms_2$H26 = H26[match(roms_2$year, H26$y2),2]
  
  # H27a Cross shelf south (31 - 34.5 °N)
  H27a = aggregate(H27a ~ y2, data=roms[roms$Month %in% c(3,4,5,6),], FUN=fun[i])        
  roms_2$H27a = H27a[match(roms_2$year, H27a$y2),2]
  
  # H27b Cross shelf north (34.5 - 37 °N)
  H27b = aggregate(H27b ~ y2, data=roms[roms$Month %in% c(3,4,5,6),], FUN=fun[i])        
  roms_2$H27b = H27b[match(roms_2$year, H27b$y2),2]
  
  # H28 Poleward Undercurrent
  H28 = aggregate(H28 ~ y2, data=roms[roms$Month %in% c(3,4,5,6),], FUN=fun[i])        
  roms_2$H28 = H28[match(roms_2$year, H28$y2),2]
  
  # H29a: Upwelling CUTI - south (31 - 34 °N)
  H29a  = aggregate(H29a ~ y2, data=roms[roms$Month %in% c(3,4,5,6),], FUN=fun[i])               
  roms_2$H29a = H29a[match(roms_2$year, H29a$y2),2]
  
  # H29b: Upwelling CUTI - north (35 - 37 °N)
  H29b  = aggregate(H29b ~ y2, data=roms[roms$Month %in% c(3,4,5,6),], FUN=fun[i])               
  roms_2$H29b = H29b[match(roms_2$year, H29b$y2),2]
  
  # H29c: Upwelling BEUTI - south (31 - 34 °N)
  H29c  = aggregate(H29c ~ y2, data=roms[roms$Month %in% c(3,4,5,6),], FUN=fun[i])               
  roms_2$H29c = H29c[match(roms_2$year, H29c$y2),2]
  
  # H29d: Upwelling BEUTI - north (35 - 37 °N)
  H29d  = aggregate(H29d ~ y2, data=roms[roms$Month %in% c(3,4,5,6),], FUN=fun[i])               
  roms_2$H29d = H29d[match(roms_2$year, H29d$y2),2]
  
  # H30 Degree days
  roms$H30 = roms$H30-temp.ref
  H30 = aggregate(H30 ~ y2, data=roms[roms$Month %in% c(3:6),], FUN=fun[i])        
  roms_2$H30 = H30[match(roms_2$year, H30$y2),2]
  
  # H31 NA. Spring Transition. Data from CBR.
  
  # H32 NA. Prey - CalCOFI Zooplankton. 
  
  
  # PELAGIC JUVENILES - AGE-0 (May - Sep)
  # H33 Alongshore 
  H33 = aggregate(H33 ~ y2, data=roms[roms$Month %in% c(5:9),], FUN=fun[i])        
  roms_2$H33 = H33[match(roms_2$year, H33$y2),2]
  
  # H34a Cross shelf south (31 - 34.5 °N)
  H34a = aggregate(H34a ~ y2, data=roms[roms$Month %in% c(5:9),], FUN=fun[i])        
  roms_2$H34a = H34a[match(roms_2$year, H34a$y2),2]
  
  # H34b Cross shelf north (34.5 - 38 °N)
  H34b = aggregate(H34b ~ y2, data=roms[roms$Month %in% c(5:9),], FUN=fun[i])        
  roms_2$H34b = H34b[match(roms_2$year, H34b$y2),2]
  
  # H35a: Upwelling CUTI - south (31 - 34 °N)
  H35a  = aggregate(H35a ~ y2, data=roms[roms$Month %in% c(5:9),], FUN=fun[i])               
  roms_2$H35a = H35a[match(roms_2$year, H35a$y2),2]
  
  # H35b: Upwelling CUTI - north (35 - 38 °N)
  H35b  = aggregate(H35b ~ y2, data=roms[roms$Month %in% c(5:9),], FUN=fun[i])               
  roms_2$H35b = H35b[match(roms_2$year, H35b$y2),2]
  
  # H35c: Upwelling BEUTI - south (31 - 34 °N)
  H35c  = aggregate(H35c ~ y2, data=roms[roms$Month %in% c(5:9),], FUN=fun[i])               
  roms_2$H35c = H35c[match(roms_2$year, H35c$y2),2]
  
  # H35d: Upwelling BEUTI - north (35 - 38 °N)
  H35d  = aggregate(H35d ~ y2, data=roms[roms$Month %in% c(5:9),], FUN=fun[i])               
  roms_2$H35d = H35d[match(roms_2$year, H35d$y2),2]
  
  
  # H36 Degree days
  roms$H36 = roms$H36-temp.ref 
  H36 = aggregate(H36 ~ y2, data=roms[roms$Month %in% c(5:9),], FUN=fun[i])        
  roms_2$H36 = H36[match(roms_2$year, H36$y2),2]
  
  # H37 NA. Spring Transition. Data from CBR.
  
  # H38 NA. Prey - CalCOFI Zooplankton.
  
  # H39a NA. Predation - California Sea Lion pup counts.
  
  # H39b NA. Predation - ATF biomass estimate from SA.
  
  # H39c NA. Predation - Age-1 Pacific hake biomass estimate from SA.
  
  
  # AGE-1 (Apr - Sep) - Hypotheses H40-H42 removed from Analysis
  # H40 Alongshore 
  #H40 = aggregate(H40 ~ y2, data=roms[roms$Month %in% c(4:9),], FUN=fun[i])        
  #roms_2$H40 = H40[match(roms_2$year, H40$y2),2]
  
  # H41a: Upwelling CUTI - south (33 - 34 °N)
  #H41a  = aggregate(H41a ~ y2, data=roms[roms$Month %in% c(4:9),], FUN=fun[i])               
  #roms_2$H41a = H41a[match(roms_2$year, H41a$y2),2]
  
  # H41b: Upwelling CUTI - central (35 - 42 °N)
  #H41b  = aggregate(H41b ~ y2, data=roms[roms$Month %in% c(4:9),], FUN=fun[i])               
  #roms_2$H41b = H41b[match(roms_2$year, H41b$y2),2]
  
  # H41c: Upwelling CUTI - north (43 - 44 °N) 
  #H41c  = aggregate(H41c ~ y2, data=roms[roms$Month %in% c(4:9),], FUN=fun[i])               
  #roms_2$H41c = H41c[match(roms_2$year, H41c$y2),2]
  
  # H41d: Upwelling BEUTI - south (33 - 34 °N)
  #H41d  = aggregate(H41d ~ y2, data=roms[roms$Month %in% c(4:9),], FUN=fun[i])               
  #roms_2$H41d = H41d[match(roms_2$year, H41d$y2),2]
  
  # H41e: Upwelling BEUTI - central (35 - 42 °N)
  #H41e  = aggregate(H41e ~ y2, data=roms[roms$Month %in% c(4:9),], FUN=fun[i])               
  #roms_2$H41e = H41e[match(roms_2$year, H41e$y2),2]
  
  # H41f: Upwelling BEUTI - north (43 - 44 °N) 
  #H41f  = aggregate(H41f ~ y2, data=roms[roms$Month %in% c(4:9),], FUN=fun[i])               
  #roms_2$H41f = H41f[match(roms_2$year, H41f$y2),2]
  
  # H42 Degree days
  #roms$H42 = roms$H42-temp.ref 
  #H42 = aggregate(H42 ~ y2, data=roms[roms$Month %in% c(4:9),], FUN=fun[i])        
  #roms_2$H42 = H42[match(roms_2$year, H42$y2),2]
  
  # H43 NA. Spring Transition. Data from CBR.
  
  # H44 NA. Prey - CalCOFI Euphausiids.
  
  # H45a NA. Predation - California Sea Lion pup counts.
  
  # H45b NA. Predation - ATF biomass estimate from SA.
  
  # Basin-scale and Productivity Indices
  # H46a SSH - south (31 - 34.5 °N) January - Apr
  H46a  = aggregate(H46a ~ y2, data=roms[roms$Month %in% c(1:4),], FUN=fun[i])               
  roms_2$H46a = H46a[match(roms_2$year, H46a$y2),2]
  
  # H46b: SSH - central (34.5 - 42.5 °N) January - Apr
  H46b  = aggregate(H46b ~ y2, data=roms[roms$Month %in% c(1:4),], FUN=fun[i])               
  roms_2$H46b = H46b[match(roms_2$year, H46b$y2),2]
  
  # H46c: SSH - north (42.5 - 47 °N) January - Apr
  H46c  = aggregate(H46c ~ y2, data=roms[roms$Month %in% c(1:4),], FUN=fun[i])               
  roms_2$H46c = H46c[match(roms_2$year, H46c$y2),2]
  
  # H46d SSH - south (31 - 34.5 °N) May - Sep
  H46d  = aggregate(H46d ~ y2, data=roms[roms$Month %in% c(5:9),], FUN=fun[i])               
  roms_2$H46d = H46d[match(roms_2$year, H46d$y2),2]
  
  # H46e: SSH - central (34.5 - 42.5 °N) May - Sep
  H46e  = aggregate(H46e ~ y2, data=roms[roms$Month %in% c(5:9),], FUN=fun[i])               
  roms_2$H46e = H46e[match(roms_2$year, H46e$y2),2]
  
  # H46f: SSH - north (42.5 - 47 °N) May - Sep
  H46f  = aggregate(H46f ~ y2, data=roms[roms$Month %in% c(5:9),], FUN=fun[i])               
  roms_2$H46f = H46f[match(roms_2$year, H46f$y2),2]
  
  # H47a EKE - south (31 - 34.5 °N) January - Apr
  H47a  = aggregate(H47a ~ y2, data=roms[roms$Month %in% c(1:4),], FUN=fun[i])               
  roms_2$H47a = H47a[match(roms_2$year, H47a$y2),2]
  
  # H47b: EKE - central (34.5 - 42.5 °N) January - Apr
  H47b  = aggregate(H47b ~ y2, data=roms[roms$Month %in% c(1:4),], FUN=fun[i])               
  roms_2$H47b = H47b[match(roms_2$year, H47b$y2),2]
  
  # H47c: EKE - north (42.5 - 47 °N) January - Apr
  H47c  = aggregate(H47c ~ y2, data=roms[roms$Month %in% c(1:4),], FUN=fun[i])               
  roms_2$H47c = H47c[match(roms_2$year, H47c$y2),2]
  
  # H47d EKE - south (31 - 34.5 °N) May - Sep 
  H47d  = aggregate(H47d ~ y2, data=roms[roms$Month %in% c(5:9),], FUN=fun[i])               
  roms_2$H47d = H47d[match(roms_2$year, H47d$y2),2]
  
  # H47e: EKE - central (34.5 - 42.5 °N) May - Sep
  H47e  = aggregate(H47e ~ y2, data=roms[roms$Month %in% c(5:9),], FUN=fun[i])               
  roms_2$H47e = H47e[match(roms_2$year, H47e$y2),2]
  
  # H47f: EKE - north (42.5 - 47 °N) May - Sep
  H47f  = aggregate(H47f ~ y2, data=roms[roms$Month %in% c(5:9),], FUN=fun[i])               
  roms_2$H47f = H47f[match(roms_2$year, H47f$y2),2]
  
  # H48 NA. Bifurcation index (Malick)
  
  # H49 NA. NPGO (Di Lorenzo)
  
  # H50 NA. Storm Index (Turley & Rykaczewski, 2019)
  
  # H51 NA. Calm Index (Turley & Rykaczewski, 2019)
  
  write.table(roms_2, paste('Data_ROMS_',fun[i],'_hake.csv',sep=''), sep=',', col.names = TRUE, row.names = FALSE)
  
} # end i ####


#### analysis using means for tranport mech ####
df = data.frame(read.table("Data_ROMS_mean_hake.csv",header=TRUE, sep=','))
temps = data.frame(read.table("Data_ROMS_sum_hake.csv",header=TRUE, sep=','))

# bring in summed degree days into main file ####
# Replace temp means with degree days for some values.
df$H2b = temps$H2b
df$H5 = temps$H5
df$H10a = temps$H10a
df$H10b = temps$H10b
df$H15 = temps$H15
df$H22 = temps$H22
df$H30 = temps$H30
df$H36 = temps$H36



vnam=data.frame(matrix(c(
  "H1", NA,
  "H2a", "TEMPpre",
  "H2b", "DDpre",
  "H3a", "UWpre.a",
  "H3b", "UWpre.b",
  "H4", "TEMPspawn",
  "H5", "DDspawn",
  "H6", "MLDegg",
  "H7a", "CSTegg.s.s",
  "H7b", "CSTegg.s.d",
  "H7c", "CSTegg.n.s",
  "H7d", "CSTegg.n.d",
  "H8a", "ASTegg.s",
  "H8b", "ASTegg.d",
  "H9", "PUegg",
  "H10a", "DDegg.s",
  "H10b", "DDegg.d",
  "H11", "MLDyolk",
  "H12a", "CSTyolk.s",
  "H12b", "CSTyolk.n",
  "H13", "ASTyolk",
  "H14", "PUyolk",
  "H15", "DDyolk",
  "H16", "ASTlarv.zp",
  "H17", "MLDlarv",
  "H18", "ASTlarv",
  "H19a", "CSTlarv.s",
  "H19b", "CSTlarv.n",
  "H20", "PUlarv",
  "H21a", "UWlarv.cs",
  "H21b", "UWlarv.cn",
  "H21c", "UWlarv.bs",
  "H21d", "UWlarv.bn",
  "H22","DDlarv",
  "H23", NA,
  "H24","ASTlatelarv.zp",
  "H25","MLDlatelarv",
  "H26","ASTlatelarv",
  "H27a","CSTlatelarv.s",
  "H27b","CSTlatelarv.n",
  "H28","PUlatelarv",
  "H29a","UWlatelarv.cs",
  "H29b","UWlatelarv.cn",
  "H29c","UWlatelarv.bs",
  "H29d","UWlatelarv.bn",
  "H30","DDlatelarv",
  "H31", NA,
  "H32", NA,
  "H33","ASTpjuv.age.0",
  "H34a","CSTpjuv.age.0.s",
  "H34b","CSTpjuv.age.0.n",
  "H35a","UWpjuv.age.0.cs",
  "H35b","UWpjuv.age.0.cn",
  "H35c","UWpjuv.age.0.bs",
  "H35d","UWpjuv.age.0.bn",
  "H36","DDpjuv.age.0",
  "H37", NA,
  "H38", NA,
  "H39a", NA,
  "H39b", NA,
  "H39c", NA,
  "H43", NA,
  "H44", NA,
  "H45a", NA,
  "H45b", NA,
  "H46a","SSH.ja.s",
  "H46b","SSH.ja.c",
  "H46c","SSH.ja.n",
  "H46d","SSH.ms.s",
  "H46e","SSH.ms.c",
  "H46f","SSH.ms.n",
  "H47a","EKE.ja.s",
  "H47b","EKE.ja.c",
  "H47c","EKE.ja.n",
  "H47d","EKE.ms.s",
  "H47e","EKE.ms.c",
  "H47f","EKE.ms.n",
  "H48", NA,
  "H49", NA,
  "H50", NA,
  "H51", NA), byrow = TRUE, ncol=2)
)
colnames(vnam) =c('cn','nam')

cn = data.frame(colnames(df)[-1])
colnames(cn) = "cn"
cn$cnam = vnam$nam[match(cn$cn,vnam$cn)]
cnames = c('year',as.character(cn$cnam))
colnames(df) <- cnames

write.table(df,"Data_ROMS.for.hake.analysis.mean.csv", col.names=TRUE, row.names=FALSE, sep=',')


PPC = data.frame(read.table(paste0(MainFile,'/00_Data/Data_Predator_Prey_Climate.for.hake.analysis.csv'),sep=',',header=TRUE))

# merge ROMS and Predator/Prey/Climate indices
ROMS_PPC <- cbind(df,PPC[2:27])

write.table(ROMS_PPC,"Data_ROMS.PPC.for.hake.analysis.mean.master.csv", col.names=TRUE, row.names=FALSE, sep=',')

setwd(MainFile)
