#######################################################################
# TITLE: NOCTURNAL URBAN TURBULENCE DATA ANALYSES
#
# AUTHOR: YUSRI YUSUP, PHD
# DATE: 2015-11-03
# 
# NOTE 1: "norm_sigU", etc. are normalised by ustar
# NOTE 2: Use "z_L2" as the final stability parameter after correction
# following recommendations by Schotanus et al. where w'T' multiplied
# by 0.10
# NOTE 3: Need to multiply wT with 0.10 for humidity correction 
# (Schotanus et al.)
#######################################################################

require(ggm)

#### 1. Load the data #################################################
secA <- read.csv('data/sectorA.csv',header=TRUE)
secD <- read.csv('data/sectorD.csv',header=TRUE)

#### 2. Data processing ###############################################
# Add a column of "eff_height"
# Height of the RM Young 81000 sonic anemometer from the roof in [m].
secA["eff_height"] <- 4.5
secA["meas_height"] <- 2.68

secD["eff_height"] <- 4.5
secD["meas_height"] <- 2.68

# Delete wrong columns of values of z/L
secA <- secA[,c(-12,-13)]
secD <- secD[,c(-10,-11)]
# Rename some columns for secD to be consistent with secA
names(secD)[17] <- "L2"

# Recalculate wT and add to dataframe
wTA <- (-1) * secA$ustar^3 * secA$Tavg/(9.80 * 0.4 * secA$L2)
wTD <- (-1) * secD$ustar^3 * secD$Tavg/(9.80 * 0.4 * secD$L2)

secA["wT"] <- wTA
secD["wT"] <- wTD
# Recalculate Tstar and add to dataframe
TstarA <- -1 * secA$wT / secA$ustar
TstarD <- -1 * secD$wT / secD$ustar

secA["Tstar"] <- TstarA
secD["Tstar"] <- TstarD
 
# Calculate the free convective scale for velocity, ufl.
# wT has to be absolute to be applicable under stable conditions
secA["ufl"] <- ((9.80 / secA$Tavg) * abs(secA$wT) * secA$eff_height)^(0.333)
secD["ufl"] <- ((9.80 / secD$Tavg) * abs(secD$wT) * secD$eff_height)^(0.333)

# Calculate the free convective scale for temperature, tfl.
secA["tfl"] <- abs(secA$wT)^(0.667) * 
  ((9.80/secA$Tavg) * (secA$eff_height))^(-0.333)
secD["tfl"] <- abs(secD$wT)^(0.667) * 
  ((9.80/secD$Tavg) * (secD$eff_height))^(-0.333)

# Normalized sigW with ufl and add to dataframe
norm2_SigWA <- secA$SigW/secA$ufl
norm2_SigWD <- secD$SigW/secD$ufl

secA["norm2_SigW"] <- norm2_SigWA
secD["norm2_SigW"] <- norm2_SigWD

rm(wTA,wTD,TstarA,TstarD,norm2_SigWA,norm2_SigWD)

#### 3. Analyses #########################################

# Separate the data into stable
sigWAstable <- secA$SigW[which(secA$z_L2>0)]
uflAstable <- secA$ufl[which(secA$z_L2>0)]
ustarAstable <- secA$ustar[which(secA$z_L2>0)]
secAstable <- data.frame(sigWAstable,uflAstable,ustarAstable)

sigWDstable <- secD$SigW[which(secD$z_L2>0)]
uflDstable <- secD$ufl[which(secD$z_L2>0)]
ustarDstable <- secD$ustar[which(secD$z_L2>0)]
secDstable <- data.frame(sigWDstable,uflDstable,ustarDstable)

rm(sigWAstable,uflAstable,ustarAstable,sigWDstable,uflDstable,ustarDstable)

# Separate the data into unstable
sigWAunstable <- secA$SigW[which(secA$z_L2<0)]

norm_sigWAunstable <- secA$norm_sigW[which(secA$z_L2<0)]
norm2_sigWAunstable <- secA$norm2_SigW[which(secA$z_L2<0)]
Az_Lunstable <- secA$z_L2[which(secA$z_L2<0)]

uflAunstable <- secA$ufl[which(secA$z_L2<0)]
ustarAunstable <- secA$ustar[which(secA$z_L2<0)]
secAunstable <- data.frame(sigWAunstable,uflAunstable,ustarAunstable,
                           norm_sigWAunstable,norm2_sigWAunstable,Az_Lunstable)

sigWDunstable <- secD$SigW[which(secD$z_L2<0)]

norm_sigWDunstable <- secD$norm_sigW[which(secD$z_L2<0)]
norm2_sigWDunstable <- secD$norm2_SigW[which(secD$z_L2<0)]
Dz_Lunstable <- secD$z_L2[which(secD$z_L2<0)]

uflDunstable <- secD$ufl[which(secD$z_L2<0)]
ustarDunstable <- secD$ustar[which(secD$z_L2<0)]
secDunstable <- data.frame(sigWDunstable,uflDunstable,ustarDunstable,
                           norm_sigWDunstable,norm2_sigWDunstable,Dz_Lunstable)

rm(sigWAunstable,uflAunstable,ustarAunstable,sigWDunstable,uflDunstable,
   ustarDunstable,sigWAunstable,uflAunstable,ustarAunstable,
   norm_sigWAunstable,norm2_sigWAunstable,Az_Lunstable,sigWDunstable,
   uflDunstable,ustarDunstable,norm_sigWDunstable,norm2_sigWDunstable,Dz_Lunstable)

# Partial correlation coefficient analysis

# Stable and for sector A-B
pcor(c('sigWAstable','ustarAstable'),var(secAstable))
pcor(c('sigWAstable','uflAstable'),var(secAstable))
# Holding ufl constant
pcor(c('sigWAstable','ustarAstable','uflAstable'),var(secAstable))
# Holding ustar constant
pcor(c('sigWAstable','uflAstable','ustarAstable'),var(secAstable))

# Stable and for sector D
pcor(c('sigWDstable','ustarDstable'),var(secDstable))
pcor(c('sigWDstable','uflDstable'),var(secDstable))
# Holding ufl constant
pcor(c('sigWDstable','ustarDstable','uflDstable'),var(secDstable))
# Holding ustar constant
pcor(c('sigWDstable','uflDstable','ustarDstable'),var(secDstable))

pl
# Unstable and for sector A-B
pcor(c('sigWAunstable','ustarAunstable'),var(secAunstable))
pcor(c('sigWAunstable','uflAunstable'),var(secAunstable))
# Holding ufl constant
pcor(c('sigWAunstable','ustarAunstable','uflAunstable'),var(secAunstable))
# Holding ustar constant
pcor(c('sigWAunstable','uflAunstable','ustarAunstable'),var(secAunstable))

# Unstable and for sector D
pcor(c('sigWDunstable','ustarDunstable'),var(secDunstable))
pcor(c('sigWDunstable','uflDunstable'),var(secDunstable))
# Holding ufl constant
pcor(c('sigWDunstable','ustarDunstable','uflDunstable'),var(secDunstable))
# Holding ustar constant
pcor(c('sigWDunstable','uflDunstable','ustarDunstable'),var(secDunstable))

# Randomising the rows to check for spurious correlation due to shared variable
# Only focus on SigW and SigT

