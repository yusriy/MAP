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
 
# Calculate the free convective scale, ufl.
secA["ufl"] <- ((9.80 / secA$Tavg) * secA$wT * secA$eff_height)^(0.333)
secD["ufl"] <- ((9.80 / secD$Tavg) * secD$wT * secD$eff_height)^(0.333)

# Normalized sigW with ufl and add to dataframe
norm2_wTA <- secA$SigW/secA$ufl
norm2_wTD <- secD$SigW/secD$ufl

secA["norm2_wT"] <- norm2_wTA
secD["norm2_wT"] <- norm2_wTD

rm(wTA,wTD,TstarA,TstarD,norm2_wTA,norm2_wTD)


