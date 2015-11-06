# Sector A
Az_LstableRand <- sample(Az_Lstable)
Az_LunstableRand <- sample(Az_Lunstable)

randAstable <- data.frame(secAstable$norm_sigWAstable,secAstable$norm2_sigWAstable,
                          secAstable$norm_sigTAstable,Az_LstableRand)

randAunstable <- data.frame(secAunstable$norm_sigWAunstable,secAunstable$norm2_sigWAunstable,
                            secAunstable$norm_sigTAunstable,Az_LunstableRand)

# Sector D
Dz_LstableRand <- sample(Dz_Lstable)
Dz_LunstableRand <- sample(Dz_Lunstable)


randDstable <- data.frame(secDstable$norm_sigWDstable,secDstable$norm2_sigWDstable,
                          secDstable$norm_sigTDstable,Dz_LstableRand)

randDunstable <- data.frame(secDunstable$norm_sigWDunstable,secDunstable$norm2_sigWDunstable,
                            secDunstable$norm_sigTDunstable,Dz_LunstableRand)

# Output the data to be imported into Matlab to curve fit using cftools
write.table(randAstable,file='data/randAstable.csv')
write.table(randAunstable,file='data/randAunstable.csv')
write.table(randDstable,file='data/randDstable.csv')
write.table(randDunstable,file='data/randDunstable.csv')

# Some initial plots
# Sector A
plot(randAstable$Az_LstableRand,randAstable$secAstable.norm_sigWAstable,pch=19)
points(secAstable$Az_Lstable,secAstable$norm_sigWAstable,pch=19,col='blue')

plot(randAunstable$Az_LunstableRand,randAunstable$secAunstable.norm_sigWAunstable,
     pch=19)
points(secAunstable$Az_Lunstable,secAunstable$norm_sigWAunstable,pch=19,col='blue')

# Sector D
plot(randDstable$Dz_LstableRand,randDstable$secDstable.norm_sigWDstable,pch=19)
points(secDstable$Dz_Lstable,secDstable$norm_sigWDstable,pch=19,col='blue')

plot(randDunstable$Dz_LunstableRand,randDunstable$secDunstable.norm_sigWDunstable,
     pch=19)
points(secDunstable$Dz_Lunstable,secDunstable$norm_sigWDunstable,pch=19,col='blue')
