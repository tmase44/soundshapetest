# PACKAGES----
library(SoundShape)
library(tuneR)
library(seewave)
library(ggplot2)

# https://cran.r-project.org/web/packages/seewave/seewave.pdf
# https://github.com/p-rocha/SoundShape 

# STORE RESULTS----
wav.at <- file.path("/Users/Tom_Macbook/soundshapetest", "HBWAV")
if(!dir.exists(wav.at)) dir.create(wav.at)

store.at <- file.path("/Users/Tom_Macbook/soundshapetest", "HBOUT")
if(!dir.exists(store.at)) dir.create(store.at)

# WAV IMPORT----

bhb <- readWave("/Users/Tom_Macbook/soundshapetest/Audio/BlackHornbill.wav")
ophb <- readWave("/Users/Tom_Macbook/soundshapetest/Audio/OPHornbill.wav")

seewave::oscillo(bhb,identify=TRUE,title = "Bhb call")
bhbc1 <- seewave::cutw(blackhb, f=44100, from=0.0426307, to=0.7144498, output = "Wave")
bhbc2 <- seewave::cutw(blackhb, f=44100, from=1.7430969, to=2.4220363, output = "Wave")
bhbc3 <- seewave::cutw(blackhb, f=44100, from=0.399096, to=1.177945, output = "Wave")
bhbc4 <- seewave::cutw(blackhb, f=44100, from=2.065730, to=2.676800, output = "Wave")


seewave::oscillo(ophb,identify=TRUE,title = "OPhb call")
oph1 <- seewave::cutw(blackhb, f=44100, from=0.05833367, to=0.39898145, output = "Wave")
oph2 <- seewave::cutw(blackhb, f=44100, from=0.62510774, to=1.07042278, output = "Wave")
oph3 <- seewave::cutw(blackhb, f=44100, from=1.09361041, to=1.66061365, output = "Wave")
oph4 <- seewave::cutw(blackhb, f=44100, from=2.24174197, to=2.6, output = "Wave")

# save cuts---- 
writeWave(bhbc1, filename = file.path(wav.at, "bhbc1.wav"), extensible = FALSE)
writeWave(bhbc2, filename = file.path(wav.at, "bhbc2.wav"), extensible = FALSE)
writeWave(bhbc3, filename = file.path(wav.at, "bhbc3.wav"), extensible = FALSE)
writeWave(bhbc4, filename = file.path(wav.at, "bhbc4.wav"), extensible = FALSE)

writeWave(oph1, filename = file.path(wav.at, "oph1.wav"), extensible = FALSE)
writeWave(oph2, filename = file.path(wav.at, "oph2.wav"), extensible = FALSE)
writeWave(oph3, filename = file.path(wav.at, "oph3.wav"), extensible = FALSE)
writeWave(oph4, filename = file.path(wav.at, "oph4.wav"), extensible = FALSE)


# ALIGNMENT----
align.wave(wav.at=wav.at, wav.to="Alig", time.length = 1, time.perc = 0.001, dBlevel = 25) 


eigensound(analysis.type = "twoDshape", wav.at = file.path(wav.at, "Alig"),
           store.at=store.at, plot.exp=TRUE, flim=c(0, 8), tlim=c(0, 0.3))
store.at

# too small for alignment lets try without

par(mfrow=c(1,2), mar=c(1,2,1,0)) 
# As "surface"
threeDspectro(bhbc4, samp.grid=TRUE, x.length=70, y.length=47, plot.type="surface", 
              dBlevel=25, flim=c(0, 6), tlim=c(0, 0.3), f=48000, wl=512, ovlp=70, main="As 'surface'",
              colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8)
# As "points"
threeDspectro(bhbc4, samp.grid=TRUE, x.length=70, y.length=47, plot.type="points", 
              dBlevel=25, flim=c(0, 6), tlim=c(0, 0.3), f=48000, wl=512, ovlp=70, main="As 'points'",
              colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8)


eig.sample <- eigensound(analysis.type="threeDshape", dBlevel=25, 
                           f=44100, wl=512, ovlp=70, flim=c(0, 6), tlim=c(0, 0.3),
                           x.length=70, y.length=47, log.scale=TRUE, plot.exp=TRUE, plot.type="points",
                           wav.at=file.path(wav.at), store.at=store.at)
# Go to folder specified by store.at and check jpeg files created
eig.sample
wav.at
pca.eig.sample <- stats::prcomp(geomorph::two.d.array(eig.sample))
summary(pca.eig.sample)

# Mean shape configuration (consensus)
hypo.surf(eig.sample,  PC="mean", flim=c(0, 6), tlim=c(0, 0.3), x.length=70, y.length=47,
          cex.lab=0.7, cex.axis=0.5, cex.main=1)
# Minimum and maximum deformations - Principal Component 1
hypo.surf(eig.sample, PC=1, flim=c(0, 6), tlim=c(0, 0.3), x.length=70, y.length=47,
          cex.lab=0.7, cex.axis=0.5, cex.main=1)
# PC4
hypo.surf(eig.sample, PC=4, flim=c(0, 6), tlim=c(0, 0.3), x.length=70, y.length=47,
          cex.lab=0.7, cex.axis=0.5, cex.main=1)

#PLOT-------
# PCA using semilandmark coordinates
pca.eig.sample <- stats::prcomp(geomorph::two.d.array(eig.sample))

# Verify names of acoustic units from sample
eig.sample
dimnames(eig.sample)[[3]]
# [1] "bhbc1" "bhbc2" "bhbc3" "bhbc4" "oph1"  "oph2"  "oph3"  "oph4" 

# Based on those names, create factor to use as groups in subsequent ordination plot
sample.grx <- factor(c(rep("Black Hornbill", 4), rep("Oriental-Pied hornbill", 4)))
sample.grx2 <- factor(c(rep("bhbc1",1), rep("bhbc2",1),rep("bhbc3",1),rep("bhbc4",1),
                        rep("oph1",1),rep("oph2",1),rep("oph3",1),rep("oph4",1)))


# Ordination plot
pca.plot(pca.eig.sample, 
         groups=sample.grx2, 
         conv.hulls=sample.grx2, 
         leg.pos="bottomright", cex.leg=0.8, cex=1.2)

# Ordination plot
pca.plot(pca.eig.sample, 
         groups=sample.grx, 
         conv.hulls=sample.grx, 
         leg.pos="topright", cex=1.2)
         
par(mfrow=c(2,2), mar=c(4,4,2,2))
seewave::spectro(bhbc1,flim = c(0, 6), wl=512, f=44100, ovlp=70, grid=FALSE,scale=FALSE,main = "Black Hornbill Spectro")
seewave::spectro(bhbc2,flim = c(0, 6), wl=512, f=44100, ovlp=70, grid=FALSE,scale=FALSE,main = "Black Hornbill Spectro")
seewave::spectro(bhbc3,flim = c(0, 6), wl=512, f=44100, ovlp=70, grid=FALSE,scale=FALSE,main = "Black Hornbill Spectro")
seewave::spectro(bhbc4,flim = c(0, 6), wl=512, f=44100, ovlp=70, grid=FALSE,scale=FALSE,main = "Black Hornbill Spectro")

par(mfrow=c(2,2), mar=c(4,4,2,2))
seewave::spectro(oph1,flim = c(0, 6), wl=512, f=44100, ovlp=70, grid=FALSE,scale=FALSE,main = "Oriental-Pied Hornbill Spectro")
seewave::spectro(oph2,flim = c(0, 6), wl=512, f=44100, ovlp=70, grid=FALSE,scale=FALSE,main = "Oriental-Pied Hornbill Spectro")
seewave::spectro(oph3,flim = c(0, 6), wl=512, f=44100, ovlp=70, grid=FALSE,scale=FALSE,main = "Oriental-Pied Hornbill Spectro")
seewave::spectro(oph4,flim = c(0, 6), wl=512, f=44100, ovlp=70, grid=FALSE,scale=FALSE,main = "Oriental-Pied Hornbill Spectro")

