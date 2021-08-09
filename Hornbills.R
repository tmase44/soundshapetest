# PACKAGES----
library(SoundShape)
library(tuneR)
library(seewave)

# https://cran.r-project.org/web/packages/seewave/seewave.pdf
# https://github.com/p-rocha/SoundShape 

# STORE RESULTS----
wav.at <- file.path("/Users/Tom_Macbook/soundshapetest/Audio", "original wave")
if(!dir.exists(wav.at)) dir.create(wav.at)

store.at <- file.path("/Users/Tom_Macbook/soundshapetest/Audio", "output")
if(!dir.exists(store.at)) dir.create(store.at)

# WAV IMPORT----

# The function to import .wav files from the hard-disk is readWave:
# > s6<-readWave("mysong.wav")

blackhb <- readWave("/Users/Tom_Macbook/soundshapetest/Audio/BlackHornbill.wav")
orientalhb <- readWave("/Users/Tom_Macbook/soundshapetest/Audio/OPHornbill.wav")

# INSPECT----
# identify=TRUE helps me pick out the timestamps 
seewave::oscillo(blackhb,identify=TRUE,title = "Black Hornbill call")
  #            time amp
  # [1,] 0.09954723 128
  # [2,] 0.98569898 120
seewave::oscillo(orientalhb,identify=TRUE,title = "Oriental Pied Hornbill call")
  #           time amp
  # [1,] 0.6281078 -38
  # [2,] 1.5273212 -24

# TRIM----
cut.blackhb <- seewave::cutw(blackhb, f=44100, from=0.099, to=0.99, output = "Wave")
cut.ophb <- seewave::cutw(orientalhb, f=48000, from=0.62, to=1.53, output = "Wave")

# save cuts---- 
writeWave(cut.blackhb, filename = file.path(wav.at, "cut.blackhb.wav"), extensible = FALSE)
writeWave(cut.ophb, filename = file.path(wav.at, "cut.ophb.wav"), extensible = FALSE)


# Spectograms----
seewave::spectro(cut.blackhb, flim = c(0, 8), wl=512, f=44100, ovlp=70, grid=FALSE,main = "Black Hornbill Spectro")
graphics::abline(v=c(0.27, 0.65), lty=2)

seewave::spectro(cut.ophb, flim = c(0, 8), wl=512, f=44100, ovlp=70, grid=FALSE,main = "Oriental-Pied Hornbill Spectro")
graphics::abline(v=c(0.19, 0.3, 0.49,0.79), lty=2)


# STR
str(cut.blackhb2)
str(cut.ophb2)

# ALIGNMENT----
align.wave(wav.at=wav.at, wav.to="Aligned", time.length = 1, time.perc = 0.001) 

eigensound(analysis.type = "twoDshape", wav.at = file.path(wav.at, "Aligned"),
           store.at=store.at, plot.exp=TRUE, flim=c(0, 8), tlim=c(0, 1))
store.at

# soundwindow dimensions----
  # flim/tlim might take some trial and error
par(mfrow=c(1,2), mar=c(4,4,2,2))
seewave::spectro(cut.blackhb, flim=c(0, 8), tlim=c(0, 0.88), main="data(Black hornbill)",
                 wl=512, f=44100, ovlp=70, grid=FALSE, scale=FALSE)
seewave::spectro(cut.ophb, flim=c(0, 8), tlim=c(0, 0.88), main="data(Oriental-pied hornbill)",
                 wl=512, f=48000, ovlp=70, grid=FALSE, scale=FALSE)


# Set relative amplitude background----

# * CUT.BLACKHB----

# 2D spectrogram with curves of relative amplitude at -25 dB
par(mfrow=c(1,2), mar=c(4,4,1,1))
s.bhb <- seewave::spectro(cut.blackhb, flim=c(0, 8), tlim = c(0, 0.88), main = "Black Hornbill spectro",  
                          grid=F, scale=F, f=44100, wl=512, ovlp=70, cont=TRUE, 
                          contlevels = seq(-25, -25, 1), collevels = seq(-40, 0, 0.1))
#> This took quite a lot of time to display this graphic, you may set 'fastdisp=TRUE' for a faster, but less accurate, display

# 3D spectrogram (with a lower dBlevel for illustrative purpuses)
threeDspectro(cut.blackhb, dBlevel=40, flim=c(0, 8), tlim=c(0, 0.88), main="Black Hornbill 3D spectro",
              colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8, resfac=2)

# Set background at -40 dB and remove -Inf values from spectrogram data 
for(i in 1:length(s.bhb$amp)){if(s.bhb$amp[i] == -Inf |s.bhb$amp[i] <= -40)
{s.bhb$amp[i] <- -40}}

# Add curve of relative amplitude
plot3D::contour3D(x=s.bhb$time, y=s.bhb$freq, colvar=t(s.bhb$amp), z=-25,
                  plot=T, add=T, addbox=F, col="black", lwd=1.9, nlevels=2, dDepth=0.25)


# * CUT.OPHB----

# 2D spectrogram with curves of relative amplitude at -25 dB
par(mfrow=c(1,2), mar=c(4,4,1,1))
s.ophb <- seewave::spectro(cut.ophb, flim=c(0, 8), tlim = c(0, 0.88), main = "Oriental-Pied Hornbill spectro",  
                          grid=F, scale=F, f=48000, wl=512, ovlp=70, cont=TRUE, 
                          contlevels = seq(-25, -25, 1), collevels = seq(-40, 0, 0.1))
#> This took quite a lot of time to display this graphic, you may set 'fastdisp=TRUE' for a faster, but less accurate, display

# 3D spectrogram (with a lower dBlevel for illustrative purpuses)
threeDspectro(cut.ophb, dBlevel=40, flim=c(0, 8), tlim=c(0, 0.88), main="Oriental-Pied Hornbill 3D spectro",
              colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8, resfac=2)

# Set background at -40 dB and remove -Inf values from spectrogram data 
for(i in 1:length(s.ophb$amp)){if(s.ophb$amp[i] == -Inf |s.ophb$amp[i] <= -40)
{s.ophb$amp[i] <- -40}}

# Add curve of relative amplitude
plot3D::contour3D(x=s.ophb$time, y=s.ophb$freq, colvar=t(s.ophb$amp), z=-25,
                  plot=T, add=T, addbox=F, col="black", lwd=1.9, nlevels=2, dDepth=0.25)

# define sampling and run eigensound----
  # blackhornbill

# Using threeDspectro to visualize sampling grid 
par(mfrow=c(1,2), mar=c(1,2,1,0)) 
# As "surface"
threeDspectro(cut.blackhb, samp.grid=TRUE, x.length=70, y.length=47, plot.type="surface", 
              dBlevel=25, flim=c(0, 6), tlim=c(0, 0.88), f=44100, wl=512, ovlp=70, main="As 'surface'",
              colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8)
# As "points"
threeDspectro(cut.blackhb, samp.grid=TRUE, x.length=70, y.length=47, plot.type="points", 
              dBlevel=25, flim=c(0, 6), tlim=c(0, 0.88), f=44100, wl=512, ovlp=70, main="As 'points'",
              colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8)
mtext("Black Hornbill call", side = 3, line = -1, outer = TRUE)


# ophb
par(mfrow=c(1,2), mar=c(1,2,1,0)) 
# As "surface"
threeDspectro(cut.ophb, samp.grid=TRUE, x.length=70, y.length=47, plot.type="surface", 
              dBlevel=25, flim=c(0, 6), tlim=c(0, 0.88), f=48000, wl=512, ovlp=70, main="As 'surface'",
              colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8)
# As "points"
threeDspectro(cut.ophb, samp.grid=TRUE, x.length=70, y.length=47, plot.type="points", 
              dBlevel=25, flim=c(0, 6), tlim=c(0, 0.88), f=48000, wl=512, ovlp=70, main="As 'points'",
              colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8)
mtext("Oriental-Pied Hornbill call", side = 3, line = -1, outer = TRUE)


# run eigensound function

# Sample semilandmarks for each ".wav" file on a folder using a logarithmic sampling grid
# Export 3D graphs with semilandmarks as colored points for inspection
  
# THIS DOES THE PCA ON ALL WAV IN THE DESTINATION FOLDER
  # IN THIS CASE NLY 2 

bheig.sample <- eigensound(analysis.type="threeDshape", dBlevel=25, 
                         f=44100, wl=512, ovlp=70, flim=c(0, 6), tlim=c(0, 0.88),
                         x.length=70, y.length=47, log.scale=TRUE, plot.exp=TRUE, plot.type="points",
                         wav.at=file.path(wav.at, "Aligned"), store.at=store.at)
# Go to folder specified by store.at and check jpeg files created
bheig.sample
wav.at
pca.bheig.sample <- stats::prcomp(geomorph::two.d.array(bheig.sample))
summary(pca.bheig.sample)
#Importance of components:
#                       PC1       PC2
#Standard deviation     170.8   1.196e-13
#Proportion of Variance   1.0   0.000e+00
#Cumulative Proportion    1.0   1.000e+00

?eigensound


# Mean shape configuration (consensus)
hypo.surf(bheig.sample,  PC="mean", flim=c(0, 6), tlim=c(0, 0.88), x.length=70, y.length=47,
          cex.lab=0.7, cex.axis=0.5, cex.main=1)


# Minimum and maximum deformations - Principal Component 1
hypo.surf(bheig.sample, PC=1, flim=c(0, 6), tlim=c(0, 0.88), x.length=70, y.length=47,
          cex.lab=0.7, cex.axis=0.5, cex.main=1)


# PCA using semilandmark coordinates
pca.bheig.sample <- stats::prcomp(geomorph::two.d.array(bheig.sample))

# Verify names of acoustic units from sample
bheig.sample
dimnames(bheig.sample)[[3]]
#> [1] "cut.blackhb" [2] "cut.ophb" 

# Based on those names, create factor to use as groups in subsequent ordination plot
sample.gr <- factor(c(rep("Black Hornbill", 1), rep("Oriental-Pied hornbill", 1)))

# Ordination plot
pca.plot(pca.bheig.sample, groups=sample.gr, conv.hulls=sample.gr, leg.pos="topright", cex=1.2)
