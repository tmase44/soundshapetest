# PACKAGES----
library(SoundShape)
library(tuneR)
library(seewave)

# https://cran.r-project.org/web/packages/seewave/seewave.pdf

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
str(cut.blackhb)
str(cut.ophb)

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
s.bhb <- seewave::spectro(cut.blackhb, flim=c(0, 8), tlim = c(0, 0.88),  
                          grid=F, scale=F, f=44100, wl=512, ovlp=70, cont=TRUE, 
                          contlevels = seq(-25, -25, 1), collevels = seq(-40, 0, 0.1))
#> This took quite a lot of time to display this graphic, you may set 'fastdisp=TRUE' for a faster, but less accurate, display

# 3D spectrogram (with a lower dBlevel for illustrative purpuses)
threeDspectro(cut.blackhb, dBlevel=40, flim=c(0, 8), tlim=c(0, 0.88), main="",
              colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8, resfac=2)

# Set background at -40 dB and remove -Inf values from spectrogram data 
for(i in 1:length(s.bhb$amp)){if(s.bhb$amp[i] == -Inf |s.bhb$amp[i] <= -40)
{s.kro$amp[i] <- -40}}

# Add curve of relative amplitude
plot3D::contour3D(x=s.bhb$time, y=s.bhb$freq, colvar=t(s.bhb$amp), z=-25,
                  plot=T, add=T, addbox=F, col="black", lwd=1.9, nlevels=2, dDepth=0.25)


