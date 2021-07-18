# R PIXEL COUNTER FOR KHALIFA PORT PHOTOTRANSECTS, 6/11/2021 by BR

# for what to do with images, read the "magick" docu: "https://cran.r-project.org/web/packages/magick/vignettes/intro.html"
# for what to do with rasters https://www.neonscience.org/resources/learning-hub/tutorials/dc-multiband-rasters-r

# HOW TO BUILD A PHOTOTRANSECT
# 1) save as TIF
# 2) The background must be BLACK, so Bare[0,0,0], else the blue channel gets confused (no idea why, but is empirical)
# 3) Platy[255,0,0]; Platy[227,0,0,]; Favia[0,255,0]; Cyph[255,127,0]; Porites[127,0,255].                        *

#========JUST TO CHECK IMAGES=================================
# Note on making images: for "imager" must be a BMP, JPG; Canvas blurs the edges of polygons if they don't fit exactly with the dimensions
# of the image. Minimum amount amount of blur is achieved with highest pixel-resolution
# the image comes in 4 layers in "imager": x,y,time,color channel
library(imager)
#library(readbitmap)
#library(jpeg)
# if you want to look at an image
im<-load.image("G:\\Riegl_March_18\\Business_files\\RC_Inc\\RC_PROJECTS\\Khalifa_Port\\2019\\Dive3_May_2021\\Pics_5_30\\P5300851.jpg")
windows(20,20)
plot(im)
#===========END=============================================

#TO WORK WITH THE PHOTOTRANSECTS, load as image rasters
#if the background is white, the layers get confused. Have BLACK background
library(raster)
library(rgeos)
library(EBImage)
#load the individual layers
picRGB_R<-raster(paste0("G:/Riegl_March_18/Business_files/RC_Inc/RC_PROJECTS/Khalifa_Port/Code_Phototrans/Patch_test.png"),band=1) #is RED band only of background is black
picRGB_G<-raster(paste0("G:/Riegl_March_18/Business_files/RC_Inc/RC_PROJECTS/Khalifa_Port/Code_Phototrans/Patch_test.png"),band=2) 
picRGB_B<-raster(paste0("G:/Riegl_March_18/Business_files/RC_Inc/RC_PROJECTS/Khalifa_Port/Code_Phototrans/Patch_test.png"),band=3) 
#now stack them into a single RGB
stk<-stack(picRGB_R,picRGB_G,picRGB_B,bands=3)
#Housekeeping: check the values
minValue(picRGB_B)
maxValue(picRGB_B)
#check how many bands the file actually has
picRGB_B@file@nbands
# check the image
windows(20,20)
plot(stk,axes=F)

#=======now count the pixels====================================
Platy<-length(intersect(intersect(which(picRGB_R[,,3]==255),which(picRGB_G[,,3]==0)),which(picRGB_B[,,3]==0)))
Platy_perc<-(Platy/(dim(picRGB_R)[1]*dim(picRGB_R)[2]))*100
Dips<-Platy<-length(intersect(intersect(which(picRGB_R[,,3]==0),which(picRGB_G[,,3]==255)),which(picRGB_B[,,3]==0)))
Dips_perc<-(Dips/(dim(picRGB_R)[1]*dim(picRGB_R)[2]))*100
Por<-length(intersect(intersect(which(picRGB_R[,,3]==127),which(picRGB_G[,,3]==0)),which(picRGB_B[,,3]==255)))
Por_perc<-(Por/(dim(picRGB_R)[1]*dim(picRGB_R)[2]))*100
Cyph<-length(intersect(intersect(which(picRGB_R[,,3]==255),which(picRGB_G[,,3]==127)),which(picRGB_B[,,3]==0)))
Cyph_perc<-(Cyph/(dim(picRGB_R)[1]*dim(picRGB_R)[2]))*100
Bare<-length(intersect(intersect(which(picRGB_R[,,3]==0),which(picRGB_G[,,3]==0)),which(picRGB_B[,,3]==0)))
Bare_perc<-(Bare/sum(Bare,Platy,Dips,Por,Cyph)*100)
#below only works if the transect is the same area as the black "bare" area, this is not always the case
#Bare_perc<-(((dim(picRGB_R)[1]*dim(picRGB_R)[2])-sum(Platy,Dips,Por,Cyph))/(dim(picRGB_R)[1]*dim(picRGB_R)[2]))*100
Result<-c(Bare_perc,Platy_perc,Dips_perc,Por_perc,Cyph_perc)
#=======now the pie chart for cover========================================
lbls<-c("Bare","Platygyra","Dipsastrea","Porites","Cyphastrea")
mypalette<-c("white","red","green","blue","orange")
windows(10,10)
pie(Result, col=mypalette,labels=lbls)
#================================================================
#PART 2: The size-class counter
#OK, now define the species, Cor is generic, change the values
Cor<-intersect(intersect(which(picRGB_R[,,3]==0),which(picRGB_G[,,3]==255)),which(picRGB_B[,,3]==0))
BG<-matrix(0,nrow=dim(stk)[1],ncol=dim(stk)[2])
BG[Cor]=255
Coral<-BG
#Coral = thresh(Coral, 5, 5, 0.05) #maybe not needed

x<-bwlabel(Coral)
fts<-computeFeatures.shape(x)
szes<-fts[1:max(x)] #this is a workaround, because there are max(x) values for labelled polygons
sizes<-sort(szes,decreasing=T)

#=====VERIFY only=================================================
#this is the slow check, define the sets of positions on every layer, then take the intersect
set.r<-which(picRGB_R[,,3]==255)
set.g<-which(picRGB_G[,,3]==0)
set.b<-which(picRGB_B[,,3]==0)
int<-length(intersect(intersect(set.r,set.g),set.b))
percentage<-(int/(dim(picRGB_R)[1]*dim(picRGB_R)[2]))*100
#=================================================================

#this here is totally equivalent to my intersect method
#stack the layers, that makes things easier
Platy_pix<-length(which(stk[[1]][]==255 & stk[[2]][]==0 & stk[[3]][]==0))
# this is wrong: Platy_pix<-length(stk[[1]][]==255 & stk[[2]][]==0 & stk[[3]][]==0)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#this is what I need to get the pixel areas!!!
#Need to work on a BW image of 1 layer only
  library(EBImage)
y = readImage("G:/Riegl_March_18/Business_files/RC_Inc/RC_PROJECTS/Khalifa_Port/Code_Phototrans/Veg_test.png")[,,2]
x = thresh(y, 10, 10, 0.05)
#x = opening(x, makeBrush(5, shape='disc'))
x = bwlabel(x) #bwlabel MUST run prior to ComputeFeatures, else it computes a single feature
#display(y, title="Vegetables")
#display(x, title="Vegetables")
## compute shape features
fts = computeFeatures.shape(x)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(EBImage)
R = readImage("G:/Riegl_March_18/Business_files/RC_Inc/RC_PROJECTS/Khalifa_Port/Code_Phototrans/Patch_test.png")[,,1]
G = readImage("G:/Riegl_March_18/Business_files/RC_Inc/RC_PROJECTS/Khalifa_Port/Code_Phototrans/Patch_test.png")[,,2]
B = readImage("G:/Riegl_March_18/Business_files/RC_Inc/RC_PROJECTS/Khalifa_Port/Code_Phototrans/Patch_test.png")[,,3]
display(R)


  
  ##EXPERIMENTAL PART

#find the 

#https://www.rdocumentation.org/packages/EBImage/versions/4.14.2/topics/computeFeatures

#Alternative path
library(EBImage)
x<-readImage("G:/Riegl_March_18/Business_files/RC_Inc/RC_PROJECTS/Khalifa_Port/Code_Phototrans/Color_test.tif")
x_sm<-resize(x,w=10,h=10) #from EBImage

#this is hopeful and work so far
Platy_pix<-which(stk[[1]][]==255 & stk[[2]][]==0 & stk[[3]][]==0)
bw<-matrix(0,nrow=dim(stk)[1],ncol=dim(stk)[2])
bw[Platy_pix]<-255
image(bw)
labels<-bwlabel(bw)
max(labels)

library(imager)
library(EBImage)
im<-load.image("G:/Riegl_March_18/Business_files/RC_Inc/RC_PROJECTS/Khalifa_Port/Code_Phototrans/Veg_test.png")

Dips_pix<-which(stk[[1]][]==0 & stk[[2]][]==255 & stk[[3]][]==0)
R<-R(im);G<-G(im);B<-B(im)
ExGreen<-2*G-R-B
#ExGreen<-isoblur(ExGreen,3)
plot(ExGreen)
ExGreen<-threshold(ExGreen,thr="auto",approx=F,adjust=1)
plot(ExGreen)
labels=bwlabel(ExGreen)
max(labels) # so now I know there are "max" polygons




