#recreating riegl's script
#
library(tidyverse)
library("EBImage")



# currently only reads one image
ppic <- readImage("/Users/Mau/OneDrive\ -\ Qatar\ University/RESTORE\ Shared\ Folder/Work\ Plan\ Research/WP2-\ Integrated\ Monitoring/Images/Transects/Umm\ Al\ Arshan/2019.10.19/UAA\ T1\ 0419\ copy.tif")

#get dimensions of image to resize later
ppic_dim <- dim(ppic)
ppic_width <- ppic_dim[1]
ppic_length <- ppic_dim[2]



#resize image
pic <- resize(ppic, 500,)

#new dimensions
pic_dim <- dim(ppic)
pic_width <- ppic_dim[1]
pic_length <- ppic_dim[2]


#find pixels with the colors for each specific species
Acanthastrea              =  intersect(which(pic[,,1] == 255/255),  intersect(which(pic[,,2]==100/255), which(pic[,,3]==100/255)))
Acropora                  =  intersect(which(pic[,,1] == 90/255),   intersect(which(pic[,,2]==50/255),  which(pic[,,3]==0)))
Anomastrea                =  intersect(which(pic[,,1] == 150/255),  intersect(which(pic[,,2]==50)/255,  which(pic[,,3]==50/255)))
Coscinarea                =  intersect(which(pic[,,1] == 150/255),  intersect(which(pic[,,2]==150/255), which(pic[,,3]==25/255)))
Cyphastrea                =  intersect(which(pic[,,1] == 10/255),   intersect(which(pic[,,2]==150/255), which(pic[,,3]==138/255)))
Dipsastrea                =  intersect(which(pic[,,1] == 255/255),  intersect(which(pic[,,2]==0),       which(pic[,,3]==0)))
Favites                   =  intersect(which(pic[,,1] == 0),        intersect(which(pic[,,2]==0),       which(pic[,,3]==255/255)))
Goniopora                 =  intersect(which(pic[,,1] == 70/255),   intersect(which(pic[,,2]==10/255),  which(pic[,,3]==150/255)))
Leptastrea_purpurea       =  intersect(which(pic[,,1] == 155/255),  intersect(which(pic[,,2]==0),       which(pic[,,3]==250/255)))
Leptastrea_transversa     =  intersect(which(pic[,,1] == 230/255),  intersect(which(pic[,,2]==165/255), which(pic[,,3]==250/255)))
Pavona                    =  intersect(which(pic[,,1] == 255/255),  intersect(which(pic[,,2]==100/255), which(pic[,,3]==0)))
Platygyra                 =  intersect(which(pic[,,1] == 50/255),   intersect(which(pic[,,2]==255/255), which(pic[,,3]==50/255)))
Plesiastrea               =  intersect(which(pic[,,1] == 200/255),  intersect(which(pic[,,2]==20/255),  which(pic[,,3]==200/255)))
Porites_harrisoni         =  intersect(which(pic[,,1] == 255/255),  intersect(which(pic[,,2]==255/255), which(pic[,,3]==0)))
Porites_lutea             =  intersect(which(pic[,,1] == 200/255),  intersect(which(pic[,,2]==200/255), which(pic[,,3]==50/255)))
Psammocora_albopicta      =  intersect(which(pic[,,1] == 0),        intersect(which(pic[,,2]==200/255), which(pic[,,3]==255/255)))
Psammocora_profundacella  =  intersect(which(pic[,,1] == 250/255),  intersect(which(pic[,,2]==150/255), which(pic[,,3]==100/255)))
Psammocora_stellata       =  intersect(which(pic[,,1] == 50/255),   intersect(which(pic[,,2]==150/255), which(pic[,,3]==0)))
Siderastrea_savignyana    =  intersect(which(pic[,,1] == 50/255),   intersect(which(pic[,,2]==150/255), which(pic[,,3]==0)))
Turbinaria_peltata        =  intersect(which(pic[,,1] == 0),        intersect(which(pic[,,2]==50/255),  which(pic[,,3]==0)))
Turbinaria_reniformis     =  intersect(which(pic[,,1] == 0),        intersect(which(pic[,,2]==0),       which(pic[,,3]==255/255)))
Unknown                   =  intersect(which(pic[,,1] == 25/255),   intersect(which(pic[,,2]==0),       which(pic[,,3]==175/255)))

#put above index vectors into Cor
Cor <- c(Acanthastrea, Acropora, Anomastrea, Coscinarea, Cyphastrea, Dipsastrea, Favites, Goniopora, Favites, Goniopora, Leptastrea_purpurea, Leptastrea_transversa, Pavona, Platygyra, Plesiastrea, Porites_harrisoni, Porites_lutea, Psammocora_albopicta, Psammocora_profundacella, Psammocora_stellata, Siderastrea_savignyana, Turbinaria_peltata, Turbinaria_reniformis, Unknown)

#Create a zeros vector the size of the new image
