#recreating riegl's script

library(tidyverse)
library(EBImage)
library(here)

#set working directory
here()


# Get the file names of files in a directory####
file.names <- list.files(path = "./", pattern = "*.tif")

# Single image read
# file.names <- c("UAA T1 0419 blobs small.tiff")

## output vectors to evaluate results
image_list <- vector("list", length(file.names)) # original images
#resized <- vector("list", length(file.names)) #resized images
labeled_images <- vector("list", length(file.names)) # label matrices
percent_cover <- vector("double", length(file.names)) # percent cover
colony_count <- vector("double", length(file.names)) # colony count


# loop over ech of the images, create bw images, label them, count the number of features and calculate cover
for (i in seq_along(file.names)) {
	pic <- readImage(file.names[[i]])
	image_list[[i]] <- pic

# #get dimensions of image to resize later
# ppic_dim <- dim(ppic)
# ppic_width <- ppic_dim[1]
# ppic_length <- ppic_dim[2]

## resize image
# pic <- resize(ppic, 500)
# resized[[i]] <- pic

## new dimensions
pic_dim <- dim(pic)
pic_width <- pic_dim[1]
pic_length <- pic_dim[2]

## check that the image is vertical, otherwise rotate it
if(pic_width < pic_length) {
	pic <- pic
} else {
rotate(pic, 90)
}

####find pixels with the colors for each specific species####
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
Siderastrea_savignyana    =  intersect(which(pic[,,1] == 250/255),  intersect(which(pic[,,2]==150/255), which(pic[,,3]==250/255)))
Turbinaria_peltata        =  intersect(which(pic[,,1] == 0),        intersect(which(pic[,,2]==50/255),  which(pic[,,3]==0)))
Turbinaria_reniformis     =  intersect(which(pic[,,1] == 130/255),  intersect(which(pic[,,2]==230/255), which(pic[,,3]==130/255)))
Unknown                   =  intersect(which(pic[,,1] == 25/255),   intersect(which(pic[,,2]==0),       which(pic[,,3]==175/255)))

## Lump by genus
Leptastrea <- c(Leptastrea_purpurea, Leptastrea_transversa)
Porites <- c(Porites_harrisoni, Porites_lutea)
Psammocora <- c(Psammocora_albopicta, Psammocora_profundacella, Psammocora_stellata)
Turbinaria <- c(Turbinaria_peltata, Turbinaria_reniformis)

## Concatenate individual species indices into a vector named Coral_sp
Coral_sp <- c(Acanthastrea, Acropora, Anomastrea, Coscinarea, Cyphastrea, Dipsastrea, Favites, Goniopora, Leptastrea_purpurea, Leptastrea_transversa, Pavona, Platygyra, Plesiastrea, Porites_harrisoni, Porites_lutea, Psammocora_albopicta, Psammocora_profundacella, Psammocora_stellata, Siderastrea_savignyana, Turbinaria_peltata, Turbinaria_reniformis, Unknown)

## Concatenate individual genus indices into a vector named Coral_gen
Coral_gen <- c(Acanthastrea, Acropora, Anomastrea, Coscinarea, Cyphastrea, Dipsastrea, Favites, Goniopora, Leptastrea, Pavona, Platygyra, Plesiastrea, Porites, Psammocora, Siderastrea_savignyana, Turbinaria, Unknown)


#### Label the areas with coral####
## Create a black image the size of the original transect
l_b <- matrix(0, pic_width, pic_length)

## replace the indices for each species with white
Acanthastrea_labs <-             replace(l_b, Acanthastrea, 1)
Acropora_labs <-                 replace(l_b, Acropora, 1)
Anomastrea_labs <-               replace(l_b, Anomastrea, 1)
Coscinarea_labs <-               replace(l_b, Coscinarea, 1)
Cyphastrea_labs <-               replace(l_b, Cyphastrea, 1)
Dipsastrea_labs <-               replace(l_b, Dipsastrea, 1)
Favites_labs <-                  replace(l_b, Favites, 1)
Goniopora_labs <-                replace(l_b, Goniopora, 1)
Leptastrea_purpurea_labs <-      replace(l_b, Leptastrea_purpurea, 1)
Leptastrea_transversa_labs <-    replace(l_b, Leptastrea_transversa, 1)
Leptastrea_spp_labs <-           replace(l_b, c(Leptastrea_purpurea, Leptastrea_transversa), 1)
Pavona_labs <-                   replace(l_b, Pavona, 1)
Platygyra_labs <-                replace(l_b, Platygyra, 1)
Plesiastrea_labs <-              replace(l_b, Plesiastrea, 1)
Porites_harrisoni_labs <-        replace(l_b, Porites_harrisoni, 1)
Porites_lutea_labs <-            replace(l_b, Porites_lutea, 1)
Porites_spp_labs <-              replace(l_b, c(Porites_harrisoni, Porites_lutea), 1)
Psammocora_albopicta_labs <-     replace(l_b, Psammocora_albopicta, 1)
Psammocora_profundacella_labs <- replace(l_b, Psammocora_profundacella, 1)
Psammocora_stellata_labs <-      replace(l_b, Psammocora_stellata, 1)
Psammocora_sp_labs <-            replace(l_b, c(Psammocora_albopicta, Psammocora_profundacella, Psammocora_stellata_labs), 1)
Siderastrea_savignyana_labs <-   replace(l_b, Siderastrea_savignyana, 1)
Turbinaria_peltata_labs <-       replace(l_b, Turbinaria_peltata, 1)
Turbinaria_reniformis_labs <-    replace(l_b, Turbinaria_reniformis, 1)
Turbinaria_spp_labs <-           replace(l_b, c(Turbinaria_peltata, Turbinaria_reniformis), 1)
Unknown_labs <-                  replace(l_b, Unknown, 1)
Coral <- replace(l_b, Coral_gen, 1)

#Label each coral
labeled <- bwlabel(Coral)
labeled_images[[i]] <- labeled

#how many corals were counted
num_corals <- max(labeled)
colony_count[[i]] <- num_corals

####calculate the area occupied by corals####
data1 <- computeFeatures.shape(labeled)
area1 <- unname(data1[, 's.area'])
ar1 <- sort(area1)

####Calculate size classes####
rad <- sqrt(ar1/pi)
Sc1 <-c(length(which(rad<5)), length(which(rad>5 & rad<10)), length(which(rad>10 & rad<30)), length(which(rad>30 & rad<100)), length(which(rad>100)))

#Size_cl=[Size_cl; Sc1] not sure what this does

####Percent cover calculation####
#get indices for bare area
Bare <- intersect(which(pic[,,1] == 1),
				intersect(which(pic[,,2] == 1),
				          which(pic[,,3] == 1)))

#cover calculation
cov <- length(Cor)/(length(Bare)+length(Cor))
percent_cover[[i]] <- cov
}

####Results Table####
results <- tibble(transect = file.names,
	                `# corals` = colony_count,
									`percent cover` = percent_cover*100)

# summary stats
stats <- map_dbl(select(results, -transect), (mean))

## get kable out of this with the results
