#recreating riegl's script

library(tidyverse)
library(here)
library(EBImage)
library(magick)

# clear the workspace
rm(list = ls())

# set working directory
here()

# Get the file names of files in a directory ####
# file.names <- list.files(path = "./", pattern = "*.tif")

# Single image read for testing
file.names <- c("FEH T1 0419_2.tif")

# Loop over each of the images, create bw images, label them, count the number of features and calculate cover ####

#### Output vectors of results to evaluate results ####
colony_data <- vector("list", length(file.names)) # all colony counts and areas if wanted
size_class_plots <- vector("list", length(file.names)) #size class plots
results_by_transect <- vector("list", length(file.names)) # percent cover, colony count and area by species

## Debugging vectors to check outputs of certain steps #####
# resized <- vector("list", length(file.names)) # the resized images
# bw_images <- vector("list", length(file.names)) # the generated bw images
label_matrices <- vector("list", length(file.names)) # label matrices

## Start for loop ####
for (i in seq_along(file.names)) {

### Import image ####
ppic <- image_read(file.names[[i]])

### resize image ####
pic <- image_scale(ppic, "500x") %>% # using magick package because resizing with EBImage causes weird artifacts
	as_EBImage()

#### save images for debugging ###
# resized[[i]] <- pic

rm(ppic)

### Get dimensions of the resized image to generate black and white image ####
pic_dim <- dim(pic)

### check that the image is vertical, otherwise rotate it
if(pic_dim[1] < pic_dim[2]) {
	pic <- pic
} else {
	rotate(pic, 90)
}

### Create a list of lists with the indices for pixels with the colors for each specific species ####
species_indices <- list(
Acanthastrea                =  intersect(which(pic[,,1] == 255/255), intersect(which(pic[,,2] == 100/255), which(pic[,,3] == 100/255))),
Acropora                    =  intersect(which(pic[,,1] == 90/255),  intersect(which(pic[,,2] == 50/255),  which(pic[,,3] == 0))),
Anomastrea                  =  intersect(which(pic[,,1] == 150/255), intersect(which(pic[,,2] == 50)/255,  which(pic[,,3] == 50/255))),
Coscinarea                  =  intersect(which(pic[,,1] == 150/255), intersect(which(pic[,,2] == 150/255), which(pic[,,3] == 25/255))),
Cyphastrea                  =  intersect(which(pic[,,1] == 10/255),  intersect(which(pic[,,2] == 150/255), which(pic[,,3] == 138/255))),
Dipsastrea                  =  intersect(which(pic[,,1] == 255/255), intersect(which(pic[,,2] == 0),       which(pic[,,3] == 0))),
Favites                     =  intersect(which(pic[,,1] == 0),       intersect(which(pic[,,2] == 0),       which(pic[,,3] == 255/255))),
Goniopora                   =  intersect(which(pic[,,1] == 70/255),  intersect(which(pic[,,2] == 10/255),  which(pic[,,3] == 150/255))),
`Leptastrea purpurea`       =  intersect(which(pic[,,1] == 155/255), intersect(which(pic[,,2] == 0),       which(pic[,,3] == 250/255))),
`Leptastrea transversa`     =  intersect(which(pic[,,1] == 230/255), intersect(which(pic[,,2] == 165/255), which(pic[,,3] == 250/255))),
Pavona                      =  intersect(which(pic[,,1] == 255/255), intersect(which(pic[,,2] == 100/255), which(pic[,,3] == 0))),
Platygyra                   =  intersect(which(pic[,,1] == 50/255),  intersect(which(pic[,,2] == 255/255), which(pic[,,3] == 50/255))),
Plesiastrea                 =  intersect(which(pic[,,1] == 200/255), intersect(which(pic[,,2] == 20/255),  which(pic[,,3] == 200/255))),
`Porites harrisoni`         =  intersect(which(pic[,,1] == 255/255), intersect(which(pic[,,2] == 255/255), which(pic[,,3] == 0))),
`Porites lutea`             =  intersect(which(pic[,,1] == 200/255), intersect(which(pic[,,2] == 200/255), which(pic[,,3] == 50/255))),
`Psammocora albopicta`      =  intersect(which(pic[,,1] == 0),       intersect(which(pic[,,2] == 200/255), which(pic[,,3] == 255/255))),
`Psammocora profundacella`  =  intersect(which(pic[,,1] == 250/255), intersect(which(pic[,,2] == 150/255), which(pic[,,3] == 100/255))),
`Psammocora stellata`       =  intersect(which(pic[,,1] == 50/255),  intersect(which(pic[,,2] == 150/255), which(pic[,,3] == 0))),
`Siderastrea savignyana`    =  intersect(which(pic[,,1] == 250/255), intersect(which(pic[,,2] == 150/255), which(pic[,,3] == 250/255))),
`Turbinaria peltata`        =  intersect(which(pic[,,1] == 0),       intersect(which(pic[,,2] == 50/255),  which(pic[,,3] == 0))),
`Turbinaria reniformis`     =  intersect(which(pic[,,1] == 130/255), intersect(which(pic[,,2] == 230/255), which(pic[,,3] == 130/255))),
Unknown                     =  intersect(which(pic[,,1] == 25/255),  intersect(which(pic[,,2] ==0),        which(pic[,,3] == 175/255)))
)

### Collapse the index list into a sinlge vector to generate an all corals list ####
Coral_sp <- unlist(species_indices, use.names = FALSE)

# Add the vector back to our list of indices
species_indices$Coral <- unname(Coral_sp)
rm(Coral_sp) # delete the loose vector

## species names list to generate factor levels
species_levels <- names(species_indices)

### Concatenate individual genus indices into a vector named Coral_gen for genus level analysis ###
# Coral_gen <- c(Acanthastrea, Acropora, Anomastrea, Coscinarea, Cyphastrea, Dipsastrea, Favites, Goniopora, Leptastrea = c(Leptastrea_purpurea, Leptastrea_transversa), Pavona, Platygyra, Plesiastrea, Porites = c(Porites_harrisoni, Porites_lutea), Psammocora = c(Psammocora_albopicta, Psammocora_profundacella, Psammocora_stellata), Siderastrea_savignyana, Turbinaria = c(Turbinaria_peltata, Turbinaria_reniformis), Unknown)


# Generate and analyze BW images ####

## Create a black image the size of the original transect
black_bg <- matrix(0, pic_dim[1], pic_dim[2])

## Replace the areas with coral with white pixels so that they can be labeled in the next step

### create the output list to capture results from for loop below
species_binary_images <- vector("list", length(species_indices))
names(species_binary_images) <- names(species_indices) # keep correct species names for each of the images

## generate binary images by combining the black background with white pixels for corals
for (j in seq_along(species_indices)) {
	a <- species_indices[[j]]
	species_binary_images[[j]] <- replace(black_bg, a, 1)
}

## Check the results by checking that the number of Dipsastreas (most common coral) match the number of blobs an
image_check <- identical(as.numeric(length(species_indices$Dipsastrea)), sum(species_binary_images$Dipsastrea))
image_check

# delete black layer
# rm(black_bg)

#### Label colonies in each of the species layers ####
### Image segmentation and labeling
species_label_matrix <- species_binary_images %>%
	                      map(bwlabel) # apply bwlabel to each of the species binary images, output them into a list

display(colorLabels(species_label_matrix$Coral))

#### output to labeled_images for debugging ####
label_matrices[[i]] <- species_label_matrix
names(label_matrices) <- file.names

## Calculate blob geometries ####
colony_area <- species_label_matrix %>%
	             map(computeFeatures.shape) %>% # calculate blob attributes for each of the species label matrices
	             map_df(~as.data.frame(.x), .id="Species") %>% # join lists into one data frame for easier data manipulation
 	             as_tibble() %>% # convert df into tibble for easier data manipulation
		           filter(s.area > 5) %>% # filter out small blobs
		           transmute(Species = factor(Species, levels = species_levels),
		            s.area = sqrt(s.area/pi), # convert the areas into circle equivalents
		           `Size Class`= cut(s.area, breaks=c(0, 5, 10, 30, 100, 10000), right = FALSE, ordered_result = TRUE, include.lowest = TRUE)) %>% # assign size classes to each of the blobs
	              mutate(`Size Class` = fct_recode(`Size Class`,
	              																 "<5"       = "[0,5)",
	              																 "5 ≤ 10"   = "[5,10)",
	              																 "10 ≤ 30"  = "[10,30)",
	              																 "30 ≤ 100" = "[30,100)",
	              																 ">100"     = "[100,1e+04]")) # rename factors to more legible forms

### summarize by species ####
species_area <- colony_area %>%
	              group_by(Species) %>%
                summarize_if(is.numeric, funs(`Number of Colonies` = n(),`Avg. Area` = mean))

#### need to figure out the resizing so we can standardize the areas!!!

#### output areas saved for analysis ####
colony_data[[i]] <- colony_area
names(colony_data) <- file.names

# plot the number of colonies per size class per species ####
p <- ggplot(colony_area) +
     geom_bar(aes(`Size Class`, fill = `Species`)) +
	   labs(x = "Size Class") +
		 theme_bw() +
	   theme(legend.position = "none") +
	   facet_wrap(~Species) +
	   labs(title = file.names[[i]])

#### save plots ####
size_class_plots[[i]] <- p
names(size_class_plots) <- file.names

# Percent cover calculation ####

## get indices for transect area
Bare =  c(intersect(which(pic[,,1] == 250/255),  intersect(which(pic[,,2] == 50/255),  which(pic[,,3] == 250/255))), species_indices$Coral) # does not include areas with coral

## make a binary image of transect area
Transect_indices <- black_bg %>%
	                  replace(Bare, 1) %>%
	                  medianFilter(10)

## get label matrix for transect area
Transect_img <- bwlabel(Transect_indices)

## Make table with cover values per species
species_cover <- species_indices %>%
	map_dbl(length) %>% # get the length of each of the index vectors
	as_tibble(rownames = "Species") %>%
	mutate(`% Cover` = value/length(Transect_indices)*100) %>% # calculate percent cover of colored pixels over white pixels
	# filter(`% Cover` == 0 | `% Cover` > 0.01) %>%  # filter out likely spurious results, will likely use this in the final table but not yet, so we can match up all species with the colony areas. although you could use a join...
	select(-value)

# join colony count and area table with cover values
joined_output <- species_area %>%
	               left_join(species_cover, by = "Species")

# output results into list
results_by_transect[[i]] <- joined_output
names(results_by_transect) <- file.names
}

# Results####
site_levels = c("BZ", "UAA", "FEH", "MM", "SH")
transect_levels = c("T1", "T2", "T3")

results_long <- results_by_transect %>%
  map_df(~as.data.frame(.x), .id = "Site") %>% # takes results list and puts into a single data frame adding an id column for the site
  as_tibble() %>%
  separate(Site, c("Site", "Transect", "Period")) %>%
  mutate(Period2 = as.numeric(Period)) %>%
  separate(Period2, c("Quarter", "Year"), 1) %>%
  mutate(Site = factor(Site, levels = site_levels),
  			 Transect = factor(Transect, levels = transect_levels),
         Period = factor(Period, levels = unique(Period)),
         Quarter = factor(Quarter, levels = c("1", "2", "3", "4")),
         Year = factor(paste0("20",Year), levels = c("2019", "2020", "2021", "2022"))) %>%
  select(Site, Transect, Year, Quarter, everything(), -Period)


