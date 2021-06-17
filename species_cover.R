#recreating riegl's script

library(tidyverse)
library(here)
library(EBImage)
library(magick)

# clear the workspace
rm(list = ls())

#set working directory
here()

# Get the file names of files in a directory ####
file.names <- list.files(path = "./", pattern = "*.tif")

# Single image read
# file.names <- c("UAA T1 0419 blobs.tif")

# Loop over each of the images, create bw images, label them, count the number of features and calculate cover ####

## Output vectors to evaluate results ####
# resized <- vector("list", length(file.names)) #resized images
# bw_images <- vector("list", length(file.names))
labeled_images <- vector("list", length(file.names)) # label matrices
areas <- vector("list", length(file.names))
# colony_count <- vector("list", length(file.names)) # colony count
sc_plots <- vector("list", length(file.names)) #size class plots
percent_cover <- vector("list", length(file.names)) # percent cover

## Start for loop ####
for (i in seq_along(file.names)) {

### Import images ####
ppic <- image_read(file.names[[i]])

### get dimensions of image to resize later
# ppic_dim <- dim(ppic)
# ppic_width <- ppic_dim[1]
# ppic_length <- ppic_dim[2]

### resize image ####
pic <- image_scale(ppic, "500x") %>%
	as_EBImage()

### save images for debugging ####
# resized[[i]] <- pic

rm(ppic)

### new dimensions
pic_dim <- dim(pic)
pic_width <- pic_dim[1]
pic_length <- pic_dim[2]

	### check that the image is vertical, otherwise rotate it
	if(pic_width < pic_length) {
		pic <- pic
	} else {
		rotate(pic, 90)
	}

	Species_list <- c("Acanthastrea", "Acropora", "Anomastrea", "Coscinarea", "Cyphastrea", "Dipsastrea", "Favites", "Goniopora", "Leptastrea_purpurea", "Leptastrea_transversa", "Pavona", "Platygyra", "Plesiastrea", "Porites_harrisoni", "Porites_lutea", "Psammocora_albopicta", "Psammocora_profundacella", "Psammocora_stellata", "Siderastrea_savignyana", "Turbinaria_peltata", "Turbinaria_reniformis", "Unknown")

	Genus_list <- c("Acanthastrea", "Acropora", "Anomastrea", "Coscinarea", "Cyphastrea", "Dipsastrea", "Favites", "Goniopora", "Leptastrea", "Pavona", "Platygyra", "Plesiastrea", "Porites", "Psammocora", "Siderastrea", "Turbinaria", "Unknown")

	## Find pixels with the colors for each specific species ####
	Acanthastrea              =  intersect(which(pic[,,1] == 255/255),  intersect(which(pic[,,2] == 100/255), which(pic[,,3] == 100/255)))
	Acropora                  =  intersect(which(pic[,,1] == 90/255),   intersect(which(pic[,,2] == 50/255),  which(pic[,,3] == 0)))
	Anomastrea                =  intersect(which(pic[,,1] == 150/255),  intersect(which(pic[,,2] == 50)/255,  which(pic[,,3] == 50/255)))
	Coscinarea                =  intersect(which(pic[,,1] == 150/255),  intersect(which(pic[,,2] == 150/255), which(pic[,,3] == 25/255)))
	Cyphastrea                =  intersect(which(pic[,,1] == 10/255),   intersect(which(pic[,,2] == 150/255), which(pic[,,3] == 138/255)))
	Dipsastrea                =  intersect(which(pic[,,1] == 255/255),  intersect(which(pic[,,2] == 0),       which(pic[,,3] == 0)))
	Favites                   =  intersect(which(pic[,,1] == 0),        intersect(which(pic[,,2] == 0),       which(pic[,,3] == 255/255)))
	Goniopora                 =  intersect(which(pic[,,1] == 70/255),   intersect(which(pic[,,2] == 10/255),  which(pic[,,3] == 150/255)))
	Leptastrea_purpurea       =  intersect(which(pic[,,1] == 155/255),  intersect(which(pic[,,2] == 0),       which(pic[,,3] == 250/255)))
	Leptastrea_transversa     =  intersect(which(pic[,,1] == 230/255),  intersect(which(pic[,,2] == 165/255), which(pic[,,3] == 250/255)))
	Pavona                    =  intersect(which(pic[,,1] == 255/255),  intersect(which(pic[,,2] == 100/255), which(pic[,,3] == 0)))
	Platygyra                 =  intersect(which(pic[,,1] == 50/255),   intersect(which(pic[,,2] == 255/255), which(pic[,,3] == 50/255)))
	Plesiastrea               =  intersect(which(pic[,,1] == 200/255),  intersect(which(pic[,,2] == 20/255),  which(pic[,,3] == 200/255)))
	Porites_harrisoni         =  intersect(which(pic[,,1] == 255/255),  intersect(which(pic[,,2] == 255/255), which(pic[,,3] == 0)))
	Porites_lutea             =  intersect(which(pic[,,1] == 200/255),  intersect(which(pic[,,2] == 200/255), which(pic[,,3] == 50/255)))
	Psammocora_albopicta      =  intersect(which(pic[,,1] == 0),        intersect(which(pic[,,2] == 200/255), which(pic[,,3] == 255/255)))
	Psammocora_profundacella  =  intersect(which(pic[,,1] == 250/255),  intersect(which(pic[,,2] == 150/255), which(pic[,,3] == 100/255)))
	Psammocora_stellata       =  intersect(which(pic[,,1] == 50/255),   intersect(which(pic[,,2] == 150/255), which(pic[,,3] == 0)))
	Siderastrea_savignyana    =  intersect(which(pic[,,1] == 250/255),  intersect(which(pic[,,2] == 150/255), which(pic[,,3] == 250/255)))
	Turbinaria_peltata        =  intersect(which(pic[,,1] == 0),        intersect(which(pic[,,2] == 50/255),  which(pic[,,3] == 0)))
	Turbinaria_reniformis     =  intersect(which(pic[,,1] == 130/255),  intersect(which(pic[,,2] == 230/255), which(pic[,,3] == 130/255)))
	Unknown                   =  intersect(which(pic[,,1] == 25/255),   intersect(which(pic[,,2] ==0),        which(pic[,,3] == 175/255)))

### List of species index vectors ####
sp_px <- list(Acanthastrea, Acropora, Anomastrea, Coscinarea, Cyphastrea, Dipsastrea, Favites, Goniopora, Leptastrea_purpurea, Leptastrea_transversa, Pavona, Platygyra, Plesiastrea, Porites_harrisoni, Porites_lutea, Psammocora_albopicta, Psammocora_profundacella, Psammocora_stellata, Siderastrea_savignyana, Turbinaria_peltata, Turbinaria_reniformis, Unknown)
		names(sp_px) <- Species_list

	# ### Concatenate individual species indices into a vector named Coral_sp ####
# Coral_sp <- c(Acanthastrea, Acropora, Anomastrea, Coscinarea, Cyphastrea, Dipsastrea, Favites, Goniopora, Leptastrea_purpurea, Leptastrea_transversa, Pavona, Platygyra, Plesiastrea, Porites_harrisoni, Porites_lutea, Psammocora_albopicta, Psammocora_profundacella, Psammocora_stellata, Siderastrea_savignyana, Turbinaria_peltata, Turbinaria_reniformis, Unknown)
#
# ## Concatenate individual genus indices into a vector named Coral_gen ####
# Coral_gen <- c(Acanthastrea, Acropora, Anomastrea, Coscinarea, Cyphastrea, Dipsastrea, Favites, Goniopora, Leptastrea = c(Leptastrea_purpurea, Leptastrea_transversa), Pavona, Platygyra, Plesiastrea, Porites = c(Porites_harrisoni, Porites_lutea), Psammocora = c(Psammocora_albopicta, Psammocora_profundacella, Psammocora_stellata), Siderastrea_savignyana, Turbinaria = c(Turbinaria_peltata, Turbinaria_reniformis), Unknown)

## Label the areas with coral ####
## Create a black image the size of the original transect
l_b <- matrix(0, pic_width, pic_length)

### Replace the indices for each species with white, and store them in a tibble ####
# x <- l_b %>%
# 	map_dbl(replace, sp_px, 1)

sp_labels <- list(
		Acanthastrea_bw              = replace(l_b, Acanthastrea, 1),
		Acropora_bw                  = replace(l_b, Acropora, 1),
		Anomastrea_bw                = replace(l_b, Anomastrea, 1),
		Coscinarea_bw                = replace(l_b, Coscinarea, 1),
		Cyphastrea_bw                = replace(l_b, Cyphastrea, 1),
		Dipsastrea_bw                = replace(l_b, Dipsastrea, 1),
		Favites_bw                   = replace(l_b, Favites, 1),
		Goniopora_bw                 = replace(l_b, Goniopora, 1),
		Leptastrea_purpurea_bw       = replace(l_b, Leptastrea_purpurea, 1),
		Leptastrea_transversa_bw     = replace(l_b, Leptastrea_transversa, 1),
		Pavona_bw                    = replace(l_b, Pavona, 1),
		Platygyra_bw                 = replace(l_b, Platygyra, 1),
		Plesiastrea_bw               = replace(l_b, Plesiastrea, 1),
		Porites_harrisoni_bw         = replace(l_b, Porites_harrisoni, 1),
		Porites_lutea_bw             = replace(l_b, Porites_lutea, 1),
		Psammocora_albopicta_bw      = replace(l_b, Psammocora_albopicta, 1),
		Psammocora_profundacella_bw  = replace(l_b, Psammocora_profundacella, 1),
		Psammocora_stellata_bw       = replace(l_b, Psammocora_stellata, 1),
		Siderastrea_savignyana_bw    = replace(l_b, Siderastrea_savignyana, 1),
		Turbinaria_peltata_bw        = replace(l_b, Turbinaria_peltata, 1),
		Turbinaria_reniformis_bw     = replace(l_b, Turbinaria_reniformis, 1),
		Unknown_bw                   = replace(l_b, Unknown, 1)
		# Corals_sp_bw                 = replace(l_b, Coral_sp, 1),
		# Corals_gn_bw                 = replace(l_b, Coral_gen, 1)
)

#### output to bw_images for debugging ####
# bw_images[[i]] <- sp_images
# names(bw_images) <- file.names

### Label colonies in each of the species layers ####
labeled <- sp_labels %>%
	map(bwlabel)

#### output to labeled_images for debugging ####
labeled_images[[i]] <- labeled
names(labeled_images) <- file.names

### Calculate feature geometries ####
area <- labeled %>%
	      map(computeFeatures.shape) %>%
	      map_df(~as.data.frame(.x), .id="Species") %>%
 	      as_tibble() %>%
		    filter(s.area > 5) %>%
		    select(Species, s.area) %>%
		    mutate(`Size Class`= cut(s.area, breaks=c(0, 5, 10, 30, 100, 10000), right = FALSE, ordered_result = TRUE, include.lowest = TRUE))

# need to figure out the resizing so we can standardize the areas!!!

#
# ### Extract the area and convert it to circle equivalents ####
# 	rad <- geometries %>%
# 		transmute(Species = Species, s.area = sqrt(s.area/pi)) %>%
# 		mutate(`Size Class`= cut(s.area, breaks=c(0, 5, 10, 30, 100, 1000), right = FALSE, ordered_result = TRUE, include.lowest = TRUE))

# output areas saved for analysis ####
areas[[i]] <- area
names(areas) <- file.names

### summary stats can be calculated at the end ####
# stats <- rad %>%
# 	summarize(across(where(is.numeric), funs(count = n(), mean = mean(.x, na.rm = TRUE))))


	# Sc1 <-tibble("<5"        = length(which(rad$s.area<5)),
	# 				     "5 - <10"   = length(which(rad$s.area>5 & rad$s.area<10)),
	# 				     "10 - <30"  = length(which(rad$s.area>10 & rad$s.area<30)),
	# 				     "30 - <100" = length(which(rad$s.area>30 & rad$s.area<100)),
	# 				     ">100"      = length(which(rad$s.area>100))) %>%
	# 	    pivot_longer(everything(), names_to = "Size Class", values_to = "Colonies") %>%
	# 			mutate(`Size Class` = as_factor(`Size Class`))

	## plot the number of colonies per size class per species ####
	p <- ggplot(area) +
	     geom_bar(aes(`Size Class`, fill = `Size Class`)) +
		   facet_wrap(~Species) +
		   labs(title = file.names[[i]]) +
		   theme(legend.position = "none")

	sc_plots[[i]] <- p
	names(sc_plots) <- file.names



	## Count the number of corals per species ####
	# num_corals <- labeled %>%
	# 	map_dbl(max) %>%
	# 	as_tibble(rownames = "Species")
	#
	# colony_count[[i]] <- num_corals
	# names(colony_count) <- file.names

	# Percent cover calculation ####
	## Get indices for bare area ####
	Bare <- intersect(which(pic[,,1] == 1),
										intersect(which(pic[,,2] == 1),
															which(pic[,,3] == 1)))

	##cover calculation
		species_area <- sp_px %>%
			map_dbl(length) %>%
			as_tibble(rownames = "Species") %>%
			mutate(cover = value/length(Bare)*100) %>%
			select(-value)

    # total_area <- species_area$value + length(Bare)
    # total_area <- as_tibble(total_area, rownames = "Species")
    #
    # pcover <- (species_area$value/total_area)*100

	percent_cover[[i]] <- species_area
	names(percent_cover) <- file.names

	#
	# map(bw_images, cov)
	# percent_cover[[i]] <- cov
}

# Results####
## Colony counts ####
# col_ct <- colony_count %>%
# 	map_df(~as.data.frame(.x), .id = "Transect") %>%
# 	as_tibble() %>%
# 	group_by(Transect)
# 	mutate(Species = str_remove(Species, "_bw$"), Colonies = value) %>%
# 	select(-value)

## Percent Cover ####
pc <- percent_cover %>%
	map_df(~as.data.frame(.x), .id = "Transect") %>%
	as_tibble() %>%
	mutate(`%Cover` = )

col_ct$`% Cover`<- pc$`% Cover`

## Summary Tables ####
results <- areas %>%
	         map_df(~as.data.frame(.x), .id = "Transect") %>%
	         as_tibble() %>%
	         group_by(Transect, Species, `Size Class`) %>%
           mutate(Species = str_remove(Species, "_bw$"))

rs <- results %>%
	summarize(across(where(is.numeric), ~ n()))


results_by_tx <- col_ct %>%
	group_by(Transect) %>%
	summarize_at(c("Colonies", "% Cover"), mean, na.rm = TRUE) %>%
	ungroup()

results_by_sp <- col_ct %>%
	group_by(Species) %>%
	summarize_at(c("Colonies", "% Cover"), mean, na.rm = TRUE) %>%
	ungroup()
