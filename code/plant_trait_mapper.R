###########################################################################################
#
#  this script create trait maps based on high-resolution drone maps of vegetation 
#  composition and field measurement of leaf traits. A permutation is used to leverage
#  all ground trait measurements to generate an average trait map and a trait uncertaity
#  map

#  assumption: pixel_trait <- sum(PFT_cover * PFT_trait)
#
#    --- Last updated:  2023.02.08 By Daryl Yang <dediyang@bnl.gov>
###########################################################################################

#******************** close all devices and delete all variables *************************#
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?
#*****************************************************************************************#

#****************************** load required libraries **********************************#
### install and load required R packages
list.of.packages <- c("ggplot2", "raster")  
# check for dependencies and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# load libraries
invisible(lapply(list.of.packages, library, character.only = TRUE))
#*****************************************************************************************#

#************************************ user parameters ************************************#
# define an output directory to store outputs
out.dir <- file.path("/Users/darylyang/Desktop/gdrive/github/sewpen_trait_maps/output")
# create output directory if not exist
if (! file.exists(out.dir)) dir.create(out.dir,recursive=TRUE)
# define plant functional type names used in the PFT cover map
pftname <- c('ET', 'DTSA', 'DTSW', 'DT', 'DLS', 'DDS', 'ES', 'FO', 'DG', 'WG', 'MO',
             'LI', 'NVS')
# define number of permutation for generating trait maps
n.itr <- 1000
# define the name of the trait that you want to create a map
target.trait <- 'CN_ratio'
#*****************************************************************************************#

#*************************************** load data ***************************************#
# load in trait data
trait.dir <- file.path("/Users/darylyang/Desktop/gdrive/github/sewpen_trait_maps/data/trait_data/trait_data_combn.csv")
trait.df <- read.csv(trait.dir)

# load in uas vegetation composition map
veg.comp.dir <- file.path("/Users/darylyang/Desktop/gdrive/github/sewpen_trait_maps/data/uas_veg_comp/kougarok_uas_fcover_map.tif")
veg.comp.rst <- brick(veg.comp.dir)
veg.comp.df <- as.data.frame(veg.comp.rst, xy = TRUE)
names(veg.comp.df) <- c('x', 'y', pftname)
#*****************************************************************************************#

#*************************************** create map **************************************#
# permute to generate a number of trait map predictions using the number defined by n.itr 
# create a dataframe to store the output
trait.mean.itr <- c()
# permutation for trait prediction
for (itr in 1:n.itr)
{
  # extract PFT composition data
  veg.comp <- veg.comp.df[, c(3:ncol(veg.comp.df))]
  # extract trait for each PFT and calculate fractional trait of the PFT in a pixel
  for (pft in 1:length(pftname))
  {
    # extract PFT name
    pft <- pftname[pft]
    # find all trait values for the PFT
    pft.trait.all <- trait.df[which(trait.df$PFT == pft), ]
    # if there are over 5 trait values, randomly sample 5 values and use the mean of
    # the 5 values for fractional trait calculation
    if(nrow(pft.trait.all) > 5)
    {
      pft.sample <- sample(1:nrow(pft.trait.all), 5, replace = FALSE)
      pft.trait.sample <- pft.trait.all[pft.sample, target.trait]
      pft.trait.mean <- mean(pft.trait.sample, na.rm = TRUE)
    }
    # if there are less than 5 trait measurement for the PFT, use the mean of all trait
    # for fractional trait calculation
    if(nrow(pft.trait.all) < 5 & nrow(pft.trait.all) > 0)
    {
      pft.trait.sample <- pft.trait.all[, target.trait]
      pft.trait.mean <- mean(pft.trait.sample, na.rm = TRUE)
    }
    # if there is no data for the PFT, then assign its trait value as NA to remove it
    # from the calculation
    if(nrow(pft.trait.all) == 0)
    {
      pft.trait.mean <- NA
    }
    veg.comp[, which(colnames(veg.comp) == pft)] <- veg.comp[, which(colnames(veg.comp) == pft)] * pft.trait.mean
  }
  # calculate the mean of current iteration
  trait.mean <- apply(veg.comp, 1, sum, na.rm = TRUE)
  # store current iteration result into dataframe
  trait.mean.itr <- cbind(trait.mean.itr, trait.mean)
}

# calculate mean and standard deviation in traits
trait.pix.mean <- apply(trait.mean.itr, 1, FUN = mean, na.rm = TRUE)
trait.pix.sd <- apply(trait.mean.itr, 1, FUN = sd, na.rm = TRUE)

# convert mean trait dataframe back to a raster
trait.pix.mean <- cbind(veg.comp.df[, c(1,2)], trait.pix.mean)
trait.mean.map <- rasterFromXYZ(trait.pix.mean)
crs(trait.mean.map) <- crs(veg.comp.rst)
# convert trait standard deviation dataframe back to a raster
trait.pix.sd <- cbind(veg.comp.df[, c(1,2)], trait.pix.sd)
trait.sd.map <- rasterFromXYZ(trait.pix.sd)
crs(trait.sd.map) <- crs(veg.comp.rst)

# save trait map
outname <- paste0(out.dir, '/', 'kougarok_trait_cnr_area_ave_5m.tif')
writeRaster(trait.mean.map, outname, format = 'GTiff', overwrite = TRUE)
# save trait uncertainty map
outname <- paste0(out.dir, '/', 'kougarok_trait_cnr_area_unc_5m.tif')
writeRaster(trait.sd.map, outname, format = 'GTiff', overwrite = TRUE)
#*****************************************************************************************#






