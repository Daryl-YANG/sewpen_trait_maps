###########################################################################################
#
#  this script perform 
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

pftname <- c('ET', 'DTSA', 'DTSW', 'DT', 'DLS', 'DDS', 'ES', 'FO', 'DG', 'WG', 'MO',
             'LI', 'NVS')
#*****************************************************************************************#

#*************************************** load data ***************************************#
# load in trait data
load("/Users/darylyang/Desktop/gdrive/github/sewpen_trait_maps/data/trait_data/NGEEArctic_CHN_LMA.RData")
# extract data collected on seward peninsula
trait.data <- chn_lma_data[which(chn_lma_data$Location == 'Seward_Peninsula'), ]
trait.data$USDA_Species_Code[trait.data$USDA_Species_Code == 'ALVIF'] <- 'ALVI5'

# load in species PFT link table
sps.pft.link.dir <- file.path("/Users/darylyang/Desktop/gdrive/github/sewpen_trait_maps/data/species_pft_table.csv")
sps.pft.link <- read.csv(sps.pft.link.dir)
#*****************************************************************************************#

data.use <- c()
for(row in 1:nrow(trait.data))
{
  row.data <- trait.data[row, ]
  pft.id <- sps.pft.link$acryoname[sps.pft.link$USDA_Plants_Symbol == row.data$USDA_Species_Code]
  if(length(pft.id) == 0)
  {
    pft.id <- NA
  }

  new.data <- data.frame(row.data[1:4], pft.id, row.data[5:ncol(row.data)])
  
  data.use <- rbind(data.use, new.data)
}

outname <- paste("/Users/darylyang/Desktop/gdrive/github/sewpen_trait_maps/data/trait_data/", 'trait_data_combn.csv')
write.csv(data.use, outname, row.names = F)







