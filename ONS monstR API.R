
#Using MonstR API for ONS data

#Set your working directory
setwd('~/../Downloads')

#Installing and loading the monstR package
remotes::install_github("HFAnalyticsLab/monstR", build_vignettes = TRUE )
library(monstR)

##Choosing a dataset

#The dataset id is key. Keywords are present for searching.  

#ons_available_datasets function will list all available datasets. Choose the id which seems most suitable. 
all_dataset_metadata = ons_available_datasets()

#You can query the editions and versions available for your preferred dataset
ons_available_editions(id = "mid-year-pop-est")
ons_available_versions(id = "mid-year-pop-est", edition = "mid-2019-april-2020-geography")


#The following pipeline downloads the chosen dataset by id into your working directory

datasets <- ons_datasets_setup(monstr_pipeline_defaults())
latest <- datasets %>% ons_dataset_by_id("mid-year-pop-est", edition="time-series")

#The following is a hacky workaround to read the csv directly into the R Studio environment. Avoid continued usage of this. 
chosen_dataset = read.csv(latest$downloads[1]$csv$href)

