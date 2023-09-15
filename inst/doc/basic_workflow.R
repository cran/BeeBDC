## ----libraryChunk, load-packages, include=FALSE-------------------------------
# markdown packages
library(rgnparser)
library(magrittr)
library(knitr)
library(rmarkdown)
library(rmdformats)
library(prettydoc)
library(htmltools)
library(pkgdown)

# Load core packages
library(devtools)
library(BiocManager)
library(purrr)
library(here)
library(renv)
library(bdc)
library(CoordinateCleaner)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyselect)
library(R.utils)
library(tidyr)
library(ggplot2)
library(forcats)
library(emld)
library(rlang)
library(xml2)
library(mgsub)
library(rvest)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(janitor)
library(circlize)
library(paletteer)
library(cowplot)
library(igraph)
library(ggspatial)
library(sf)
library(parallel)
library(terra)

# Dont detect cores to avoid GitHbub error
old <- options()         # code line i 
on.exit(options(old))      # code line i+1 
options(mc.cores = parallel::detectCores())

## ----secretRootPath, include=FALSE--------------------------------------------
# Set the RootPath to tempdir
RootPath <- tempdir()
if (!dir.exists(paste0(RootPath, "/Data_acquisition_workflow"))) {
    dir.create(paste0(RootPath, "/Data_acquisition_workflow"), recursive = TRUE)
}

## ----global-options, include=FALSE--------------------------------------------
knitr::opts_chunk$set(error = TRUE, 
                      eval = TRUE, 
                      tidy = TRUE, 
                      warning = FALSE,
                      root.dir = normalizePath(tempdir()))

## ----falseRootPath, eval=FALSE------------------------------------------------
#  RootPath <- paste0("/your/path/here")

## ----CreateRootPath, warning=FALSE, collapse = TRUE---------------------------
  # Create the working directory in the RootPath if it doesn't exist already
if (!dir.exists(paste0(RootPath, "/Data_acquisition_workflow"))) {
    dir.create(paste0(RootPath, "/Data_acquisition_workflow"), recursive = TRUE)
}
  # Set the working directory
setwd(paste0(RootPath,"/Data_acquisition_workflow"))

## ----activate, collapse = TRUE------------------------------------------------
renv::activate(project = paste0(RootPath,"/Data_acquisition_workflow"))

## ----installPackages, message=FALSE, warning=FALSE, results=FALSE, collapse = TRUE, eval = FALSE----
#  if (!require("BiocManager", quietly = TRUE))
#      install.packages("BiocManager", repos = "http://cran.us.r-project.org")
#  
#  BiocManager::install("ComplexHeatmap")

## ----rnaturalearthhires, eval=FALSE-------------------------------------------
#    # Install remotes if needed
#  if (!require("remotes", quietly = TRUE))
#      install.packages("remotes", repos = "http://cran.us.r-project.org")
#    # Download and then load rnaturalearthhires
#  remotes::install_github("ropensci/rnaturalearthhires")
#  install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev", type = "source")
#  library(rnaturalearthhires)

## ----installBeeBDC, results=TRUE, message=TRUE, eval = FALSE, collapse = TRUE----
#  devtools::install_github("https://github.com/jbdorey/BeeBDC.git", ref = "main",
#                          force = FALSE)
#  library(BeeBDC)

## ----snapshot, collapse = TRUE------------------------------------------------
renv::snapshot(project = paste0(RootPath,"/Data_acquisition_workflow"),
                 prompt = FALSE)

## ----dirMaker, collapse = TRUE, eval = FALSE----------------------------------
#  BeeBDC::dirMaker(
#      RootPath = RootPath,
#      RDoc = "vignettes/BeeBDC_main.Rmd") %>%
#        # Add paths created by this function to the environment()
#      list2env(envir = environment())

## ----dirMakerSECRETELY, include = FALSE---------------------------------------
# For the sake of this tutorial, we will not use here::i_am in dirMaker, because we aren't allowed
  # to mess with package directories in this way. This will work-around to use the tempdir()
DataPath <- paste0(RootPath, "/Data_acquisition_workflow")
OutPath_Check <- paste0(RootPath, "/Data_acquisition_workflow/Output/Check")
OutPath_Figures <- paste0(RootPath, "/Data_acquisition_workflow/Output/Figures")
OutPath_Intermediate <- paste0(RootPath, "/Data_acquisition_workflow/Output/Intermediate")
OutPath_Report <- paste0(RootPath, "/Data_acquisition_workflow/Output/Report")
  # Create these files
if (!dir.exists(DataPath)) {
    dir.create(DataPath, recursive = TRUE)}
if (!dir.exists(OutPath_Check)) {
    dir.create(OutPath_Check, recursive = TRUE)}
if (!dir.exists(OutPath_Figures)) {
    dir.create(OutPath_Figures, recursive = TRUE)}
if (!dir.exists(OutPath_Intermediate)) {
    dir.create(OutPath_Intermediate, recursive = TRUE)}
if (!dir.exists(OutPath_Report)) {
    dir.create(OutPath_Report, recursive = TRUE)}

## ----lapply_library, results=FALSE, collapse = TRUE---------------------------
lapply(c("ComplexHeatmap", "magrittr"), 
       library, character.only = TRUE)

## ----2.0----------------------------------------------------------------------
# Load some package data — the taxonomy and a flagged example dataset
  # Download the full beesTaxonomy file
beesTaxonomy <- BeeBDC::beesTaxonomy()
  # Load the example beesFlagged dataset
beesFlagged <- BeeBDC::beesFlagged

selectedGenera <- beesTaxonomy %>%
    # Select only tribe anthophorini (for example)
  dplyr::filter(tolower(tribe) == tolower("anthophorini")) %>%
  distinct(genus)
  
  # Filter the data
taxonData <- beesFlagged %>%
  dplyr::filter(genus %in% selectedGenera$genus)
  # View the data
taxonData

## ----3.0----------------------------------------------------------------------
  # Select your study area
studyArea <- c("Canada", "United states", "Mexico", "Guatemala")
# Filter the data to that area
countryData <- beesFlagged %>%
  dplyr::filter(country %in% studyArea)
  # View the data
countryData

## ----4.1----------------------------------------------------------------------
filteredData <- 
  BeeBDC::summaryFun(data = beesFlagged,
   # Choose the columns to NOT filter (or NULL to filter all columns)
   dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms",
                      ".uncertaintyThreshold"),
    # In the output, do you want to REMOVE all filtering columns (TRUE), or keep them (FALSE)
   removeFilterColumns = TRUE,
   # In the output, do you want to only keep clean data according to your filtering (TRUE),
    # Or keep all data and simply update the .summary column (FALSE)
  filterClean = TRUE) 

## ----4.2----------------------------------------------------------------------
filteredData <- beesFlagged %>%
  # Remove any exiting .uncertaintyThreshold column
  dplyr::select(!tidyselect::any_of(".uncertaintyThreshold")) %>%
    # Chose the coordinate uncertainty to filter to...
  BeeBDC::coordUncerFlagR(data = .,
                  uncerColumn = "coordinateUncertaintyInMeters",
                    # 10 km here
                  threshold = 10000) %>%
    # Now re-do the .summary column and filter the data using this new value
  BeeBDC::summaryFun(
  data = .,
  dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms"),
  removeFilterColumns = TRUE,
  filterClean = TRUE)

## ----4.2a---------------------------------------------------------------------
filteredData <- beesFlagged %>%
    # Remove any exisitng .year_outOfRange column
  dplyr::select(!".year_outOfRange") %>%
    # Chose the minimum year to filter to...
  bdc::bdc_year_outOfRange(data = .,
                           eventDate = "year",
                           year_threshold = 1970) %>%
    # Now re-do the .summary column and filter the data using this new value
  BeeBDC::summaryFun(
    data = .,
    dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms",
                        ".uncertaintyThreshold"),
    removeFilterColumns = TRUE,
    filterClean = TRUE)

## ----4.2b---------------------------------------------------------------------
filteredData <- 
  # The input dataset
  beesFlagged %>%
  # Chose the year range...
  dplyr::filter(year > 1950 & year < 1970) %>%
  # Now re-do the .summary column and filter the data using this new value
  BeeBDC::summaryFun(
    # Select the input dataset to filter
    data = .,
    # Choose the columns to NOT filter (or NULL to filter all columns)
    dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms",
                        ".uncertaintyThreshold"),
    # In the output, do you want to REMOVE all filtering columns (TRUE), or keep them (FALSE)
    removeFilterColumns = TRUE,
    # In the output, do you want to only keep clean data according to your filtering (TRUE),
    # Or keep all data and simply update the .summary column (FALSE)
    filterClean = TRUE)

## ----5.1, eval = FALSE--------------------------------------------------------
#  if(!require("BiocManager", quietly = TRUE)){
#    install.packages("BiocManager")}
#  BiocManager::install("ComplexHeatmap", force = TRUE)
#  renv::snapshot()

## ----5.1ii, eval = FALSE------------------------------------------------------
#  duplicates <- fileFinder(path = "PATH TO A FOLDER CONTAINING THE duplicateRun_ — could be supp. materials folder",
#                            fileName = "duplicateRun_") %>%
#    readr::read_csv() %>%
#    # Select only the stingless bee data
#    dplyr::filter(database_id %in% stinglessData$database_id |
#                    database_id_match %in% stinglessData$database_id)

## ----5.1on.exit, include = FALSE----------------------------------------------
oldpar <- par(no.readonly = TRUE) 
on.exit(oldpar)

## ----5.1iii, eval = FALSE-----------------------------------------------------
#  # Choose the global figure parameters
#    par(mar = c(2, 2, 2, 2)/2, mfrow = c(1,1))
#  
#  # Create the chorDiagram. You can leave many of the below values out but we show here
#  # the defaults
#  
#  BeeBDC::chordDiagramR(
#    # The duplicate data from the dupeSummary function output
#    dupeData = duplicates,
#    outPath = OutPath_Figures,
#    fileName = "ChordDiagram.pdf",
#    # These can be modified to help fit the final pdf that's exported.
#    width = 9,
#    height = 7.5,
#    bg = "white",
#    # How few distinct dataSources should a group have to be listed as "other"
#    smallGrpThreshold = 3,
#    title = "Duplicated record sources",
#    # The default list of colour palettes to choose from usign the paleteer package
#    palettes = c("cartography::blue.pal", "cartography::green.pal",
#                 "cartography::sand.pal", "cartography::orange.pal", "cartography::red.pal",
#                 "cartography::purple.pal", "cartography::brown.pal"),
#    canvas.ylim = c(-1.0,1.0),
#    canvas.xlim = c(-0.6, 0.25),
#    text.col = "black",
#    legendX = grid::unit(6, "mm"),
#    legendY = grid::unit(18, "mm"),
#    legendJustify = c("left", "bottom"),
#    niceFacing = TRUE)

## ----5.2----------------------------------------------------------------------
data("beesFlagged", package = "BeeBDC")

# Create a figure shoring the total number of duplicates, kept duplicates, and unique
# records for each datasource (simplified to the text before the first underscore) and
# the proportion of the above for each data source
BeeBDC::dupePlotR(
  data = beesFlagged,
  # The outPath to save the plot as
  outPath = tempdir(),
  fileName = "Fig3_duplicatePlot.pdf",
  # Colours in order: duplicate, kept duplicate, unique
  dupeColours = c("#F2D2A2","#B9D6BC", "#349B90"),
  # Plot size and height
  base_height = 7, base_width = 7,
  legend.position = c(0.85, 0.8),
  # Extra variables can be fed into forcats::fct_recode() to change names on plot
  GBIF = "GBIF", SCAN = "SCAN", iDigBio = "iDigBio", USGS = "USGS", ALA = "ALA", 
  ASP = "ASP",
  returnPlot = TRUE
)

## ----5.3b---------------------------------------------------------------------
# Visualise all flags for each dataSource (simplified to the text before the first underscore)
  # A clever user might also realise the potential to summarise and produce outputs in other columns
BeeBDC::plotFlagSummary(
  # WARNING: alternate path if wanting to produce figures for the selected taxonData (2.0 above)
  # Select only the taxonData data
  data = beesFlagged,
  # Colours in order of pass (TRUE), fail (FALSE), and NA
  flagColours = c("#127852", "#A7002D", "#BDBABB"),
  fileName = paste0("FlagsPlot_Amell", Sys.Date(),".pdf"),
  outPath = tempdir(),
  width = 15, height = 9,
  # OPTIONAL:
         #  # Filter to species
           speciesName = "Apis mellifera Linnaeus, 1758",
             # column to look in
           nameColumn = "scientificName",
           # Save the filtered data
           saveFiltered = FALSE,
     # Filter column to display on map
           filterColumn = ".summary",
           plotMap = TRUE,
       # amount to jitter points if desired, e.g. 0.25 or NULL
     jitterValue = NULL,
       # Map opacity value for points between 0 and 1
     mapAlpha = 1,
  returnPlot = TRUE,
  # Extra variables can be fed into forcats::fct_recode() to change names on plot
  GBIF = "GBIF", SCAN = "SCAN", iDigBio = "iDigBio", USGS = "USGS", ALA = "ALA", 
  ASP = "ASP", CAES = "CAES", 'B. Mont.' = "BMont", 'B. Minkley' = "BMin", Ecd = "Ecd",
  Gaiarsa = "Gai", EPEL = "EPEL"
)

## ----5.4----------------------------------------------------------------------
BeeBDC::summaryMaps(
  data = beesFlagged,
  width = 10, height = 10,
  class_n = 3,
  class_Style = "jenks",
  outPath = tempdir(),
  fileName = "CountryMaps_jenks.pdf",
  returnPlot = TRUE
)

## ----6.0, eval = FALSE--------------------------------------------------------
#  mapData %>%
#    readr::write_excel_csv(paste0(DataPath, "/Output/Intermediate/", "cleanTaxon_",
#                            Sys.Date(), ".csv"))

## ----cleanup, include=FALSE, collapse = TRUE----------------------------------
# Remove the webpage folder
unlink(paste0(dirname(getwd()), "/inst/extdata/WebDir"), recursive = TRUE)

