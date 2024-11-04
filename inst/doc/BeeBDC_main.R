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
#  install.packages("BeeBDC")
#  library(BeeBDC)

## ----snapshot, collapse = TRUE------------------------------------------------
renv::snapshot(project = paste0(RootPath,"/Data_acquisition_workflow"),
                 prompt = FALSE)

## ----dirMaker, collapse = TRUE, eval = FALSE----------------------------------
#  BeeBDC::dirMaker(
#      RootPath = RootPath,
#      RDoc = "vignettes/BeeBDC_main.Rmd") %>%
#        # Add paths created by this function to the environment()
#      list2env(envir = parent.env(environment()))

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

## ----3.0, collapse = TRUE-----------------------------------------------------
data("bees3sp", package = "BeeBDC")
data("beesRaw", package = "BeeBDC")
db_standardized <- dplyr::bind_rows(beesRaw, 
                                      # Only keep a subset of columns from bees3sp
                             bees3sp %>% dplyr::select(tidyselect::all_of(colnames(beesRaw)), countryCode))

## ----3.1, collapse = TRUE-----------------------------------------------------
check_pf <- bdc::bdc_scientificName_empty(
  data = db_standardized,
  sci_name = "scientificName")
  # now that this is saved, remove it to save space in memory
rm(db_standardized) 

## ----3.2, collapse = TRUE-----------------------------------------------------
check_pf <- bdc::bdc_coordinates_empty(
  data = check_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")

## ----3.3, collapse = TRUE-----------------------------------------------------
check_pf <- bdc::bdc_coordinates_outOfRange(
  data = check_pf,
  lat = "decimalLatitude",
  lon = "decimalLongitude")

## ----3.4, collapse = TRUE-----------------------------------------------------
check_pf <- bdc::bdc_basisOfRecords_notStandard(
  data = check_pf,
  basisOfRecord = "basisOfRecord",
  names_to_keep = c(
    # Keep all plus some at the bottom.
    "Event",
    "HUMAN_OBSERVATION",
    "HumanObservation",
    "LIVING_SPECIMEN",
    "LivingSpecimen",
    "MACHINE_OBSERVATION",
    "MachineObservation",
    "MATERIAL_SAMPLE",
    "O",
    "Occurrence",
    "MaterialSample",
    "OBSERVATION",
    "Preserved Specimen",
    "PRESERVED_SPECIMEN",
    "preservedspecimen Specimen",
    "Preservedspecimen",
    "PreservedSpecimen",
    "preservedspecimen",
    "S",
    "Specimen",
    "Taxon",
    "UNKNOWN",
    "",
    NA,
    "NA",
    "LITERATURE", 
    "None", "Pinned Specimen", "Voucher reared", "Emerged specimen"
  ))

## ----3.5a, collapse = TRUE----------------------------------------------------
check_pf_noNa <- BeeBDC::countryNameCleanR(
  data = check_pf,
    # Create a Tibble of common issues in country names and their replacements
  commonProblems = dplyr::tibble(problem = c('U.S.A.', 'US','USA','usa','UNITED STATES',
                                              'United States','U.S.A','MX','CA','Bras.','Braz.',
                                              'Brasil','CNMI','USA TERRITORY: PUERTO RICO'),
                                  fix = c('United States of America','United States of America',
                                          'United States of America','United States of America',
                                          'United States of America','United States of America',
                                          'United States of America','Mexico','Canada','Brazil',
                                          'Brazil','Brazil','Northern Mariana Islands','PUERTO.RICO'))
  )

## ----3.5b, message=FALSE, warning=FALSE, collapse = TRUE----------------------
suppressWarnings(
  countryOutput <- BeeBDC::jbd_CfC_chunker(data = check_pf_noNa,
                                   lat = "decimalLatitude",
                                   lon = "decimalLongitude",
                                   country = "country",
                                    # How many rows to process at a time
                                   stepSize = 1000000,
                                    # Start row
                                   chunkStart = 1,
                                   path = OutPath_Intermediate,
                                    # Normally, please use scale = "large"
                                   scale = "medium",
                                   mc.cores = 1),
  classes = "warning")

## ----3.5ci, collapse = TRUE---------------------------------------------------
check_pf <- dplyr::left_join(check_pf, 
                             countryOutput, 
                             by = "database_id",
                             suffix = c("", "CO"))  %>% 
    # Take the new country name if the original is NA
  dplyr::mutate(country = dplyr::if_else(is.na(country),
                                         countryCO,
                                         country)) %>%
    # Remove duplicates if they arose from left_join!
  dplyr::distinct()

## ----3.5cii, eval = FALSE, collapse = TRUE------------------------------------
#  check_pf %>%
#    readr::write_excel_csv(.,
#                     paste(OutPath_Intermediate, "01_prefilter_database.csv",
#                           sep = "/"))

## ----3.5ciii, eval = FALSE, collapse = TRUE-----------------------------------
#  if(!exists("check_pf")){
#  check_pf <- readr::read_csv(paste(DataPath,
#               "Output", "Intermediate", "01_prefilter_database.csv", sep = "/"),
#               col_types = BeeBDC::ColTypeR())}

## ----3.5civ, collapse = TRUE--------------------------------------------------
rm(check_pf_noNa, countryOutput)

## ----3.6, collapse = TRUE-----------------------------------------------------
  # Standardise country names and add ISO2 codes if needed
check_pf <- bdc::bdc_country_standardized(
  # Remove the countryCode and country_suggested columns to avoid an error with 
    # where two "countryCode" and "country_suggested" columns exist (i.e. if the dataset has been  
    # run before)
  data = check_pf %>% dplyr::select(!tidyselect::any_of(c("countryCode", "country_suggested"))),
  country = "country"
) 

## ----3.7, message=FALSE, warning=FALSE, collapse = TRUE-----------------------
check_pf <- BeeBDC::jbd_Ctrans_chunker(
  # bdc_coordinates_transposed inputs
  data = check_pf,
  id = "database_id",
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  country = "country",
  countryCode = "countryCode",
  border_buffer = 0.2, # in decimal degrees (~22 km at the equator)
  save_outputs = TRUE,
  sci_names = "scientificName",
  # chunker inputs
  stepSize = 1000000,  # How many rows to process at a time
  chunkStart = 1,  # Start row
  append = FALSE,  # If FALSE it may overwrite existing dataset
  progressiveSave = FALSE,
    # In a normal run, please use scale = "large"
  scale = "medium",
  path = OutPath_Check,
  mc.cores = 1
) 

## ----3.7ii, eval = FALSE, collapse = TRUE-------------------------------------
#  table(check_pf$coordinates_transposed, useNA = "always")

## ----3.7iii, eval = FALSE, collapse = TRUE------------------------------------
#  check_pf %>%
#    readr::write_excel_csv(.,
#                     paste(OutPath_Intermediate, "01_prefilter_database.csv",
#                           sep = "/"))

## ----3.7iv, eval = FALSE, collapse = TRUE-------------------------------------
#  if(!exists("check_pf")){
#    check_pf <- readr::read_csv(paste(OutPath_Intermediate, "01_prefilter_database.csv",
#                                      sep = "/"), col_types = BeeBDC::ColTypeR())}

## ----3.8, collapse = TRUE-----------------------------------------------------
check_pf <- BeeBDC::jbd_coordCountryInconsistent(
  data = check_pf,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  scale = 50,
  pointBuffer = 0.01)

## ----3.8ii, eval = FALSE------------------------------------------------------
#  check_pf %>%
#    readr::write_excel_csv(.,
#                     paste(OutPath_Intermediate, "01_prefilter_database.csv",
#                           sep = "/"))

## ----3.9, eval = TRUE, collapse = TRUE----------------------------------------
xyFromLocality <- bdc::bdc_coordinates_from_locality(
  data = check_pf,
  locality = "locality",
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  save_outputs = FALSE
) 

## ----3.9ii, eval = FALSE------------------------------------------------------
#  # Save the resultant data
#    xyFromLocality %>% readr::write_excel_csv(paste(OutPath_Check, "01_coordinates_from_locality.csv",
#                           sep = "/"))

## ----3.9iii, eval = FALSE-----------------------------------------------------
#  rm(xyFromLocality)

## ----3.10, collapse = TRUE----------------------------------------------------
check_pf <- BeeBDC::flagAbsent(data = check_pf,
                   PresAbs = "occurrenceStatus")

## ----3.11, collapse = TRUE----------------------------------------------------
check_pf <- BeeBDC::flagLicense(data = check_pf,
                    strings_to_restrict = "all",
                    # DON'T flag if in the following dataSource(s)
                    excludeDataSource = NULL)

## ----3.12, collapse = TRUE----------------------------------------------------
check_pf <- BeeBDC::GBIFissues(data = check_pf, 
                   issueColumn = "issue", 
                   GBIFflags = c("COORDINATE_INVALID", "ZERO_COORDINATE")) 

## ----3.13a, eval = FALSE------------------------------------------------------
#  flagFile <- BeeBDC::flagRecorder(
#    data = check_pf,
#    outPath = paste(OutPath_Report, sep =""),
#    fileName = paste0("flagsRecorded_", Sys.Date(),  ".csv"),
#      # These are the columns that will be kept along with the flags
#    idColumns = c("database_id", "id", "catalogNumber", "occurrenceID", "dataSource"),
#      # TRUE if you want to find a file from a previous part of the script to append to
#    append = FALSE)
#  

## ----3.13b, collapse = TRUE---------------------------------------------------
check_pf <- BeeBDC::summaryFun(
  data = check_pf,
    # Don't filter these columns (or NULL)
  dontFilterThese = NULL,
    # Remove the filtering columns?
  removeFilterColumns = FALSE,
    # Filter to ONLY cleaned data?
  filterClean = FALSE)

## ----3.13c, eval = FALSE------------------------------------------------------
#  (report <- bdc::bdc_create_report(data = check_pf,
#                                    database_id = "database_id",
#                                    workflow_step = "prefilter",
#                                    save_report = TRUE)
#  )

## ----3.14, eval = FALSE-------------------------------------------------------
#  check_pf %>%
#    readr::write_excel_csv(., paste(OutPath_Intermediate, "01_prefilter_output.csv",
#                              sep = "/"))

## ----4.0, collapse = TRUE-----------------------------------------------------
if(!exists("check_pf")){
database <-
  readr::read_csv( paste(OutPath_Intermediate, "01_prefilter_output.csv",
                         sep = "/"), col_types = BeeBDC::ColTypeR())
}else{
    # OR rename and remove
  database <- check_pf
  # Remove spent dataset
  rm(check_pf)}

## ----4.0ii, collapse = TRUE---------------------------------------------------
			database <- database %>%
			  dplyr::select(!tidyselect::any_of("names_clean"))

## ----4.1, eval = FALSE, collapse = TRUE---------------------------------------
#  parse_names <-
#    bdc::bdc_clean_names(sci_names = database$scientificName, save_outputs = FALSE)

## ----4.1ii, collapse = TRUE, eval = FALSE-------------------------------------
#  parse_names <-
#    parse_names %>%
#    dplyr::select(.uncer_terms, names_clean)

## ----4.1iii, collapse = TRUE--------------------------------------------------
database <- dplyr::bind_cols(database)
rm(parse_names)

## ----4.2, collapse = TRUE, eval = FALSE---------------------------------------
#  taxonomyFile <- BeeBDC::beesTaxonomy()

## ----4.2secret, collapse=TRUE, include=FALSE----------------------------------
  # load in the small test dataset in the background
system.file("extdata", "testTaxonomy.rda", package="BeeBDC") |>
  load()
  # Rename the file
taxonomyFile <- testTaxonomy
rm(testTaxonomy)

## ----4.2ii, collapse = TRUE---------------------------------------------------
database <- BeeBDC::harmoniseR(path = DataPath, #The path to a folder that the output can be saved
                       taxonomy = taxonomyFile, # The formatted taxonomy file
                       data = database,
                       mc.cores = 1)

## ----4.2iii, collapse = TRUE--------------------------------------------------
rm(taxonomyFile)

## ----4.2iv, eval = FALSE, collapse = TRUE-------------------------------------
#  database %>%
#    readr::write_excel_csv(.,
#                     paste(DataPath, "Output", "Intermediate", "02_taxonomy_database.csv",
#                           sep = "/"))

## ----4.3, eval = FALSE, collapse = TRUE---------------------------------------
#  flagFile <- BeeBDC::flagRecorder(
#    data = database,
#    outPath = paste(OutPath_Report, sep =""),
#    fileName = paste0("flagsRecorded_", Sys.Date(),  ".csv"),
#    idColumns = c("database_id", "id", "catalogNumber", "occurrenceID", "dataSource"),
#    append = TRUE,
#    printSummary = TRUE)

## ----5.0, collapse = TRUE-----------------------------------------------------
if(!exists("database")){
database <-
  readr::read_csv(paste(OutPath_Intermediate, "02_taxonomy_database.csv", sep = "/"),
                  col_types = BeeBDC::ColTypeR())}

## ----5.1, collapse = TRUE-----------------------------------------------------
check_space <-
  BeeBDC::jbd_coordinates_precision(
    data = database,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    ndec = 2 # number of decimals to be tested
  )

## ----5.1ii, collapse = TRUE---------------------------------------------------
rm(database)

## ----5.1iii, eval = FALSE, collapse = TRUE------------------------------------
#  check_space %>%
#    readr::write_excel_csv(.,
#                     paste(OutPath_Intermediate, "03_space_inter_database.csv",
#                           sep = "/"))

## ----5.2, eval = FALSE, collapse = TRUE---------------------------------------
#  tempSpace <- check_space %>%
#    dplyr::filter(!.coordinates_empty == FALSE) %>%
#    dplyr::filter(!.coordinates_outOfRange == FALSE)

## ----5.2ii, message=TRUE, warning=FALSE, eval = FALSE, collapse = TRUE--------
#  tempSpace <-
#    CoordinateCleaner::clean_coordinates(
#      x =  tempSpace,
#      lon = "decimalLongitude",
#      lat = "decimalLatitude",
#      species = "scientificName",
#      countries = NULL, # Tests if coords are from x country. This is not needed.
#      tests = c(
#        "capitals",     # records within 0.5 km of capitals centroids
#        "centroids",    # records within 1 km around country and province centroids
#        "equal",      # records with equal coordinates
#        "gbif",         # records within 1 km of GBIF headquarters. (says 1 degree in package, but code says 1000 m)
#        "institutions", # records within 100m of zoo and herbaria
#        "zeros"       # records with coordinates 0,0
#        # "seas"        # Not flagged as this should be flagged by coordinate country inconsistent
#      ),
#      capitals_rad = 1000,
#      centroids_rad = 500,
#      centroids_detail = "both", # test both country and province centroids
#      inst_rad = 100, # remove zoo and herbaria within 100m
#      range_rad = 0,
#      zeros_rad = 0.5,
#      capitals_ref = NULL,
#      centroids_ref = NULL,
#      country_ref = NULL,
#      country_refcol = "countryCode",
#      inst_ref = NULL,
#      range_ref = NULL,
#      # seas_scale = 50,
#      value = "spatialvalid" # result of tests are appended in separate columns
#    ) %>%
#        # Remove duplicate .summary column that can be replaced later and turn into a tibble
#    dplyr::select(!tidyselect::starts_with(".summary")) %>%
#    dplyr::tibble()

## ----5.2iii, eval = FALSE, collapse = TRUE------------------------------------
#  check_space <- tempSpace %>%
#    # Re-bind with the records that were removed earlier
#    dplyr::bind_rows(check_space %>%
#                       dplyr::filter(.coordinates_empty == FALSE |
#                                       .coordinates_outOfRange == FALSE) )

## ----5.2iv, eval = FALSE, collapse = TRUE-------------------------------------
#  rm(tempSpace)

## ----5.2v, eval = FALSE, collapse = TRUE--------------------------------------
#  check_space %>%
#    readr::write_excel_csv(paste(OutPath_Intermediate, "03_space_inter_database.csv",
#                           sep = "/"))

## ----5.3, collapse = TRUE-----------------------------------------------------
check_space <- BeeBDC::diagonAlley(
  data = check_space,
  # The minimum number of repeats needed to find a sequence in for flagging
  minRepeats = 6,
  ndec = 3,
  groupingColumns = c("eventDate", "recordedBy", "datasetName"),
  mc.cores = 1)

## ----5.3ii, collapse = TRUE---------------------------------------------------
griddingDF <- check_space %>%
  # Exclude NA lat and lon values
  tidyr::drop_na(c("decimalLatitude", "decimalLongitude")) %>%
  # Group by the dataset name
  dplyr::group_by(datasetName) %>%
  # Remove rows that aren't unique for lat and long
  dplyr::distinct(decimalLongitude, decimalLatitude,
                  .keep_all = TRUE) %>%
  # Find the groups with 4 or more occurrence records 
  dplyr::filter(dplyr::n() >= 4) %>%
  dplyr::ungroup()


## ----5.3iii, eval = FALSE, collapse = TRUE------------------------------------
#  gridded_datasets <- CoordinateCleaner::cd_round(
#    x = griddingDF,
#    lon = "decimalLongitude",
#    lat = "decimalLatitude",
#    ds = "datasetName",
#    T1 = 7,
#    min_unique_ds_size = 4,
#    test = "both",
#    value = "dataset",
#    graphs = FALSE,
#    verbose = TRUE,
#    reg_out_thresh = 2,
#    reg_dist_min = 0.1,
#    reg_dist_max = 2
#  ) %>%
#    dplyr::tibble()
#  # The griddingDF is no longer needed. remove it.
#  rm(griddingDF)

## ----5.3iv, eval = FALSE, collapse = TRUE-------------------------------------
#  check_space <- check_space %>%
#    # Join the datasets
#    dplyr::left_join(
#      # Select the columns of interest
#      dplyr::select(gridded_datasets, dataset, lon.flag, lat.flag, summary),
#      by = c("datasetName" = "dataset")) %>%
#    # Make new columns with more-consistent naming and change the NA vlaues to = TRUE (not flagged)
#    dplyr::mutate(.lonFlag = tidyr::replace_na(lon.flag, TRUE),
#                  .latFlag = tidyr::replace_na(lat.flag, TRUE),
#                  .gridSummary = tidyr::replace_na(summary, TRUE)) %>%
#    # Remove old columns
#    dplyr::select(!c(lon.flag, lat.flag, summary))

## ----5.3 v, eval = FALSE, collapse = TRUE-------------------------------------
#  gridded_datasets %>%
#    readr::write_excel_csv(paste(OutPath_Intermediate, "03_space_griddedDatasets.csv",
#                           sep = "/"))

## ----5.3vi, eval = FALSE, collapse = TRUE-------------------------------------
#  rm(gridded_datasets)

## ----5.4, collapse = TRUE-----------------------------------------------------
check_space <- BeeBDC::coordUncerFlagR(data = check_space,
                               uncerColumn = "coordinateUncertaintyInMeters",
                               threshold = 1000)

## ----5.5, collapse = TRUE, eval = FALSE---------------------------------------
#  checklistFile <- BeeBDC::beesChecklist()

## ----5.5secret, collapse = TRUE, eval = TRUE----------------------------------
  # load in the small test dataset in the background
system.file("extdata", "testChecklist.rda", package="BeeBDC") |>
  load()
  # Rename the file
taxonomyFile <- testChecklist
rm(testChecklist)

## ----5.5iia, collapse = TRUE--------------------------------------------------
check_space <- BeeBDC::countryOutlieRs(checklist = checklistFile,
                        data = check_space,
                        keepAdjacentCountry = TRUE,
                        pointBuffer = 0.05,
                          # Scale of map to return, one of 110, 50, 10 OR 'small', 'medium', 'large'
                          # Smaller numbers will result in much longer calculation times. 
                          # We have not attempted a scale of 10.
                        scale = 50,
                        mc.cores = 1)

## ----5.5iib, collapse = TRUE--------------------------------------------------
check_space <- BeeBDC::continentOutlieRs(checklist = checklistFile,
                        data = check_space,
                        keepAdjacentContinent = FALSE,
                        pointBuffer = 0.05,
                          # Scale of map to return, one of 110, 50, 10 OR 'small', 'medium', 'large'
                          # Smaller numbers will result in much longer calculation times. 
                          # We have not attempted a scale of 10.
                        scale = 50,
                        mc.cores = 1)

## ----5.5iii, eval = FALSE, collapse = TRUE------------------------------------
#    # A list of failed species-country combinations and their numbers can be output here
#  check_space %>%
#    dplyr::filter(.countryOutlier == FALSE) %>%
#    dplyr::select(database_id, scientificName, country) %>%
#    dplyr::group_by(scientificName) %>%
#    dplyr::mutate(count_scientificName = n()) %>%
#    dplyr::distinct(scientificName, country, .keep_all = TRUE) %>%
#    readr::write_excel_csv(paste(OutPath_Intermediate, "03_space_failedCountryChecklist.csv",
#                           sep = "/"))

## ----5.6, eval = FALSE, collapse = TRUE---------------------------------------
#  check_space <- BeeBDC::summaryFun(
#    data = check_space,
#    dontFilterThese = NULL,
#    removeFilterColumns = FALSE,
#    filterClean = FALSE)

## ----5.6ii, eval = FALSE, collapse = TRUE-------------------------------------
#  check_space %>%
#    dplyr::filter(.summary == FALSE) %>% # map only records flagged as FALSE
#    bdc::bdc_quickmap(
#      data = .,
#      lon = "decimalLongitude",
#      lat = "decimalLatitude",
#      col_to_map = ".summary",
#      size = 0.9
#    )
#  

## ----5.7, eval = FALSE, collapse = TRUE---------------------------------------
#  (report <-
#     bdc::bdc_create_report(
#       data = dplyr::tibble(check_space %>% dplyr::select(!.uncer_terms)),
#       database_id = "database_id",
#       workflow_step = "space",
#       save_report = TRUE)
#  )

## ----5.8, eval = FALSE, collapse = TRUE---------------------------------------
#  (figures <-
#      BeeBDC::jbd_create_figures(
#        data = dplyr::tibble(check_space %>% dplyr::select(!.uncer_terms)),
#        path = DataPath,
#        database_id = "database_id",
#        workflow_step = "space",
#        save_figures = TRUE)
#  )

## ----5.8ii, eval = FALSE, collapse = TRUE-------------------------------------
#  check_space %>%
#    readr::write_excel_csv(paste(OutPath_Intermediate, "03_space_inter_database.csv",
#                           sep = "/"))

## ----5.9, eval = FALSE, collapse = TRUE---------------------------------------
#  BeeBDC::flagRecorder(
#    data = check_space,
#    outPath = paste(OutPath_Report, sep =""),
#    fileName = paste0("flagsRecorded_", Sys.Date(),  ".csv"),
#    idColumns = c("database_id", "id", "catalogNumber", "occurrenceID", "dataSource"),
#    append = TRUE,
#    printSummary = TRUE)

## ----5.10, eval = FALSE, collapse = TRUE--------------------------------------
#  check_space %>%
#    readr::write_excel_csv(.,
#                     paste(OutPath_Intermediate, "03_space_database.csv",
#                           sep = "/"))

## ----6.0, collapse = TRUE-----------------------------------------------------
if(!exists("check_space")){
  check_time <-
    readr::read_csv(paste(OutPath_Intermediate, "03_space_database.csv", sep = "/"),
                    col_types = BeeBDC::ColTypeR())
  }else{
  check_time <- check_space
      # Remove the spent file
  rm(check_space)}

## ----6.0ii, collapse = TRUE---------------------------------------------------
hist(lubridate::ymd_hms(check_time$eventDate, truncated = 5), breaks = 20,
     main = "Histogram of eventDates")

## ----6.0iii, collapse = TRUE--------------------------------------------------
check_time$year <- ifelse(check_time$year > lubridate::year(Sys.Date()) | check_time$year < 1600,
                        NA, check_time$year)
check_time$month <- ifelse(check_time$month > 12 | check_time$month < 1,
                         NA, check_time$month)
check_time$day <- ifelse(check_time$day > 31 | check_time$day < 1,
                       NA, check_time$day)

## ----6.1, collapse = TRUE-----------------------------------------------------
check_time <- BeeBDC::dateFindR(data = check_time,
                        # Years above this are removed (from the recovered dates only)
                        maxYear = lubridate::year(Sys.Date()),
                        # Years below this are removed (from the recovered dates only)
                        minYear = 1700)

## ----6.2, collapse = TRUE-----------------------------------------------------
check_time <-
  bdc::bdc_eventDate_empty(data = check_time, eventDate = "eventDate")

## ----6.3, collapse = TRUE-----------------------------------------------------
check_time <-
  bdc::bdc_year_outOfRange(data = check_time,
                           eventDate = "year",
                           year_threshold = 1950)

## ----6.4, eval = TRUE, collapse = TRUE----------------------------------------
check_time <- BeeBDC::summaryFun(
  data = check_time,
  # Don't filter these columns (or NULL)
  dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms"),
  # Remove the filtering columns?
  removeFilterColumns = FALSE,
  # Filter to ONLY cleaned data?
  filterClean = FALSE)


## ----6.4ii, eval = FALSE, collapse = TRUE-------------------------------------
#  ( report <-
#      bdc::bdc_create_report(data = check_time,
#                             database_id = "database_id",
#                             workflow_step = "time",
#                             save_report = FALSE)
#  )

## ----6.5, eval = FALSE, collapse = TRUE---------------------------------------
#  figures <-
#    BeeBDC::jbd_create_figures(data = check_time,
#                       path = DataPath,
#                       database_id = "database_id",
#                       workflow_step = "time",
#                       save_figures = TRUE)

## ----6.5ii, eval = FALSE, collapse = TRUE-------------------------------------
#  figures$year

## ----6.5iii, eval = FALSE, collapse = TRUE------------------------------------
#  check_time %>%
#    readr::write_excel_csv(.,
#                     paste(OutPath_Intermediate, "04_time_database.csv",
#                           sep = "/"))

## ----eval = FALSE, collapse = TRUE--------------------------------------------
#  BeeBDC::flagRecorder(
#    data = check_time,
#    outPath = paste(OutPath_Report, sep =""),
#    fileName = paste0("flagsRecorded_", Sys.Date(),  ".csv"),
#    idColumns = c("database_id", "id", "catalogNumber", "occurrenceID", "dataSource"),
#    append = TRUE,
#    printSummary = TRUE)
#  

## ----7.0, eval = FALSE, collapse = TRUE---------------------------------------
#  if(!exists("check_time")){
#    check_time <-
#      readr::read_csv(paste(OutPath_Intermediate, "04_time_database.csv",
#                            sep = "/"),
#                      col_types = BeeBDC::ColTypeR())}

## ----7.1, collapse = TRUE-----------------------------------------------------
check_time <- BeeBDC::dupeSummary(
  data = check_time,
  path = OutPath_Report,
   # options are "ID","collectionInfo", or "both"
  duplicatedBy = "collectionInfo", 
    # The columns to generate completeness info from (and to sort by completness)
  completeness_cols = c("decimalLatitude",  "decimalLongitude",
                        "scientificName", "eventDate"),
   # The columns to ADDITIONALLY consider when finding duplicates in collectionInfo
  collectionCols = c("decimalLatitude", "decimalLongitude", "scientificName", "eventDate", 
                     "recordedBy"),
    # The columns to combine, one-by-one with the collectionCols
  collectInfoColumns = c("catalogNumber", "otherCatalogNumbers"),
    # Custom comparisons — as a list of columns to compare
     # RAW custom comparisons do not use the character and number thresholds
  CustomComparisonsRAW = dplyr::lst(c("catalogNumber", "institutionCode", "scientificName")),
     # Other custom comparisons use the character and number thresholds
  CustomComparisons = dplyr::lst(c("gbifID", "scientificName"),
                                  c("occurrenceID", "scientificName"),
                                  c("recordId", "scientificName"),
                                  c("id", "scientificName")),
   # The order in which you want to KEEP duplicated based on data source
   # try unique(check_time$dataSource)
  sourceOrder = c("CAES", "Gai", "Ecd","BMont", "BMin", "EPEL", "ASP", "KP", "EcoS", "EaCO",
                  "FSCA", "Bal", "SMC", "Lic", "Arm",
                  "USGS", "ALA", "VicWam", "GBIF","SCAN","iDigBio"),
    # Paige ordering is done using the database_id prefix, not the dataSource prefix.
  prefixOrder = c("Paige", "Dorey"),
    # Set the complexity threshold for id letter and number length
     # minimum number of characters when WITH the numberThreshold
  characterThreshold = 2,
     # minimum number of numbers when WITH the characterThreshold
  numberThreshold = 3,
     # Minimum number of numbers WITHOUT any characters
  numberOnlyThreshold = 5
) %>% # END dupeSummary
  dplyr::as_tibble(col_types = BeeBDC::ColTypeR())

## ----7.1ii, eval = FALSE, collapse = TRUE-------------------------------------
#  check_time %>%
#    readr::write_excel_csv(.,
#                     paste(OutPath_Intermediate, "04_2_dup_database.csv",
#                           sep = "/"))

## ----7.2, eval = FALSE, collapse = TRUE---------------------------------------
#  BeeBDC::flagRecorder(
#    data = check_time,
#    outPath = paste(OutPath_Report, sep =""),
#    fileName = paste0("flagsRecorded_", Sys.Date(),  ".csv"),
#    idColumns = c("database_id", "id", "catalogNumber", "occurrenceID", "dataSource"),
#    append = TRUE,
#    printSummary = TRUE)

## ----8.0, eval = FALSE, collapse = TRUE---------------------------------------
#  if(!exists("check_time")){
#   check_time <-
#     readr::read_csv(paste(OutPath_Intermediate, "04_2_dup_database.csv",
#                            sep = "/"), col_types = ColTypeR())}

## ----8.1, eval = TRUE, collapse = TRUE----------------------------------------
if(!exists("duplicates")){
  duplicates <- BeeBDC::fileFinder(path = DataPath,
                            fileName = "duplicateRun_") %>%
    readr::read_csv()}

## ----8.2, eval = TRUE, collapse = TRUE----------------------------------------
  # Make sure that the .summary column is updated
check_time <- BeeBDC::summaryFun(
  data = check_time,
  dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms",
                      ".uncertaintyThreshold"),
  removeFilterColumns = FALSE,
  filterClean = FALSE)

## ----8.2ii, eval = FALSE, collapse = TRUE-------------------------------------
#    # Save the uncleaned dataset
#  check_time %>% readr::write_excel_csv(.,
#                                  paste(OutPath_Intermediate, "05_unCleaned_database.csv",
#                                        sep = "/"))

## ----8.3, eval = TRUE, collapse = TRUE----------------------------------------
cleanData <- BeeBDC::summaryFun(
  data = check_time,
  dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms",
                      ".uncertaintyThreshold"),
  # Remove the filtering columns?
  removeFilterColumns = TRUE,
  # Filter to ONLY cleaned data?
  filterClean = TRUE)

## ----8.3ii, eval = FALSE, collapse = TRUE-------------------------------------
#  # Save this CLEANED dataset
#   cleanData %>% readr::write_excel_csv(.,
#                     paste(OutPath_Intermediate, "05_cleaned_database.csv",
#                           sep = "/"))

## ----9.1, message=FALSE, warning=FALSE, eval = FALSE, collapse = TRUE---------
#  if (!require("BiocManager", quietly = TRUE))
#    install.packages("BiocManager", repos = "http://cran.us.r-project.org")
#  BiocManager::install("ComplexHeatmap")

## ----9.1ii, eval = TRUE, collapse = TRUE--------------------------------------
if(!exists("duplicates")){
  duplicates <- BeeBDC::fileFinder(path = DataPath,
                            fileName = "duplicateRun_") %>%
    readr::read_csv()}

## ----9.1on.exit, include = FALSE----------------------------------------------
oldpar <- par(no.readonly = TRUE) 
on.exit(oldpar)

## ----9.1iii, eval = FALSE, collapse = TRUE------------------------------------
#    par(mar = c(2, 2, 2, 2)/2, mfrow = c(1,1))

## ----9.1iv, eval=FALSE, fig.fullwidth=TRUE, fig.height=7.5, fig.width=9, collapse = TRUE----
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

## ----9.2, eval = TRUE, collapse = TRUE----------------------------------------
if(!exists("check_time")){
beeData <- readr::read_csv(paste(OutPath_Intermediate, "05_unCleaned_database.csv",
                                 sep = "/"),
                           col_types = BeeBDC::ColTypeR())
}else{
  beeData <- check_time
  rm(check_time)
}

## ----9.2ii, warning=FALSE, eval=TRUE, collapse = TRUE-------------------------
BeeBDC::dupePlotR(
  data = beeData,
  # The outPath to save the plot as
  outPath = OutPath_Figures,
  fileName = "duplicatePlot.pdf",
  # Colours in order: duplicate, kept duplicate, unique
  dupeColours = c("#F2D2A2","#B9D6BC", "#349B90"),
  # Plot size and height
  base_height = 7, base_width = 7,
  legend.position = c(0.85, 0.8),
  # Extra variables can be fed into forcats::fct_recode() to change names on plot
  GBIF = "GBIF", SCAN = "SCAN", iDigBio = "iDigBio", USGS = "USGS", ALA = "ALA", 
  ASP = "ASP", CAES = "CAES", Ecd = "Ecd",
  returnPlot = TRUE
)

## ----9.3, fig.width=15, fig.height=9, fig.fullwidth=TRUE, eval=TRUE, collapse = TRUE----
BeeBDC::plotFlagSummary(
  data = beeData,
  # Colours in order of pass (TRUE), fail (FALSE), and NA
  flagColours = c("#127852", "#A7002D", "#BDBABB"),
  fileName = paste0("FlagsPlot_", Sys.Date(),".pdf"),
  outPath = paste0(OutPath_Figures),
  width = 15, height = 9,
    # OPTIONAL:
      #   # Filter to a single species
      #       speciesName = "Holcopasites heliopsis",
      #         # column to look in
      #       nameColumn = "species",
      #        # Save the filtered data
      #       saveFiltered = TRUE,
      #   # Filter column to display on map
      #       filterColumn = ".summary",
      #       plotMap = TRUE,
      #   # amount to jitter points if desired, e.g. 0.25 or NULL
      #       jitterValue = NULL,
      #        # Map opacity value for points between 0 and 1
      #   mapAlpha = 1,
      #        # If a user wants to output the table used to make the figure, change this to TRUE
      #   saveTable = FALSE,
  # Extra variables can be fed into forcats::fct_recode() to change names on plot
  GBIF = "GBIF", SCAN = "SCAN", iDigBio = "iDigBio", USGS = "USGS", ALA = "ALA", 
  ASP = "ASP", CAES = "CAES", 'BMont' = "BMont", 'BMin' = "BMin", Ecd = "Ecd",
  Gaiarsa = "Gai", EPEL = "EPEL", VicWam = "VicWam",
  returnPlot = TRUE
)

## ----9.4, eval = TRUE, collapse = TRUE----------------------------------------
if(!exists("cleanData")){
cleanData <- readr::read_csv(paste(OutPath_Intermediate, "05_cleaned_database.csv",
                                 sep = "/"),
                           col_types = BeeBDC::ColTypeR())}

## ----9.4a, eval=FALSE, collapse = TRUE----------------------------------------
#  BeeBDC::summaryMaps(
#    data = cleanData,
#    width = 10, height = 10,
#    class_n = 3,
#    class_Style = "fisher",
#    fileName = "CountryMaps_fisher.pdf",
#    outPath = OutPath_Figures,
#    returnPlot = TRUE
#  )

## ----9.4b, eval = FALSE, collapse = TRUE--------------------------------------
#  BeeBDC::interactiveMapR(
#     # occurrence data
#    data = beeData,
#     # Directory where to save files
#    outPath = paste0(OutPath_Figures, "interactiveMaps", sep = "/"),
#    lon = "decimalLongitude",
#    lat = "decimalLatitude",
#      # Occurrence dataset column with species names
#    speciesColumn = "scientificName",
#      # Which species to map — a character vector of names or "ALL"
#      # Note: "ALL" is defined AFTER filtering for country
#    speciesList = "ALL",
#    countryList = NULL, # study area
#      # Point jitter to see stacked points — jitters an amount in decimal degrees
#    jitterValue = 0.01
#  )

## ----9.5, eval = FALSE, collapse = TRUE---------------------------------------
#  if(!exists("cleanData")){
#    cleanData <- readr::read_csv(paste(OutPath_Intermediate, "05_cleaned_database.csv",
#                                  sep = "/"),
#                            col_types = BeeBDC::ColTypeR(),
#    locale = readr::locale(encoding = "UTF-8"))}

## ----9.5ii, eval = TRUE, collapse = TRUE--------------------------------------
  # Note, if outPath = NULL then no file will be saved
dataProvTable <- BeeBDC::dataProvTables(data = cleanData,
                                        runBeeDataChecks = TRUE,
                                        outPath = NULL,
                                        fileName = "dataProvTable.csv")

## ----9.6, eval = TRUE, collapse = TRUE----------------------------------------
  # Note, if outPath = NULL then no file will be saved
summaryTable <- BeeBDC::flagSummaryTable(data = beeData, 
                                         column = "scientificName", 
                                         outPath = NULL,
                                         fileName = "flagTable.csv")

## ----cleanup, include=FALSE, collapse = TRUE----------------------------------
# Remove the webpage folder
unlink(paste0(dirname(getwd()), "/inst/extdata/WebDir"), recursive = TRUE)

