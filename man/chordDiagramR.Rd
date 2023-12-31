% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chordDiagramR.R
\name{chordDiagramR}
\alias{chordDiagramR}
\title{Build a chord diagram of duplicate occurrence links}
\usage{
chordDiagramR(
  dupeData = NULL,
  outPath = NULL,
  fileName = NULL,
  width = 7,
  height = 6,
  bg = "white",
  smallGrpThreshold = 3,
  title = "Duplicated record sources",
  palettes = c("cartography::blue.pal", "cartography::green.pal",
    "cartography::sand.pal", "cartography::orange.pal", "cartography::red.pal",
    "cartography::purple.pal", "cartography::brown.pal"),
  canvas.ylim = c(-1, 1),
  canvas.xlim = c(-0.6, 0.25),
  text.col = "black",
  legendX = grid::unit(6, "mm"),
  legendY = grid::unit(18, "mm"),
  legendJustify = c("left", "bottom"),
  niceFacing = TRUE,
  self.link = 2
)
}
\arguments{
\item{dupeData}{A tibble or data frame. The duplicate file produced by \code{\link[=dupeSummary]{dupeSummary()}}.}

\item{outPath}{Character. The path to a directory (folder) in which the output should be saved.}

\item{fileName}{Character. The name of the output file, ending in '.pdf'.}

\item{width}{Numeric. The width of the figure to save (in inches). Default = 7.}

\item{height}{Numeric. The height of the figure to save (in inches). Default = 6.}

\item{bg}{The plot's background colour. Default = "white".}

\item{smallGrpThreshold}{Numeric. The upper threshold of sub-dataSources to be listed as "other".
Default = 3.}

\item{title}{A character string. The figure title. Default = "Duplicated record sources".}

\item{palettes}{A vector of the palettes to be used. One palette for each major dataSource and "other"
using the \code{paletteer} package. Default = c("cartography::blue.pal", "cartography::green.pal",
"cartography::sand.pal", "cartography::orange.pal", "cartography::red.pal",
"cartography::purple.pal", "cartography::brown.pal")}

\item{canvas.ylim}{Canvas limits from \code{\link[circlize:circos.par]{circlize::circos.par()}}. Default = c(-1.0,1.0).}

\item{canvas.xlim}{Canvas limits from \code{\link[circlize:circos.par]{circlize::circos.par()}}. Default = c(-0.6, 0.25).}

\item{text.col}{A character string. Text colour}

\item{legendX}{The x position of the legends, as measured in current viewport.
Passed to ComplexHeatmap::draw(). Default = grid::unit(6, "mm").}

\item{legendY}{The y position of the legends, as measured in current viewport.
Passed to ComplexHeatmap::draw(). Default = grid::unit(18, "mm").}

\item{legendJustify}{A character vector declaring the justification of the legends.
Passed to ComplexHeatmap::draw(). Default = c("left", "bottom").}

\item{niceFacing}{TRUE/FALSE. The niceFacing option automatically adjusts the text facing
according to their positions in the circle. Passed to \code{\link[circlize:highlight.sector]{circlize::highlight.sector()}}.}

\item{self.link}{1 or 2 (numeric). Passed to \code{\link[circlize:chordDiagram]{circlize::chordDiagram()}}:
if there is a self link in one sector, 1 means the link will be degenerated as a 'mountain' and the width corresponds to the value for this connection. 2 means the width of the starting root and the ending root all have the width that corresponds to the value for the connection.}
}
\value{
Saves a figure to the provided file path.
}
\description{
This function outputs a figure which shows the relative size and direction of occurrence points
duplicated between data providers, such as, SCAN, GBIF, ALA, etc. This function requires the
outputs generated by \code{\link[=dupeSummary]{dupeSummary()}}.
}
\examples{
\dontrun{
  # Create a basic example dataset of duplicates to visualise
basicData <- dplyr::tribble(
                            ~dataSource,    ~dataSource_keep,
                      "GBIF_Halictidae",         "USGS_data",
                      "GBIF_Halictidae",         "USGS_data",
                      "GBIF_Halictidae",         "USGS_data",
                      "GBIF_Halictidae",         "USGS_data",
                      "GBIF_Halictidae",         "USGS_data",
                      "GBIF_Halictidae",         "USGS_data",
                      "SCAN_Halictidae",   "GBIF_Halictidae",
                   "iDigBio_halictidae",   "GBIF_Halictidae",
                   "iDigBio_halictidae",   "SCAN_Halictidae",
                   "iDigBio_halictidae",   "SCAN_Halictidae",
                      "SCAN_Halictidae",   "GBIF_Halictidae",
                       "iDigBio_apidae",       "SCAN_Apidae",
                          "SCAN_Apidae",    "Ecd_Anthophila",
                       "iDigBio_apidae",    "Ecd_Anthophila",
                          "SCAN_Apidae",    "Ecd_Anthophila",
                       "iDigBio_apidae",    "Ecd_Anthophila",
                    "SCAN_Megachilidae", "SCAN_Megachilidae",
                      "CAES_Anthophila",   "CAES_Anthophila",
                      "CAES_Anthophila",   "CAES_Anthophila"
 )


 chordDiagramR(
dupeData = basicData,
outPath = tempdir(),
fileName = "ChordDiagram.pdf",
# These can be modified to help fit the final pdf that's exported.
width = 9,
height = 7.5,
bg = "white",
# How few distinct dataSources should a group have to be listed as "other"
smallGrpThreshold = 3,
title = "Duplicated record sources",
# The default list of colour palettes to choose from using the paleteer package
palettes = c("cartography::blue.pal", "cartography::green.pal", 
             "cartography::sand.pal", "cartography::orange.pal", "cartography::red.pal",
             "cartography::purple.pal", "cartography::brown.pal"),
canvas.ylim = c(-1.0,1.0), 
canvas.xlim = c(-0.6, 0.25),
text.col = "black",
legendX = grid::unit(6, "mm"),
legendY = grid::unit(18, "mm"),
legendJustify = c("left", "bottom"),
niceFacing = TRUE)}
}
