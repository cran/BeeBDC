% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/beesTaxonomy.R
\name{beesTaxonomy}
\alias{beesTaxonomy}
\title{Download a nearly complete taxonomy of bees globally}
\usage{
beesTaxonomy(
  URL = "https://open.flinders.edu.au/ndownloader/files/47089969",
  ...
)
}
\arguments{
\item{URL}{A character vector to the FigShare location of the dataset. The default will be to
the most-recent version.}

\item{...}{Extra variables that can be passed to \code{\link[utils:download.file]{utils::download.file()}}}
}
\value{
A downloaded beesTaxonomy.Rda file in the \code{\link[=tempdir]{tempdir()}} and the same tibble returned to
the environment.
}
\description{
Downloads the taxonomic information for the bees of the world.
Source of taxonomy is listed under "source" but are mostly derived from the Discover Life
website. The data will be sourced from the BeeBDC article's Figshare.

Note that sometimes the download might not work without restarting R. In this case, you could
alternatively download the dataset from the URL below and then read it in using
\code{base::readRDS("filePath.Rda")}.
}
\details{
\strong{Column details}

\strong{flags} Flags or comments about the taxon name.

\strong{taxonomic_status} Taxonomic status. Values are "accepted" or "synonym"

\strong{source} Source of the name.

\strong{accid} The id of the accepted taxon name or "0" if taxonomic_status == accepted.

\strong{id} The id number for the taxon name.

\strong{kingdom} The biological kingdom the taxon belongs to. For bees, kingdom == Animalia.

\strong{phylum} The biological phylum the taxon belongs to. For bees, phylum == Arthropoda.

\strong{class} The biological class the taxon belongs to. For bees, class == Insecta.

\strong{order} The biological order the taxon belongs to. For bees, order == Hymenoptera.

\strong{family} The family of bee which the species belongs to.

\strong{subfamily} The subfamily of bee which the species belongs to.

\strong{tribe} The tribe of bee which the species belongs to.

\strong{subtribe} The subtribe of bee which the species belongs to.

\strong{validName} The valid scientific name as it should occur in the 'scientificName" column in a Darwin Core file.

\strong{canonical} The scientificName without the scientificNameAuthority.

\strong{canonical_withFlags} The scientificName without the scientificNameAuthority and with Discover Life taxonomy flags.

\strong{genus} The genus the bee species belongs to.

\strong{subgenus} The subgenus the bee species belongs to.

\strong{species} The specific epithet for the bee species.

\strong{infraspecies} The infraspecific epithet for the bee addressed.

\strong{authorship} The author who described the bee species.

\strong{taxon_rank} Rank for the bee taxon addressed in the entry.

\strong{notes} Additional notes about the name/taxon.
}
\examples{
\dontrun{
beesTaxonomy <- BeeBDC::beesTaxonomy()
}


}
\references{
This dataset was created using the Discover Life taxonomy.
Dataset is from the publication:
Dorey, J.B., Fischer, E.E., Chesshire, P.R., Nava-Bolaños, A., O’Reilly, R.L., Bossert, S., Collins, S.M., Lichtenberg, E.M., Tucker, E., Smith-Pardo, A., Falcon-Brindis, A., Guevara, D.A., Ribeiro, B.R., de Pedro, D., Hung, J.K.-L., Parys, K.A., McCabe, L.M., Rogan, M.S., Minckley, R.L., Velzco, S.J.E., Griswold, T., Zarrillo, T.A., Jetz, W., Sica, Y.V., Orr, M.C., Guzman, L.M., Ascher, J., Hughes, A.C. & Cobb, N.S. (2023) A globally synthesised and flagged bee occurrence dataset and cleaning workflow. Scientific Data, 10, 1–17. https://www.doi.org/10.1038/S41597-023-02626-W
The taxonomy data are mostly compiled from Discover Life data, www.discoverlife.org:
Ascher, J.S. & Pickering, J. (2020) Discover Life bee species guide and world checklist (Hymenoptera: Apoidea: Anthophila). http://www.discoverlife.org/mp/20q?guide=Apoidea_species
}
\seealso{
\code{\link[=taxadbToBeeBDC]{taxadbToBeeBDC()}} to download any other taxonomy (of any taxa or of bees)
and \code{\link[=harmoniseR]{harmoniseR()}} for the
taxon-cleaning function where these taxonomies are implemented. It may also be worth seeing
\code{\link[=beesChecklist]{beesChecklist()}}.
}
