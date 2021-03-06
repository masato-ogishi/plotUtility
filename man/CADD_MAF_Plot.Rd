% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CADD_MAF_Plot.R
\name{CADD_MAF_Plot}
\alias{CADD_MAF_Plot}
\title{CADD-MAF plot.}
\usage{
CADD_MAF_Plot(
  df_popviz,
  df_private = NULL,
  selectedMutTypes = c(Frameshift = "frameshift", Inframe = "missense", Inframe =
    "inframe_deletion"),
  homOnly = F,
  color_by = "MutType",
  shape_by = NULL,
  facet_by = NULL,
  msc = 3.33,
  minAF = 10^-6
)
}
\arguments{
\item{df_popviz}{A data.frame which has the following columns: HOM, AF, CADD, MutType, and optionally, MutLabel}

\item{df_private}{(Optional) A data.frame which has the following columns: HOM, CADD, MutType, and MutLabel}

\item{selectedMutTypes}{A named characters that will be internally used to define the factor levels.}

\item{homOnly}{Logical. Whether only homozygous variants (HOM>=1) should be plotted.}

\item{color_by}{A string indicating the variable name to be used for coloring.}

\item{shape_by}{A string indicating the variable name to be used for formatting the shape of the points.}

\item{facet_by}{A string or a pair of strings indicating the column names of variables to be used for facetting.}

\item{msc}{Minimal significance cutoff. Can be identified using PopViz[http://shiva.rockefeller.edu/PopViz/].}

\item{minAF}{The minimal allele frequency. Private variants will be plotted based on this number.}
}
\description{
CADD-MAF plot.
}
\examples{
\dontrun{
df_popviz <- data.table::fread("PopViz_Table_PDCD1_MSC99_AR.txt", check.names=T) \%>\%
  dplyr::mutate(MutType=CONSEQUENCE, MutLabel=NA_character_)
df_private <- readr::read_csv("JL5657.V4.2.csv") \%>\%
  dplyr::filter(gene=="PDCD1"&ID=="rs373081859") \%>\%
  dplyr::transmute(HOM=1, AF=NA, CADD=CADD_Phred_V1.3, MutType="frameshift", MutLabel="T36Hfs*70")
plt <- CADD_MAF_Plot(
  df_popviz,
  df_private,
  selectedMutTypes=c(
   "Frameshift"="frameshift",
   "Inframe"="missense",
   "Inframe"="inframe_deletion"
  ),
  homOnly=F,
  color_by="MutType",
  facet_by="HomType",
  msc=3.3313,
  minAF=10^-6
) +
  scale_color_manual(values=c("firebrick3")) +
  scale_fill_manual(values=c("firebrick3","Medium Turquoise","dimgrey"))
}
}
