\name{load_arcgis}
\alias{load_arcgis}

\title{
Load the arcgis python package into your environment.
}
\description{
This function ensures that you have installed the arcgis python package. If it is installed the package will be loaded into your environment for future use by other functions.
}
\usage{
load_arcgis()
}


\value{
Returns an R object that represents the python package.
}
\references{
\href{https://developers.arcgis.com/python/}{Arcgis Python Package Documentation}
}
\author{
Patrick Rekieta \email{Patrick.Rekieta@gmail.com}
}
\note{
Python needs to be installed on your machine and linked to Rstudio for this to work.
}

\examples{
arcgis <- gisDSTR::load_arcgis()
arcgis
}

