\name{get_geocoders}
\alias{get_geocoders}

\title{
Easy method to obtain the available geocoders at a given arcgis portal.
}
\description{
Easy method to obtain the available geocoders at a given arcgis portal. This will provide the geocoders available to the user and can be used to provide additional information about the geocoder objects if needed.
}
\usage{
get_geocoders(arcgis = NULL, arcgis_instance = NULL)
}

\arguments{
  \item{arcgis}{
A loaded in version of the arcgis python package using \link[gisDSTR]{load_arcgis}.
}
  \item{arcgis_instance}{
An active instance of arcgis loging which can be provided with \link[gisDSTR]{login_arcgis}.
}
}

\value{
Returns a list of geocoder objects that correspond to the available geocoders on the arcgis account that was supplied to the function.
}
\references{
\href{https://developers.arcgis.com/python/}{Arcgis Python Package Documentation}
}
\author{
Patrick Rekieta \email{Patrick.Rekieta@gmail.com}
}

\examples{
library(gisDSTR)

arcgis <- gisDSTR::load_arcgis()
arcgis_inst <- gisDSTR::login_arcgis(arcgis = arcgis, username = "")

geocoders <- gisDSTR::get_geocoders(arcgis = arcgis, arcgis_instance = arcgis_inst)
geocoders
}

