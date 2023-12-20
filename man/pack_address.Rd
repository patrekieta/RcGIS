\name{pack_address}
\alias{pack_address}

\title{
Tool to properly format addresses for geocoding with arcgis. Formats Addresses as a python dictionary.
}
\description{
Tool to properly format addresses for geocoding with arcgis. Formats Addresses as a python dictionary. Creates a nested list that allows for any number of multifield addresses or singleline addresses. This is required before using \link[gisDSTR]{batch_geocode}. The required input is a dataframe and a mapping of which columns in the dataframe map to which required input field from the geocoder.
}
\usage{
pack_address(x, mapping)
}

\arguments{
  \item{x}{
A dataframe containing the addresses needed for geocoding. The addresses should be in multiple columns for multifield geocode or a single column for single line geocode.
}
  \item{mapping}{
A named list that indicates the name of the input field for the geocode used as well as the name of the column in arugment X that contains the address data for that field. ex: c("name of input field in geocoder" = "name of column in X")
}
}

\value{
A nested list of addresses that mimic a python dictionary.
}
\references{
\href{https://developers.arcgis.com/python/}{Arcgis Python Package Documentation}
\href{https://developers.arcgis.com/python/guide/part4-batch-geocoding/}{Arcgis batch_geocode Documentation}
}
\author{
Patrick Rekieta \email{Patrick.Rekieta@gmail.com}
}


\examples{
library(gisDSTR)

data <- data.frame("street1" = c("123 Main street","234 S Main"), "ZIPCODE" = c("12345","23456"))

mapping <- c("street1" = "street1", "ZIP" = "ZIPCODE")

address_list <- gisDSTR::pack_address(x = data, mapping = mapping)
address_list
}

