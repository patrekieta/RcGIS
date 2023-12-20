\name{full_batch_geocode}
\alias{full_batch_geocode}

\title{
full service function for batch geocode.
}
\description{
This function is a wrapper of other functions in this package. It will run all the necessary functions in sequence in order to geocode a file.
}
\usage{
full_batch_geocode(x, mapping, username = "", password = rstudioapi::askForPassword(), geocoder = "", url = "")
}

\arguments{
  \item{x}{
A dataframe containing the addresses needed for geocoding. The addresses should be in multiple columns for multifield geocode or a single column for single line geocode.

}
  \item{mapping}{
A named list that indicates the name of the input field for the geocode used as well as the name of the column in arugment X that contains the address data for that field. ex: c("name of input field in geocoder" = "name of column in X")
}
  \item{username}{
The username for the login at the requested portal.
}
  \item{password}{
The password for the login at the requested portal. If no password is provided, R will ask you to insert the password through a secure popup.
}
  \item{geocoder}{
the name or url of the geocoder that should be used. These can be found by running \link[gisDSTR]{get_geocoders}.
}
  \item{url}{
The url of the arcgis portal that you are attempting to log into.
}
}

\value{
Returns a dataframe containing the addresses that were supplied to the addresses argument. Will also have a column called "ID" which corresponds to the row numbers of the dataframe that was passed to \link[gisDSTR]{pack_address}. The columns that get returned are determined by the geocoder that you specify.
}
\references{
\href{https://developers.arcgis.com/python/}{Arcgis Python Package Documentation}
}
\author{
Patrick Rekieta \email{Patrick.Rekieta@gmail.com}
}



\seealso{
\link[gisDSTR]{pack_address}
\link[gisDSTR]{load_arcgis}
\link[gisDSTR]{login_arcgis}
\link[gisDSTR]{get_geocoders}
\link[gisDSTR]{batch_geocode}
}
\examples{
library(gisDSTR)

data <- data.frame("street1" = c("123 Main street","234 S Main"), "ZIPCODE" = c("12345","23456"))

mapping <- c("street1" = "street1", "ZIP" = "ZIPCODE")

geocoded_address <- gisDSTR::full_batch_geocode(x = data, mapping = mapping, username = "", geocoder = "name of geocoder")
}

