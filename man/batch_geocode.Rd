\name{batch_geocode}
\alias{batch_geocode}

\title{
R wrapper for arcgis python batch_geocode() function.
}
\description{
The batch_geocode() function in the arcgis.geocoding module geocodes an entire list of addresses. Geocoding many addresses at once is also known as bulk geocoding. You can use this method upon finding the following types of locations:

    Street addresses (e.g. 27488 Stanford Ave, Bowden, North Dakota, or 380 New York St, Redlands, CA 92373)
    Administrative place names, such as city, county, state, province, or country names (e.g. Seattle, Washington, State of Mahārāshtra, or Liechtenstein)
    Postal codes: (e.g. 92591 or TW9 1DN)

}
\usage{
batch_geocode(addresses, geocoder, arcgis)
}

\arguments{
  \item{addresses}{
A nested list of addresses obtained using the \link[gisDSTR]{pack_address} function.
}
  \item{geocoder}{
A specific geocoder obtained by using the \link[gisDSTR]{get_geocoders} function.
}
  \item{arcgis}{
An active instance of arcgis obtained using the \link[gisDSTR]{load_arcgis} function.
}
}
\details{
Batches are limited to 500 lines per request.
}
\value{
Returns a dataframe containing the addresses that were supplied to the addresses argument. Will also have a column called "ID" which corresponds to the row numbers of the dataframe that was passed to \link[gisDSTR]{pack_address}. The columns that get returned are determined by the geocoder that you specify.
}
\references{
\href{https://developers.arcgis.com/python/}{Arcgis Python Package Documentation}
\href{https://developers.arcgis.com/python/guide/part4-batch-geocoding/}{Arcgis batch_geocode Documentation}
}
\author{
Patrick Rekieta \email{Patrick.Rekieta@gmail.com}
}
\note{
If the API is not provided a geocoder, it will default to the first geocoder that exists in the account. This is typically the arcgis world geocode service.
}



\seealso{
\link[gisDSTR]{pack_address}
}
\examples{
library(gisDSTR)

arcgis <- gisDSTR::load_arcgis()
data <- data.frame("street1" = c("123 Main street","234 S Main"), "ZIPCODE" = c("12345","23456"))

mapping <- c("street1" = "street1", "ZIP" = "ZIPCODE")

address_list <- gisDSTR::pack_address(x = data, mapping = mapping)

geocoders <- gisDSTR::get_geocoders(arcgis = arcgis, arcgis_instance = gisDSTR::login_arcgis())

final_data <- gisDSTR::batch_geocode(addresses = address_list, arcgis = arcgis, geocoder = geocoders[1])
}

