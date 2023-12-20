\name{login_arcgis}
\alias{login_arcgis}

\title{
Supply login to arcgis package for future authorization in functions.
}
\description{
This function will log the user into a specific arcgis portal. This is a necessary step in order to receive an authroization token that can be used in other functions within the arcgis package.
}
\usage{
login_arcgis(arcgis = NULL, url = "", username = "", password = rstudioapi::askForPassword())
}

\arguments{
  \item{arcgis}{
An object representing your arcgis package that was loaded in using \link[gisDSTR]{load_arcgis}.
}
  \item{url}{
The url of the arcgis portal that you are attempting to log into.
}
  \item{username}{
The username for the login at the requested portal.
}
  \item{password}{
The password for the login at the requested portal. If no password is provided, R will ask you to insert the password through a secure popup.
}
}

\value{
Returns an active instance of arcgis that can be used in later functions.
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
}

