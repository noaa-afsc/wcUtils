\name{autoplot.HistoPercentDry}
\alias{autoplot.HistoPercentDry}
\title{autoplot.HistoPercentDry}
\usage{
  autoplot.HistoPercentDry(d, ids = c(1:3), ..., xlab = "",
    ylab = "", title = "Percent Dry Timeline")
}
\arguments{
  \item{d}{dataframe object output from MeltHistos
  function}

  \item{ids}{vector representing the index of DeployIDs to
  use. Cannot be more than 3.}
}
\value{
  ggplot
}
\description{
  autoplot.HistoPercentDry
}
\note{
  future plans to evolve this to an autoplot function
}
\author{
  Josh M London \email{josh.london@noaa.gov}
}

