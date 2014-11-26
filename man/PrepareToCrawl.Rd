\name{PrepareToCrawl}
\alias{PrepareToCrawl}
\title{PrepareToCrawl}
\usage{
  PrepareToCrawl(d)
}
\arguments{
  \item{d}{dataframe as output from the \code{MeltHistos}
  function}
}
\value{
  dataframe with three columns: 'DeployID', 'Time' and
  'stopVar'
}
\description{
  Format output from \code{MeltHistos} for the
  \code{mergeToStop} function within the \code{crawl}
  package
}
\details{
  A key feature of the \code{crowl} package is the ability
  to include information regarding the animal's haul-out
  behavior as a component of the movement model. The
  \code{crawl} package includes a \code{mergeToStop}
  function to merge the stop data with a location dataset.
}
\note{
  fairly simple function at this point. I plan to add some
  checks for proper data structure and formating to insure
  full functionality with \code{crawl}.
}

