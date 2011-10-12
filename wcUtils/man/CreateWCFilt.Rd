\name{CreateWCFilt}
\alias{CreateWCFilt}
\title{CreateWCFilt}
\usage{
  CreateWCFilt(tagdata, projectid, dateformat = "Y-M-D
  h:m:s", minclass = "B")
}
\arguments{
  \item{tagdata}{a dataframe with columns 'Name', 'Ptt',
  and 'Start'}

  \item{projectid}{a string specifying the name for the
  project or filter}

  \item{dateformat}{a string describing the dateformat}

  \item{minclass}{a string value of either
  "B","A","0","1","2", or "3"}
}
\value{
  an xml formated output that can be written to a .wcfilt
  file
}
\description{
  Create a *.wcfilt file from a dataframe of deployment
  information
}
\details{
  The Wildlife Computers' WC-DAP program requires the
  creation of 'filters' to organize PTT deployments within
  an Argos program. These filters can be created within the
  WC-DAP GUI interface, but this can be tedious for large
  numbers of deployments and there is no method for
  importing this data. Often researchers have data on
  deployments already organized into a spreadsheet,
  database or flat file. This function uses the XML package
  within R to process a dataframe into an XML file
  compatible with the WC-DAP filter file (*.wcfilt).
}
\note{
  future plans to have the function accept a destination
  path and actually write the output file.
}

