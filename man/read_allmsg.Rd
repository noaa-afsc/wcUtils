% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_allmsg.R
\name{read_allmsg}
\alias{read_allmsg}
\title{Parse a *-All.csv file into a proper data.frame}
\usage{
read_allmsg(allmsg_file, to_lower = TRUE, fix_csv = FALSE)
}
\arguments{
\item{allmsg_file}{file path or file connection to a *-All.csv file}

\item{to_lower}{whether to convert the column names to lower case}

\item{fix_csv}{whether to attemtp to fix any comma, csv issues}
}
\value{
a data frame
}
\description{
Parse a *-All.csv file into a proper data.frame
}
