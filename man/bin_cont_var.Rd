% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basic.R
\name{bin_cont_var}
\alias{bin_cont_var}
\title{bin_cont_var}
\usage{
bin_cont_var(VAR, NUM_GROUPS, ROUND = 3, binNUM = FALSE)
}
\arguments{
\item{VAR}{A numeric vector of values to bin}

\item{NUM_GROUPS}{A positive integer for the
number of bins}

\item{ROUND}{A nonnegative integer for displaying
bin labels through binned intervals}

\item{binNUM}{Boolean set to TRUE to map bins to
numbers. Otherwise, bins are characterized by intervals}
}
\value{
A character or integer vector of 
collapsed/binned values
}
\description{
Transform numeric vector into
discrete bins
}
