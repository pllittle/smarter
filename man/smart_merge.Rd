% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basic.R
\name{smart_merge}
\alias{smart_merge}
\title{smart_merge}
\usage{
smart_merge(x, y, mess = FALSE, ...)
}
\arguments{
\item{x}{A data.frame}

\item{y}{A data.frame}

\item{mess}{Default to \code{FALSE}.
Otherwise a message is printed.}

\item{...}{arguments passed to merge}
}
\value{
A merged data.frame
}
\description{
Merges two data.frames assuming
they have at least one shared column name
}
\examples{

aa = smart_df(a = c(1,2,3),b = c("a","b","c"),c = c(4,5,6))
bb = smart_df(a = c(2,4,5),b = c("b","d","e"),d = c("alpha","beta","gamma"))
smart_merge(aa,bb,all.x = TRUE)
smart_merge(aa,bb,all.y = TRUE)
smart_merge(aa,bb,all = TRUE)

}
