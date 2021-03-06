% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/levenshtein.R
\name{levenshtein}
\alias{levenshtein}
\title{Calculate the similarity based on the Levenshtein distance}
\usage{
levenshtein(str_1, str_2)
}
\arguments{
\item{str_1}{first string for calculating the similarity}

\item{str_2}{second string for calculating the similarity}
}
\value{
the similarity based on the Levenshtein distance between the
        specified strings
}
\description{
Calculate the similarity for two strings \code{str_1} and \code{str_2} based
on the Levenshtein distance
}
\examples{
levenshtein('saturday', 'sunday')

}
\author{
Daniel Rodriguez Perez
}

