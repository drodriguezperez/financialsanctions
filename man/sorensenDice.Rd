% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sorensenDice.R
\name{sorensenDice}
\alias{sorensenDice}
\title{Calculate Sorensen-Dice distance}
\usage{
sorensenDice(str_1, str_2)
}
\arguments{
\item{str_1}{first string or bigrams for calculating the distance}

\item{str_2}{second string or bigram for calculating the distance}
}
\value{
the Sorensen-Dice distance between the specified strings
}
\description{
Calculate Sorensen-Dice distance for two strings \code{str_1} and
\code{str_2}
}
\examples{
sorensenDice('saturday', 'sunday')

}
\author{
Daniel Rodriguez Perez
}

