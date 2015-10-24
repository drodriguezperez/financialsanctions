##
##  Sorensen-Dice coefficient
##
##  Created by Daniel Rodríguez Pérez on 3/10/2015.
##
##  Copyright (c) 2015 Daniel Rodríguez Pérez.
##
##  This program is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>
##

#' Calculate Sorensen-Dice distance
#'
#' Calculate Sorensen-Dice distance for two strings \code{str_1} and
#' \code{str_2}
#'
#' @param str_1 first string or bigrams for calculating the distance
#' @param str_2 second string or bigram for calculating the distance
#'
#' @return the Sorensen-Dice distance between the specified strings
#'
#' @examples
#' sorensenDice('saturday', 'sunday')
#'
#' @author Daniel Rodriguez Perez
#'
#' @rdname sorensenDice
#' @export sorensenDice
sorensenDice <- function(str_1, str_2) {
  if (!inherits(str_1, 'bigrams')) {
    bigram_1 <- bigrams(str_1)
  } else {
    bigram_1 <- str_1
  }

  if (!inherits(str_2, 'bigrams')) {
    bigram_2 <- bigrams(str_2)
  } else {
    bigram_2 <- str_2
  }

  result <- NULL

  if (is.list(bigram_1)) {
    result <- rep(0, length(bigram_1))

    for (i in 1:length(bigram_1)) {
      result[i] <- 2 * sum(bigram_1[[i]] %in% bigram_2) /
        (length(bigram_1[[i]]) + length(bigram_2))
    }
  } else {
    result <- 2 * sum(bigram_1 %in% bigram_2) /
      (length(bigram_1) + length(bigram_2))
  }

  return(result)
}

#' Calculate the bigrams
#'
#' Calculate the bigrams of a string
#'
#' @param string a string for calculating the bigrams
#'
#' @return the bigrams of the string
#'
#' @examples
#' bigrams('saturday')
#'
#' @author Daniel Rodriguez Perez
#'
#' @rdname bigrams
#' @export bigrams
bigrams <- function(string) {
  if (length(string) == 1) {
    result <- c()

    string <- tolower(string)

    if (str_length(string) > 1) {
      for (i in 2:str_length(string)) {
        str_i  <- str_trim(str_sub(string, i - 1, i))
        if (str_length(str_i) == 2)
          result <- c(result, str_i)
      }
    }
  } else {
    result <- list()
    for (i in 1:length(string)) {
      result[[i]] <- bigrams(string[i])
    }
  }

  if (!is.null(result))
    class(result) <- c('bigrams', class(result))

  return(result)
}
