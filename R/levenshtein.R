##
##  Levenshtein distance
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

#' Calculate Levenshtein distance
#'
#' Calculate Levenshtein distance for two strings str_1 and str_2
#'
#' @param str_1 first string for calculating the distance
#' @param str_2 second string for calculating the distance
#'
#' @return the Levenshtein distance of two strings
#'
#' @details
#' The Levenshtein distance is the minimum number of operations required to
#' transform one string into another. Operation is defined as either an
#' insertion, deletion or substitution.
#'
#' @examples
#' levenshtein('saturday', 'sunday')
#'
#' @references
#' "Levenshtein distance." Wikipedia: The Free Encyclopedia. Wikimedia
#' Foundation, Inc., 27 Sep. 2015.
#' <http://en.wikipedia.org/wiki/Levenshtein_distance>
#'
#' @author Daniel Rodriguez Perez
#'
#' @rdname levenshtein
#' @export levenshtein
levenshtein <- function(str_1, str_2) {
  str_1 <- tolower(str_1)
  str_2 <- tolower(str_2)

  if (str_length(str_1) == 0 || str_length(str_2) == 0) {
    return(max(str_length(str_1), str_length(str_2)))
  }

  previous_row <- 0:str_length(str_2);

  for (i in 1:str_length(str_1)) {
    current_row    <- rep(0, str_length(str_2))
    current_row[1] <- i

    for (j in 1:str_length(str_2)) {
      insertions         <- previous_row[j + 1] + 1
      deletions          <- current_row[j] + 1
      substitutions      <- previous_row[j] + (str_sub(str_1, i, i) != str_sub(str_2, j, j))
      current_row[j + 1] <- min(insertions, deletions, substitutions)
    }

    previous_row <- current_row
  }

  return(current_row[length(current_row)])
}

#' Calculate the similarity of Levenshtein distance
#'
#' Calculate the similarity of Levenshtein distance for two strings str_1 and
#' str_2
#'
#' @param str_1 first string for calculating the distance
#' @param str_2 second string for calculating the distance
#'
#' @return the similarity based on the Levenshtein distance of two strings
#'
#' @examples
#' levenshtein_similarity('saturday', 'sunday')
#'
#' @author Daniel Rodriguez Perez
#'
#' @rdname levenshtein_similarity
#' @export levenshtein_similarity
levenshtein_similarity <- function(str_1, str_2) {
  distance <- levenshtein(str_1, str_2)
  similarity <- 1 - distance / max(str_length(str_1), str_length(str_2))
  return(similarity)
}
