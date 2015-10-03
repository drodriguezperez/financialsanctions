##
##  Jaro-Winkler distance
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

#' Calculate Jaro-Winkler distance
#'
#' Calculate Jaro-Winkler distance for two strings \code{str_1} and
#' \code{str_2}
#'
#' @param str_1 first string for calculating the distance
#' @param str_2 second string for calculating the distance
#' @param weight_threshold percent to apply the Winkler modification
#' @param num_chars size of the prefix to be concidered by the Winkler
#'                  modification
#'
#' @return the Jaro-Winkler distance between the specified strings
#'
#' @details
#' The Jaro-Winkler distance metric is a string edit distance. The Jaro-Winkler
#' distance metric is designed and best suited for short strings such as person
#' names. The score is normalized such that 0 equates to no similarity and 1 is
#' an exact match.
#'
#' @examples
#' jaroWinkler('saturday', 'sunday')
#'
#' @references
#' M. A. Jaro, "Advances in record linkage methodology as applied to the 1985
#' census of Tampa Florida." Journal of the American Statistical Association,
#' vol. 84, no. 406, pp. 414-420, Jun. 1989.
#'
#' W. E. Winkler, "String Comparator Metrics and Enhanced Decision Rules in the
#' Fellegi-Sunter Model of Record Linkage." Proceedings of the Section on
#' Survey Research Methods (American Statistical Association), pp. 354-359,
#' 1990.
#'
#' M. A. Jaro, "Probabilistic linkage of large public health data file."
#' Statistics in Medicine vol. 14, no. 5-7, pp. 491-498, March - April 1995.
#'
#' @author Daniel Rodriguez Perez
#'
#' @rdname jaroWinkler
#' @export jaroWinkler
jaroWinkler <- function(str_1, str_2,
                        weight_threshold = 0.7,
                        num_chars        = 4) {
  str_1 <- tolower(str_1)
  str_2 <- tolower(str_2)
  len_1 <- str_length(str_1)
  len_2 <- str_length(str_2)

  if (len_1 == 0 || len_2 == 0) {
    if (len_1 == 0 && len_2 == 0) {
      return(1)
    } else {
      return(0)
    }
  }

  search_range <- max(len_1, len_2) / 2
  search_range <- floor(max(1, search_range))

  match_1 <- rep(FALSE, len_1)
  match_2 <- rep(FALSE, len_2)

  num_common <- 0

  for (i in 1:len_1) {
    ini <- max(1, i - search_range)
    end <- min(i + search_range, len_2)

    for (j in ini:end) {
      if (match_2[j]) {
        next
      }

      if (str_sub(str_1, i, i) != str_sub(str_2, j, j)) {
        next
      }

      match_1[i] <- TRUE
      match_2[j] <- TRUE
      num_common <- num_common + 1
      break;
    }
  }

  if (num_common == 0) {
    return(0)
  }

  num_half_transposed <- 0
  k                   <- 1

  for (i in 1:len_1) {
    if (!match_1[i]) {
      next
    }

    while (!match_2[k]) {
      k <- k + 1
    }

    if (str_sub(str_1, i, i) != str_sub(str_2, k, k)) {
      num_half_transposed <- num_half_transposed + 1
    }

    k <- k + 1
  }

  num_transposed <- floor(num_half_transposed / 2)
  weight         <- (num_common / len_1 + num_common / len_2 +
                       (num_common - num_transposed) / num_common) / 3

  if (weight <= weight_threshold) {
    return(weight)
  }

  num_max <- min(num_chars, min(len_1, len_2));
  pos     <- 1
  while (pos <= num_max && str_sub(str_1, pos, pos) == str_sub(str_2, pos, pos)) {
    pos <- pos + 1
  }

  if (pos == 1) {
    return(weight)
  }

  return(weight + 0.1 * (pos - 1) * (1 - weight))
}
