##
##  Soundex distance
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

#' Calculate the Soundex code
#'
#' Calculate the Soundex code for a string
#'
#' @param string a string for calculating the code
#'
#' @return the Soundex code
#'
#' @details
#' Soundex is a phonetic algorithm for indexing names by sound. The Soundex
#' code for a name consists of a letter followed by three numerical digits: the
#' letter is the first letter of the name, and the digits encode the remaining
#' consonants. Consonants at a similar place of articulation share the same
#' digit so.
#'
#' @examples
#' soundex_code('Soundex')
#'
#' @author Daniel Rodriguez Perez
#'
#' @rdname soundex_code
#' @export soundex_code
soundex_code <- function(string) {
  if (str_length(string) == 0) {
    return(NULL)
  }

  string  <- toupper(string)
  code    <- str_split(toupper(string), pattern = '')[[1]]
  result  <- code[1]

  for (i in str_length(string):1) {
    str_i <- str_sub(string, i, i)

    if (str_i %in% c('A', 'E', 'I', 'O', 'U')) {
      code[i] <- ''
    } else if (str_i %in% c('B', 'F', 'P', 'V')) {
      code[i] <- '1'
    } else if (str_i %in% c('C', 'G', 'J', 'K', 'Q', 'S', 'X', 'Z')) {
      code[i] <- '2'
    } else if (str_i %in% c('D', 'T')) {
      code[i] <- '3'
    } else if (str_i %in% c('L')) {
      code[i] <- '4'
    } else if (str_i %in% c('M', 'N')) {
      code[i] <- '5'
    } else if (str_i %in% c('R')) {
      code[i] <- '6'
    }
  }

  for (i in 2:length(code)) {
    if (code[i] != code[i - 1] && code[i] %in% c('1', '2', '3', '4', '5', '6')) {
      result <- paste(result, code[i], sep = '')
    }
  }

  if (str_length(result) < 4)  {
    for (i in str_length(result):3) {
      result <- paste(result, '0', sep = '')
    }
  }

  return(str_sub(result, 1, 4))
}

#' Calculate the Soundex difference
#'
#' Calculate the Soundex difference for two strings \code{str_1} and \code{str_2}
#'
#' @param str_1 first string for calculating the distance
#' @param str_2 second string for calculating the distance
#'
#' @return an integer that indicates the difference between the Soundex codes
#'
#' @details
#' The returned value is the number of characters in the Soundex values that
#' are the same. The return value ranges from 0 through 4: 0 indicates weak or
#' no similarity, and 4 indicates strong similarity or the same values.
#'
#' @examples
#' soundex_difference('saturday', 'sunday')
#'
#' @author Daniel Rodriguez Perez
#'
#' @rdname soundex_difference
#' @export soundex_difference
soundex_difference <- function(str_1, str_2) {
  if (str_length(str_1) == 0 || str_length(str_2) == 0) {
    return(0)
  }

  # Calculate Soundex strings
  soundex_1 = soundex_code(str_1)
  soundex_2 = soundex_code(str_2)

  if (soundex_1 == soundex_2) {
    result = 4
  } else {
    if (str_sub(soundex_1, 1, 1) == str_sub(soundex_2, 1, 1)) {
      result = 1
    } else {
      result = 0
    }

    if (!is.na(str_match(soundex_2, str_sub(soundex_1, 2, 4)))) {
      return(3)
    }

    if (!is.na(str_match(soundex_2, str_sub(soundex_1, 3, 4)))) {
      return(2)
    }

    if (!is.na(str_match(soundex_2, str_sub(soundex_1, 2, 3)))) {
      return(2)
    }

    if (!is.na(str_match(soundex_2, str_sub(soundex_1, 2, 2)))) {
      result = result + 1
    }

    if (!is.na(str_match(soundex_2, str_sub(soundex_1, 3, 3)))) {
      result = result + 1
    }

    if (!is.na(str_match(soundex_2, str_sub(soundex_1, 4, 4)))) {
      result = result + 1
    }
  }

  return(result)
}

#' Calculate the similarity based on the Soundex distance
#'
#' Calculate the similarity for two strings \code{str_1} and \code{str_2} based
#' on the Soundex distance
#'
#' @param str_1 first string for calculating the similarity
#' @param str_2 second string for calculating the similarity
#'
#' @return the similarity based on the Soundex distance between the specified
#'         strings
#'
#' @examples
#' soundex('saturday', 'sunday')
#'
#' @author Daniel Rodriguez Perez
#'
#' @rdname soundex
#' @export soundex
soundex <- function(str_1, str_2) {
  return(soundex_difference(str_1, str_2) / 4)
}
