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

#' Soundex algorithm for the specified language
#'
#' Returns an object that can be used in the Soundex funtions to execute in
#' different languages.
#'
#' @param lang the language code
#'
#' @return an object to be used in the Soundex funtions
#'
#' @details
#' This function returns the soundex code algorithms for use in different
#' languages. The languages currently supported are:
#' \itemize{
#'  \item{\code{en} English}
#' }
#'
#' @examples
#' algorithm <- soundex_algorithm(lang = 'en')
#' soundex_code('Soundex', algorithm = algorithm)
#'
#' @author Daniel Rodriguez Perez
#'
#' @rdname soundex_algorithm
#' @export soundex_algorithm
soundex_algorithm <- function(lang = 'en') {
  switch(tolower(lang),
         en = list(' ' = c('A', 'E', 'I', 'O', 'U'),
                   '1' = c('B', 'F', 'P', 'V'),
                   '2' = c('C', 'G', 'J', 'K', 'Q', 'S', 'X', 'Z'),
                   '3' = c('D', 'T'),
                   '4' = c('L'),
                   '5' = c('M', 'N'),
                   '6' = c('R')))
}

#' Calculate the Soundex code
#'
#' Calculate the Soundex code for a string
#'
#' @param string a string or an array of strings for calculating the code
#' @param algorithm object with the soundex algorithm
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
soundex_code <- function(string, algorithm = soundex_algorithm('en')) {
  if (length(string) == 0) {
    return(NULL)
  }

  result <- c()
  string <- toupper(string)

  for (i in 1:length(string)) {
    if (str_length(string[i]) == 0) {
      sub_result <- NULL
      break
    }

    code      <- str_split(string[i], pattern = '')[[1]]
    sub_result <- code[1]

    for (j in 1:str_length(string[i])) {
      str_i <- str_sub(string[i], j, j)

      for (k in 1:length(algorithm)) {
        if (str_i %in% algorithm[[k]]) {
          code[j] <- names(algorithm[k])
          break
        }
      }
    }

    for (j in 2:length(code)) {
      if (code[j] != code[j - 1] && code[j] %in% c('1', '2', '3', '4', '5', '6')) {
        sub_result <- paste(sub_result, code[j], sep = '')
      }
    }

    if (str_length(sub_result) < 4)  {
      for (j in str_length(sub_result):3) {
        sub_result <- paste(sub_result, '0', sep = '')
      }
    }

    result[i] <- str_sub(sub_result, 1, 4)
  }

  if (!is.null(result))
    class(result) <- c('soundex', class(result))

  return(result)
}

#' Calculate the Soundex difference
#'
#' Calculate the Soundex difference for two strings \code{str_1} and \code{str_2}
#'
#' @param str_1 first string for calculating the distance
#' @param str_2 second string for calculating the distance
#' @param algorithm object with the soundex algorithm
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
soundex_difference <- function(str_1, str_2, algorithm = soundex_algorithm('en')) {
  if (length(str_1) == 0 || length(str_2) == 0) {
    return(NULL)
  }

  if (str_length(str_1) == 0 || str_length(str_2) == 0) {
    return(0)
  }

  # Calculate Soundex strings
  soundex_1 = soundex_code(str_1, algorithm = algorithm)
  soundex_2 = soundex_code(str_2, algorithm = algorithm)

  result <- c()

  for (i in 1:length(soundex_1)) {
    if (soundex_1[i] == soundex_2) {
      result[i] = 4
    } else {
      if (str_sub(soundex_1[i], 1, 1) == str_sub(soundex_2, 1, 1)) {
        result = 1
      } else {
        result[i] = 0
      }

      if (!is.na(str_match(soundex_2, str_sub(soundex_1[i], 2, 4)))) {
        result[i] = 3
        next
      }

      if (!is.na(str_match(soundex_2, str_sub(soundex_1[i], 3, 4)))) {
        result[i] = 2
        next
      }

      if (!is.na(str_match(soundex_2, str_sub(soundex_1[i], 2, 3)))) {
        result[i] = 2
        next
      }

      if (!is.na(str_match(soundex_2, str_sub(soundex_1[i], 2, 2)))) {
        result[i] = result[i] + 1
      }

      if (!is.na(str_match(soundex_2, str_sub(soundex_1[i], 3, 3)))) {
        result[i] = result[i] + 1
      }

      if (!is.na(str_match(soundex_2, str_sub(soundex_1[i], 4, 4)))) {
        result[i] = result[i] + 1
      }
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
#' @param algorithm object with the soundex algorithm
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
soundex <- function(str_1, str_2, algorithm = soundex_algorithm('en')) {
  return(soundex_difference(str_1, str_2, algorithm = algorithm) / 4)
}
