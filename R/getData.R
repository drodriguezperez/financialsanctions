##
##  Get santions list
##
##  Created by Daniel Rodríguez Pérez on 24/10/2015.
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

#' Get the consolitate list of santions from HM Treasury
#'
#' Get the consolidate list of santions from the HM Treasury
#'
#' @param file the URL to the file
#'
#' @return a santions objec
#'
#' @author Daniel Rodriguez Perez
#'
#' @rdname getSanctionsHMTreasury
#' @export getSanctionsHMTreasury
getSanctionsHMTreasury <- function(file = NULL) {
  if (is.null(file)) {
    file = url("http://hmt-sanctions.s3.amazonaws.com/sanctionsconlist.csv")
  }

  rawData <- read.csv(file             = file,
                      header           = FALSE,
                      stringsAsFactors = FALSE)

  rawData <- data.frame(mapply(function(x) str_replace_all(x, "[^[:graph:]]", " "), rawData),
                        stringsAsFactors = FALSE)

  lastUpdate        <- rawData[1,2]
  titles            <- rawData[2, ]
  rawData           <- rawData[-c(1,2), ]
  colnames(rawData) <- titles

  num_santions <- dim(rawData)[1]

  useData <- data.frame(type        = rep('', num_santions),
                        gropupID    = '',
                        name        = '',
                        birth       = '',
                        nationality = '',
                        passport    = '',
                        alias       = '',
                        stringsAsFactors = FALSE)

  for (i in 1:num_santions) {
    useData$type[i]        <- rawData[i, 'Group Type']
    useData$gropupID[i]    <- rawData[i, 'Group ID']
    useData$name[i]        <- str_trim(paste(rawData[i, 'Name 1'], rawData[i, 'Name 2'],
                                             rawData[i, 'Name 3'], rawData[i, 'Name 4'],
                                             rawData[i, 'Name 5'], rawData[i, 'Name 6'], sep = ' '))
    if (str_length(rawData[i, 'DOB']) != 0) {
      useData$birth[i]     <- as.Date(str_replace_all(rawData[i, 'DOB'], '00/', '01/'))
    }
    useData$nationality[i] <- rawData[i, 'Nationality']
    useData$passport[i]    <- rawData[i, 'Passport Details']
    useData$alias[i]       <- rawData[i, 'Alias Type']
  }

  result        <- list(source = 'HM Treasury',
                        update = lastUpdate,
                        data   = useData)
  class(result) <- 'santions'

  return(result)
}
