##
##  Santions class methods
##
##  Created by Daniel Rodríguez Pérez on 25/10/2015.
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

#' @method dim santions
#' @export
dim.santions <- function(x) {
  dim(x$data)
}

#' @method length santions
#' @export
length.santions <- function(x) {
  dim(x)[1]
}

#' @method print santions
#' @export
print.santions <- function(x, ...) {
  cat('Santion list from:', x$source, '\n')
  cat('Last update:', x$update, '\n')
  cat('Number of registers:', length(x), '\n\n')
}
