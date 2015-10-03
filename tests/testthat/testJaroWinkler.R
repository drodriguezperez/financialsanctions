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

context("Jaro-Winkler distance tests")

MAXERROR <- 1e-8

test_that("calculation of Jaro-Winkler distances", {
  expect_that(jaroWinkler('car',      ''),         equals(0.000000000, tolerance = MAXERROR))
  expect_that(jaroWinkler('car',      'cat'),      equals(0.822222222, tolerance = MAXERROR))
  expect_that(jaroWinkler('kitten',   'sitting'),  equals(0.746031746, tolerance = MAXERROR))
  expect_that(jaroWinkler('saturday', 'sunday'),   equals(0.777500000, tolerance = MAXERROR))
  expect_that(jaroWinkler('source',   'target'),   equals(0.555555556, tolerance = MAXERROR))
  expect_that(jaroWinkler('ABCVWXYZ', 'CABVWXYZ'), equals(0.958333333, tolerance = MAXERROR))
  expect_that(jaroWinkler('saturday', 'saturday'), equals(1.000000000, tolerance = MAXERROR))
})
