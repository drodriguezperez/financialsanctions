##
##  Levenshtein distance test
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

context("Levenshtein distance tests")

MAXERROR <- 1e-8

test_that("calculation of Levenshtein distance", {
  expect_that(levenshtein_distance('car',      ''),         equals(3))
  expect_that(levenshtein_distance('car',      'cat'),      equals(1))
  expect_that(levenshtein_distance('kitten',   'sitting'),  equals(3))
  expect_that(levenshtein_distance('saturday', 'sunday'),   equals(3))
  expect_that(levenshtein_distance('source',   'target'),   equals(5))
  expect_that(levenshtein_distance('ABCVWXYZ', 'CABVWXYZ'), equals(2))
  expect_that(levenshtein_distance('saturday', 'saturday'), equals(0))
})

test_that("calculation of Levenshtein similarity", {
  expect_that(levenshtein('car',      ''),         equals(0.000000000, tolerance = MAXERROR))
  expect_that(levenshtein('car',      'cat'),      equals(0.666666667, tolerance = MAXERROR))
  expect_that(levenshtein('kitten',   'sitting'),  equals(0.571428571, tolerance = MAXERROR))
  expect_that(levenshtein('saturday', 'sunday'),   equals(0.625000000, tolerance = MAXERROR))
  expect_that(levenshtein('source',   'target'),   equals(0.166666667, tolerance = MAXERROR))
  expect_that(levenshtein('ABCVWXYZ', 'CABVWXYZ'), equals(0.750000000, tolerance = MAXERROR))
  expect_that(levenshtein('saturday', 'saturday'), equals(1.000000000, tolerance = MAXERROR))
})
