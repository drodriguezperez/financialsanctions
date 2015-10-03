##
##  Sorensen-Dice coefficient tests
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

context("Sorensen-Dice coefficient tests")

MAXERROR <- 1e-8

test_that("bigrams of strings", {
  expect_that(bigrams(''),          equals(c()))
  expect_that(bigrams('car'),       equals(c('ca', 'ar')))
  expect_that(bigrams('cat'),       equals(c('ca', 'at')))
  expect_that(bigrams('kitten'),    equals(c('ki', 'it', 'tt', 'te', 'en')))
  expect_that(bigrams('Gutierrez'), equals(c('Gu', 'ut', 'ti', 'ie', 'er' ,'rr', 're', 'ez')))
})

test_that("calculation of Sorensen-Dice distance", {
  expect_that(sorensenDice('car',      ''),         equals(0.000000000, tolerance = MAXERROR))
  expect_that(sorensenDice('car',      'cat'),      equals(0.500000000, tolerance = MAXERROR))
  expect_that(sorensenDice('kitten',   'sitting'),  equals(0.363636364, tolerance = MAXERROR))
  expect_that(sorensenDice('saturday', 'sunday'),   equals(0.333333333, tolerance = MAXERROR))
  expect_that(sorensenDice('source',   'target'),   equals(0.000000000, tolerance = MAXERROR))
  expect_that(sorensenDice('ABCVWXYZ', 'CABVWXYZ'), equals(0.714285714, tolerance = MAXERROR))
  expect_that(sorensenDice('saturday', 'saturday'), equals(1.000000000, tolerance = MAXERROR))
})
