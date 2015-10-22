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

test_that("bigrams of class", {
  text_bigram <- bigrams('kitten')

  expect_is(text_bigram, 'bigrams')
  expect_is(text_bigram, 'character')
})

test_that("bigrams of strings", {
  expect_identical(bigrams(''), c())

  sol        <- c('ca', 'ar')
  class(sol) <- c('bigrams', 'character')
  expect_identical(bigrams('car'), sol)

  sol        <- c('ca', 'at')
  class(sol) <- c('bigrams', 'character')
  expect_identical(bigrams('cat'), sol)

  sol        <- c('ki', 'it', 'tt', 'te', 'en')
  class(sol) <- c('bigrams', 'character')
  expect_identical(bigrams('kitten'), sol)

  sol        <- c('Gu', 'ut', 'ti', 'ie', 'er' ,'rr', 're', 'ez')
  class(sol) <- c('bigrams', 'character')
  expect_identical(bigrams('Gutierrez'), sol)
})

test_that("calculation of Sorensen-Dice distance", {
  expect_equal(sorensenDice('car',      ''),         0.000000000, tolerance = MAXERROR)
  expect_equal(sorensenDice('car',      'cat'),      0.500000000, tolerance = MAXERROR)
  expect_equal(sorensenDice('kitten',   'sitting'),  0.363636364, tolerance = MAXERROR)
  expect_equal(sorensenDice('saturday', 'sunday'),   0.333333333, tolerance = MAXERROR)
  expect_equal(sorensenDice('source',   'target'),   0.000000000, tolerance = MAXERROR)
  expect_equal(sorensenDice('ABCVWXYZ', 'CABVWXYZ'), 0.714285714, tolerance = MAXERROR)
  expect_equal(sorensenDice('saturday', 'saturday'), 1.000000000, tolerance = MAXERROR)
})
