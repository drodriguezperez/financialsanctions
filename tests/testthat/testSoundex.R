##
##  Soundex distance test
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

context("Soundex distance tests")

MAXERROR <- 1e-8

test_that("test Soundex class", {
  text_soundex <- soundex_code('kitten')

  expect_is(text_soundex, 'soundex')
  expect_is(text_soundex, 'character')
})

test_that("calculation of Soundex code", {
  expect_null(soundex_code(c()))
  expect_null(soundex_code(''))

  sol        <- 'S532'
  class(sol) <- c('soundex', 'character')
  expect_identical(soundex_code('Soundex'), sol)

  sol        <- 'C600'
  class(sol) <- c('soundex', 'character')
  expect_identical(soundex_code('car'), sol)

  sol        <- 'C300'
  class(sol) <- c('soundex', 'character')
  expect_identical(soundex_code('cat'), sol)

  sol        <- 'K350'
  class(sol) <- c('soundex', 'character')
  expect_identical(soundex_code('kitten'), sol)

  sol        <- 'S352'
  class(sol) <- c('soundex', 'character')
  expect_identical(soundex_code('sitting'), sol)

  sol        <- 'S363'
  class(sol) <- c('soundex', 'character')
  expect_identical(soundex_code('saturday'), sol)

  sol        <- 'S530'
  class(sol) <- c('soundex', 'character')
  expect_identical(soundex_code('sunday'), sol)

  sol        <- 'S620'
  class(sol) <- c('soundex', 'character')
  expect_identical(soundex_code('source'), sol)

  sol        <- 'T623'
  class(sol) <- c('soundex', 'character')
  expect_identical(soundex_code('target'), sol)

  sol        <- 'A121'
  class(sol) <- c('soundex', 'character')
  expect_identical(soundex_code('ABCVWXYZ'), sol)

  sol        <- 'C122'
  class(sol) <- c('soundex', 'character')
  expect_identical(soundex_code('CABVWXYZ'), sol)

  sol        <- 'G362'
  class(sol) <- c('soundex', 'character')
  expect_identical(soundex_code('Gutierrez'), sol)

  sol        <- 'P236'
  class(sol) <- c('soundex', 'character')
  expect_identical(soundex_code('Pfister'), sol)

  sol        <- 'J250'
  class(sol) <- c('soundex', 'character')
  expect_identical(soundex_code('Jackson'), sol)

  sol        <- 'T522'
  class(sol) <- c('soundex', 'character')
  expect_identical(soundex_code('Tymczak'), sol)
})

test_that("calculation of Soundex diference using array", {
  soundex_str <- c('cat', 'sitting', 'sunday', 'saturday')
  soundex_val <- c(soundex_code('cat'), soundex_code('sitting'),
                   soundex_code('sunday'), soundex_code('saturday'))

  class(soundex_val) <- c('soundex', 'character')

  expect_equal(soundex_code(soundex_str), soundex_val)
})

test_that("calculation of Soundex distance", {
  expect_null(soundex_difference(c(), 'cat'))
  expect_null(soundex_difference('cat', c()))

  expect_equal(soundex_difference('car',      ''),         0)
  expect_equal(soundex_difference('car',      'cat'),      2)
  expect_equal(soundex_difference('kitten',   'sitting'),  2)
  expect_equal(soundex_difference('saturday', 'sunday'),   3)
  expect_equal(soundex_difference('source',   'target'),   2)
  expect_equal(soundex_difference('ABCVWXYZ', 'CABVWXYZ'), 2)
  expect_equal(soundex_difference('saturday', 'saturday'), 4)
})

test_that("calculation of Soundex diference using array", {
  soundex_str <- c('cat', 'sitting', 'sunday', 'saturday')
  soundex_val <- c(2, 0, 1, 1)

  expect_equal(soundex_difference(soundex_str, 'car'), soundex_val)
})

test_that("calculation of Soundex distance", {
  expect_equal(soundex('car',      ''),         0.000000000, tolerance = MAXERROR)
  expect_equal(soundex('car',      'cat'),      0.500000000, tolerance = MAXERROR)
  expect_equal(soundex('kitten',   'sitting'),  0.500000000, tolerance = MAXERROR)
  expect_equal(soundex('saturday', 'sunday'),   0.750000000, tolerance = MAXERROR)
  expect_equal(soundex('source',   'target'),   0.500000000, tolerance = MAXERROR)
  expect_equal(soundex('ABCVWXYZ', 'CABVWXYZ'), 0.500000000, tolerance = MAXERROR)
  expect_equal(soundex('saturday', 'saturday'), 1.000000000, tolerance = MAXERROR)
})

test_that("calculation of Soundex distance using array", {
  soundex_str <- c('cat', 'sitting', 'sunday', 'saturday')
  soundex_val <- c(0.5, 0, 0.25, 0.25)

  expect_equal(soundex(soundex_str, 'car'), soundex_val,  tolerance = MAXERROR)
})
