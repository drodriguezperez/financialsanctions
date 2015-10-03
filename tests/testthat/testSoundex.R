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

test_that("calculation of Jaro-Winkler distances", {
  expect_that(soundex_code(''),          is_null())
  expect_that(soundex_code('Soundex'),   equals('S532'))
  expect_that(soundex_code('car'),       equals('C600'))
  expect_that(soundex_code('cat'),       equals('C300'))
  expect_that(soundex_code('kitten'),    equals('K350'))
  expect_that(soundex_code('sitting'),   equals('S352'))
  expect_that(soundex_code('saturday'),  equals('S363'))
  expect_that(soundex_code('sunday'),    equals('S530'))
  expect_that(soundex_code('source'),    equals('S620'))
  expect_that(soundex_code('target'),    equals('T623'))
  expect_that(soundex_code('ABCVWXYZ'),  equals('A121'))
  expect_that(soundex_code('CABVWXYZ'),  equals('C122'))
  expect_that(soundex_code('Gutierrez'), equals('G362'))
  expect_that(soundex_code('Pfister'),   equals('P236'))
  expect_that(soundex_code('Jackson'),   equals('J250'))
  expect_that(soundex_code('Tymczak'),   equals('T522'))

})
