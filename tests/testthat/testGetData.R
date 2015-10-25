##
##  Tests for get santions list methods
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

context("Get data tests")

test_that("test get sanctions from HM Treasury", {
  santions = getSanctionsHMTreasury(file = 'data/sanctionsHM.csv')

  expect_equal(dim(santions),    c(4, 7))
  expect_equal(length(santions), 4)

  expect_equal(santions$data[1, 1], 'Entity')
  expect_equal(santions$data[1, 2], '13188')
  expect_equal(santions$data[1, 3], 'ANSAR AL SHARIA')
  expect_equal(santions$data[1, 4], '')
  expect_equal(santions$data[1, 5], '')
  expect_equal(santions$data[1, 6], '')
  expect_equal(santions$data[1, 7], 'AKA')
})
