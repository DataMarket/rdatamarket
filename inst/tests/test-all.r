context("DataMarket dataset info")

test_that("Dataset 17tm looks as expected", {
  info <- dminfo('17tm')[[1]]
  expect_equal(info$id, '17tm')
  expect_equal(info$ds, '17tm')
  expect_equal(info$title, 'Oil: Production tonnes')
  expect_is(info$dimensions, 'list')
  dim1 <- info$dimensions[[1]]
  expect_equal(dim1$id, 'kqc')
  expect_equal(dim1$title, 'Country')
  expect_equal(dim1$type, 'simple')
  expect_is(dim1$values, 'list')
  expect_equal(dim1$values[[1]]$id, 'a')
  expect_equal(dim1$values[[1]]$title, 'Algeria')
  expect_equal(dim1$values[[2]]$id, '17')
  expect_equal(dim1$values[[2]]$title, 'Angola')
  expect_equal(dim1$values[[3]]$id, 'd')
  expect_equal(dim1$values[[3]]$title, 'Argentina')
})

context("DataMarket timeseries")

test_that("Timeseries from dataset 17tm works", {
  series <- dmseries('17tm|kqc=a.17.d')
  expect_is(series, 'zoo')
  expect_identical(names(series), c('Algeria', 'Angola', 'Argentina'))
  expect_equal(as.numeric(series[1]), c(26.481, 0.655, 13.7647586207))
  expect_equal(as.numeric(series[2]), c(33.872, 0.631, 14.6439655172))
  times = index(series)
  expect_identical(times[1], as.Date('1965-07-01'))
  expect_identical(times[2], as.Date('1966-07-01'))
})

context("DataMarket long-form data ('list')")

test_that("Long-form data from dataset 17tm works", {
  lis <- dmlist('17tm|kqc=a.17.d')
  expect_is(lis, 'data.frame')
  expect_identical(names(lis), c('Country', 'Year', 'Value'))
  expect_identical(as.character(lis$Country), c(
    replicate(46, 'Algeria'),
    replicate(46, 'Angola'),
    replicate(46, 'Argentina')
  ))
  expect_identical(lis$Year, c(replicate(3, 1965:2010)))
  expect_equal(lis$Value[1:4], c(26.481, 33.872, 39.076, 42.904))
})

