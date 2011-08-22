context("DataMarket dataset info")

test_that("Dataset 17tm looks as expected", {
  info <- dminfo('17tm')
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

test_that("Info is kept around if passed to interpret_ds", {
  mock <- list(id='17tm', ds='17tm', meta='dummy', dimensions='dummy',
    status='dummy', title='dummy')
  expect_identical(interpret_ds(mock), list(
    base='http://datamarket.com',
    qs=list(ds='17tm'),
    infos=list(`17tm`=mock)
    ))

  mock2 <- list(id='foo', ds='foo', meta='dummy', dimensions='dummy',
    status='dummy', title='dummy')
  expect_identical(interpret_ds(list(`17tm`=mock, foo=mock2)), list(
    base='http://datamarket.com',
    qs=list(ds='17tm/foo'),
    infos=list(`17tm`=mock, foo=mock2)
    ))

  # and also if unnamed
  expect_identical(interpret_ds(list(mock, mock2)), list(
    base='http://datamarket.com',
    qs=list(ds='17tm/foo'),
    infos=list(`17tm`=mock, foo=mock2)
    ))
})

test_that("Info is reused if given to dminfo", {
  mock <- list(id='17tm', ds='17tm', meta='dummy', dimensions='dummy',
    status='dummy', title='dummy')
  expect_identical(dminfo(mock), list(`17tm`=mock))
  mock <- list(`17tm`=mock)
  expect_identical(dminfo(mock), mock)
})

context("DataMarket timeseries")

test_that("Timeseries from dataset 17tm works", {
  series <- dmseries('17tm|kqc=a.17.d')
  expect_is(series, 'zoo')
  expect_identical(names(series), c('Algeria', 'Angola', 'Argentina'))
  expect_equal(as.numeric(series[1]), c(26.481, 0.655, 13.7647586207))
  expect_equal(as.numeric(series[2]), c(33.872, 0.631, 14.6439655172))
  times <- index(series)
  expect_identical(times[1], as.Date('1965-07-01'))
  expect_identical(times[2], as.Date('1966-07-01'))
})

test_that("Timeseries with parameter dimension filtering works", {
  series <- dmseries('17tm', Country='Algeria')
  expect_is(series, 'zoo')
  expect_equal(as.numeric(series[1]), c(26.481))
  expect_equal(as.numeric(series[2]), c(33.872))
  times <- index(series)
  expect_identical(times[1], as.Date('1965-07-01'))
  expect_identical(times[2], as.Date('1966-07-01'))

  # or, in short:
  series_from_param <- dmseries('17tm', Country='Algeria')
  series_from_dsstring <- dmseries('17tm|kqc=a')
  expect_identical(series_from_param, series_from_dsstring)

})

test_that("Timeseries with multi-valued parameter dimension filtering works", {
  series_from_param <- dmseries('17tm', Country=c('Algeria', 'Angola',
    'Argentina'))
  series_from_dsstring <- dmseries('17tm|kqc=a.17.d')
  expect_identical(series_from_param, series_from_dsstring)
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

test_that("Long-form data with parameter dimension filtering works", {
  list_from_param <- dmlist('17tm', Country='Algeria')
  list_from_dsstring <- dmlist('17tm|kqc=a')
  expect_identical(list_from_param, list_from_dsstring)
})

test_that("Long-form with multi-valued parameter dimension filtering works", {
  list_from_param <- dmlist('17tm', Country=c('Algeria', 'Angola', 'Argentina'))
  list_from_dsstring <- dmlist('17tm|kqc=a.17.d')
  expect_identical(list_from_param, list_from_dsstring)
})

context("Util functions")

test_that("dimfilter forms DS strings from named args correctly", {
  mockinfos <- list(`17tm`=list(dimensions=list(kqc=list(
    id='kqc', title='Country', values=list(
      a=list(id='a', title='Algeria'),
      `17`=list(id='17', title='Angola'),
      d=list(id='d', title='Argentina')
    )
  ))))
  result <- dimfilter('17tm', mockinfos, Country='Algeria')
  expect_equal(result, '17tm|kqc=a')
  result <- dimfilter('17tm', mockinfos, Country=c('Angola', 'Argentina'))
  expect_equal(result, '17tm|kqc=17.d')
})

