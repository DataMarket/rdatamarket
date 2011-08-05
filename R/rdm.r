library(zoo)
library(RCurl)
library(RJSONIO)

api_base <- 'http://datamarket.com'
path_info <- '/api/v1/info.json'
path_series <- '/api/v1/series.csv'
path_list <- '/api/v1/list.csv'
short_url_services <- c(
  'http://data.is',
  'http://bit.ly',
  'http://is.gd',
  'http://t.co',
  'http://url.is'
)

#' Fetch information about a DataMarket dataset.
#'
#' This function makes an \code{info} API call to fetch metadata (including
#' dimension structure) of a DataMarket dataset.
#'
#' @param ds a dataset ID, DS string, URL query-string, or whole URL. The DS
#'           string to send is extracted from the URL as needed, and short URLs
#'           at data.is, bit.ly, is.gd, t.co and url.is are expanded.
#' @param .params extra GET parameters to pass along in the API request.
#' @return a structure of named lists representing the dataset metadata.
#' @export
#' @examples
#' dminfo("17tm")
#' dminfo("17tm|kqc=a")
#' dminfo("ds=17tm")
#' dminfo("ds=17tm|kqc=a")
#' dminfo("foo=bar&ds=17tm&baz=xyzzy")
#' dminfo("http://datamarket.com/api/v1/series.json?foo=bar&ds=17tm&baz=xyzzy")
#' dminfo("http://datamarket.com/data/set/17tm/#ds=17tm")
dminfo <- function(ds, .params=list()) {
  ctx <- interpret_ds(ds)
  if ('infos' %in% names(ctx)) {
    return(ctx$infos)
  }
  infojson <- getForm(
    paste(ctx$base, path_info, sep=''),
    .params=c(ctx$qs, callback='', .params=.params)
    )
  infolist <- fromJSON(infojson)
  names(infolist) <- lapply(infolist, FUN=function(i) i$ds)
  for (name in names(infolist)) {
    names(infolist[[name]]$dimensions) <- lapply(infolist[[name]]$dimensions,
      FUN=function(dim) dim$id
    )
    for (dimid in names(infolist[[name]]$dimensions)) {
      names(infolist[[name]]$dimensions[[dimid]]$values) <- lapply(
        infolist[[name]]$dimensions[[dimid]]$values,
        FUN=function(dimvalue) dimvalue$id
      )
    }
  }
  return(infolist)
}

#' Fetch dimensions of a DataMarket dataset.
#'
#' This is just shorthand for
#' \code{lapply(dminfo(ds, .params=.params), FUN=function(info)
#' info$dimensions)}
#'
#' @param ds a dataset ID, DS string, URL query-string, or whole URL. The DS
#'           string to send is extracted from the URL as needed, and short URLs
#'           at data.is, bit.ly, is.gd, t.co and url.is are expanded.
#'           If the DS string contains dimension filter specifications (the
#'           stuff after the | character, so it's not just a dataset ID), these
#'           are preserved in the request to the API, but for normal DataMarket
#'           datasets they do not affect the response.
#' @param .params extra GET parameters to pass along in the API request.
#' @return named list of dataset dimension information. Each name is a dataset
#'         ID and each element is a named list of dimensions of that dataset.
#'         Each dimension is named for its dimension ID in that list, and is
#'         itself a named list of the four properties \code{id, title, type,
#'         values}. The first three of these properties are character strings,
#'         while \code{values} is a named list of dimension values. Each of
#'         these is a list of two properties \code{id, title}, and the \code{id}
#'         is also the name of the dimension value
#' @export
#' @examples
#' dmdims("17tm")
#' dmdims("17tm|kqc=a")
#' dmdims("ds=17tm")
#' dmdims("ds=17tm|kqc=a")
#' dmdims("foo=bar&ds=17tm&baz=xyzzy")
#' dmdims("http://datamarket.com/api/v1/series.json?foo=bar&ds=17tm&baz=xyzzy")
#' dmdims("http://datamarket.com/data/set/17tm/#ds=17tm")
dmdims <- function(ds, .params=list()) {
  infolist <- dminfo(ds, .params=.params)
  lapply(infolist, FUN=function(info) info$dimensions)
}

#' Fetch timeseries from a DataMarket dataset.
#'
#' This performs a \code{series} API request at DataMarket.com, fetching the
#' requested data and wrapping it in a \code{zoo} object.
#'
#' @param ds a dataset ID, DS string, URL query-string, or whole URL. The DS
#'           string to send is extracted from the URL as needed, and short URLs
#'           at data.is, bit.ly, is.gd, t.co and url.is are expanded.
#' @param .params extra GET parameters to pass along in the API request.
#' @param ... named parameters whose names are dimension titles or IDs, and
#'            whose values are titles or IDs of values of those dimensions.
#'            E.g. if dataset \code{17tm} has a dimension named Country, then
#'            \code{dmseries("17tm", Country='Algeria')} filters on that
#'            dimension.
#' @return a zoo object representing the fetched timeseries.
#' @export
#' @examples
#' dmseries("17tm")
#' dmseries("17tm|kqc=a")
#' dmseries("ds=17tm")
#' dmseries("ds=17tm|kqc=a")
#' dmseries("foo=bar&ds=17tm&baz=xyzzy")
#' dmseries("http://datamarket.com/api/v1/series.json?foo=bar&ds=17tm&baz=xyzzy")
#' dmseries("http://datamarket.com/data/set/17tm/#ds=17tm")
#' dmseries("17tm", Country='Algeria')
#' dmseries("17tm", Country=c('Algeria', 'Angola'))
dmseries <- function(ds, .params=list(), ...) {
  ctx <- interpret_ds(ds)
  if (!(identical(c(...), c()))) {
    infos <- dminfo(ds)
    ctx$qs$ds <- dimfilter(ctx$qs$ds, infos, ...)
  }
  content <- getForm(
    paste(ctx$base, path_series, sep=''),
    .params=c(ctx$qs, use_mid_dates=1, callback='', .params)
    )
  conn <- textConnection(content)
  csv <- read.csv(conn, header=TRUE)
  close(conn)
  .check_csv_errors(csv)
  for (name in names(csv)) {
    if (all(is.na(csv[[name]]))) {
      csv[[name]] <- NULL
    }
  }
  for (name in c("Date", "Year.and.week", "Year.and.month", "Year.and.quarter", "Year")) {
    if (name %in% names(csv)) {
      timecolname <- name
      break
    }
  }
  zindex <- as.Date(csv[[timecolname]])
  csv[[timecolname]] <- NULL
  zoo(csv, order.by=zindex)
}

#' Fetch data in long form as a data.frame from a DataMarket dataset.
#'
#' This performs a \code{list} API request at DataMarket.com, fetching the
#' requested data and wrapping it in a \code{zoo} object.
#'
#' @param ds a dataset ID, DS string, URL query-string, or whole URL. The DS
#'           string to send is extracted from the URL as needed, and short URLs
#'           at data.is, bit.ly, is.gd, t.co and url.is are expanded.
#' @param .params extra GET parameters to pass along in the API request.
#' @param ... named parameters whose names are dimension titles or IDs, and
#'            whose values are titles or IDs of values of those dimensions.
#'            E.g. if dataset \code{17tm} has a dimension named Country, then
#'            \code{dmlist("17tm", Country='Algeria')} filters on that
#'            dimension.
#' @return a zoo object representing the fetched timeseries.
#' @export
#' @examples
#' dmlist("17tm")
#' dmlist("17tm|kqc=a")
#' dmlist("ds=17tm")
#' dmlist("ds=17tm|kqc=a")
#' dmlist("foo=bar&ds=17tm&baz=xyzzy")
#' dmlist("http://datamarket.com/api/v1/series.json?foo=bar&ds=17tm&baz=xyzzy")
#' dmlist("http://datamarket.com/data/set/17tm/#ds=17tm")
#' dmlist("17tm", Country='Algeria')
#' dmlist("17tm", Country=c('Algeria', 'Angola'))
dmlist <- function(ds, .params=list(), ...) {
  ctx <- interpret_ds(ds)
  if (!(identical(c(...), c()))) {
    infos <- dminfo(ds)
    ctx$qs$ds <- dimfilter(ctx$qs$ds, infos, ...)
  }
  content <- getForm(
    paste(ctx$base, path_list, sep=''),
    .params=c(ctx$qs, use_mid_dates=1, callback='', .params)
    )
  conn <- textConnection(content)
  csv <- read.csv(conn, header=TRUE)
  close(conn)
  .check_csv_errors(csv)
  csv
}
