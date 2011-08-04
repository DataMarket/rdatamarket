library(zoo)
library(RCurl)
library(RJSONIO)

api_base <- 'http://localhost'
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
#' @export
#' @examples
#' dminfo('17tm')
#' dminfo('17tm|kqc=a')
#' dminfo('ds=17tm')
#' dminfo('ds=17tm|kqc=a')
#' dminfo('foo=bar&ds=17tm&baz=xyzzy')
#' dminfo('http://datamarket.com/api/v1/series.json?foo=bar&ds=17tm&baz=xyzzy')
#' dminfo('http://datamarket.com/data/set/17tm/#ds=17tm')
dminfo <- function(ds) {
  ctx <- interpret_ds(ds)
  infojson <- getForm(
    paste(ctx$base, path_info, sep=''),
    .params=c(ctx$qs, callback='')
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
#' \code{lapply(dminfo(ds), FUN=function(info) info$dimensions)}
#'
#' @param ds a dataset ID, DS string, URL query-string, or whole URL. The DS
#'           string to send is extracted from the URL as needed, and short URLs
#'           at data.is, bit.ly, is.gd, t.co and url.is are expanded.
#'           If the DS string contains dimension filter specifications (the
#'           stuff after the | character, so it's not just a dataset ID), these
#'           are preserved in the request to the API, but for normal DataMarket
#'           datasets they do not affect the response.
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
#' dmdims('17tm')
#' dmdims('17tm|kqc=a')
#' dmdims('ds=17tm')
#' dmdims('ds=17tm|kqc=a')
#' dmdims('foo=bar&ds=17tm&baz=xyzzy')
#' dmdims('http://datamarket.com/api/v1/series.json?foo=bar&ds=17tm&baz=xyzzy')
#' dmdims('http://datamarket.com/data/set/17tm/#ds=17tm')
dmdims <- function(ds) {
  infolist <- dminfo(ds)
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
#' @return a zoo object representing the fetched timeseries.
#' @export
#' @examples
#' dmseries(17tm')
#' dmseries(17tm|kqc=a')
#' dmseries('ds=17tm')
#' dmseries('ds=17tm|kqc=a')
#' dmseries('foo=bar&ds=17tm&baz=xyzzy')
#' dmseries('http://datamarket.com/api/v1/series.json?foo=bar&ds=17tm&baz=xyzzy')
#' dmseries('http://datamarket.com/data/set/17tm/#ds=17tm')
dmseries <- function(ds, .params=list()) {
  ctx <- interpret_ds(ds)
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
      timecolname = name
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
#' @return a zoo object representing the fetched timeseries.
#' @export
#' @examples
#' dmlist(17tm')
#' dmlist(17tm|kqc=a')
#' dmlist('ds=17tm')
#' dmlist('ds=17tm|kqc=a')
#' dmlist('foo=bar&ds=17tm&baz=xyzzy')
#' dmlist('http://datamarket.com/api/v1/series.json?foo=bar&ds=17tm&baz=xyzzy')
#' dmlist('http://datamarket.com/data/set/17tm/#ds=17tm')
dmlist <- function(ds, .params=list()) {
  ctx <- interpret_ds(ds)
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

interpret_ds <- function(ds) {
  base <- api_base
  if (grepl('^https?:', ds)) {
    spliturl <- urlsplit(ds)
    base <- spliturl$base
    path <- spliturl$path
    anchor <- parse_qs(spliturl$anchor)
    qs <- parse_qs(spliturl$qs)
    if ('ds' %in% names(anchor)) {
      qs = anchor
    } else if ('ds' %in% names(qs)) {
    } else if (base %in% short_url_services) {
      h=basicTextGatherer()
      getURL(ds, headerFunction=h$update)
      conn = textConnection(paste(h$value(NULL)[-1], collapse=""))
      headers=as.list(read.dcf(conn)[1,])
      close(conn)
      if ('Location' %in% names(headers)) {
        return(interpret_ds(headers$Location))
      } else {
        stop("No redirect found for URL ", ds)
      }
    } else {
        stop("of URL ", ds)
    }
  } else if (grepl('&|^ds=', ds)) {
    qs <- parse_qs(ds)
  } else {
    qs <- list(ds=ds)
  }
  # FIXME: hardcoding use of stage while testing
  if (base == 'http://datamarket.com') {
    base = 'http://stage.datamarket.com'
  }
  list(base=base, qs=qs)
}

urlsplit <- function(url) {
  uri_and_query_and_anchor <- strsplit(url, '\\#')[[1]]
  uri_and_query <- strsplit(uri_and_query_and_anchor[[1]], '\\?')[[1]]
  uri <- uri_and_query[[1]]
  slashinds <- gregexpr('/', uri)[[1]]
  if (length(slashinds) >= 3 &&
      (slashinds[1:2]==c(6,7) || slashinds[1:2] == c(7,8)) &&
      substring(uri, 1, 4) == 'http'
    ) {
    slashind <- slashinds[3]
  } else {
    slashind <- slashinds[1]
  }
  list(
    base=substring(uri, 1, slashind - 1),
    path=substring(uri, slashind),
    qs=uri_and_query[2],
    anchor=uri_and_query_and_anchor[2]
  )
}

parse_qs <- function(qs) {
  if (class(qs) == 'list') {
    return(qs)
  }
  l = lapply(as.list(strsplit(qs, '&', fixed=TRUE)[[1]]), FUN=function(pair) {
    keyval <- as.list(strsplit(pair, '=', fixed=TRUE)[[1]])
    val <- ifelse(length(keyval) > 1, paste(keyval[-1], collapse='='), '')
    names(val) <- keyval[[1]]
    return(val)
  })
  names(l) = sapply(l, names)
  return(l)
}

.check_csv_errors <- function(csv) {
  if (names(csv)[[1]] == 'request-error') {
    stop("Request error: ", as.character(csv[[1]]))
  }
  if (names(csv)[[1]] == 'server-error') {
    stop("Server error: ", as.character(csv[[1]]))
  }
}
