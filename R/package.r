#' Data access API for DataMarket.com
#'
#' Fetches data from DataMarket.com, either as timeseries in
#' zoo form (dmseries) or as long-form data frames (dmlist). Metadata
#' including dimension structure is fetched with dminfo, or just the
#' dimensions with dmdims.
#'
#' DS parameters
#'
#' Functions that take a parameter named \code{ds} can accept a dataset ID or DS
#' string (see http://datamarket.com/api/v1/) or a query string or URL
#' containing a DS string, or a short URL (\code{data.is, bit.ly, is.gd, t.co,
#' url.is} redirecting to such a URL.
#'
#' Generally, when browsing a dataset on datamarket.com, you should be able to
#' copy the URL from your browser bar and paste into a call to \code{dmseries}
#' or \code{dmlist} to get the same data into R.
#'
#' @import zoo RCurl RJSONIO
#' @docType package
#' @name rdm
#' @aliases rdm package-rdm
NULL
