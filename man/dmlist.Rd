\name{dmlist}
\alias{dmlist}
\title{Fetch data in long form as a data.frame from a DataMarket dataset.}
\usage{
  dmlist(ds, .params = list(), ...)
}
\arguments{
  \item{ds}{a dataset ID, DS string, URL query-string, or
  whole URL. The DS string to send is extracted from the
  URL as needed, and short URLs at data.is, bit.ly, is.gd,
  t.co and url.is are expanded.}

  \item{.params}{extra GET parameters to pass along in the
  API request.}

  \item{...}{named parameters whose names are dimension
  titles or IDs, and whose values are titles or IDs of
  values of those dimensions. E.g. if dataset \code{17tm}
  has a dimension named Country, then \code{dmlist("17tm",
  Country='Algeria')} filters on that dimension.}
}
\value{
  a zoo object representing the fetched timeseries.
}
\description{
  This performs a \code{list} API request at
  DataMarket.com, fetching the requested data and wrapping
  it in a \code{zoo} object.
}
\examples{
dmlist("17tm")
dmlist("17tm|kqc=a")
dmlist("ds=17tm")
dmlist("ds=17tm|kqc=a")
dmlist("foo=bar&ds=17tm&baz=xyzzy")
dmlist("http://datamarket.com/api/v1/series.json?foo=bar&ds=17tm&baz=xyzzy")
dmlist("http://datamarket.com/data/set/17tm/#ds=17tm")
dmlist("17tm", Country='Algeria')
dmlist("17tm", Country=c('Algeria', 'Angola'))
}

