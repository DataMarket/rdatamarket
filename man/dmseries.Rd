\name{dmseries}
\alias{dmseries}
\title{Fetch timeseries from a DataMarket dataset.}
\usage{
  dmseries(ds, .params = list())
}
\arguments{
  \item{ds}{a dataset ID, DS string, URL query-string, or
  whole URL. The DS string to send is extracted from the
  URL as needed, and short URLs at data.is, bit.ly, is.gd,
  t.co and url.is are expanded.}

  \item{.params}{extra GET parameters to pass along in the
  API request.}
}
\value{
  a zoo object representing the fetched timeseries.
}
\description{
  This performs a \code{series} API request at
  DataMarket.com, fetching the requested data and wrapping
  it in a \code{zoo} object.
}
\examples{
dmseries("17tm")
dmseries("17tm|kqc=a")
dmseries("ds=17tm")
dmseries("ds=17tm|kqc=a")
dmseries("foo=bar&ds=17tm&baz=xyzzy")
dmseries("http://datamarket.com/api/v1/series.json?foo=bar&ds=17tm&baz=xyzzy")
dmseries("http://datamarket.com/data/set/17tm/#ds=17tm")
}

