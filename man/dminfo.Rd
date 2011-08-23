\name{dminfo}
\alias{dminfo}
\title{Fetch information about a DataMarket dataset.}
\usage{
  dminfo(ds, .params = list())
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
  a structure of named lists representing the dataset
  metadata. If the resolved DS string is a multiple-dataset
  DS string (that is, contains / forward-slash characters),
  then the returned value is a named list of such
  structures, with `strsplit(ds, '/')` as names.
}
\description{
  This function makes an \code{info} API call to fetch
  metadata (including dimension structure) of a DataMarket
  dataset.
}
\examples{
dminfo("17tm")
dminfo("17tm|kqc=a")
dminfo("ds=17tm")
dminfo("ds=17tm|kqc=a")
dminfo("foo=bar&ds=17tm&baz=xyzzy")
dminfo("http://datamarket.com/api/v1/series.json?foo=bar&ds=17tm&baz=xyzzy")
dminfo("http://datamarket.com/data/set/17tm/#ds=17tm")
dminfo("http://datamarket.com/data/set/12r9/male-population-thousands")
}

