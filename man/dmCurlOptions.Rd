\name{dmCurlOptions}
\alias{dmCurlOptions}
\title{Set extra RCurl options for full HTTP control.}
\usage{
  dmCurlOptions(..., .opts = list())
}
\arguments{
  \item{...}{name-value pairs specifying curl options. See
  full list of options with
  `names(getCurlOptionsConstants())`, their types with
  `getCurlOptionTypes()`, and full documentation in
  \href{http://curl.haxx.se/docs/manpage.html}{the curl
  manpage}.}

  \item{.opts}{a named list of options, or a previously
  created `CURLOptions` object. These are merged with the
  options specified in `...`.}
}
\description{
  Use this to control the options with which RCurl
  connections are created, see
  \href{http://www.omegahat.org/RCurl/installed/RCurl/html/curlOptions.html}{RCurl::curlOptions}.
  A common use is setting a proxy to work with your
  company's firewall setup, see example below.
}
\examples{
\dontrun{dmCurlOptions(proxy='http://outproxy.mycompany.com')}
}

