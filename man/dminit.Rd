\name{dminit}
\alias{dminit}
\title{Initialize DataMarket client with an API key}
\usage{
  dminit(api.key)
}
\arguments{
  \item{api.key}{a DataMarket API key, found in
  \href{http://datamarket.com/accounts/profile/}{your
  account profile}, or NULL to forget the previously
  entered API key.}
}
\description{
  An API key is required to access locked and non-public
  data. Access to public datasets does not require an API
  key, for now at least.
}
\examples{
dminit("1234567890abcdef1234567890abcdef")
dminit(NULL)
}

