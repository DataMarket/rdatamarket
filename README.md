# rdatamarket

The `rdatamarket` package is an R client for the [DataMarket.com
API](http://datamarket.com/api/v1/), fetching the contents and metadata
of datasets on DataMarket.com into R.

# Quick start

Just find the data you want on datamarket.com, then copy the URL from your
browser (or a short URL to it) into `dmlist` or `dmseries`:

    > plot(dmseries("http://datamarket.com/data/set/17tm/#ds=17tm|kqc=17.v.i"))
    > plot(dmseries("http://data.is/nyFeP9"))
    > l <- dmlist("http://data.is/nyFeP9"))

If you need to go through an HTTP proxy, set it up this way:

    > dmCurlOptions(proxy="http://outproxy.mycompany.com")

# Reading metadata

Get a dataset object (find the ID in a datamarket URL, or just paste in
the whole URL if you like):

    > oil <- dminfo("17tm")
    > oil <- dminfo("http://datamarket.com/data/set/17tm/#ds=17tm|kqc=17.v.i"))
    > print(oil)
    Title: "Oil: Production tonnes"
    Provider: "BP"
    Dimensions:
      "Country" (60 values):
        "Algeria"
        "Angola"
        "Argentina"
        "Australia"
        "Azerbaijan"
        [...]

See all the values of the `Country` dimension:

    > oil$dimensions[[1]]$values
      a  "Algeria"
     17  "Angola"
      d  "Argentina"
      z  "Australia"
     1l  "Azerbaijan"
     1b  "Brazil"
      v  "Brunei"
     1h  "Cameroon"
     13  "Canada"
     1o  "Chad"
    [...]

Here's a dataset with two dimensions (besides time):

    > p<-dminfo("http://datamarket.com/data/set/12r9/male-population-thousands")
    > print(p)
    Title: "Male population (thousands)"
    Provider: "United Nations" (citing "United Nations Population Division")
    Dimensions:
      "Country or Area" (229 values):
        "Afghanistan"
        "Africa"
        "Albania"
        "Algeria"
        "Angola"
        [...]
      "Variant" (5 values):
        "Constant-fertility scenario"
        "Estimate variant"
        "High variant"
        "Low variant"
        "Medium variant" 

# Reading data

From that last dataset, fetch the UN's population prediction for Sweden and Somalia in the
constant-fertility scenario (note the “(thousands)” in the dataset title):

    > dmseries(p, 'Country or Area'=c("Somalia", "Sweden"),
               Variant="Constant-fertility scenario")
                 Somalia   Sweden
    2010-07-01  4642.070 4613.551
    2015-07-01  5357.233 4725.918
    2020-07-01  6211.305 4840.434
    2025-07-01  7243.572 4942.865
    2030-07-01  8490.929 5021.646
    2035-07-01  9990.910 5083.680
    2040-07-01 11793.524 5144.685
    2045-07-01 13966.319 5211.212
    2050-07-01 16597.110 5281.437

    > dmlist(p, 'Country or Area'=c("Somalia", "Sweden"),
             Variant="Constant-fertility scenario")
       Country.or.Area                     Variant Year     Value
    1          Somalia Constant-fertility scenario 2010  4642.070
    2          Somalia Constant-fertility scenario 2015  5357.233
    3          Somalia Constant-fertility scenario 2020  6211.305
    4          Somalia Constant-fertility scenario 2025  7243.572
    5          Somalia Constant-fertility scenario 2030  8490.929
    6          Somalia Constant-fertility scenario 2035  9990.910
    7          Somalia Constant-fertility scenario 2040 11793.524
    8          Somalia Constant-fertility scenario 2045 13966.319
    9          Somalia Constant-fertility scenario 2050 16597.110
    10          Sweden Constant-fertility scenario 2010  4613.551
    11          Sweden Constant-fertility scenario 2015  4725.918
    12          Sweden Constant-fertility scenario 2020  4840.434
    13          Sweden Constant-fertility scenario 2025  4942.865
    14          Sweden Constant-fertility scenario 2030  5021.646
    15          Sweden Constant-fertility scenario 2035  5083.680
    16          Sweden Constant-fertility scenario 2040  5144.685
    17          Sweden Constant-fertility scenario 2045  5211.212
    18          Sweden Constant-fertility scenario 2050  5281.437

The above demonstrates *dimension filtering*; dimensions and their values can
be specified by their `$id` or their `$title`, to fetch the data filtered to
specific values of a dimension. If no filtering is specified, all of the
dataset is fetched (careful: some datasets are enormous, and the DataMarket.com
API may truncate extremely large responses).

