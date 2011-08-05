
interpret_ds <- function(ds) {
  base <- api_base
  if (class(ds) == 'list' && setequal(
      names(ds),
      c('status', 'dimensions', 'meta', 'title', 'ds', 'id')
      )) {
    infos = list(ds)
    names(infos) = c(ds$id)
    return(list(base=base, qs=list(ds=ds$id), infos=infos))
  } else if (class(ds) == 'list' && length(ds) >= 1 && setequal(
      names(ds[[1]]),
      c('status', 'dimensions', 'meta', 'title', 'ds', 'id')
      )) {
    infos = ds
    names(infos) = sapply(ds, FUN=function(s) s$id)
    return(list(
      base=base,
      qs=list(ds=paste(sapply(ds, FUN=function(s) s$id), collapse='/')),
      infos=infos
      ))
  }
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

