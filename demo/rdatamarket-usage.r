info <- dminfo("17tm")
dim1 <- info$dimensions[[1]]
series <- dmseries(paste(
  info$id, "|",
  dim1$id, "=", dim1$values[[1]]$id, ".", dim1$values[[2]]$id,
  collapse=""))
plot(series, main=info$title, xlab=info$meta$granularity)

series <- dmseries(info, Country=c("Algeria", "Angola"))
plot(series, main=info$title, xlab=info$meta$granularity)

