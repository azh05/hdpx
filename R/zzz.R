.onLoad <- function(libname, pkgname) {
  assign("stir.closure", xmake.s(), envir = asNamespace("hdpx"))
}

.onAttach <- function(libname, pkgname) {
  uu <- utils::sessionInfo()$otherPkgs
  extra <- ifelse(is.null(uu), "", uu$hdpx$Version)
  # packageStartupMessage("This is ", libname, " and ", pkgname, " v", extra)
}
