# Create metadata cache environment
.onLoad <- function(libname, pkgname) {
    assign(".meta_env",
           new.env(parent = emptyenv()),
           envir = parent.env(environment()))
}
