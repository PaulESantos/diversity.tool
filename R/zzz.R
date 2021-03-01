
.onAttach <- function(lib, pkg)  {
  packageStartupMessage("This is diversity.tool ",
                        utils::packageDescription("diversity.tool",
                                                  fields="Version"),
                        appendLF = TRUE)
}


# -------------------------------------------------------------------------

show_progress <- function() {
  isTRUE(getOption("diversity.tool.show_progress")) && # user disables progress bar
    interactive()  # Not actively knitting a document
}



.onLoad <- function(libname, pkgname) {
  opt <- options()
  opt_diversity.tool <- list(
    diversity.tool.show_progress = TRUE
  )
  to_set <- !(names(opt_diversity.tool) %in% names(opt))
  if(any(to_set)) options(opt_diversity.tool[to_set])
  invisible()
}


# -------------------------------------------------------------------------


