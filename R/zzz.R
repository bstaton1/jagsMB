
.onLoad = function(libname, pkgname) {
  # this is required to ensure
  # build_model() works even if ran non-interactively
  options(keep.source = TRUE, styler.colored_print.vertical = FALSE)
}
