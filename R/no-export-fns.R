#' Drop Leading Whitespace
#'
#' Shortcut for `stringr::str_remove(x, "^\\s+")`
#'
#' @param x Character string that has leading white space to be removed
#' @param n number of spaces to remove, default is `NULL` to remove all
#' @note For internal use only, users need not concern
#'   themselves with this function

drop_lws = function(x, n = NULL) {

  # build pattern to remove
  # either a number of spaces to remove or all spaces
  if (is.null(n)) {
    pattern = "^\\s+"
  } else {
    pattern = paste(c("^", rep("\\s", n)), collapse = "")
  }

  # return with spaces removed
  stringr::str_remove(x, pattern)
}

#' Drop Trailing Whitespace
#'
#' Shortcut for `stringr::str_remove(x, "\\s+$")`
#'
#' @param x Character string that has trailing white space to be removed
#' @note For internal use only, users need not concern
#'   themselves with this function

drop_tws = function(x) {
  stringr::str_remove(x, "\\s+$")
}

