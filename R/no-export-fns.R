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

#' Check `FUN` Argument
#'
#' Standardized check for function
#'
#' @inheritParams model_write
#' @details Simply checks if `is.function(FUN)`, and if `FALSE`,
#'   returns an error
#' @note For internal use only, users need not concern
#'   themselves with this function

check_FUN = function(FUN) {
  if (!is.function(FUN) & !is.null(FUN)) stop("'FUN' must be a function object")
}

#' Check `model_file` Argument
#'
#' Standardized check for JAGS model file names
#'
#' @param model_file Name of file containing JAGS model code
#' @details Currently simply checks if is `NULL`,
#'   but more informative checks will likely be added in future versions
#'
check_model_file = function(model_file = NULL) {

  if (is.null(model_file)) stop ("'model_file' cannot be NULL")

}

#' Convert JAGS Model from Function to Character Format
#'
#' Isolates the function body into a standard
#' character vector
#'
#' @param FUN Function containing JAGS model code
#' @param keep_comments Logical flag indicating whether to retain comments
#'   (i.e., lines starting with `#`) in the output (default is `TRUE`)
#' @note For internal use only, users need not concern
#'   themselves with this function
#' @return Character vector with elements containing distinct lines of code from
#'   the body of the function supplied to `FUN`

FUN2char = function(FUN, keep_comments = TRUE) {

  # check if FUN is a function object
  check_FUN(FUN)

  # extract the function body, including comments
  code = attr(FUN, "srcref")
  code = as.character(code)

  # remove first line: this is either "{" or "function() {"
  code = code[-1]

  # remove the last line: this is always "}"
  code = code[-length(code)]

  # remove white space at the front of each line
  code = drop_lws(code)

  # remove any comment lines if requested
  if (!keep_comments) code = code[!stringr::str_detect(code, "^#")]

  # return the output
  return(code)
}

#' Convert JAGS Model from Character to Function Format
#'
#' @param char Character vector with elements storing distinct lines of JAGS
#'   model code
#' @note For internal use only, users need not concern
#'   themselves with this function
#' @return Function containing JAGS model code

char2FUN = function(char) {

  # cut of model head and tail if needed
  if (char[1] == "model {") char = char[-1]
  if (char[length(char)] == "}  # END OF MODEL") char = char[-length(char)]

  # wrap the code into a function
  char = c("function() {", char, "}")

  # style the code
  char = jags_styler(char)

  # create it as an object
  eval(parse(text = char))
}

#' Style code for JAGS
#'
#' Ensures printed code is formatted nicely and easily readable by humans
#'
#' @param x Character vector with elements storing distinct lines of JAGS
#'   model code
#' @note For internal use only, users need not concern themselves
#'   with this function
#' @details Primarily used for managing indentation when multiline
#'   code is wrapped in `{}`

jags_styler = function(x) {
  protect_T_I = function(x) {
    x |>
      stringr::str_replace("\\)\\s?T\\(", ") %_% T(") |>
      stringr::str_replace("\\)\\s?I\\(", ") %_% I(")
  }

  styler::style_text(
    protect_T_I(x),
    math_token_spacing = styler::specify_math_token_spacing(zero = c("'/'", "'^'")),
    scope = I(c("line_breaks", "tokens", "indention")), strict = TRUE
  )
}
