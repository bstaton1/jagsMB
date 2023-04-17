#' Create a Section Header to Insert into JAGS Model
#'
#' Constructs a section header to be placed in a JAGS model file.
#'
#' @param text Character string with the text to insert in the
#'   header block
#' @param decor Character string containing the "decoration" that
#'   will precede the text in the header. Will be reversed and placed at end of
#'   header text as well (default is `"### --- "`).
#' @param toupper Logical flag indicating whether to convert all letters to
#'   upper case (default is `TRUE`).
#' @note If the first character of `decor` is not `#` or if either `text` or
#'  `decor` have length > 1, an error will be returned.
#' @return Function ready to be included as a list element
#'   and passed to [`model_build()`].
#' @export

model_header = function(text = "text", decor = "### --- ", toupper = TRUE) {

  # error checks to ensure proper functionality based on supplied arguments
  if (length(text) > 1) {
    stop ("'text' must have only one element")
  }

  if (length(decor) > 1) {
    stop("'decor' must have only one element")
  }

  if (!stringr::str_detect(decor, "^#")) {
    stop ("the first character in 'decor' must be '#'")
  }

  # convert to upper case if requested
  header_text = ifelse(toupper, toupper(text), text)

  # handle the "decor"
  front = decor
  back = paste(rev(stringr::str_split_1(front, pattern = "")), collapse = "")

  # construct the output code
  code = paste0(front, header_text, back)
  code = c("function() {", code, "}")

  # return as function
  eval(parse(text = code))
}

#' Combine Components into One Model
#'
#' Extracts the body from multiple functions (which store individual
#' model components), combines them, and returns the combined function.
#'
#' @param FUN_list List object where each element stores a function
#'   (containing a JAGS model component) to be combined with other
#'   model components.
#' @return Function with the function bodies contained in the elements of
#'  `FUN_list` combined into the body of one function.
#' @export

model_build = function(FUN_list) {

  # put the pieces together
  code = unlist(lapply(FUN_list, function(f) c(FUN2char(f), "")))

  # convert the code into a function
  char2FUN(code)
}

