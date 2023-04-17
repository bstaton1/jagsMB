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

#' Write JAGS Model Definition File
#'
#' Wrapper for [base::writeLines()] but with defaults and
#' styling to handle indentation
#'
#' @param FUN Function containing JAGS model code.
#' @param model_file File name to write model code to;
#'   must have `".txt"` extension.
#'   Default (`NULL`) is to create a temporary file name with
#'   `tempfile(pattern = "", fileext = ".txt")`.
#' @param keep_commments Logical flag indicating whether to retain comments
#'  (i.e., lines starting with `#`) in the output (default `TRUE`).
#' @param keep_empty Logical flag indicating whether to retain entirely
#'   empty lines.
#' @return File name of the written model (returned invisibly); same value
#'   as `model_file`.
#' @note `model_write()` and [model_replace()] are the only functions that
#'   accept a `model_file` argument but do not default to
#'  `jagsMB_opts("model_file)"`. Instead they default to `NULL`, which returns
#'   an error requiring the user to supply a file path manually.
#' @source This function was heavily influenced by [R2OpenBUGS::write.model()].
#' @export

model_write = function(FUN,
                       model_file = NULL,
                       keep_comments = TRUE,
                       keep_empty = TRUE
                       ) {

  # check if FUN is a function object
  check_FUN(FUN)

  # create a random file name if one not supplied
  if (is.null(model_file)) {
    model_file = tempfile(pattern = "", fileext = ".txt")
  }

  # check if file is a .txt file
  file_ext = stringr::str_extract(basename(model_file), "\\.([[:alnum:]]+)$")
  if (file_ext != ".txt") {
    stop ("The specified extention of 'model_file' must be .txt")
  }

  # extract the function body
  code = FUN2char(FUN, keep_comments = keep_comments)

  # identify totally empty lines
  is_empty = nchar(code) == 0

  # identify adjacent totally empty lines and discard them
  if (length(code) > 1) {
    is_adj_empty = c(
      FALSE,
      sapply(2:length(code), function(i) {
        is_empty[i-1] & is_empty[i]
      })
    )
    code = code[!is_adj_empty]
    is_empty = nchar(code) == 0
  }

  # remove any empty lines if instructed
  if (!keep_empty) code = code[!is_empty]

  # add beginning and ending brackets
  code = c("{", code, "}  # END OF MODEL")

  # format the code
  code = jags_styler(code)

  # handle starting/ending spacing
  if (keep_empty) {
    code[1] = "model {\n"
    code[length(code)] = paste0("\n", code[length(code)])
  } else {
    code[1] = "model {"
  }

  # remove any instances of "%_%"
  code = stringr::str_remove(code, "%_%\\s?")

  # write the code to a file
  writeLines(code, model_file)

  # invisibly return the file path
  invisible(model_file)
}

