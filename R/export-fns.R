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

#' Read Model File
#'
#' Wrapper for [base::readLines()] but with defaults and
#' styling to handle indentation
#'
#' @param model_file Name of file containing JAGS model code;
#'   defaults to `jagsMB_opts("model_file")`.
#' @param keep_lws Logical flag indicating whether to retain leading white space
#'   on each line (default is `TRUE`).
#' @return A character vector with elements storing each line of the model file
#'   contents.
#' @export

model_read = function(model_file = jagsMB_opts("model_file"),
                      keep_lws = TRUE
                      ) {

  # check the model_file argument for validity
  check_model_file(model_file)

  # read in the model file
  code = readLines(model_file, warn = FALSE)

  # remove leading white space if instructed
  if (!keep_lws) code = drop_lws(code)

  # return the model code
  return(code)
}

#' View Lines from a JAGS Model File
#'
#' Print specific lines from a JAGS model file
#' to the console. Enables quick printing
#' for Rmd explanations or viewing line numbers
#' referenced in syntax errors return by JAGS.
#'
#' @inheritParams model_read
#' @param lines Numeric vector specifying the line numbers to display.
#' @param show_labels Logical flag indicating whether to display line
#'   numbers with the output (defaults to `TRUE`).
#' @param keep_first_last Logical flag indicating whether to display
#'   the first and last line in the model code.
#'   These lines purely show `"model {"` and `"} # END OF MODEL"`,
#'   so sometimes it may be desirable to omit them.
#' @export

model_lines = function(model_file = jagsMB_opts("model_file"),
                       lines = NULL,
                       show_labels = TRUE,
                       keep_first_last = TRUE
                       ) {

  # check the model_file argument for validity
  check_model_file(model_file)

  # read in model code
  model_code = model_read(model_file)

  # count the lines
  n_lines = length(model_code)

  # stop if requested lines are outside of the model
  if (any(lines > n_lines)) {
    stop ("the model only contains ", n_lines, " lines")
  }

  # get line numbers
  line_nums = 1:n_lines
  max_digits = max(nchar(line_nums))
  line_nums = stringr::str_pad(line_nums, width = max_digits, pad = "0")
  line_nums = paste0("L", line_nums, "| ")

  # if lines == NULL, print all lines
  if (is.null(lines)) {lines = 1:n_lines}

  # drop if not keeping the first and last lines
  if (!keep_first_last) {
    drop = c(1, n_lines)
    lines = lines[-drop] - 1
    line_nums = line_nums[-drop]
    model_code = model_code[-drop]
  }

  # extract the lines to print
  show_code = model_code[lines]

  # find the amount of whitespace to trim off the front of all lines
  n_lws = show_code |>
    stringr::str_extract("^\\s+") |>
    stringr::str_count("\\s")
  n_lws[is.na(n_lws)] = 0

  # trim this whitespace off
  show_code = drop_lws(show_code, n = min(n_lws))

  # if labeling lines, paste on line labels
  if (show_labels) show_code = paste0(line_nums[lines], show_code)

  # print the output to the console
  cat(show_code, sep = "\n")
}

#' List all Variable Names in JAGS Model
#'
#' Returns the names of all variables defined
#' within the JAGS model code
#' (i.e., found on the left-hand side of `<-` or `~`).
#'
#' @inheritParams model_read
#' @return A list object with elements storing the types of variables
#'   created within the JAGS model:
#'   * `$logical` variables defined to the left of a `<-` operator.
#'   * `$stochastic`: variables defined to the left of a `~` operator.
#' @export

model_vars = function(model_file = jagsMB_opts("model_file")) {

  # check the model_file argument for validity
  check_model_file(model_file)

  # read in model code
  model_code = model_read(model_file, keep_lws = FALSE)

  # define patterns
  stoch_pattern = "\\s?~\\s?d[:alpha:]+\\(.+$"  # stochastic nodes match this
  index_pattern = "\\[.+\\]"                    # this matches the indices
  logic_pattern = "\\s?<-\\s?.+"                # this matches logical nodes
  linkf_pattern = "^[:alpha:]+\\("              # this matches link functions

  # find all stochastic nodes
  stoch_matches = model_code[stringr::str_detect(model_code, stoch_pattern)]
  stoch_nodes = stringr::str_remove(stoch_matches, stoch_pattern)
  stoch_nodes = stringr::str_remove(stoch_nodes, index_pattern)

  # all logical portions
  logic_matches = model_code[stringr::str_detect(model_code, logic_pattern)]
  logic_nodes = stringr::str_remove(logic_matches, logic_pattern)
  logic_nodes = stringr::str_remove(logic_nodes, linkf_pattern)
  logic_nodes = stringr::str_remove(logic_nodes, paste0(index_pattern, "\\)?"))

  # bundle the output into a list
  out = list(
    logical = unique(logic_nodes),
    stochastic = unique(stoch_nodes)
  )

  # return the output
  return(out)
}

