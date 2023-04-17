
jagsMB_OPTIONS = settings::options_manager(model_file = NULL, model = NULL)

#' Set or Get jagsMB Options
#'
#' @param ... Option names to retrieve options values or `key = value` pairs
#'   to set options. The only value currently accepted is `model_file`,
#'   which serves as the default argument value for [read_model()],
#'   [model_lines()], [model_vars()], and [model_search()].
#' @export

jagsMB_opts = function(...) {

  # protect against the use of reserved words in options package
  settings::stop_if_reserved(...)

  # call jagsMB options function
  jagsMB_OPTIONS(...)

}

#' Reset Global Options for jagsMB
#'
#' @export

jagsMB_opts_reset = function() {
  settings::reset(jagsMB_OPTIONS)
}
