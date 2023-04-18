# *NEWS*

# jagsMB 0.1.0 (2023-04-18)

First stable version released; contains a formalized version of ideas the developer has implemented in less transparent ways over several modeling projects.

`R CMD check` completed with 0 errors, warnings, or notes. This version is theoretically ready for CRAN submission, but I plan to leave it on GitHub and see if there are any urgent improvements needed first.

### Summary of Exported Functions

| Function              | Description                                              |
|----------------------|--------------------------------------------------|
| `model_header()`      | Inserts a section header into model code                 |
| `model_build()`       | Combines model components into the complete model        |
| `model_write()`       | Exports the complete model definition to a file          |
| `model_read()`        | Reads the complete model definition to workspace         |
| `model_lines()`       | Prints desired lines of model definition to console      |
| `model_vars()`        | Summarize the names of all variables on LHS in the model |
| `model_search()`      | Find line numbers that match a regular expression        |
| `model_replace()`     | Replace a line of code that matches a regular expression |
| `jagsMB_opts()`       | Set the global default value of `model_file` argument    |
| `jagsMB_opts_reset()` | Reset package option settings                            |

### Summary of Available Documentation

| Item                                                                 | Progress                                                                                                                                                              |
|------------------------|------------------------------------------------|
| R Help Files                                                         | Written for all exported and non-exported functions                                                                                                                   |
| DESCRIPTION File                                                     | Populated with information about package, dependencies declared                                                                                                       |
| Vignette                                                             | Has sections for motivation, definition workflow, inspecting output, and replacing lines complete with description and reproducible examples.                         |
| Issue [#1](https://github.com/bstaton1/jagsMB/issues/1)              | Contains a laundry list of ideas for improvements, currently separated into (a) user-friendliness, (b) user-facing new features, and (c) under-the-hood new features. |
| GitHub Repo [README](https://github.com/bstaton1/jagsMB#readme) file | Contains a basic (collapsible) pseudo-code example and basic instructions for installation and accessing the vignette                                                 |
