
## 'jagsMB': JAGS Model Builder

> R package providing tools for building JAGS model files up from smaller, swappable components.

### Purpose

'jagsMB' facilitates reproducibly running multiple JAGS models without the need to keep track of multiple JAGS model definition files.
The trick is to define multiple versions of the model components (e.g., prior, likelihood, derived quantities), some of which may apply to all models while some may be specific to some models.
These components are then swapped in according to the context, and the resulting complete JAGS model definition file is written.

The approach is intended to reduce repetition, code-editing errors, and overall effort needed to manage JAGS model definitions.

### Basic Demo

A "pseudo-code" demo is provided below.

<details>
  <summary>Click to View</summary>
  
```R
library(jagsMB)

## ------------ ##
## DEFINE PARTS ##
## ------------ ##

## define the components that 
## make-up different models

priors_1 = function() {
  # define type 1 priors here
  # ...
}

priors_2 = function() {
  # define type 2 priors here
  # ...
}

Lhood_1 = function() {
  # define type 1 likelihood here
  # ...
}

Lhood_2 = function() {
  # define type 2 likelihood here
  # ...
}

## ------------ ##
## DEFINE RULES ##
## ------------ ##

## define rules for which 
## components to use and when

selector = function(prior_type, Lhood_type) {
  list(
    model_header("priors"),
    switch(prior_type, priors_1, priors_2),
    model_header("likelihood"),
    switch(Lhood_type, Lhood_1, Lhood_2)
  )
}

## ------------ ##
## EXPORT MODEL ##
## ------------ ##

## combine components following rules
## and export to a text file

model_file = selector(prior_type = 1, Lhood_type = 2) |>
  model_build() |>
  model_write()

# view model code
model_lines(model_file)
```

Gives (line numbers printed only, not found in `model_file` contents):

```
L01| model {
L02|
L03|   ### --- PRIORS --- ###
L04|
L05|   # define type 1 priors here
L06|   # ...
L07|
L08|   ### --- LIKELIHOOD --- ###
L09|
L10|   # define type 2 likelihood here
L11|   # ...
L12|
L13| }  # END OF MODEL
```

In this example, 'jagsMB' allows easily and reproducibly fitting four models (two priors by two likelihood combinations).
Without 'jagsMB', this would typically need to be done by having four unique model definition statements, each saved under a different name.

</details>

### Installation

'jagsMB' is currently available only from GitHub:

```R
remotes::install_github("bstaton1/jagsMB", build_vignettes = TRUE)
```

### Usage

Please view the vignette for a detailed overview:

```R
vignette("jagsMB-overview", package = "jagsMB")
```

The [NEWS](https://github.com/bstaton1/jagsMB/blob/main/NEWS.md) file contains recent updates in package development.
