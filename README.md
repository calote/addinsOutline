
# addinsOutline

<!-- badges: start -->
<!-- badges: end -->

The goal of "addinsOutline" is to facilitate navigation through the files that constitute a R Markdown, LaTeX or other project.

This RStudio addins will show a list of the different sections established in the project and clicking on any element shown in the list will cause RStudio to show that section by opening the file that contains it if it was not already open previously.

In addition, by modifying the configuration file "addinsOutline_ini.txt" that can be copied to the project folder, not only the sections could be searched, but it could also be located for example: tables created with `knitr::kable()`, graphics inserted with `knitr::insert_graphics()`, labels created with `\label` in documents LaTeX, etc.

There are different functions to activate the addins corresponding to the type of project:

- `run_addinsOutline_Rmd()`: R Markdown projects
- `run_addinsOutline_Rmd_bookdown()`: Bookdown projects
- `run_addinsOutline_tex()`: LaTeX projects


## Installation

You can install the released version of addinsOutline from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("addinsOutline")
```

## Example

This is a basic example which shows you how activate the Addins for R Markdown projects:

``` r
library(addinsOutline)
run_addinsOutline_Rmd()
```

The next code will copy the configuration file "addinsOutline_ini.txt" to the current working directory

``` r
copy_file_config_ini_new()
```