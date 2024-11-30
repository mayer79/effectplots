#=============================================================================
# Put together the package
#=============================================================================

# WORKFLOW: UPDATE EXISTING PACKAGE
# 1) Modify package content and documentation.
# 2) Increase package number in "use_description" below.
# 3) Go through this script and carefully answer "no" if a "use_*" function
#    asks to overwrite the existing files. Don't skip that function call.
# devtools::load_all()

library(usethis)

# Sketch of description file
use_description(
  fields = list(
    Title = "Effect Plots",
    Version = "0.1.1",
    Description = "High-performance implementation of various effect plots
    useful for regression and probabilistic classification tasks.
    The package includes partial dependence plots
    (Friedman, 2021, <doi:10.1214/aos/1013203451>), accumulated local effect plots
    and M-plots (both from Apley and Zhu, 2016, <doi:10.1111/rssb.12377>),
    as well as plots that describe the statistical associations between model
    response and features.
    It supports visualizations with either 'ggplot2' or 'plotly',
    and is compatible with most models, including 'Tidymodels', models wrapped in
    'DALEX' explainers, or models with case weights.",
    `Authors@R` = "person('Michael', 'Mayer', email = 'mayermichael79@gmail.com', role = c('aut', 'cre'))",
    Depends = "R (>= 4.1.0)",
    LazyData = NULL
  ),
  roxygen = TRUE
)

use_package("collapse", "Imports")
use_package("ggplot2", "Imports")
use_package("grDevices", "Imports")
use_package("patchwork", "Imports")
use_package("plotly", "Imports")
use_package("stats", "Imports")
use_package("h2o", "Enhances")

use_gpl_license()

# Your files that do not belong to the package itself (others are added by "use_* function")
use_build_ignore(
  c("^packaging.R$", "[.]Rproj$", "^logo.png$", "^claims.parquet$"), escape = FALSE
)

# Add short docu in Markdown (without running R code)
use_readme_md()

# If you want to add unit tests
use_testthat()

# On top of NEWS.md, describe changes made to the package
use_news_md()

# Add logo
use_logo("logo.png")

# If package goes to CRAN: infos (check results etc.) for CRAN
use_cran_comments()

use_github_links() # use this if this project is on github

use_rcpp()

# Github actions
# use_github_action("check-standard")
# use_github_action("test-coverage")
# use_github_action("pkgdown")

#=============================================================================
# Finish package building (can use fresh session)
#=============================================================================

library(devtools)

document()
test()
check(manual = TRUE, cran = TRUE)
build()
devtools::install(upgrade = FALSE)

# Run only if package is public(!) and should go to CRAN
if (FALSE) {
  check_win_devel()
  check_rhub()

  # Wait until above checks are passed without relevant notes/warnings
  # then submit to CRAN
  release()
}
