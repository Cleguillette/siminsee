
usethis::use_package_doc()
# the ggh4x package from CRAN has a bug
# Error in `FUN()`:
# ! Don't know how to get height of <integer> object
# (see https://github.com/teunbrand/ggh4x/issues/151)
# need to use the dev version instead
usethis::use_dev_package("ggh4x")

# écriture d'un readme en rmd (idem vignette)
usethis::use_readme_rmd()

# initialisation des fichiers nécessaires pour publier sur github.com
usethis::use_pkgdown()

#pkgdown::build_site()
#usethis::use_pkgdown_github_pages()

usethis::use_build_ignore("devtools_history.R")
