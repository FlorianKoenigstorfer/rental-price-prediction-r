# install_packages.R
# Run this script once to install all required R packages.

required_packages <- c(
  "tidyverse",
  "caTools",
  "caret",
  "stargazer",
  "car"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

invisible(lapply(required_packages, install_if_missing))

cat("All packages installed successfully.\n")
