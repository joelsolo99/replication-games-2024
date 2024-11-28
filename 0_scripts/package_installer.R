# These are all the packages used across all the analysis scripts, feel free to remove whichever ones you don't need. 
cran_packages <- c("tidyverse", "sjstats", "foreach", "doParallel", "rstan", "devtools")

# Install missing CRAN packages
for (pkg in cran_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Install eyetrackingR: not on CRAN so we need to install it from GitHub. 
if (!"eyetrackingR" %in% installed.packages()[, "Package"]) {
  devtools::install_github("jwdink/eyetrackingR")
}

message("All packages installed and loaded successfully.\n")
