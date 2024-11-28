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


# Some of the packages have to be installed from source (at least on Linux, apologies if this slows you down slightly for no reason)

# Install goeveg if not already installed
if (!"goeveg" %in% installed.packages()[, "Package"]) {
  install.packages("https://cloud.r-project.org/src/contrib/goeveg_0.7.5.tar.gz", repos = NULL, type = "source")
}

# Install lsr if not already installed
if (!"lsr" %in% installed.packages()[, "Package"]) {
  install.packages("https://cloud.r-project.org/src/contrib/lsr_0.5.2.tar.gz", repos = NULL, type = "source")
}

message("All packages installed and loaded successfully.\n")
