# Function to install (if necessary) and load an R package

install_and_load <- function(packages) {
  
  for(package_name in packages) {
    if (!require(package_name, character.only = TRUE)) {
      install.packages(package_name, dependencies = TRUE, type = "source")
    }
    library(package_name, character.only = TRUE)
  }
    
}
  
