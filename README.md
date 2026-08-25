# NPACK
This contains NPACK(NHANES Practical Analysis Creator Kit). NPACK allows users to design and run NHANES studies with 9 different analysis types without requiring any programming experience.


#HOW TO SETUP

First install R.

Windows: https://cran.r-project.org/bin/windows/base/

Mac: https://cran.r-project.org/bin/macosx/

Ubuntu: https://cran.r-project.org/bin/linux/ubuntu/fullREADME.html

Then install RStudio

All OS: https://docs.posit.co/ide/user/#rstudio-ide-oss-downloads

In RStudio run this command line:

install.packages(c("shiny", "shinyjs", "here", "jsonlite", "nhanesA", 
                   "nhanesdata", "dplyr", "survey", "ggplot2", "gWQS", 
                   "qgcomp", "bkmr", "bkmrhat", "future", "future.apply"))

This installs the required packages for NPACK. It may take up to 10-15 minutes. 
If prompted to restart RStudio, click yes. 

Then navigate to open a project in the top ribbon and select nhanes-gui.Rproj 

Once you are in the nhanes-gui.Rproj, run this line to open NPACK

shiny::runApp("app.R")
