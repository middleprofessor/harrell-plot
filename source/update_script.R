library(rprojroot)
library("devtools")
library(roxygen2)

root_dir <- rprojroot::find_root("HarrellPlot.Rproj")
setwd(paste(root_dir,"/package", sep=''))

# from project directory
# document() # what does this do?

# install - current directory needs to be directory containing the "HarrellPlot" directory with all the bits
install("HarrellPlot")

