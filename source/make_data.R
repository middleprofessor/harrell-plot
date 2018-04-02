# make data for HarrellPlot package

library(data.table)
root_dir <- rprojroot::find_root("HarrellPlot.Rproj")
setwd(root_dir)
file_path <- "data/fly_burst.txt"
fly <- fread(file_path)

out_path <- "package/HarrellPlot/data/fly.RData"
save(fly, file=out_path)
