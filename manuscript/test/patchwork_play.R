# patchwork play
# January 15, 2018
# Jeffrey A. Walker

# install.packages("devtools")
devtools::install_github("thomasp85/patchwork")
library(ggplot2)
library(patchwork)

# patchwork not installing with this error
# object ‘ggplot_add’ is not exported by 'namespace:ggplot2'

library(cowplot)

library(data.table)
fish <- fread('data/zebra_sprint.txt', stringsAsFactors = TRUE)
data <- copy(fish)

gg.bot <- ggplot(data=fish, aes(x=Treatment, y=Sprint, fill=Time)) +
  geom_boxplot() +
  coord_flip() +
  theme(plot.margin = margin(0, 2, 2, 2, "cm"))
gg.bot

gg.top <- ggplot(data=fish, aes(x=Treatment, y=Sprint, color=Time)) +
  geom_point() +
  scale_y_continuous(position = "right") +
  coord_flip() +
  theme(plot.margin = margin(2, 2, 0, 2, "cm"))
gg.top

plot_grid(gg.top, gg.bot, nrow=2)
gg <- plot_grid(gg.top, gg.bot, nrow=2)
saveRDS(gg, "output/plot.rds")

gg.test <- readRDS('output/plot.rds')
