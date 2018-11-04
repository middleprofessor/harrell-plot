# test data

library(ggplot2)
library(cowplot)
library(broom)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(data.table)
library(Hmisc) # smean.cl.boot
library(MCMCpack) # bayes
library(coda) # bayes

fit.model='lm'
error='Normal'
add_interaction=FALSE
interaction.group = TRUE
interaction.treatment=TRUE
mean_intervals.method='lm'
conf.mean=0.95
contrasts.method='revpairwise'
contrasts.scaling='percent'
conf.contrast=0.95
adjust=FALSE
show.contrasts=TRUE
show.treatments=TRUE
display.treatment='box'
short=FALSE
show.mean=TRUE
show.dots=TRUE
horizontal=TRUE
color_palette='Greys'
jtheme='minimal'

frb <- fread('data/FRB5.txt', stringsAsFactors = TRUE)
data <- copy(frb)[depth==5]
x <- 'treatment'
y <- 'FRB'
g <- 'season'
rintcols <- c('plot', 'month')
rslopecols <- NULL
covcols <- NULL
add_interaction <- TRUE
REML <- TRUE
fit.model <- 'lmm'


moss <- fread('data/moss2.txt', stringsAsFactors = TRUE)
data <- copy(moss)
x <- 'MFW'
y <- 'moss_biomass_gain'
g <- 'Precipitation'
rintcols <- NULL
rslopecols <- NULL
covcols <- NULL
fit.model <- 'lm'

fly <- fread('data/CTmin_2d_8_19,23.csv', stringsAsFactors = TRUE)
fly[, dev.temp:=factor(dev.temp)]
data <- copy(fly)
x <- 'dev.temp'
y <- 'ctmin'
g <- 'dev.treat'
rintcols <- NULL
rslopecols <- NULL
covcols <- NULL
fit.model <- 'lm'

fish <- fread('data/zebra_sprint.txt', stringsAsFactors = TRUE)
data <- copy(fish)
x <- 'Treatment'
y <- 'Sprint'
g <- 'Time'
rintcols <- 'ID'
rslopecols <- 'Time'
covcols <- NULL
fit.model <- 'lmm'

fly <- fread('data/fly_burst.txt', stringsAsFactors = TRUE)
data <- copy(fly)
x <- 'Treatment'
y <- 'Vburst'
g <- 'Sex'
g <- 'None'
rintcols <- NULL
rslopecols <- NULL
covcols <- NULL
fit.model <- 'lm'


res <- Hdotplot(x, y, g, data=fish)

gg1 <- readRDS('data/contrasts.RDS')
gg2 <- readRDS('data/treatments.RDS')
plot_grid(gg1, gg2, nrow=2, align = "v")
