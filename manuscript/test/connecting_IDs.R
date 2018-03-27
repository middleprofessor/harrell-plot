library(shiny)
library(gridExtra) # needed for download
library(ggplot2)
library(cowplot)
library(broom)
library(lme4)
library(lmerTest)
library(lsmeans)
library(car)
library(data.table)
library(Hmisc) # smean.cl.boot
library(MCMCpack) # bayes
library(coda) # bayes

fit.model='lm'
error='Normal'
add_interaction=TRUE
interaction.group = TRUE
interaction.treatment=TRUE
mean_intervals.method='lm'
conf.mean=0.95
contrasts.method='revpairwise'
contrasts.scaling='raw'
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

fish <- fread('data/zebra_sprint.txt', stringsAsFactors = TRUE)
data <- copy(fish)
x <- 'Treatment'
y <- 'Sprint'
g <- 'Time'
rintcols <- 'ID'
rslopecols <- 'Time'
covcols <- NULL
fit.model <- 'lmm'

if(g == 'None'){
  xcols <- x
  grouping <- FALSE
  add_interaction <- FALSE
  interaction.group <- FALSE
}else{
  xcols <- c(x,g)
  grouping <- TRUE
}
data <- data.table(data)
dt <- data[, .SD, .SDcols=unique(c(xcols, y, rintcols, rslopecols, covcols))]
dt <- na.omit(dt) # has to be after groups read in

# add empty grouping variable column if grouping == FALSE to make subsequent code easier
if(grouping == FALSE){
  g <- 'dummy_g'
  dt[, (g):='dummy']
}

# abbreviate levels if TRUE
if(short==TRUE){
  dt[, (x):=abbreviate(get(x))]
  dt[, (g):=abbreviate(get(g))]
}
x_order <- dt[,.(i=min(.I)),by=get(x)][, get]
dt[, (x):=factor(get(x), x_order)]
g_order <- dt[,.(i=min(.I)),by=get(g)][, get]
dt[, (g):=factor(get(g), g_order)]

res <- fit_model(x, y, g, covcols, rintcols, rslopecols, dt, fit.model, error, add_interaction, interaction.group, interaction.treatment, mean_intervals.method, conf.mean, contrasts.method, contrasts.scaling, conf.contrast, adjust)

#res <- fit_model(x, y, g, covcols, rintcols, rslopecols=NULL, dt, fit.model, error, add_interaction, interaction.group, interaction.treatment, mean_intervals.method, conf.mean, contrasts.method, contrasts.scaling, conf.contrast, adjust)


ci_means <- res$ci_means
ci_diffs <- res$ci_diffs
tables <- res$tables
fit <- res$fit

current_gg <- function(){
  dt[, yhat:=predict(fit)]
  dtbar <- dt[, .(Sprint=mean(Sprint)), by=.(Treatment, Time, ID)]
  dtbar_wide <- dcast(dtbar, Treatment+ID~Time, value.var = 'Sprint')
  dtbar_wide[, x1:=as.integer(Treatment)-0.25]
  dtbar_wide[, x2:=as.integer(Treatment)+0.25]
  
  gg_treatments <- ggplot(data=dt, aes(x=Treatment, y=Sprint))
  gg_treatments <- gg_treatments + geom_point(aes(color=Time, group=Time:ID), position=position_dodge(width=0.5))
  gg_treatments
  # gg_treatments <- gg_treatments + geom_line(data=dtbar, aes(x=Treatment, y=Sprint, group=ID), position=position_dodge(width=0.5))
  # gg_treatments
  
  gg_treatments <- gg_treatments + geom_segment(data=dtbar_wide, aes(x=x1, y=pre, xend=x2, yend=post))
  gg_treatments
}

gg_treatments <- ggplot(data=dt, aes(x=Treatment, y=Sprint, color=Time, group=Time:ID))
gg_treatments <- gg_treatments + geom_point(position=position_dodge(width=0.5))
gg_treatments

gg_treatments <- gg_treatments + geom_line(aes(group=Time:ID), position=position_dodge(width=0.5))
gg_treatments


dt[, yhat:=predict(fit)]
lsm <- lsmeans(fit, specs=c('Treatment', 'Time'))

dtbar <- dt[, .(Sprint=mean(Sprint)), by=.(Treatment, Time, ID)]
gg_treatments <- ggplot(data=dt, aes(x=Treatment, y=Sprint, color=Time, group=Time:ID))
gg_treatments <- gg_treatments + geom_point(position=position_dodge(width=0.5))
gg_treatments

gg_treatments <- gg_treatments + geom_line(data=dtbar, aes(x=Treatment, y=Sprint, group=ID), position=position_dodge(width=0.5))
gg_treatments

gg_treatments <- ggplot(data=dt, aes(x=Time, y=Sprint)) +
  geom_boxplot(aes(fill=Time))
gg_treatments

gg_treatments <- gg_treatments +
  geom_line(data=dtbar, aes(x=Time, y=Sprint, group=ID), linetype='dashed')
gg_treatments

gg_treatments <- gg_treatments +
  facet_grid(Treatment~.) +
  coord_flip()
gg_treatments


