```{r mossBar, echo=FALSE, out.width="100%", fig.cap="Bar plot of moss data. Error bars are 1 SEM. Letters indicate statistically significant tests of marginal means pooled over levels of other factor"}
path <- '../data/' # for knit
# path <- 'manuscript/data/' # for console
fn <- paste(path, 'moss2.txt', sep='') # for knit
moss <- fread(fn, stringsAsFactors = TRUE)
# quickie to check against supplement - I get different results (small)
# moss[, .(N=.N, SE=sd(moss_biomass_gain)/sqrt(.N)), by=.(MFW, Precipitation, N_addition)]

moss[, MFW:=factor(MFW, c('C', 'B', 'BS', 'BX'))]
moss[, Precipitation:=factor(Precipitation, c('Frequent', 'Moderate', 'Infrequent'))]

pd <- 0.9
gg_bar <- ggplot(data=moss, aes(x=MFW, y=moss_biomass_gain, fill=Precipitation, group=Precipitation)) +
  stat_summary(fun.data = 'mean_se', geom = c('errorbar'), width = 0.2, position = position_dodge(pd)) +
  stat_summary(fun.y=mean, geom="col", position = position_dodge(pd), color='darkGrey') +
  theme_minimal() +
  ylab('Moss biomass gain (g dry mass per microcosm)') +
  scale_fill_brewer(palette = 'Greys') +
  #coord_cartesian(ylim = 0.50) +
  annotate("text", x=1:4, y=c(.5, .5, .5, .5), label=c('B', 'A', 'AB', 'A')) +
  annotate("text", x=c(1-.25, 1, 1+.25), y=c(.275, .275, .275), label=c('a', 'a', 'b')) + 
  annotate("text", x=c(2-.25, 2, 2+.25), y=c(.4, .4, .4), label=c('a', 'a', 'b')) + 
  annotate("text", x=c(3-.25, 3, 3+.25), y=c(.35, .35, .35), label=c('a', 'a', 'b')) + 
  annotate("text", x=c(4-.25, 4, 4+.25), y=c(.475, .475, .475), label=c('a', 'a', 'b'))

gg_bar
```


```{r make_mossHarrell, echo=FALSE, results='hide'}
# average standard error of raw group values
se_sum <- moss[, .(N=.N, SE=sd(moss_biomass_gain)/sqrt(.N)), by=.(MFW, Precipitation)]
mean_se <- mean(se_sum[, SE])

res <- HarrellPlot(x='MFW', y='moss_biomass_gain', g='Precipitation', data=moss, add_interaction = FALSE, interaction.group = TRUE, contrasts.method = 'revpairwise', contrasts.scaling = 'percent', y_label = ('Moss biomass gain (g dry mass per microcosm)'))
gg_hplot <- res$gg

path <- '../output/' # for knit
#path <- 'manuscript/output/' # for console
image_path <- paste(path, 'moss_Hplot.pdf', sep='') # for knit
pwidth <- 8
pheight <- 6
pdf(image_path, width=pwidth, height=pheight, onefile = TRUE)
grid.arrange(gg_hplot, nrow=1, ncol=1) 
dev.off()

# average SE difference
mean_sed <- mean(res$tables$contrasts[, SE])
```

```{r mossHarrell, echo=FALSE, out.width="90%", fig.cap="Harrell plot of moss linear model results. Error bars are 95\\% confidence intervals"}
path <- '../output/' # for knit
#path <- 'manuscript/output/' # for console
image_path <- paste(path, 'moss_Hplot.pdf', sep='') # for knit
knitr::include_graphics(image_path)

```
