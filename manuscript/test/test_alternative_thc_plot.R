# use ggplot facets for THC plot
pd <- position_dodge(0.5)
gg <- ggplot(data=ci, aes(x=dev.temp, y=ctmin, color=dev.treat))
gg <- gg + geom_point(position=pd)
#gg <- gg + facet_grid(estimate~., scales='free')
gg <- gg + facet_wrap(~estimate, scales='free', nrow=2, strip.position='right')
gg <- gg + coord_flip()
gg

# different plots per panel
pd <- position_dodge(1)
gg <- ggplot(data=dt, aes(x=dev.temp, y=ctmin))
gg <- gg + geom_point(data=dt[estimate=='contrasts',])
gg <- gg + geom_point(data=dt[estimate=='means',], aes(color=dev.treat), position=position_jitterdodge())
gg <- gg + geom_boxplot(data=dt[estimate=='means',], aes(color=dev.treat), position=pd)
gg <- gg + geom_point(data=dt[estimate=='means',], aes(color=dev.treat), position=position_jitterdodge())
gg <- gg + facet_wrap(~estimate, scales='free', nrow=2, strip.position='right')
gg <- gg + coord_flip()
gg <- gg + theme_bw()
gg <- gg + theme(axis.text.x = element_blank())
gg

