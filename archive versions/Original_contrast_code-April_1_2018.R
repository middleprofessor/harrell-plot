if(grouping==FALSE | add_interaction==TRUE){
  ci_diffs <- summary(contrast(lsm, method=contrasts.method), adjust=ci.adjust, level=conf.contrast, infer=c(TRUE,TRUE))
  tables$contrasts.raw <- ci_diffs
  if(grouping==TRUE & contrasts.method=='revpairwise'){ # subset into pairwise within each group
    # another method
    # fread(paste(as.character(ci_diffs$contras), collapse='\n'), sep='-')
    inc <- NULL
    split1 <- data.frame(t(do.call("cbind", strsplit(as.character(ci_diffs$contrast)," - "))))
    split2a <- data.frame(t(do.call("cbind", strsplit(as.character(split1$X1),","))))
    colnames(split2a) <- c('x1','g1')
    split2b <- data.frame(t(do.call("cbind", strsplit(as.character(split1$X2),","))))
    colnames(split2b) <- c('x2','g2')
    splits <- data.table(split2a, split2b)
    # splits[, x1:=as.character(x1)]
    # splits[, x2:=as.character(x2)]
    # splits[, g1:=as.character(g1)]
    # splits[, g2:=as.character(g2)]
    if(interaction.group==TRUE){
      inc <- c(inc, which(splits[,g1]==splits[,g2]))
    }
    if(interaction.treatment==TRUE){
      inc.x <- which(splits[,x1]==splits[,x2])
      t.x <- factor(splits[inc.x, x1], levels(dt[, get(x)]))
      inc <- c(inc, inc.x[order(t.x)])
    }
    ci_diffs <- ci_diffs[inc,]
  }
  tables$contrasts <- ci_diffs
  ci_diffs <- data.table(ci_diffs, g='dummy')
}
if(grouping==TRUE & add_interaction==FALSE){
  if(contrasts.method=='revpairwise'){
    p_levels <- n_levels*(n_levels-1)/2
    p_groups <- n_groups*(n_groups-1)/2
  }else{
    p_levels <- n_levels-1
    p_groups <- n_groups-1
  }
  diffs.x <- summary(contrast(lsm, method=contrasts.method, by=g), adjust=ci.adjust, level=conf.contrast, infer=c(TRUE,TRUE))
  ci_diffs.x <- data.table(diffs.x)[1:p_levels]
  setnames(ci_diffs.x, old=c(g), new='by')
  diffs.g <- summary(contrast(lsm, method=contrasts.method, by=x), adjust=ci.adjust, level=conf.contrast, infer=c(TRUE,TRUE))
  ci_diffs.g <- data.table(diffs.g)[1:p_groups]
  setnames(ci_diffs.g, old=c(x), new='by')
  # save to tables
  tables$contrasts.raw <- list(by_treatment=diffs.x, by_grouping=diffs.g)
  # combine
  ci_diffs <- data.table(NULL)
  if(interaction.treatment==TRUE){ci_diffs <- rbind(ci_diffs, ci_diffs.x)}
  if(interaction.group==TRUE){ci_diffs <- rbind(ci_diffs, ci_diffs.g)}
  tables$contrasts <- copy(ci_diffs)
  setnames(ci_diffs, old=c('by'), new='g')
  # ci_diffs.x[, g:='x']
  # ci_diffs.g[, g:='g']
}
