#' @import stats
#' @import survival
#' @export

drsurv <- function(time, status, factor, ref=NULL, tgrid=NULL, paired=FALSE,id=NULL, clevel=0.95, nboot=1000) {
  if(paired & is.null(id)) stop("id is a required argument for paired data")
  if(length(time) != length(status) | length(time)!=length(factor)){
    stop("The objects given for time, status and factor are required to have the same length")
  }
  if(!is.null(ref))
    if(!ref %in% levels(as.factor(factor))) stop("the reference level is not recognised")
  y <- data.frame(t=time, cens=status, f=factor)
  if(is.null(ref)){
    y[,3] <- as.factor(y[,3])
  } else{
    y[,3] <- relevel(as.factor(y[,3]), ref)
  }
  stest <- survival::survdiff(Surv(time, cens) ~ f, data=y)

  if(paired){
    Sfit <- survival::survfit(Surv(t, cens) ~ f + survival::cluster(id), data=y)
  } else{
    Sfit <- survival::survfit(Surv(t, cens) ~ f, data=y)
  }

  flevels <- levels(y[,3])
  maxt <- numeric(2)
  sfun1 <- list()
  sfun2 <- list()
  if (is.null(tgrid)){
    tgrid <- seq(min(Sfit$time), max(Sfit$time), length.out = 100)
  }
  boot.diff <- data.frame(time=tgrid)
  boot.ratio <- data.frame(time=tgrid)
  gsize <- length(tgrid)

  ##### bootstrap CI ratio and diff
  for (j in 1:nboot){
    if (paired){
      samp <- sample(unique(id), replace=T, length(unique(id)))
      y.rnd <- y[unlist(lapply(samp, function(x) which(x==id))),]
    } else{
      samp <- sample(1:dim(y)[1], replace=T)
      y.rnd <- y[samp,]
    }
    for (i in 1:2){
      y2 <- y.rnd[y.rnd$f==flevels[i],]
      fit1 <- survival::survfit(survival::Surv(t, cens)~1, data=y2)
      maxt[i] <- max(fit1$time[fit1$n.event>0])
      sss <- as.data.frame(summary(fit1)[c("time","n.risk","n.event","n.censor","surv","lower","upper")])
      if (sss$time[1] != 0){
        sff2 <- sss[1, ]
        sff2$time <- 0
        sff2$n.risk <- fit1$n
        sff2$surv <- 1
        s.f1 <- rbind(sff2, sss)
      } else {
        s.f1 <- cbind(sss, Factor=flevels()[i])
      }
      sfun1[[i]] <- stepfun(s.f1$time[-1], s.f1$surv)
      sfun2[[i]] <- stepfun(s.f1$time[-1], s.f1$n.risk)
    }

    boot.diff$nrisk <- sfun2[[1]](tgrid) + sfun2[[2]](tgrid)
    boot.ratio$nrisk <- sfun2[[1]](tgrid) + sfun2[[2]](tgrid)
    boot.diff$d <- sfun1[[1]](tgrid) - sfun1[[2]](tgrid)
    names(boot.diff)[j+2] <- paste("d",j, sep="")

    boot.ratio$r <- sfun1[[1]](tgrid) / sfun1[[2]](tgrid)
    boot.ratio$r[boot.ratio$r==Inf] <- NA
    names(boot.ratio)[j+2] <- paste("r",j, sep="")
  }

  ##### estimate ratio and diff
  sfun3 <- list()
  sfun4 <- list()
  for (i in 1:2){
    y2 <- y[y$f==flevels[i],]
    fit1 <- survival::survfit(survival::Surv(t, cens)~1, data=y2)
    maxt[i] <- max(fit1$time[fit1$n.event>0])
    sss <- as.data.frame(summary(fit1)[c("time","n.risk","n.event","n.censor","surv","lower","upper")])
    if (sss$time[1] != 0){
      sff2 <- sss[1, ]
      sff2$time <- 0
      sff2$n.risk <- fit1$n
      sff2$surv <- 1
      s.f1 <- rbind(sff2, sss)
    } else {
      s.f1 <- cbind(sss, Factor=flevels()[i])
    }
    sfun1[[i]] <- stepfun(s.f1$time[-1], s.f1$surv)
    sfun2[[i]] <- stepfun(s.f1$time[-1], s.f1$n.risk)
    sfun3[[i]] <- stepfun(s.f1$time[-1], s.f1$lower)
    sfun4[[i]] <- stepfun(s.f1$time[-1], s.f1$upper)
  }

  dat.surv <- data.frame(time=tgrid, nrisk1=sfun2[[1]](tgrid), nrisk2=sfun2[[2]](tgrid))
  dat.surv$surv1 <- sfun1[[1]](tgrid)
  dat.surv$l.surv1 <- sfun3[[1]](tgrid)
  dat.surv$u.surv1 <- sfun4[[1]](tgrid)
  dat.surv$surv2 <- sfun1[[2]](tgrid)
  dat.surv$l.surv2 <- sfun3[[2]](tgrid)
  dat.surv$u.surv2 <- sfun4[[2]](tgrid)
  dat.surv$diff <- dat.surv$surv1 - dat.surv$surv2
  dat.surv$lower.diff <- apply(boot.diff[,-c(1:2)], 1, function(x) quantile(x, probs=c((1-clevel)/2,1-(1-clevel)/2)))[1,]
  dat.surv$upper.diff <- apply(boot.diff[,-c(1:2)], 1, function(x) quantile(x, probs=c((1-clevel)/2,1-(1-clevel)/2)))[2,]
  dat.surv$ratio <- dat.surv$surv1 / dat.surv$surv2
  dat.surv$ratio[dat.surv$ratio==Inf] <- NA
  dat.surv$lower.ratio <- apply(boot.ratio[,-c(1:2)], 1, function(x) quantile(x, probs=c((1-clevel)/2,1-(1-clevel)/2), na.rm=T))[1,]
  dat.surv$upper.ratio <- apply(boot.ratio[,-c(1:2)], 1, function(x) quantile(x, probs=c((1-clevel)/2,1-(1-clevel)/2), na.rm=T))[2,]

  if(paired){
    return(list(surv=dat.surv, levels=flevels, paired=paired, clevel=clevel, nboot=nboot))
  } else{
    pval <- signif(pchisq(stest$chisq, length(stest$n)-1, lower.tail = FALSE), 3)
    return(list(surv=dat.surv, levels=flevels, paired=paired, clevel=clevel, nboot=nboot, pvalue=pval))
  }
}
