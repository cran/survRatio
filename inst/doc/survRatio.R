## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- include=F---------------------------------------------------------------
library(knitr)
library(survival)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(survRatio)

## source functions
lung.time = lung$time
lung.cens = lung$status
gender = as.factor(lung$sex)
levels(gender) <- c("Male", "Female")

## -----------------------------------------------------------------------------
fit1 <- drsurv(time = lung.time, status = lung.cens, factor = gender)
ls(fit1)

## ---- eval=FALSE--------------------------------------------------------------
#  head(fit1$surv)

## ---- echo=FALSE, results='asis'----------------------------------------------
kable(head(signif(fit1$surv, 3)))

## ---- warning=FALSE-----------------------------------------------------------
ggsurv(fit1) +
  ylab("Estimated Survival Ratio") +
  xlab("Follow Up Time (days)")

## ---- warning=FALSE-----------------------------------------------------------
ggsurv(fit1) +
  ylab("Estimated Survival Ratio") +
  xlab("Follow Up Time (days)") +
  theme_bw()

## ---- warning=FALSE-----------------------------------------------------------
ggsurv(fit1, statistics = "diff") +
  ylab("Estimated Difference in Survival") +
  xlab("Follow Up Time (days)")

## ---- warning=FALSE-----------------------------------------------------------
ggsurv(fit1, statistics = "diff",
       palette = c("blue"),
       xlab = "Time (days)", ylab = "Estimated Difference in Survival", 
       theme = "gray",
       table = TRUE)

## ---- warning=FALSE-----------------------------------------------------------
ggsurv(fit1, statistics = "surv", p.value = TRUE) +
  ylab("Estimated Survival") +
  xlab("Follow Up Time (days)")

## ---- warning=FALSE, fig.height=10, fig.width=12------------------------------
ggsurv(fit1, statistics = "all",
       table = TRUE)

## ---- include=F---------------------------------------------------------------
ret.time = retinopathy$futime
ret.status = retinopathy$status
ret.treatment = as.factor(retinopathy$trt)
levels(ret.treatment) <- c("Control eye", "Treated eye")
ret.id = retinopathy$id

## ---- eval=FALSE--------------------------------------------------------------
#  fit2 <- drsurv(ret.time, ret.status, ret.treatment, paired = TRUE, id = ret.id)
#  head(fit2$surv)

## ---- echo=FALSE, results='asis'----------------------------------------------
fit2 <- drsurv(time = ret.time, status = ret.status, ret.treatment, paired = TRUE, id = ret.id)
kable(head(signif(fit2$surv, 3)))

## ---- warning=FALSE-----------------------------------------------------------
p1 <- ggsurv(fit2, statistics = "ratio")
p1 + labs(title = "Estimated Survival Ratio for Time to Loss of Vision after Laser Coagulation",
          subtitle = "Ratio = (Control / Treated)",
          y = "Estimated Survival Ratio", 
          x = "Follow Up (Months)")  +
      theme(legend.position="bottom",
           legend.background = element_rect(fill="lightblue", linetype="solid"))

## ---- warning=FALSE-----------------------------------------------------------
p1 <- ggsurv(fit2, statistics = "diff")
p1 + labs(title = "Estimated Survival Difference for Time to Loss of Vision after Laser Coagulation",
          subtitle = "Difference = (Control - Treated)",
          y = "Estimated Survival Difference", 
          x = "Follow Up (Months)")  +
      theme(legend.position="bottom",
           legend.background = element_rect(fill="white", linetype="solid"))

