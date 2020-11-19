#' @import stats
#' @import ggplot2
#' @importFrom gridExtra arrangeGrob
#' @importFrom methods hasArg
#' @importFrom ggpubr as_ggplot ggarrange
#' @export

ggsurv <- function(fit, statistics = c("surv", "diff", "ratio", "all"),
                   palette = NULL,
                   xlab = NULL, ylab = NULL,
                   theme = NULL,
                   table = FALSE,
                   p.value = FALSE,
                   flag = NULL,
                   flag.col = NULL,
                   break.times.by = NULL) {

  diffout <- diffyout <- l.surv1 <- l.surv2 <- lower.diff <- lower.ratio <- n.risk <- ratio <- NULL
  ratioout <- ratioyout <- surv1 <- surv2 <- u.surv1 <- u.surv2 <- upper.diff <- upper.ratio <- NULL

  #### risk table function:
  risk.table <- function(surv.dat,
                         timeby = NULL,
                         ystratalabs = NULL,
                         col = NULL){

    ### setting time breaks
    if (is.null(timeby)){
      maxTime <- max(surv.dat$time)/10
      timeby <- ifelse(maxTime < 100, 10,
                       ifelse(maxTime < 1000, 100,
                              ifelse(maxTime < 10000, 1000,floor(max(maxTime)/10)*10)))

    }


    times <- seq (0, max (surv.dat$time), by = timeby)
    m <- max (nchar (ystratalabs))

    ### setting the ystratalabs
    if(isFALSE(methods::hasArg(ystratalabs))|
       length(ystratalabs)!=2){
      stop("The ystratalabs should consist of two labels indicating the labels for factor levels.")
    }

    ### repeat color for ratio and difference table!
    if(isFALSE(methods::hasArg(col))){
      col = "black"
    }

    if(length(col) == 1) col = rep(col,2)

    ## create risk table data

    ## this line should be fixed based on n.risk for each strata
    n.risk1 <- sapply(1:length(times), function(x) surv.dat$nrisk1[min(which(surv.dat$time >= times[x]))])
    n.risk2 <- sapply(1:length(times), function(x) surv.dat$nrisk2[min(which(surv.dat$time >= times[x]))])

    risk.data <- data.frame(strata = rep(ystratalabs,each=length(times)),
                            time = rep(times,2),
                            n.risk = c(n.risk1,n.risk2))

    risk.data$strata <- factor(risk.data$strata, levels = rev(ystratalabs))

    ### plot the data table:
    data.table <- ggplot2::ggplot(risk.data,
                                  aes (x = time,
                                       y = strata,
                                       label = format(n.risk, nsmall = 0), color = strata)) +
      ggplot2::geom_text (size = 4) +
      ggplot2::theme_bw () +
      ggplot2::scale_y_discrete(breaks = as.character (levels(risk.data$strata)),
                                labels = as.character (levels(risk.data$strata))) +
      # scale_y_discrete (#format1ter = abbreviate,
      # breaks = 1:3,
      # labels = ystratalabs) +
      ggplot2::scale_x_continuous ("Numbers at risk",
                                   limits = c (0, max (surv.dat$time))) +
      ggplot2::theme (text = element_text(size=10, face = "bold"),
                      axis.title.x = element_text (size = 10, vjust = 1),
                      panel.grid.major = element_blank (),
                      panel.grid.minor = element_blank (),
                      panel.border = element_blank (),
                      axis.text.x = element_blank (),
                      axis.ticks = element_blank (),
                      axis.text.y = element_text (face = "bold",
                                                  hjust = 1))

    data.table <- data.table +
      ggplot2::theme(legend.position = "none") +
      ggplot2::xlab (NULL) +
      ggplot2::ylab (NULL)

    data.table <- data.table +
      ggplot2::scale_colour_manual(values=rev(col)) +
      ggplot2::theme (plot.margin = unit (c (-1.5, 1, 0.1, ifelse (m < 10, 2.5, 3.5) - 0.38 * m), "lines"),
                      axis.text.y = element_text(color = rev(col)))

    return(data.table)
  }

  #### End of risk table function:

  ### choosing the statistics
  if(isFALSE(methods::hasArg(statistics))){
    statistics <- "ratio"
  } else{
    statistics <- match.arg(statistics)
  }


  if(statistics == "all"){
    ## sts indicates which statistics should be plotted!
    sts = c("surv", "diff", "ratio")
  } else{
    sts = statistics
  }

  ### flag on and off:
  if(is.null(flag)){
    if(statistics != "surv"){
      flag = TRUE
    } else{
      flag = FALSE
    }
  }


  ### calling the data:
  surv.dat <- fit$surv

  if(isTRUE(flag)){

    if(statistics == "surv"){
      warning("The flag will not be displayed when statistics is 'surv'.")
    }

    surv.dat$diffout <- ifelse(0 > surv.dat$upper.diff |  0 < surv.dat$lower.diff, "S","NS")
    surv.dat$diffyout <- round(min(surv.dat$diff,surv.dat$lower.diff) - (0.2*sd(c(surv.dat$diff,surv.dat$lower.diff))),2)

    surv.dat$ratioout <- ifelse(1 > surv.dat$upper.ratio |  1 < surv.dat$lower.ratio, "S","NS")
    surv.dat$ratioyout <- round(min(surv.dat$ratio,surv.dat$lower.ratio) - (0.2*sd(c(surv.dat$ratio,surv.dat$lower.ratio))),2)

    if(is.null(flag.col)){
      flag.col <- c("green","red")
    } else{
      if(length(flag.col) != 2){
        stop("The flag.col should consist of two colors. The first color indicate no difference between the two factor levels while the second color indicate the difference. For example flag.col <- c('green','red').")
      } else{
        flag.col <- flag.col
      }
    }

  }


  ### choosing the palette:
  if(is.null(palette)){
    if(statistics == "surv") palette = c("red", "blue", NA, NA)
    if(statistics == "diff") palette = c(NA, NA, "black",NA)
    if(statistics == "ratio") palette = c(NA, NA, NA, "black")
    if(statistics == "all") palette = c("red", "blue", "black", "black")
  } else{
    if(statistics == "all"){
      if(length(palette) != 4){
        stop("The palette should consist of four colors when statistics is 'all'. The first two colors corresond to the levels of the factor variable. The third color indicates the color for difference plot and the fourth color indicates the color for ratio plot. For example palette = c('red', 'blue', 'black', 'black').")
      } else{
        palette <- palette
      }
    }

    if(statistics == "surv"){
      if(length(palette) != 2){
        stop("The palette should consist of two colors when statistics is 'surv'. The two colors corresond to the levels of the factor variable. For example palette = c('red', 'blue').")
      } else{
        palette <- c(palette, NA, NA)
      }
    }

    if(statistics == "diff"){
      if(length(palette) != 1){
        stop("The palette should consist of one color when statistics is 'diff'. For example palette = 'black'")
      } else{
        palette <- c(NA, NA, palette, NA)
      }
    }

    if(statistics == "ratio"){
      if(length(palette) != 1){
        stop("The palette should consist of one color when statistics is 'ratio'. For example palette = 'black'")
      } else{
        palette <- c(NA, NA, NA, palette)
      }
    }
  }

  ### setting up break.times.by
  if (is.null(break.times.by)){
    maxTime <- max(surv.dat$time)/10
    break.times.by <- ifelse(maxTime < 100, 10,
                             ifelse(maxTime < 1000, 100,
                                    ifelse(maxTime < 10000, 1000,floor(max(maxTime)/10)*10)))

  }

  ### setting up the theme:
  if (is.null(theme)) theme <- "minimal"
  if (theme == 'bw') ggplot2::theme_set(theme_bw())
  if (theme == 'classic') ggplot2::theme_set(theme_classic())
  if (theme == 'dark') ggplot2::theme_set(theme_dark())
  if (theme == 'gray') ggplot2::theme_set(theme_gray())
  if (theme == 'light') ggplot2::theme_set(theme_light())
  if (theme == 'linedraw') ggplot2::theme_set(theme_linedraw())
  if (theme == 'minimal') ggplot2::theme_set(theme_minimal())


  ### setting up the labels
  if(is.null(xlab))  xlab = "Follow up times"


  if(is.null(ylab)){
    ylab = c("Estimated Survival Probability", "Estimated Survival Difference", "Estimated Survival Ratio")
  } else{
    if(statistics == "all"){
      if(length(ylab) != 3){
        stop("The ylab should consist of three labels when statistics is 'all'. The order should be survival ylab, difference ylab, ratio ylab. For example ylab = c('Estimated Survival Probability', 'Estimated Survival Difference', 'Estimated Survival Ratio').")
      } else{
        ylab <- ylab
      }
    } else{
      if(length(ylab) != 1){
        stop("The ylab should consist of only one label if the statistics chosen is not 'all'. For example ylab = c('Estimated Survival Probability') if Statistics is surv.")
      } else{
        if(statistics == "surv") ylab = c(ylab, NA, NA)
        if(statistics == "diff") ylab = c(NA, ylab, NA)
        if(statistics == "ratio") ylab = c(NA, NA, ylab)
      }
    }
  }


  ########  ########   ########   ########
  ######## Visualising the data:
  ########  ########   ########   ########

  if("surv" %in% sts){

    psurv <- ggplot2::ggplot(data = surv.dat) +
      ggplot2::geom_step(aes(x = time, y = surv1, color = "surv1")) +
      ggplot2::geom_ribbon(aes(x = time, ymin = l.surv1, ymax = u.surv1, fill = "surv1"), linetype = 1, alpha = 0.2) +
      ggplot2::geom_step(aes(x = time, y = surv2, color = "surv2")) +
      ggplot2::geom_ribbon(aes(x = time, ymin = l.surv2, ymax = u.surv2, fill = "surv2"), linetype = 1, alpha = 0.2) +
      ggplot2::scale_colour_manual(name = "", labels = c(as.expression(bquote(paste(hat(S)[.(fit$levels[1])],'(t)'))),as.expression(bquote(paste(hat(S)[.(fit$levels[2])],'(t)')))), values = palette[1:2]) +
      ggplot2::scale_fill_manual(name = "", labels = c(as.expression(bquote(paste(hat(S)[.(fit$levels[1])],'(t)'))),as.expression(bquote(paste(hat(S)[.(fit$levels[2])],'(t)')))), values = palette[1:2]) +
      ggplot2::labs(title = NULL, x = xlab, y = ylab[1]) +
      ggplot2::theme(text = element_text(face = "bold", size = 10),
                     legend.position = "top") +
      ggplot2::scale_x_continuous(breaks = seq (0, max (surv.dat$time), by = break.times.by))
  }

  if("diff" %in% sts){
    if(isTRUE(flag)){
      pdiff <- ggplot2::ggplot(data = surv.dat) +
        ggplot2::geom_step(aes(x = time, y =  diff, color = "sdiff")) +
        ggplot2::geom_ribbon(aes(x = time, ymin = lower.diff, ymax = upper.diff, fill = "sdiff"), linetype = 1, alpha = 0.2) +
        ggplot2::geom_hline(aes(yintercept = 0), linetype = "dashed") +
        ggplot2::geom_line(aes(x=time, y=diffyout, colour=diffout,  group=1),size=2) +
        ggplot2::scale_fill_manual(name = "", labels = paste("S(",fit$levels[1],") - S(",fit$levels[2],")", sep = ""), values = palette[3]) +
        ggplot2::guides(fill = FALSE)+
        ggplot2::scale_colour_manual(name = "",values=c("sdiff"=palette[3],"S"=flag.col[2], "NS"=flag.col[1]),
                                     labels = c("S"=as.expression(bquote(paste(S[.(fit$levels[1])],'(t)' != S[.(fit$levels[2])],'(t)'))),
                                                "NS"=as.expression(bquote(paste(S[.(fit$levels[1])],'(t)', " = ",S[.(fit$levels[2])],'(t)'))),
                                                "sdiff"=as.expression(bquote(paste(hat(S)[.(fit$levels[1])],'(t)', " - ",hat(S)[.(fit$levels[2])],'(t)')))
                                     )) +
        ggplot2::guides(colour = guide_legend(reverse = TRUE)) +
        ggplot2::labs(title = NULL, x = xlab, y = ylab[2]) +
        ggplot2::theme(text = element_text(face = "bold", size = 10),
                       legend.position = "top") +
        ggplot2::scale_x_continuous(breaks = seq (0, max (surv.dat$time), by = break.times.by))
    } else{
      pdiff <- ggplot2::ggplot(data = surv.dat) +
        ggplot2::geom_step(aes(x = time, y =  diff, color = "sdiff")) +
        ggplot2::geom_ribbon(aes(x = time, ymin = lower.diff, ymax = upper.diff, fill = "sdiff"), linetype = 1, alpha = 0.2) +
        ggplot2::geom_hline(aes(yintercept = 0), linetype = "dashed") +
        ggplot2::scale_colour_manual(name = "", labels = as.expression(bquote(paste(hat(S)[.(fit$levels[1])],'(t)', " - ",hat(S)[.(fit$levels[2])],'(t)'))), values = palette[3]) +
        ggplot2::scale_fill_manual(name = "", labels = as.expression(bquote(paste(hat(S)[.(fit$levels[1])],'(t)', " - ",hat(S)[.(fit$levels[2])],'(t)'))), values = palette[3]) +
        ggplot2::labs(title = NULL, x = xlab, y = ylab[2]) +
        ggplot2::theme(text = element_text(face = "bold", size = 10),
                       legend.position = "top") +
        ggplot2::scale_x_continuous(breaks = seq (0, max (surv.dat$time), by = break.times.by))
    }


  }


  if("ratio" %in% sts){
    if(isTRUE(flag)){
      pratio <- ggplot2::ggplot(data = surv.dat) +
        ggplot2::geom_step(aes(x = time, y =  ratio, color = "sratio")) +
        ggplot2::geom_ribbon(aes(x = time, ymin = lower.ratio, ymax = upper.ratio, fill = "sratio"), linetype = 1, alpha = 0.2) +
        ggplot2::geom_hline(aes(yintercept = 1), linetype = "dashed") +
        ggplot2::geom_line(aes(x=time, y=ratioyout, colour=ratioout,  group=1),size=2) +
        ggplot2::scale_fill_manual(name = "", labels = paste("S(",fit$levels[1],") / S(",fit$levels[2],")", sep = ""), values =palette[4]) +
        ggplot2::guides(fill = FALSE)+
        ggplot2::scale_colour_manual(name = "",values=c("sratio"=palette[4],"S"=flag.col[2], "NS"=flag.col[1]),
                                     labels = c("S"=as.expression(bquote(paste(S[.(fit$levels[1])],'(t)' != S[.(fit$levels[2])],'(t)'))),
                                                "NS"=as.expression(bquote(paste(S[.(fit$levels[1])],'(t)', " = ",S[.(fit$levels[2])],'(t)'))),
                                                "sratio"=as.expression(bquote(paste(hat(S)[.(fit$levels[1])],'(t)', " / ",hat(S)[.(fit$levels[2])],'(t)')))
                                     )) +
        ggplot2::guides(colour = guide_legend(reverse = TRUE)) +
        ggplot2::labs(title = NULL, x = xlab, y = ylab[3]) +
        ggplot2::theme(text = element_text(face = "bold", size = 10),
                       legend.position = "top") +
        ggplot2::scale_x_continuous(breaks = seq (0, max (surv.dat$time), by = break.times.by))
    } else{
      pratio <- ggplot2::ggplot(data = surv.dat) +
        ggplot2::geom_step(aes(x = time, y =  ratio, color = "sratio")) +
        ggplot2::geom_ribbon(aes(x = time, ymin = lower.ratio, ymax = upper.ratio, fill = "sratio"), linetype = 1, alpha = 0.2) +
        ggplot2::geom_hline(aes(yintercept = 1), linetype = "dashed") +
        ggplot2::scale_colour_manual(name = "", labels = as.expression(bquote(paste(hat(S)[.(fit$levels[1])],'(t)', " / ",hat(S)[.(fit$levels[2])],'(t)'))), values = palette[4]) +
        ggplot2::scale_fill_manual(name = "", labels = as.expression(bquote(paste(hat(S)[.(fit$levels[1])],'(t)', " / ",hat(S)[.(fit$levels[2])],'(t)'))), values = palette[4]) +
        ggplot2::labs(title = NULL, x = xlab, y = ylab[3]) +
        ggplot2::theme(text = element_text(face = "bold", size = 10),
                       legend.position = "top") +
        ggplot2::scale_x_continuous(breaks = seq (0, max (surv.dat$time), by = break.times.by))
    }

  }

  ### adding p.value:
  if(isTRUE(p.value)) {

    if(fit$paired == TRUE){
      warning("The p.value can not be displayed as it is not reported in the drsurv fit object when paired = TRUE.")
    } else{

      ## defining the text for p.value to be displayed in the figure
      pval.text <- ifelse(fit$pvalue < 0.001, paste("p < 0.001"), paste("p = ", fit$pvalue, sep=""))

      ## adding p.value to figures
      if(statistics == "surv" | statistics == "all"){
        ypoint = (min(surv.dat$surv1, surv.dat$surv2) + (0.4* min(sd(surv.dat$surv2),sd(surv.dat$surv2))))
        psurv <- psurv + ggplot2::annotate(geom = "text", x = (min(surv.dat$time) + (0.2*sd(surv.dat$time))), y = ypoint, label = pval.text,
                                           color="black")
      }


      if(statistics == "diff"){
        ypoint = (min(surv.dat$diff) + (0.4*sd(surv.dat$diff)))
        pdiff <- pdiff + ggplot2::annotate(geom = "text", x = (min(surv.dat$time) + (0.2*sd(surv.dat$time))), y = ypoint, label = pval.text,
                                           color="black")
      }

      if(statistics == "ratio"){
        ypoint = (min(surv.dat$ratio) + (0.4*sd(surv.dat$ratio)))

        pratio <- pratio + ggplot2::annotate(geom = "text", x = (min(surv.dat$time) + (0.2*sd(surv.dat$time))), y = ypoint, label = pval.text,
                                             color="black")
      }
    }


  }

  ### adding table:
  if(isTRUE(table)){

    ## Create a blank plot for place-holding
    blank.pic <- ggplot2::ggplot(surv.dat, aes (time, surv1)) +
      ggplot2::geom_blank () +
      ggplot2::theme_classic () +
      ggplot2::theme(axis.text.x = element_blank (),
                     axis.text.y = element_blank (),
                     axis.title.x = element_blank (),
                     axis.title.y = element_blank (),
                     axis.ticks = element_blank (),
                     panel.grid.major = element_blank (),
                     panel.border = element_blank (),
                     axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "white"),
                     axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "white")
      )


    if(statistics == "surv" | statistics == "all"){
      psurv <- gridExtra::arrangeGrob(psurv,
                                      blank.pic,
                                      risk.table(surv.dat = surv.dat, ystratalabs = fit$levels, col = palette[1:2], timeby = break.times.by),
                                      clip = FALSE,
                                      nrow = 3,
                                      ncol = 1,
                                      heights = unit (c (2, .2, .25),
                                                      c ("null", "null", "null")))
      psurv <- ggpubr::as_ggplot(psurv) ## return ggplot object
    }

    if(statistics == "diff"){
      pdiff <- gridExtra::arrangeGrob(pdiff,
                                      blank.pic,
                                      risk.table(surv.dat = surv.dat, ystratalabs = fit$levels, col = palette[3], timeby = break.times.by),
                                      clip = FALSE,
                                      nrow = 3,
                                      ncol = 1,
                                      heights = unit (c (2, .2, .25),
                                                      c ("null", "null", "null")))
      pdiff <- ggpubr::as_ggplot(pdiff) ## return ggplot object
    }

    if(statistics == "ratio"){
      pratio <- gridExtra::arrangeGrob(pratio,
                                       blank.pic,
                                       risk.table(surv.dat = surv.dat, ystratalabs = fit$levels, col = palette[4], timeby = break.times.by),
                                       clip = FALSE,
                                       nrow = 3,
                                       ncol = 1,
                                       heights = unit (c (2, .2, .25),
                                                       c ("null", "null", "null")))
      pratio <- ggpubr::as_ggplot(pratio) ## return ggplot object
    }
  }

  ### return the output plot:
  if(statistics == "surv") p <- psurv
  if(statistics == "diff") p <- pdiff
  if(statistics == "ratio") p <- pratio
  if(statistics == "all"){
    p <- ggpubr::ggarrange(psurv,
                           ggpubr::ggarrange(pdiff, pratio, ncol = 2, labels = c("(b)", "(c)")),
                           nrow = 2,
                           labels = "(a)",
                           heights = c(1.3,1) # first row's height is 1.3 times bigger than 2nd row's height.
    )
  }

  return(p)
}

