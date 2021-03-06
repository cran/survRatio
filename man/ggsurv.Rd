\name{ggsurv}
\alias{ggsurv}
\title{Plot method for \code{\link{drsurv}()} objects}
\description{
\code{ggsurv} is a generic function to plot survival functions,  difference in survival functions and ratios of survivor using ggplot2. The function plots or all plots as generated by the \code{\link{drsurv}()} function.
}
\usage{
ggsurv(fit,
       statistics = c("surv", "diff", "ratio", "all"),
       palette = NULL,
       xlab = NULL, ylab = NULL,
       theme = NULL,
       table = FALSE,
       p.value = FALSE,
       flag = NULL,
       flag.col = NULL,
       break.times.by = NULL)
}
\arguments{
  \item{fit}{
a \code{\link{drsurv}()} object.
}
  \item{statistics}{
the statistics of interest to be plotted. The default is \code{statistics = "ratio"}.
\itemize{
\item If \code{statistics = "surv"}, the Kaplan Meier estimated survivor function with confidence intervals will be plotted.
\item If \code{statistics = "diff"}, the difference in survival functions with the associated bootstrap pointwise confidence bands will be plotted.
\item If \code{statistics = "ratio"}, the ratio of survival functions with the associated bootstrap pointwise confidence bands will be plotted.
\item If \code{statistics = "all"} is selected, all of the survival, difference and ratio functions will be plotted in a single labelled graphic.
}
}
\item{palette}{
the color palette to be used for coloring or filling the survival, difference and ratio curves.
\itemize{
\item If \code{statistics = "surv"}, the palette should consist of two colors that corresond to the levels of the factor variable in the \code{\link{drsurv}()} object. The defaults is \code{palette = c("red", "blue")}.
\item If \code{statistics = "diff"}, the palette should consist of one color. The defaults is \code{palette = "black"}.
\item If \code{statistics = "ratio"}, the palette should consist of one color. The defaults is \code{palette = "black"}.
\item If \code{statistics = "all"}, the palette should consist of four colors where the first two colors correspond to the levels of the factor variable in the \code{\link{drsurv}()} object. The third color indicates the color for the difference plot and the forth color indicates the color for the ratio plot. The defaults are \code{palette = c("red", "blue", "black", "black")}.
}
}
\item{xlab}{
a character specifying the x axis label. The default is \code{xlab = "Follow up times"}.
}
\item{ylab}{
a character specifying y axis label.
\itemize{
\item If \code{statistics = "surv"}, the ylab should consist of only one character indicating the y axis label for the Kaplan Meier plot. The default is \code{ylab = "Estimated Survival Probability"}.
\item If \code{statistics = "diff"}, the ylab should consist of only one character indicating the y axis label for the Survival Difference plot. The default is \code{ylab = "Estimated Survival Difference"}.
\item If \code{statistics = "ratio"}, the ylab should consist of only one character indicating the y axis label for the Survival Ratio plot. The default is \code{ylab = "Estimated Survival Ratio"}.
\item If \code{statistics = "all"}, the ylab should consist of three characters indicating the y axis labels for the survival, difference and ratio plots, respectively. The default is \code{ylab = c("Estimated Survival Probability", "Estimated Survival Difference", "Estimated Survival Ratio")}.
}
}
\item{theme}{
ggplot2 themes. Allowed values include ggplot2 official themes: \code{"bw"} for \code{theme_bw()}, \code{"classic"} for \code{theme_classic()}, \code{"dark"} for \code{theme_dark()}, \code{"gray"} for \code{theme_gray()}, \code{"light"} for \code{theme_light()}, \code{"linedraw"} for \code{theme_linedraw()}, \code{"minimal"} for \code{theme_minimal()}. The default is \code{theme = "minimal"}.
}
  \item{table}{
Allowed values include \code{TRUE} or \code{FALSE} specifying whether to display the table of the number of subjects at risk over time. Default is \code{table = FALSE}.
}
  \item{p.value}{
Allowed values include \code{TRUE} or \code{FALSE} specifying whether to display the p-value for the log-ranke test of equality for independent designs. Default is \code{p.value = FALSE}.
}
\item{flag}{
Allowed values include \code{TRUE} or \code{FALSE} specifying whether or not to show regions where survival is different from 1 for the ratio and 0 for the difference. Default is \code{flag = TRUE}.
}
\item{flag.col}{
the color palette to be used for coloring the time regions where survival is identifed as being different when \code{flag = TRUE}. The default is \code{flag.col = c("green", "red")} where "green" indicates survival times where the (pointwise) confidence intervals contained 1 for the ratio or 0 for the difference while "red" indicates survival times where the (pointwise) confidence intervals did not contain 1 for the ratio or 0 for the difference.
}
\item{break.times.by}{
numeric value controlling the time axis breaks. Default value is \code{NULL}.
}
}

\value{
Returns an object of class ggsurv and ggplot2 that displays either one (or all) of the Kaplan Meier estimated survivor function, the ratio of survivor functions or the difference of survivor functions as a single graphic using ggplot2.
}

\note{
The \code{\link{ggsurv}()} object is a flexible ggplot2 object that can be further modified and customised using ggplot commands when the arguments \code{table = FALSE} and statistics is not all. See the example provided below.
}

\references{
https://github.com/michaelway/ggkm
}

\author{
Davood Roshan, Amirhossein Jalali, Shirin Moghaddam, John Newell
}

\seealso{
\link{drsurv}
}
\examples{
\donttest{
# Fit the drsurv model
#++++++++++++++++++++++++++++++++++++
require(survival)
lung$sex <- as.factor(lung$sex)
levels(lung$sex) <- c("Male", "Female")
fit <- drsurv(time = lung$time, status = lung$status, factor = lung$sex, ref = "Female")

# Visualise the drsurv model output
#++++++++++++++++++++++++++++++++++++

# Basic survival curves
ggsurv(fit, statistics = "surv")

# Customized survival curves
ggsurv(fit, statistics = "surv",
       palette = c("Orange", "purple"),
       xlab = "Time (days)", ylab = "Estimated Survival Probability",
       theme = "classic",
       table = TRUE,
       p.value = TRUE,
       break.times.by = 200)


# Survival difference curve
ggsurv(fit, statistics = "diff")

# Survival difference curve without the point wise significant different times flag
ggsurv(fit, statistics = "diff", flag = FALSE)

# Customized survival difference curve
ggsurv(fit, statistics = "diff",
       palette = c("gray"),
       xlab = "Time (days)", ylab = "Estimated Survival Difference",
       theme = "classic",
       flag = TRUE,
       flag.col = c("darkseagreen", "red4"),
       break.times.by = 200)


# Survival ratio curve
ggsurv(fit, statistics = "ratio")

# Customized survival ratio curve with further ggplot2 objects
ggsurv(fit, statistics = "ratio") +
  ggtitle("Overall survival from lung cancer") +
  theme(legend.position="bottom",
        legend.background = element_rect(fill="lightblue", linetype="solid")
        )

# Displaying all the survival, difference and ratio curves in one graphic
## For better visualisation, user may need to increase the ratio of "Plots" pannel
## in Rstudio working environment.
ggsurv(fit, statistics = "all")

# Customized all in one curves
ggsurv(fit, statistics = "all",
       palette = c("Orange", "purple", "dimgray", "dimgray"),
       xlab = "Time (days)",
       ylab = c("Estimated Survivor Function", "Estimated Survival Difference",
       "Estimated Survival Ratio"),
       theme = "light",
       table = TRUE, ## table will be only shown for the survival plot.
       p.value = TRUE, ## p.value will be only displayed for the survival plot.
       flag = TRUE,
       flag.col = c("limegreen", "darkred"),
       break.times.by = 150)

# personalized all in one curves
survp <- ggsurv(fit, statistics = "surv",
                p.value = TRUE,
                table = TRUE)

diffp <- ggsurv(fit, statistics = "diff")

ratiop <- ggsurv(fit, statistics = "ratio")

## changing the labels
ggpubr::ggarrange(survp,
          ggpubr::ggarrange(diffp, ratiop, ncol = 2, labels = c("(ii)", "(iii)")),
          nrow = 2,
          labels = "(i)",
          heights = c(1.3,1) # first row's height is 1.3 times bigger than 2nd row's height.
)

## changing the alignment and save it locally

#png("all_in_one.png", height = 1000, res=100)
#ggpubr::ggarrange(ratiop, diffp, survp,
#          nrow = 3,
#          labels = c("(i)","(ii)", "(iii)"),
#          heights = c(1,1,1.5) # 1st and 2nd rows' heights are equal while
#                               # the 3rd row's height is 1.5 times bigger than the first #two rows.
#)
#dev.off()

## get the directory where the plot is saved
getwd()

### See ?ggpubr::ggarrange for more customizations.
}
}
