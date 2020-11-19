\name{drsurv}
\alias{drsurv}
\title{Bootstrap pointwise confidence bands for survival ratio and survival difference
}
\description{Estimate the survival ratio and survival difference accompanied by their pointwise confidence bands to provide a comparison of the survival prospects (\code{S(t)}) for independent and paired survival problems.
}
\usage{
drsurv(time, status, factor, ref = NULL, tgrid = NULL, paired = FALSE, id = NULL,
  clevel = 0.95, nboot = 1000)
}
\arguments{
  \item{time}{a vector of the observed survival times.
}
  \item{status}{a binary vector indicating the censoring status (typically 0 = 'censored', 1 = 'event').
}
  \item{factor}{a binary vector indicating the group levels.
}
  \item{ref}{the reference level (typically a string) used as the level to compare the survival prospects with.
}
  \item{tgrid}{a sequence of follow-up time points to calculate survival estimates for.
}
  \item{paired}{a logical indicator whether the survival data is paired. The defaults is \code{paired = FALSE}.
}
  \item{id}{a pair identifier variable which must be provided for paired survival data problems (i.e. when \code{paired = TRUE}).
}
  \item{clevel}{the confidence level used for constructing the pointwise confidence bands. The default is \code{clevel = 0.95}.
}
  \item{nboot}{an integer indicating the number of bootstrap replicates. The defaults is \code{nboot = 1000}.
}
}
\value{
A list of objects including a data frame of the estimates of survival ratio and survival difference accompanied by their pointwise confidence bands. In addition, it includes the confidence level used, the factor levels, the number of bootstrap replicates, whether an independent or paired design was used and the p-value from the log-ranke test of equality for independent designs.
}
\references{
Newell, J., Kay, J. W., & Aitchison, T. C. (2006). Survival ratio plots with permutation envelopes in survival data problems. Computers in biology and medicine, 36(5), 526-541.
}
\author{
Amirhossein Jalali, Shirin Moghaddam, Davood Roshan, John Newell
}

\seealso{
\link{Surv}, \link{ggsurv}
}
\examples{
require(survival)

##### Independent survival data
\donttest{
data(lung)
fit1 <- drsurv(time = lung$time, status = lung$status, factor = lung$sex)
ggsurv(fit1)

##### Paired survival data
data(retinopathy)
fit2 <- drsurv(retinopathy$futime, retinopathy$status, retinopathy$trt,
  paired = TRUE, id = retinopathy$id)
head(fit2$surv)
ggsurv(fit2, statistics = "all")}
}
