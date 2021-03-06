% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rq_tidiers.R
\name{rq_tidiers}
\alias{rq_tidiers}
\alias{tidy.rq}
\alias{tidy.rqs}
\alias{tidy.nlrq}
\alias{glance.rq}
\alias{glance.nlrq}
\alias{augment.rq}
\alias{augment.rqs}
\alias{augment.nlrq}
\alias{augment.rqss}
\title{Tidying methods for quantile regression models}
\usage{
\method{tidy}{rq}(x, se.type = "rank", conf.int = TRUE, conf.level = 0.9,
  alpha = 1 - conf.level, ...)

\method{tidy}{rqs}(x, se.type = "rank", conf.int = TRUE, conf.level = 0.9,
  alpha = 1 - conf.level, ...)

\method{tidy}{nlrq}(x, conf.int = FALSE, conf.level = 0.95, ...)

\method{glance}{rq}(x, ...)

\method{glance}{nlrq}(x, ...)

\method{augment}{rq}(x, data = model.frame(x), newdata, ...)

\method{augment}{rqs}(x, data = model.frame(x), newdata, ...)

\method{augment}{nlrq}(x, data = NULL, newdata = NULL, ...)

\method{augment}{rqss}(model, data, confint = FALSE, ...)
}
\arguments{
\item{x}{model object returned by \code{rq} or \code{nlrq}}

\item{se.type}{Type of standard errors to calculate; see \code{summary.rq}}

\item{conf.int}{boolean; should confidence intervals be calculated, ignored
if \code{se.type = "rank"}}

\item{conf.level}{confidence level for intervals}

\item{alpha}{confidence level when \code{se.type = "rank"}; defaults to the same
as \code{conf.level} although the specification is inverted}

\item{\dots}{other arguments passed on to \code{summary.rq}}

\item{data}{Original data, defaults to extracting it from the model}

\item{newdata}{If provided, new data frame to use for predictions}
}
\value{
All tidying methods return a data.frame without rownames, whose
structure depends on the method chosen.

\code{tidy.rq} returns a data frame with one row for each coefficient.
The columns depend upon the confidence interval method selected.

\code{tidy.rqs} returns a data frame with one row for each coefficient at
each quantile that was estimated. The columns depend upon the confidence interval 
method selected.

\code{tidy.nlrq} returns one row for each coefficient in the model,
with five columns:
  \item{term}{The term in the nonlinear model being estimated and tested}
  \item{estimate}{The estimated coefficient}
  \item{std.error}{The standard error from the linear model}
  \item{statistic}{t-statistic}
  \item{p.value}{two-sided p-value}

\code{glance.rq} returns one row for each quantile (tau)
with the columns:
 \item{tau}{quantile estimated}
 \item{logLik}{the data's log-likelihood under the model}
 \item{AIC}{the Akaike Information Criterion}
 \item{BIC}{the Bayesian Information Criterion}
 \item{df.residual}{residual degrees of freedom}

\code{glance.rq} returns one row for each quantile (tau)
with the columns:
 \item{tau}{quantile estimated}
 \item{logLik}{the data's log-likelihood under the model}
 \item{AIC}{the Akaike Information Criterion}
 \item{BIC}{the Bayesian Information Criterion}
 \item{df.residual}{residual degrees of freedom}

\code{augment.rq} returns a row for each original observation
with the following columns added:
 \item{.resid}{Residuals}
 \item{.fitted}{Fitted quantiles of the model}
 \item{.tau}{Quantile estimated}
 
 Depending on the arguments passed on to \code{predict.rq} via \code{\dots}
 a confidence interval is also calculated on the fitted values resulting in
 columns:
     \item{.conf.low}{Lower confidence interval value}
     \item{.conf.high}{Upper confidence interval value}
     
 See \code{predict.rq} for details on additional arguments to specify
 confidence intervals. \code{predict.rq} does not provide confidence intervals
 when \code{newdata} is provided.

\code{augment.rqs} returns a row for each original observation
and each estimated quantile (\code{tau}) with the following columns added:
 \item{.resid}{Residuals}
 \item{.fitted}{Fitted quantiles of the model}
 \item{.tau}{Quantile estimated}
 
 \code{predict.rqs} does not return confidence interval estimates.

\code{augment.rqs} returns a row for each original observation
with the following columns added:
 \item{.resid}{Residuals}
 \item{.fitted}{Fitted quantiles of the model}
}
\description{
These methods tidy the coefficients of a quantile regression
model into a summary, augment the original data with information
on the fitted values and residuals, and construct a glance of
the model's statistics.
}
\details{
If \code{se.type != "rank"} and \code{conf.int = TRUE} confidence
intervals are calculated by \code{summary.rq}. Otherwise they are standard t
based intervals.

This simply calls \code{augment.nls} on the "nlrq" object.
}
