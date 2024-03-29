% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lcmmtp.R
\name{lcmmtp}
\alias{lcmmtp}
\title{Causal Mediation with Longitudinal Data Using Modified Treatment Policies}
\usage{
lcmmtp(data, vars, d_prime, d_star, control = .lcmmtp_control())
}
\arguments{
\item{data}{A data frame containing all necessary variables}

\item{vars}{An `lcmmtp_variables` object mapping observed variables to
the assumed variable structure.}

\item{control}{}
}
\value{
An object of class `lcmmtp`
}
\description{
Causal Mediation with Longitudinal Data Using Modified Treatment Policies
}
\examples{
vars <- lcmmtp_variables$new(
    L = list(c("L_1"), c("L_2")),
    A = c("A_1", "A_2"),
    Z = list(c("Z_1"), c("Z_2")),
    M = c("M_1", "M_2"),
    Y = "Y",
    cens = c("c1", "c2")
)

d_ap <- function(data, trt) rep(1, length(data[[trt]]))
d_as <- function(data, trt) rep(0, length(data[[trt]]))

lcmmtp(lcmmtp_foo, vars, d_ap, d_as, .lcmmtp_control(folds = 5))
}
