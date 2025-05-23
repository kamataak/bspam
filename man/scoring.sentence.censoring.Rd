% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scoring_sentence_censoring.R
\name{scoring.sentence.censoring}
\alias{scoring.sentence.censoring}
\title{scoring.sentence.censoring function}
\usage{
scoring.sentence.censoring(
  Count = NULL,
  logT10 = NULL,
  N = NULL,
  Passage = NULL,
  a = NULL,
  b = NULL,
  alpha = NULL,
  beta = NULL,
  gamma1 = NULL,
  gamma2 = NULL,
  sigma = NULL,
  rho = NULL,
  rhoTestlet = NULL,
  C = NULL
)
}
\arguments{
\item{a, }{b: Model parameters related to the count data model (K-dim)}

\item{alpha, }{beta: Model parameters related to the time data model (K-dim)}

\item{gamma1, }{gamma2: Hyperparameters related to the testlet model}

\item{Count:}{A vector with the number of words correct per passage
It should be K-dimensional}

\item{logT10:}{The log-scale reading time per 10 words per passage
It should be K-dimensional}

\item{N:}{A vector of passage lengths
It should be K-dimensional}

\item{Passage:}{Task ids}

\item{sigma:}{The latent standard deviation of the time latent component}

\item{rho:}{The correlation between count and time latent components}

\item{rhoTestlet:}{The testlet-based reliability}

\item{C:}{A vector of indicators whether a specific sentence was 
censored (1) or fully observed (0) -- K-dim}
}
\value{
list
}
\description{
Copyright (C) 2021-2024 The ORF Project Team
}
\details{
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

A copy of the GNU General Public License is available at
http://www.gnu.org/licenses/


Pre-processing data: Function prepares data for Stan model
Enter a single student's data for preparation

Assume a student has read K >= 2 passages
}
