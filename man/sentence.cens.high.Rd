% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{sentence.cens.high}
\alias{sentence.cens.high}
\title{Sentence-level data set with high censoring}
\format{
1334 rows and 9 variables:
\describe{
  \item{\code{id.student}}{unique student identifier}
  \item{\code{grade}}{student grade level}
  \item{\code{id.passage}}{unique passage identifier}
  \item{\code{ind.passage}}{passage index}
  \item{\code{id.sentence}}{unique sentence sequence}   
  \item{\code{numwords.sent}}{the total number of words in the sentence}
  \item{\code{wrc}}{the number of words read correct}
  \item{\code{sec}}{time to read the sentence in seconds}
  \item{\code{cens}}{whether a specific passage was censored (0) or fully observed (1)}
}
}
\source{
\url{https://jnese.github.io/core-blog/}
}
\usage{
sentence.cens.high
}
\description{
This is an example data set. It is a sentence-level Oral Reading Fluency 
assessment data set with high proportion of censoring data for 58 students who were assigned to read 4 passages 
with a total of 23 sentences.
The data is a small subset of the data collected by Nese and Kamata (2014-2018).
}
\references{
Nese, J. F. T. & Kamata, A. (2014-2018). Measuring Oral Reading Fluency: 
     Computerized Oral Reading Evaluation (Project No. R305A140203) [Grant]. 
     Institute of Education Sciences, U.S. Department of Education. 
     \url{https://ies.ed.gov/funding/grantsearch/details.asp?ID=1492}
}
\keyword{datasets}
