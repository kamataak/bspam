bspam
================

# Speed-Accuracy Psychometric Modeling for Binomial Count Outcome Data with R

`bspam` is an R package that contains functions to fit the
speed-accuracy psychometric model for count outcome data (Potgieter,
Kamata & Kara, 2017; Kara, Kamata, Potgieter & Nese, 2020), where the
accuracy is modeled by a binomial count latent variable model.

For example, the use of this modeling technique allows model-based
calibration and scoring for oral reading fluency (ORF) assessment data.
This document demonstrates some uses of the `bspam` package by using an
ORF assessment data set.

## Installation:

To install the `bspam` package, follow the steps below.

1.  Install `remotes` package, by running the following R code.

``` r
if(!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
```

2.  Install `bspam` by running the following R code.

``` r
remotes::install_github("kamataak/bspam")
```

### Optional:

`bspam` can implement Bayesian estimation by using JAGS and Stan. If you
desire to use the Bayesian estimation, please download JAGS and Stan
software on your computer.

To download and install JAGS, download the installation file from
<https://sourceforge.net/projects/mcmc-jags/files/JAGS/> as per
operating system requirements and follow the installation steps. `bspam`
internally uses `runjags` package as an interface to JAGS. `runjags`
will be automatically installed along with other R packages that `bspam`
depends on.

Stan can be installed by following the steps explained here:
<https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started>. Note
that these steps show the installation of `RStan`, which `bspam` uses as
an internal package to run Stan code. Please follow the installation
steps carefully along with recommended versions of R and RStudio to
prevent any issues releated to running Stan with `bspam`.

## Basic Usage:

### Data Preparation

It is required that the data are prepared as a long-format data, where
each row is data for a unique case, namely, a specific task from a
specific student in a specific testing occasion. A data file can be
prepared in any file format that is readable into R.

For example, a CSV formatted data can be read into R by using the
`read.csv()` function that is part of the base packages. Also, data
files in some statistical software formats can be read in to R with
functions in packages, such as the `haven` package. For example,
`read_spss()` function can be used to read in an SPSS formatted data
file into R.

The data set should minimally contain the following 7 variables: (1)
individual ID, (2) group ID, (3) testing occasion ID, (4) task ID, (5)
the maximum number of attempts in the task, (6) the number of successes
in the task, and (7) the time took to complete the task. Note if the
data are coming from only one testing occasion and one group, the
variables (2) group ID, and (3) testing occasion ID should contain an
arbitrary constant value, such as all 1’s.

Variable names and the order of the variables can be flexible in the
data frame. When running functions in this package, variable names for
required variables need to be specified, as shown in examples below.

Then, the data need to be read into R as a data frame.

### Data Set For the Demonstration

The `bspam` package comes with several data sets. To demonstrate some
basic usage of key functions in the package, a sample ORF assessment
data set is used here. In the context of an ORF assessment, a task is a
passage given to the student to read aloud, and an attempt is student’s
reading on a word in the passage, which is scored correct (i.e.,
success) or not. Accordingly, the number of successes in the task is the
number of words correctly read in the passage.

The data set `passage2` is a passage-level student data and consisted of
reading accuracy and time data for 12 passages from 85 students.
Although the 85 students were assigned to all 12 passages, the number of
passages read by the 85 students varied from 2 to 12 passages. The
number of students per passage were between 59 to 79.

### Load Packages

Load required packages, and view the example data set `passage2`.

``` r
library(tidyverse)
library(bspam)
View(passage2)
```

### Passage Calibration

Calibrate the passages using the `fit.model()` function by implementing
the Monte Carlo EM algorithm described in Potgieter et al. (2017).

``` r
MCEM_run <- fit.model(person.data = passage2,
                      person.id = "id.student",
                      task.id = "id.passage",
                      max.counts = "numwords.pass",
                      obs.counts = "wrc",
                      time = "sec",
                      k.in = 5,
                      reps.in = 50,
                      est = "mcem")
MCEM_run
```

By default, the standard errors for the model parameters are not
estimated. This will allow one to increase the number of Monte-Carlo
iterations `reps.in` to improve the quality of the model parameter
estimates, while minimizing the computation time. The number of
`reps.in` should be 50 to 100 in realistic calibrations. Note that SE’s
for model parameters are not required for running the `scoring()`
function to estimate latent factor scores and WCPM scores in the next
step. If standard errors for model parameters are desired, an additional
argument `se = "analytic"` or `se = "bootstrap"` needs to be added to
the `fit.model()` function.

Passage calibration can also be done by using the `est = "bayes"`
option, which implements a fully Bayesian approach through Gibbs
sampling or Hamiltonian Monte Carlo with JAGS and Stan, respectively.
Currently, Stan will be used with complete data, that is, if all
students have data on all passages. Otherwise, JAGS will be used as it
provides faster computations with missing observations. Note that
`bspam` runs JAGS in the auto mode, which does not require the user to
supply any specifications for Bayesian estimation (e.g., number of the
iterations) or monitor convergence. The standard deviations of the
posterior distributions are reported as the standard errors. A
specification for the `fit.model()` function can look like as follows.

``` r
Bayes_run <- fit.model(person.data = passage2,
                       person.id = "id.student",
                       task.id = "id.passage",
                       max.counts = "numwords.pass",
                       obs.counts = "wrc",
                       time = "sec",
                       est = "bayes",
                       verbose=TRUE)
Bayes_run
```

### Estimating Latent Factor Scores and WCPM Scores 1

In order to estimate WCPM scores and/or latent factor scores, the
`scoring()` function needs to be run.

Note that we use the output object `MCEM_run` from the passage
calibration phase. By default (`type = "general"`), only factor scores
($\theta$ and $\tau$) and their standard errors are reported. By
providing an argument `type = "orf"`, WCPM scores and their standard
errors are reported, rather than the factor scores. Also, latent factor
scores or WCPM scores and their standard errors will be estimated for
all cases in the data by default.

There are several estimator options and standard error estimation
options. In this example, maximum a priori (MAP) estimators for model
parameter estimation and analytic approach to estimate standard errors
are used.

#### Estimation for All Cases

To estimate WCPM scores for all observations in the dataset `passage2`,
we use the `scoring()` function as follows.

\#Note: Did not run initially and gave an error: \#Error in
e$fun(obj, substitute(ex), parent.frame(), e$data) : \# worker
initialization failed: there is no package called ‘miscTools’

\#Installed `miscTools` package, then it ran.

``` r
WCPM_all <- scoring(calib.data=MCEM_run, 
                    person.data = passage2,
                    person.id = "id.student",
                    occasion = "occasion",
                    group = "grade",
                    task.id = "id.passage",
                    max.counts = "numwords.pass",
                    obs.counts = "wrc",
                    time = "sec",
                    est = "map", 
                    se = "analytic",
                    type="general")
summary(WCPM_all)
```

#### Estimation for Selected Cases

If the computations of WCPM scores and/or factor scores for only
selected cases are desired, a list of cases needs to be provided by the
`cases =` argument. The list of cases has to be a one-variable data
frame with a variable name `cases`. The format of case values should be:
`studentid_occasion`. This one-variable data frame can be created
manually, just like shown below. Also, it can be generated by the
`get.cases()` function, which will be shown later in this document.

``` r
sample.cases <- data.frame(cases = c("2033_fall", "2043_fall", "2089_fall"))
WCPM_sample <- scoring(calib.data = MCEM_run, 
                       person.data = passage2,
                       person.id = "id.student",
                       occasion = "occasion",
                       group = "grade",
                       task.id = "id.passage",
                       max.counts = "numwords.pass",
                       obs.counts = "wrc",
                       time = "sec",
                       cases = sample.cases,
                       est = "eap", 
                       se = "analytic",
                       type = "orf")
summary(WCPM_sample)
```

#### Estimation with External Task Set

Also, we can specify a set of passages to scale the WCPM scores. If WCPM
scores are scaled with a set of passages that is different from the set
of passages the student read, the set of passages is referred to as an
**external task set**, or **external passage set** for the ORF
assessment context.

The use of an external passage set is particularly important to make the
estimated WCPM scores to be comparable between students who read
different sets of passages, as well as within students for longitudinal
data, where a student are likely to read different sets of passages.

``` r
WCPM_sample_ext1 <- scoring(calib.data = MCEM_run, 
                            person.data = passage2,
                            person.id = "id.student",
                            occasion = "occasion",
                            group = "grade",
                            task.id = "id.passage",
                            max.counts = "numwords.pass",
                            obs.counts = "wrc",
                            time = "sec",
                            cases = sample.cases, 
                            external = c("32004","32010","32015","32016","33003","33037"),
                            est = "eap", 
                            se = "analytic",
                            type = "orf")
summary(WCPM_sample_ext1)
```

Fully Bayesian approach can also be used as an estimator in the
`scoring()` function. Note that if `est="bayes"` is specified, there is
no need to use the `se=` argument. By default, the standard deviations
of the posterior distributions are reported as the standard errors,
along with 95% high density intervals (i.e., analogous to confidence
intervals). Here is an example of using the fully Bayesian approach for
estimating the same WCPM scores from external passages used in the
former example:

``` r
WCPM_sample_ext1_bayes <- scoring(calib.data=MCEM_run, 
                                  person.data = passage2,
                                  person.id = "id.student",
                                  occasion = "occasion",
                                  group = "grade",
                                  task.id = "id.passage",
                                  max.counts = "numwords.pass",
                                  obs.counts = "wrc",
                                  time = "sec",
                                  cases = sample.cases, 
                                  external = c("32004","32010","32015","32016","33003","33037"),
                                  est = "bayes")
summary(WCPM_sample_ext1_bayes)
```

### Estimating WCPM scores 2

Alternatively, we can run the `scoring()` in two steps.

**Step 1:** Prepare the data using the `prep()` function, where required
data for the `scoring()` function are prepared, including changing
variable names and a generation of the natural-logarithm of the time
data.

The output from the `prep()` function is a list of two components. The
`data.long` component is a data frame, which is a long format of student
response data, and the `data.wide` is list that contains four
components, including a wide format of the data, as well as other
information such as the number of passages and the number of words for
each passage.

One benefit of this two-step approach is that we can use another utility
function `get.cases()` to generate a list of unique cases with the
output of the `prep()` function. This list can be useful when our
interest is to estimate WCPM scores only for selected cases.

``` r
data <- prep(data = passage2,
             person.id = "id.student",
             occasion = "occasion",
             group = "grade",
             task.id = "id.passage",
             max.counts = "numwords.pass",
             obs.counts = "wrc",
             time = "sec")
```

Generate a list of unique cases:

``` r
all.cases <- get.cases(data$data.long)
all.cases
```

**Step 2:** Run the `scoring()` function to estimate WCPM scores. Note
that we pass the output object `MCEM_run` from the passage calibration
phase, as well as the manipulated data `data.long` from Step 1. By
default, WCPM scores will be estimated for all cases in the data.
Additionally, there are several estimator options and standard error
estimation options. In this example, maximum a priori (MAP) estimators
for model parameter estimation and analytic approach to estimate
standard errors are used.

``` r
WCPM_all <- scoring(calib.data=MCEM_run, 
                    person.data = data$data.long,
                    est = "map", 
                    se = "analytic",
                    type = "orf")
summary(WCPM_all)
```

``` r
WCPM_sample_ext2 <- scoring(calib.data=MCEM_run, 
                            person.data = data$data.long,
                            cases = sample.cases, 
                            external = c("32004","32010","32015","32016","33003","33037"),
                            est = "map", 
                            se = "analytic",
                            type = "orf")
summary(WCPM_sample_ext2)
```

Please see the [package website](https://github.com/kamataak/bspam/) for
more detailed usage of the package.

## Citation

## Copyright Statement

Copyright (C) 2022-2023 The ORF Project Team

The bspam package is free software: you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or any
later version.

The bspam package is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details. You should have received a copy of the
GNU General Public License along with this package. If not, see
<http://www.gnu.org/licenses/>.
