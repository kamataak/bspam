# Tests for fit.model()

# These tests focus on the fit.model() interface and mock expensive model-fitting
# internals so the suite can run quickly during development.

test_that("fit.model mcem with se = 'none' returns a fit.model object", {
  local_mocked_bindings(
    run.mcem = function(Y, logT10, N, I, k.in = 5, reps.in = 2,
                        ests.in = NA, verbose = FALSE) {
      list(
        a = rep(1, I),
        b = rep(0, I),
        alpha = rep(2, I),
        beta = rep(1, I),
        max.counts = N,
        vartau = 0.5,
        rho = 0.2
      )
    }
  )

  out <- fit.model(
    data = passage2,
    person.id = "id.student",
    task.id = "id.passage",
    max.counts = "numwords.pass",
    obs.counts = "wrc",
    time = "sec",
    k.in = 1,
    reps.in = 1,
    est = "mcem",
    se = "none"
  )

  expect_s3_class(out, "fit.model")
  expect_named(out, c("task.param", "hyper.param"))
  expect_true(all(c("a", "b", "alpha", "beta", "task.id", "max.counts") %in% names(out$task.param)))
  expect_true(all(c("vartau", "rho") %in% names(out$hyper.param)))
})


test_that("fit.model mcem with se = 'analytic' includes parameter SE columns", {
  local_mocked_bindings(
    run.mcem = function(Y, logT10, N, I, ...) {
      list(
        a = rep(1, I),
        b = rep(0, I),
        alpha = rep(2, I),
        beta = rep(1, I),
        max.counts = N,
        vartau = 0.5,
        rho = 0.2
      )
    },
    numerical.cov = function(Y, logT10, N, I, MCEMout, h.val = 1e-10, M = 100) {
      diag(0.01, 4 * I + 2)
    }
  )

  out <- fit.model(
    data = passage2,
    person.id = "id.student",
    task.id = "id.passage",
    max.counts = "numwords.pass",
    obs.counts = "wrc",
    time = "sec",
    k.in = 1,
    reps.in = 1,
    est = "mcem",
    se = "analytic"
  )

  expect_s3_class(out, "fit.model")
  expect_true(all(c("se.a", "se.b", "se.alpha", "se.beta") %in% names(out$task.param)))
  expect_true(all(c("se.vartau", "se.rho") %in% names(out$hyper.param)))
})


test_that("fit.model mcem with se = 'bootstrap' calls bootmodel.cov and includes SE columns", {
  local_mocked_bindings(
    run.mcem = function(Y, logT10, N, I, ...) {
      list(
        a = rep(1, I),
        b = rep(0, I),
        alpha = rep(2, I),
        beta = rep(1, I),
        max.counts = N,
        vartau = 0.5,
        rho = 0.2
      )
    },
    bootmodel.cov = function(Y, logT10, N, I, MCEMout, k.in, reps.in, B = 10) {
      diag(0.01, 4 * I + 2)
    }
  )

  out <- fit.model(
    data = passage2,
    person.id = "id.student",
    task.id = "id.passage",
    max.counts = "numwords.pass",
    obs.counts = "wrc",
    time = "sec",
    k.in = 1,
    reps.in = 1,
    est = "mcem",
    se = "bootstrap"
  )

  expect_s3_class(out, "fit.model")
  expect_true("se.a" %in% names(out$task.param))
  expect_true("se.vartau" %in% names(out$hyper.param))
})


test_that("fit.model bayes branch returns a fit.model object", {
  local_mocked_bindings(
    bayes = function(person.data, person.id, task.id, max.counts, obs.counts, time, ...) {
      list(
        task.param = tibble::tibble(
          a = 1,
          b = 0,
          alpha = 2,
          beta = 1,
          task.id = 1,
          max.counts = 100
        ),
        hyper.param = tibble::tibble(vartau = 0.5, rho = 0.2)
      )
    }
  )

  out <- fit.model(
    data = passage2,
    person.id = "id.student",
    task.id = "id.passage",
    max.counts = "numwords.pass",
    obs.counts = "wrc",
    time = "sec",
    est = "bayes"
  )

  expect_s3_class(out, "fit.model")
  expect_named(out, c("task.param", "hyper.param"))
})


test_that("fit.model accepts prepared.task input", {
  prepared <- prep(
    data = passage2,
    person.id = "id.student",
    task.id = "id.passage",
    occasion = "occasion",
    group = "grade",
    max.counts = "numwords.pass",
    obs.counts = "wrc",
    time = "sec"
  )

  local_mocked_bindings(
    run.mcem = function(Y, logT10, N, I, ...) {
      list(
        a = rep(1, I),
        b = rep(0, I),
        alpha = rep(2, I),
        beta = rep(1, I),
        max.counts = N,
        vartau = 0.5,
        rho = 0.2
      )
    }
  )

  out <- fit.model(data = prepared, k.in = 1, reps.in = 1, est = "mcem")

  expect_s3_class(out, "fit.model")
})


test_that("fit.model testlet branch delegates to fit.model.testlet", {
  local_mocked_bindings(
    fit.model.testlet = function(data = NULL, person.id = "", sub.task.id = "",
                                 obs.counts = "", time = "", task.id = "",
                                 max.counts = "") {
      structure(
        list(
          task.param = tibble::tibble(
            a = 1,
            b = 0,
            alpha = 2,
            beta = 1,
            task.id = 1,
            sub.task.id = 1
          ),
          hyper.param = tibble::tibble(
            sigma = 1,
            gamma1 = 0.1,
            gamma2 = 0.1,
            rho.theta = 0.2,
            rho.testlet = 0.3
          )
        ),
        class = "fit.model.testlet"
      )
    }
  )

  dat <- tibble::tibble(
    id.student = c(1, 1, 2, 2),
    id.passage = c(1, 1, 1, 1),
    id.sentence = c(1, 2, 1, 2),
    numwords.sent = c(10, 12, 10, 12),
    wrc = c(9, 11, 8, 12),
    sec = c(5, 6, 5.5, 6.5)
  )

  out <- fit.model(
    data = dat,
    person.id = "id.student",
    task.id = "id.passage",
    sub.task.id = "id.sentence",
    max.counts = "numwords.sent",
    obs.counts = "wrc",
    time = "sec",
    testlet = TRUE
  )

  expect_s3_class(out, "fit.model.testlet")
})
