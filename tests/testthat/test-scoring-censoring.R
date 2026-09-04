# Tests for scoring() censoring/testlet routes.

make_testlet_calib <- function() {
  structure(
    list(
      task.param = tibble::tibble(
        a = c(1, 1),
        b = c(0, 0),
        alpha = c(2, 2),
        beta = c(1, 1),
        task.id = c(1, 1),
        sub.task.id = c(1, 2)
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

make_sentence_data <- function() {
  tibble::tibble(
    id.student = c(1, 1, 2, 2),
    occasion = c(1, 1, 1, 1),
    grade = c(3, 3, 3, 3),
    id.passage = c(1, 1, 1, 1),
    id.sentence = c(1, 2, 1, 2),
    numwords.sent = c(10, 12, 10, 12),
    wrc = c(9, 11, 8, 12),
    sec = c(5, 6, 5.5, 6.5),
    cens = c(0, 0, 0, 0)
  )
}


test_that("scoring with testlet = TRUE forces censoring path and returns scoring.censoring", {
  local_mocked_bindings(
    scoring.sentence.censoring = function(...) {
      list(
        theta_acc_est = c(0.1, 0.2),
        theta_spd_est = c(0.3, 0.4),
        theta_acc_sd = c(0.01, 0.02),
        theta_spd_sd = c(0.03, 0.04),
        count_est = c(20, 21),
        time_est = c(10, 11),
        count_sd = c(1, 1),
        time_sd = c(1, 1),
        wcpm_est = c(120, 115),
        wcpm_sd = c(5, 5)
      )
    }
  )

  out <- scoring(
    calib.data = make_testlet_calib(),
    data = make_sentence_data(),
    person.id = "id.student",
    task.id = "id.passage",
    sub.task.id = "id.sentence",
    occasion = "occasion",
    group = "grade",
    max.counts = "numwords.sent",
    obs.counts = "wrc",
    time = "sec",
    cens = "cens",
    type = "orf",
    testlet = TRUE
  )

  expect_s3_class(out, "scoring.censoring")
})

