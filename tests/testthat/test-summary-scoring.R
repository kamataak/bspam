# Tests for summary.scoring()

make_passage_scoring_object <- function() {
  structure(
    list(
      person.id = "101",
      occasion = "1",
      group = 3,
      task.n = 2,
      max.counts.total = 200,
      obs.counts.obs = 150,
      secs.obs = 100,
      wcpm.obs = 90,
      tau.mle = 0.1,
      theta.mle = 0.2,
      se.tau.mle = 0.3,
      se.theta.mle = 0.4,
      obs.counts.mle = 155,
      secs.mle = 101,
      task.n.wcpm = 2,
      max.counts.total.wcpm = 200,
      wcpm.mle = 92,
      se.wcpm.mle = 5
    ),
    class = "scoring"
  )
}

make_testlet_scoring_object <- function() {
  structure(
    list(
      person.id = 903,
      occasion = 1,
      group = 1,
      task.n = 1,
      sub.task.n = 8,
      max.counts.total = 80,
      obs.counts.total = 74,
      secs.obs = 35,
      wcpm.obs = 126,
      tau.bayes = -0.1,
      theta.bayes = -0.2,
      se.tau.bayes = 0.3,
      se.theta.bayes = 0.2,
      count.bayes = 87,
      se.count.bayes = 1.5,
      secs.bayes = 55,
      se.secs.bayes = 5.2,
      wcpm.bayes = 95,
      se.wcpm.bayes = 9.1
    ),
    class = "scoring"
  )
}


test_that("summary.scoring handles passage-level output", {
  out <- summary.scoring(make_passage_scoring_object(), verbose = FALSE, show = "short")
  expect_true(is.data.frame(out) || is.matrix(out))
  expect_false("obs.counts.obs" %in% colnames(out))
})


test_that("summary.scoring handles testlet output with sub.task.n and obs.counts.total", {
  expect_no_error(
    out <- summary.scoring(make_testlet_scoring_object(), verbose = FALSE, show = "short")
  )
  expect_false("sub.task.n" %in% colnames(out))
  expect_false("obs.counts.total" %in% colnames(out))
})


test_that("summary.scoring can hide factor scores", {
  out <- summary.scoring(
    make_testlet_scoring_object(),
    verbose = FALSE,
    factor.scores = FALSE,
    show = "long"
  )

  expect_false(any(grepl("theta", colnames(out))))
  expect_false(any(grepl("tau", colnames(out))))
})


test_that("summary.scoring show = 'long' keeps observed columns", {
  out <- summary.scoring(make_testlet_scoring_object(), verbose = FALSE, show = "long")
  expect_true("obs.counts.total" %in% colnames(out))
  expect_true("secs.obs" %in% colnames(out))
})
