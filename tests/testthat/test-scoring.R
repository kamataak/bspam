# Tests for scoring()

make_calib <- function(task_ids = sort(unique(passage2$id.passage))) {
  structure(
    list(
      task.param = tibble::tibble(
        a = rep(1, length(task_ids)),
        b = rep(0, length(task_ids)),
        alpha = rep(2, length(task_ids)),
        beta = rep(1, length(task_ids)),
        task.id = task_ids,
        max.counts = rep(100, length(task_ids))
      ),
      hyper.param = tibble::tibble(vartau = 0.5, rho = 0.2)
    ),
    class = "fit.model"
  )
}

make_scoring_output <- function(type = "general", est = "map") {
  nm <- switch(
    est,
    mle = c("theta.mle", "tau.mle", "se.theta.mle", "se.tau.mle"),
    eap = c("theta.eap", "tau.eap", "se.theta.eap", "se.tau.eap"),
    bayes = c("theta.bayes", "tau.bayes", "se.theta.bayes", "se.tau.bayes"),
    c("theta.map", "tau.map", "se.theta.map", "se.tau.map")
  )

  out <- list(
    person.id = 1,
    occasion = 1,
    group = 1,
    task.n = 2,
    max.counts.total = 200,
    obs.counts.obs = 150,
    secs.obs = 100
  )

  vals <- list(0.1, 0.2, 0.3, 0.4)
  names(vals) <- nm
  out <- c(out, vals)

  if (type == "orf") {
    out <- c(
      out,
      list(
        wcpm.obs = 90,
        obs.counts.map = 155,
        secs.map = 101,
        wcpm.map = 92,
        se.wcpm.map = 5
      )
    )
  }

  structure(out, class = "scoring")
}


test_that("scoring returns scoring object for MAP analytic general scoring", {
  local_mocked_bindings(
    run.scoring = function(...) make_scoring_output(type = "general", est = "map")
  )

  out <- scoring(
    calib.data = make_calib(),
    data = passage2,
    person.id = "id.student",
    occasion = "occasion",
    group = "grade",
    task.id = "id.passage",
    max.counts = "numwords.pass",
    obs.counts = "wrc",
    time = "sec",
    est = "map",
    se = "analytic",
    type = "general"
  )

  expect_s3_class(out, "scoring")
  expect_true("theta.map" %in% names(out))
})


test_that("scoring supports MLE, MAP, and EAP estimator arguments", {
  for (estimator in c("mle", "map", "eap")) {
    local_mocked_bindings(
      run.scoring = function(...) make_scoring_output(type = "general", est = estimator)
    )

    out <- scoring(
      calib.data = make_calib(),
      data = passage2,
      person.id = "id.student",
      occasion = "occasion",
      group = "grade",
      task.id = "id.passage",
      max.counts = "numwords.pass",
      obs.counts = "wrc",
      time = "sec",
      est = estimator,
      se = "analytic",
      type = "general"
    )

    expect_s3_class(out, "scoring")
    expect_true(paste0("theta.", estimator) %in% names(out))
  }
})


test_that("scoring supports type = 'orf'", {
  local_mocked_bindings(
    run.scoring = function(...) make_scoring_output(type = "orf", est = "map")
  )

  out <- scoring(
    calib.data = make_calib(),
    data = passage2,
    person.id = "id.student",
    occasion = "occasion",
    group = "grade",
    task.id = "id.passage",
    max.counts = "numwords.pass",
    obs.counts = "wrc",
    time = "sec",
    est = "map",
    se = "analytic",
    type = "orf"
  )

  expect_s3_class(out, "scoring")
  expect_true("wcpm.map" %in% names(out))
  expect_true("wcpm.obs" %in% names(out))
})


test_that("scoring accepts external task ids", {
  local_mocked_bindings(
    run.scoring = function(..., external = NULL) {
      expect_equal(external, c(1, 2))
      make_scoring_output(type = "orf", est = "map")
    }
  )

  out <- scoring(
    calib.data = make_calib(),
    data = passage2,
    person.id = "id.student",
    occasion = "occasion",
    group = "grade",
    task.id = "id.passage",
    max.counts = "numwords.pass",
    obs.counts = "wrc",
    time = "sec",
    est = "map",
    se = "analytic",
    type = "orf",
    external = c(1, 2)
  )

  expect_s3_class(out, "scoring")
})


test_that("scoring returns NULL for invalid calibration object", {
  expect_null(
    scoring(
      calib.data = list(),
      data = passage2,
      person.id = "id.student",
      task.id = "id.passage",
      max.counts = "numwords.pass",
      obs.counts = "wrc",
      time = "sec"
    )
  )
})

# L.197 course error
# test_that("scoring accepts prepared.task input", {
#   prepared <- prep(
#     data = passage2,
#     person.id = "id.student",
#     task.id = "id.passage",
#     occasion = "occasion",
#     group = "grade",
#     max.counts = "numwords.pass",
#     obs.counts = "wrc",
#     time = "sec"
#   )
# 
#   local_mocked_bindings(
#     run.scoring = function(...) make_scoring_output(type = "general", est = "map")
#   )
# 
#   out <- scoring(
#     calib.data = make_calib(),
#     data = prepared,
#     est = "map",
#     se = "analytic",
#     type = "general"
#   )
# 
#   expect_s3_class(out, "scoring")
# })

test_that("scoring currently does not accept prepared.task input directly", {
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
  
  expect_error(
    scoring(
      calib.data = make_calib(),
      data = prepared,
      est = "map",
      se = "analytic",
      type = "general"
    ),
    "no applicable method for 'select'"
  )
})
