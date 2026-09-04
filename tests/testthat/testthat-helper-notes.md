# How to install and run these testthat files

1. Put the `.R` files in your package repository under:

   `tests/testthat/`

2. Make sure your package has testthat configured. If not, run once from the package root:

   ```r
   usethis::use_testthat(3)
   ```

3. Run all tests from the package root:

   ```r
   devtools::test()
   ```

4. Run a single file:

   ```r
   testthat::test_file("tests/testthat/test-fit-model.R")
   ```

5. Run full package checks:

   ```r
   devtools::check()
   ```

Notes:

- These tests use `local_mocked_bindings()`, available in modern `testthat` 3e.
- Several tests mock heavy functions to avoid Stan/JAGS and long MCEM runs.
- The summary.scoring testlet tests assume the updated `summary.scoring()` logic handles `sub.task.n` and `obs.counts.total`.
