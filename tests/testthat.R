Sys.unsetenv("LC_ALL")
library(testthat)

test_dir("tests/testthat", reporter = "summary")
