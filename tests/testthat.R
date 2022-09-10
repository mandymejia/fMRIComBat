# Build --> Install and Restart
# [Edit this] path to the Workbench for your computer.
my_wb <- "../workbench"

library(testthat)
library(fMRIComBat)
library(ciftiTools)
if (interactive()) { ciftiTools.setOption("wb_path", my_wb) }

tests_dir <- "testthat"
if (!endsWith(getwd(), "tests")) { tests_dir <- file.path("tests", tests_dir) }
source(file.path(tests_dir, "test-CIFTI.R"))
