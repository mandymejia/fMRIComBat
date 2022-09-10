# Build --> Install and Restart
# [Edit this] path to the Workbench for your computer.
my_wb <- "~/Desktop/workbench"

library(testthat)
#library(fMRIComBat)
roxygen2::roxygenize()
library(ciftiTools)
if (interactive()) { ciftiTools.setOption("wb_path", my_wb) }

cii <- "/Volumes/GoogleDrive/My Drive/MEJIA_LAB-Damon/Archive/BayesfMRI_testData/test/tfMRI_GAMBLING_LR_Atlas.dtseries.nii"
cii2 <- "~/Desktop/temp.dscalar.nii"
ciftiTools::run_wb_cmd(paste(
  "-cifti-create-dense-scalar",
  ciftiTools:::sys_path(cii2),
  "-volume"
))

# Mean across time
ciftiTools::run_wb_cmd(paste(
  "-cifti-reduce",
  ciftiTools:::sys_path(cii),
  "MEAN",
  ciftiTools:::sys_path(cii2),
  "-direction ROW"
))
xii2 <- read_cifti(cii2)

sii <- separate_cifti(cii)
q <- read_cifti(cii)
q$data$cortex_left[] <- rep(
  fMRIscrub::dct_bases(253, 3)[,3],
  each=29696
)
giiL <- "~/Desktop/temp.L.func.gii"
q <- write_xifti2(q)

ciftiTools::run_wb_cmd(paste(
  "-metric-regression",
  ciftiTools:::sys_path(sii["cortexL"]),
  ciftiTools:::sys_path(cii2),
  "-remove",
  "/Users/ddpham/Documents/GitHub/fMRIComBat/sep.L.func.gii"
))
p <- gifti::readgii("/Users/ddpham/Documents/GitHub/fMRIComBat/sep.L.func.gii")
