#' Pre-process BOLD data for ComBat modelling
#' 
#' Pre-process the BOLD data in preparation for ComBat modelling. 
#' 
#' @param BOLD The \eqn{V \times T} fMRI data.
#' @param return_value Return the \code{BOLD} data after
#'  pre-processing (\code{"BOLD"}, default) or the measurements needed
#'  to reconstruct it (\code{"params"})? 
fMRIComBat_preproc <- function(
  BOLD, return_value=c("BOLD", "params"),
  #brainstructures=c("left","right"), mask=NULL,
  nreg=NULL){

  # Read in the data.
  stopifnot(is.matrix(BOLD)) # TEMP
  nV <- nrow(BOLD)
  nT <- ncol(BOLD)

  # Get the mean image, and then demean `BOLD`.
  BOLD_rowMeans <- rowMeans(BOLD, na.rm=TRUE)
  BOLD <- BOLD - BOLD_rowMeans

  # Nuisance regression.
  if (!is.null(nreg)) {
    # Clean up `nreg`: remove constant columns and scale.
    # (Constant columns not needed since `BOLD` is demeaned.)
    nreg_const <- colVars(nreg, na.rm=TRUE) < 1e-8
    nreg <- nreg[,!nreg_const,drop=FALSE]
    nreg <- scale(nreg)
    # Do the nuisance regression.
    nuisance_regression(BOLD, nreg)
  }

  # Scale.
  # (`BOLD_rowMeans` is unchanged.)
  BOLD_rowVars <- rowVars(BOLD, na.rm=TRUE)
  BOLD_rowVars_mean <- mean(BOLD_rowVars)
  stopifnot(BOLD_rowVars_mean > 1e-8)
  BOLD <- (BOLD - mean(BOLD_rowMeans)) / BOLD_rowVars_mean

  # Return.
  switch(return_value,
    BOLD = BOLD,
    params = list(
      BOLD_rowMeans=BOLD_rowMeans,
      BOLD_rowVars_mean=BOLD_rowVars_mean,
      BOLD_rowMeans_postProc=rowMeans(BOLD, na.rm=TRUE)
    )
  )
}

nreg.HCP <- function(
  idx=seq(1200), idx_scrub=NULL,
  TR=.72, hpf=.01, #hpf_method=c("DCT", "FFT"),
  nii=NULL, nii_labels=NULL, 
  ROI_noise=c("wm_cort", "csf"), noise_nPC=5, 
  noise_erosion=NULL,
  RP=NULL, RP_expand=NULL,
  nreg_more=NULL
){
  
  # Check args
  stopifnot(is.numeric(idx) && length(idx) > 1 && all(idx==round(idx)) && min(idx)>0)
  nT <- length(idx)
  if (!is.null(idx_scrub)) { stopifnot(all(idx_scrub %in% idx)) }
  if (!is.null(hpf) && hpf <=0) { hpf <- NULL } 
  if (!is.null(hpf)) {
    stopifnot(!is.null(TR) && is.numeric(TR) && length(TR)==1 && TR > 0)
  }
  if (!is.null(nii)) {
    if (is.character(nii)) { stopifnot(file.exists(nii)) }
    stopifnot(is.null(nii_labels))
    if (is.character(nii_labels)) { stopifnot(file.exists(nii_labels)) }
    # Check rest of args within the CompCor function.
  }
  if (!is.null(RP)) {
    if (is.character(RP)) { RP <- read.table(RP); stopifnot(ncol(RP)>=6); RP <- RP[,seq(6)] }
    stopifnot(is.numeric(RP) && is.matrix(RP) && nrow(RP)==length(idx))
    if (!is.null(RP_expand)) {
      RP_expand <- match.arg(RP_expand, c("derivatives", "squares"), several.ok=TRUE)
      if ("derivatives" %in% RP_expand) { RP_expand <- cbind(RP_expand, rbind(0, diff(RP_expand))) }
      if ("squares" %in% RP_expand) { RP_expand <- cbind(RP_expand, RP_expand^2) }
    }
  }

  nreg <- NULL
  # LEFT OFF HERE
  # DCT
  if (!is.null(hpf)) { nreg <- cbind(nreg, fMRIscrub::dct_bases(nT, NULL)) }

  # 

}

fMRIComBat_estimate.HCP <- function(
  BOLD, brainstructures=c("left","right"), mask=NULL,
  TR=.72, hpf=.01, 
  nii){
  
  NULL
}