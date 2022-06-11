#' Pre-clean the data
#' 
#' Perform nuisance regression and temporal filtering.
#' 
#' @param X The \eqn{T \times V} data.
#' @return The value
#' @export
preclean <- function(X, nreg=NULL, TR, hpf=.01, lpf=NULL){
  NULL
}

#' Scale the data globally
#' 
#' Scale and center by the global mean and SD
#' 
#' @param X The \eqn{T \times V} data.
#' @return The value
#' @export
gscale <- function(X){
  NULL
}

#' Mean image of time series
#' 
#' Compute the mean across time
#' 
#' @param X The \eqn{T \times V} data.
#' @return The value
#' @export
mean_img <- function(X){
  NULL
}

#' Infer fMRI data format
#'
#' @param BOLD The fMRI data
#' @param verbose Print the format? Default: \code{FALSE}.
#' @return The format: \code{"CIFTI"} file path, \code{"xifti"} object,
#'  \code{"NIFTI"} file path, \code{"nifti"} object, or \code{"data"}.
#' @keywords internal
infer_BOLD_format <- function(BOLD, verbose=FALSE){

  # Character vector: CIFTI or NIFTI
  if (is.character(BOLD)) {
    format <- ifelse(
      endsWith(BOLD, ".dtseries.nii") | endsWith(BOLD, ".dscalar.nii"),
      "CIFTI", "NIFTI"
    )
    if (length(unique(format)) == 1) {
      format <- format[1]
    } else {
      if (all(endsWith(BOLD, ".nii"))) {
        stop("BOLD format seems to be a mix of CIFTI files and NIFTI files. Use the same format or rename the files.")
      } else {
        stop("BOLD format seems to be a mix of CIFTI files and something else. Use the same format or rename the files.")
      }
    }

  } else if (inherits(BOLD, "xifti")) {
    format <- "xifti"
  } else if (inherits(BOLD, "nifti")) {
    format <- "nifti"

  # Non-character vector: xifti, nifti, or data
  } else if (inherits(BOLD[[1]], "xifti")) {
    if (all(vapply(BOLD, inherits, what="xifti", FALSE))) {
      format <- "xifti"
    } else {
      stop("BOLD format seems to be a mix of `xifti` files and something else. Use the same format for all.")
    }
  } else if (inherits(BOLD[[1]], "nifti")) {
    if (all(vapply(BOLD, inherits, what="nifti", FALSE))) {
      format <- "nifti"
    } else {
      stop("BOLD format seems to be a mix of `nifti` files and something else. Use the same format for all.")
    }
  } else {
    if (!is.list(BOLD)) { BOLD <- list(BOLD) }
    BOLD_dims <- lapply(BOLD, dim)
    BOLD_dims_lens <- sort(unique(vapply(BOLD_dims, length, 0)))
    if (length(BOLD_dims_lens) > 1) {
      stop("BOLD data have inconsistent dimension lengths. fMRI data should be provided as matrices, not vectors or arrays.")
    } else if (BOLD_dims_lens==4) {
      format <- "nifti" # 4D array: treat as a "nifti"
    } else if (BOLD_dims_lens!=2) {
      stop("BOLD data should be provided as matrices, not vectors or arrays.")
    } else {
      format <- "data"
    }
  }
  if (verbose) { cat("Inferred input format:", format, "\n") }
  format
}