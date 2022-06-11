#' fMRI ComBat
#' 
#' Batch correction for fMRI data
#' 
#' @param BOLD file paths to the BOLD data as CIFTI, NIFTI, or GIFTI files.
#' @param batch Key for the batch of each entry in \code{BOLD}.
#' @param ... Description
#' @return The value
#' @export
fMRIComBat <- function(BOLD, batch, nreg=NULL, TR=NULL, hpf=.01, scale=TRUE){

  # Format of `BOLD`
  format <- infer_BOLD_format(BOLD)
  FORMAT <- switch(format,
    CIFTI = "CIFTI",
    xifti = "CIFTI",
    NIFTI = "NIFTI",
    nifti = "NIFTI",
    data = "DATA"
  )

  if (FORMAT == "NIFTI") {
    if (!requireNamespace("RNifti", quietly = TRUE)) {
      stop("Package \"RNifti\" needed to read NIFTI data. Please install it.", call. = FALSE)
    }
  }

  nN <- length(BOLD)

  # If BOLD (and BOLD2) is a CIFTI or NIFTI file, check that the file paths exist.
  if (format %in% c("CIFTI", "NIFTI")) {
    missing_BOLD <- !file.exists(BOLD)
    if (all(missing_BOLD)) stop('The files in `BOLD` do not exist.')
    if (any(missing_BOLD)) {
      warning('There are ', missing_BOLD, ' scans in `BOLD` that do not exist. These scans will be excluded from template estimation.')
      BOLD <- BOLD[!missing_BOLD]
    }
  }

  # Check `batch`
  stopifnot(length(batch)==nN && is.numeric(batch))
  stopifnot(!any(batch %in% c(NA, NaN)))

  nB <- length(unique(batch))

  # Process each scan ----------------------------------------------------------
  format2 <- if (format == "data") { "numeric matrix" } else { format }
  if (verbose) {
    cat("Data input format:             ", format2, "\n")
    cat('Number of sessions:            ', nN, "\n")
    cat('Number of batches:             ', nB, "\n")
  }

}