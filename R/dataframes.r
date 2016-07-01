#' Convert a tidy data.frame to an ExpressionSet object
#'
#' @param data tidy data.frame to convert.
#' @param sample name of column that stores sample identifiers
#' @param feature name of column that stores feature identifiers
#' @param value name of column that stores values
#' @param pvars phenotype variables, names of columns that contain metadata
#'   associated with samples
#' @param fvars feature variables, names of columns that contains metadata
#'   associated with features
#' @importFrom reshape2 acast
#' @importFrom stats as.formula
#' @export

from_dataframe <-
  function(data, sample, feature, value, pvars = NULL, fvars = NULL) {
  UseMethod("from_dataframe")
}

#' @export
from_dataframe.data.frame <-
  function(data, sample, feature, value, pvars = NULL, fvars = NULL) {

  check_vars(c(sample, feature, value, pvars, fvars), names(data))

  # verify sample/feature combinations are unique identifiers
  if (any(table(data[c(sample, feature)]) > 1)) {
    stop("The ", sample, " and ", feature,
         " variables do not uniquely identify all observations",
        call. = FALSE)
  }

  cast.by <- substitute(x ~ y, c(x = as.name(feature), y = as.name(sample)))
  mat <- reshape2::acast(data, as.formula(cast.by), fill = 0, value.var = value)
  eset <- Biobase::ExpressionSet(mat)

  dupes <- duplicated(data[[sample]])
  if (!is.null(pvars)) {
    pdata <- data[!dupes, c(sample, pvars), drop = FALSE]
    index <- match(pdata[[sample]], Biobase::sampleNames(eset))
    Biobase::pData(eset)[pvars] <- pdata[index, pvars, drop = FALSE]
  }

  if (!is.null(fvars)) {
    fdata <- data[!dupes, c(feature, fvars), drop = FALSE]
    index <- match(fdata[[feature]], Biobase::featureNames(eset))
    Biobase::fData(eset)[fvars] <- pdata[index, fvars, drop = FALSE]
  }
  eset
}


#' Convert an ExpressionSet object to a tidy data.frame
#'
#' @param data an \code{ExpressionSet} object
#' @param add.pvars should phenotype variables stored in \code{pData(data)} be included in the \code{data.frame}?
#' @param add.fvars should feature variables stored in \code{fData(data)} be included in the \code{data.frame}?
#' @export

to_dataframe <-
  function(data, add.pvars = TRUE, add.fvars = TRUE) {
  UseMethod("to_dataframe")
}

#' @export
to_dataframe.ExpressionSet <-
  function(data, add.pvars = TRUE, add.fvars = TRUE) {

  out <- as.data.frame.table(Biobase::exprs(data))
  names(out) <- c("feature", "sample", "value")

  pdata <- Biobase::pData(data)
  fdata <- Biobase::fData(data)

  if (ncol(pdata) & add.pvars) {
    p.index <- match(out$sample, rownames(pdata))
    out[names(pdata)] <- pdata[p.index, ]
  }

  if (ncol(fdata) & add.pvars) {
    f.index <- match(out$feature, rownames(fdata))
    out[names(fdata)] <- pdata[f.index, ]
  }

  out$sample  <- factor(out$sample,  levels = Biobase::sampleNames(data))
  out$feature <- factor(out$feature, levels = Biobase::featureNames(data))
  out
}
