#' Read metaphalan results
#'
#' @param file the name of the file which the data are to be read from.
#' @param taxa.level the taxonomic level for which the relative abundance output will be loaded.
#' @return ExpressionSet
#' @export
#' @importFrom utils read.table
#' @examples
#' \dontrun{
#' prof.file <- system.file("inst/extdata/profiles.txt", package = "metafiler")
#' read_metaphalan(prof.file)
#' }

read_metaphalan <- function(file, taxa.level = "species") {

  taxa.label <- c(
    "kingdom" = 1,
    "phyla"   = 2,
    "class"   = 3,
    "order"   = 4,
    "family"  = 5,
    "genus"   = 6,
    "species" = 7
  )

  taxa.level <- match.arg(taxa.level, names(taxa.label))
  taxa.i     <- taxa.label[taxa.level]

  # filter data for specified taxa level
  data <- read.table(file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

  taxa.levels  <- strsplit(data$sid, split = "|", fixed = TRUE)
  n.levels     <- vapply(taxa.levels, length, FUN.VALUE = numeric(1))
  i.levels     <- which(n.levels == taxa.i)

  data <- data[i.levels, ]
  data$sid <- vapply(taxa.levels[i.levels], "[", i = taxa.i, FUN.VALUE = "")
  data$sid <- substring(data$sid, first = 4)

  if (length(unique(data$sid)) != nrow(data)) {
    stop(taxa.level, " sids are not unique", call. = FALSE)
  }

  rownames(data) <- data$sid
  data <- as.matrix(data[-1])

  Biobase::ExpressionSet(assayData = data)
}
