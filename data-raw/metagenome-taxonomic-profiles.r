# See http://huttenhower.sph.harvard.edu/metaphlan
# for explanation of data and file format

data.url  <- "http://huttenhower.sph.harvard.edu/sites/default/files/HMP.PF.txt"
data.file <- "inst/extdata/profiles.txt"

dir.create(dirname(data.file), showWarnings = FALSE, recursive = TRUE)
download.file(data.url, data.file)

profiles <- metafiler::read_metaphalan(data.file, taxa.level = "species")

devtools::use_data(profiles, overwrite = TRUE)
