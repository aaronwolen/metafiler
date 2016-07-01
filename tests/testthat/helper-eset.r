# example ExpressionSet object
mat <- matrix(
  c(1, 4, 2,
    2, 3, 1),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(paste0("f", 1:2),
                  paste0("s", 1:3))
)
eset <- Biobase::ExpressionSet(mat)
