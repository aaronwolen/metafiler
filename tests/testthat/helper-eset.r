# example ExpressionSet object
mat <- matrix(
  c(1, 5, 4,
    2, 3, 1,
    3, 2, 10),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(paste0("f", 1:3),
                  paste0("s", 1:3))
)
eset <- Biobase::ExpressionSet(mat)
