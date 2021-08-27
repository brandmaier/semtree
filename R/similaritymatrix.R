similarityMatrix <- function(forest, ...){
  combs <- combn(1:nrow(forest$data), 2)
  dat <- forest$data
  forst <- forest$forest
  vals <- future.apply::future_lapply(FUN = function(tree){
    vec <- semtree:::traverse(tree = tree, data = dat)
    vec[combs[1,]] == vec[combs[2,]]
  }, X = forst)
  vals <- do.call(cbind, vals)
  vals <- rowMeans(vals)
  locs <- cbind(combs, combs[c(2,1), ], matrix(1:nrow(dat), ncol = nrow(dat), nrow = 2, byrow = TRUE))
  vals <- c(vals, vals, rep(1, nrow(dat)))
  as.matrix(Matrix::sparseMatrix(i = locs[1,], j = locs[2, ], x = vals))
}