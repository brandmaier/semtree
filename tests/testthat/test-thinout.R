testthat::test_that(
  "",
  {
    
    create_dummy_tree <- function() {
      tree<-list(caption="root")
      tree$left_child <- list(caption="TERMINAL")
      tree$right_child <- list(caption="TERMINAL")
      class(tree) <- "semtree"
      tree$lr <- 1
      tree$df <- 1
      tree$N <- 10
      return(tree)
    }
    
    create_dummy_root <- function() {
      tree<-list(caption="TERMINAL")
      #tree$left_child <- list(caption="TERMINAL")
      #tree$right_child <- list(caption="TERMINAL")
      class(tree) <- "semtree"
      tree$lr <- 0
      tree$df <- 1
      tree$N <- 10
      return(tree)
    }
    
    create_dummy_forest <- function(k) {
      dat <- data.frame(x=c(1,2,3))
      forest <- list(forest=replicate(k, create_dummy_tree(), simplify=FALSE), forest.data=rep(dat,k))
      class(forest) <- "semforest"
      return(forest)
    }
    
    forest<-create_dummy_forest(4)
    
    f_thin <- thinOut(forest)

    testthat::expect_equal(length(forest$forest), 4)    
    testthat::expect_equal(length(f_thin$forest), 4)
    
    forest$forest[[2]] <- create_dummy_root()
    f_thin <- thinOut(forest)
    
    testthat::expect_equal(length(f_thin$forest),3)
  }
)