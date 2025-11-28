# rounds only up to given number of digits if a number is passed
# @param x Numeric. The number to be rounded
# @param digits integer indicating the number of decimpal places to be used
# @keywords internal
saferound <- function(x, digits = 0)
{
  if (is.numeric(x)) {
    return(round(x, digits))
  } else {
    return(x)
  }
}


treeToTable <- function(tree, colDataList=list(), result=list())
{
  
  colDataListLeft <- colDataList
  colDataListRight <- colDataList
  
  colDataListLeft[[length(colDataList)+1]] <- list(tree$rule, tree$left_child$edge_label)
  colDataListRight[[length(colDataList)+1]] <- list(tree$rule, tree$right_child$edge_label)
  
  
  if (tree$caption != "TERMINAL") {
    result <- treeToTable(tree$left_child,colDataListLeft, result)
    result <- treeToTable(tree$right_child,colDataListRight, result)  
  } else {
    result[[length(result)+1]] <- list(colDataList,tree$params)
  }
  
  return(result);
}



#' Tabular Representation of a SEM Tree
#' 
#' Converts a tree into a tabular representation. This may be useful as a
#' textual representation for use in manuscripts.
#' 
#' 
#' @param tree A SEM Tree object.
#' @param added.param.cols String. Add extra columns with parameter estimates. Pass a vector with the names of the parameters that should be rendered in the table.
#' @param round.param Integer. Number of digits to round parameter estimates. Default is rounding to three digits after the decimal point.
#' @author Andreas M. Brandmaier
#' @references
#' 
#' Brandmaier, A. M., Ram, N., Wagner, G. G., & Gerstorf, D. (in press).
#' Terminal decline in well-being: The role of multi-indicator constellations
#' of physical health and psychosocial correlates. \emph{Developmental
#' Psychology}.
#' @export
toTable <- function(tree, added.param.cols=NULL, round.param=3) {
  
  
  
  # collect all data
  alls <- c()
  rowdata <- treeToTable(tree)
  for (i in 1:length(rowdata)) {
    myrow <- rowdata[[i]][[1]]
    for (j in 1:length(myrow)) {
      myitem <- myrow[[j]]
      alls[length(alls)+1] <- myitem[[1]]$name
    }
  }
  alls <- unique(alls)
  # collect all variables
  
  
  # create table
  covariate.names <- getCovariatesFromTree(tree)
  
  # default is to display all parameters
  if (is.null(added.param.cols)) {
    added.param.cols <- names(tree$params)
  }
  
  # all column names for the table to be generated (covariate names and parameter names)
  all.names <- c(covariate.names, added.param.cols)
  
  str.matrix <- matrix(NA, nrow = length(rowdata),ncol=length(all.names))
  
  # convert to a data frame to avoid coercion to string
  str.matrix <- data.frame(str.matrix)
  
  colnames(str.matrix) <- all.names
  
  for (i in 1:length(rowdata)) {
    myrow <- rowdata[[i]][[1]]
    for (j in 1:length(myrow)) {
      myitem <- myrow[[j]]
      varid <- which(covariate.names==myitem[[1]]$name)
      state <- myitem[[2]]
      if (state==1) { # state encodes whether TRUE OR FALSE
        if (myitem[[1]]$relation==">=") {
          rule <- paste(">=",saferound(myitem[[1]]$value,2))
        } else if (myitem[[1]]$relation=="<") {
          rule <- paste("<",saferound(myitem[[1]]$value,2))
        } else if (myitem[[1]]$relation=="%in%") {
          rule <- paste(myitem[[1]]$value, collapse=" or ")
        } else if (myitem[[1]]$relation==">") {
          rule <- paste(">",saferound(myitem[[1]]$value,2))
        } else {
          rule <- "UNKNOWN"
        }
      } else {
        
        # invert rule
        rule <- ""
        if (myitem[[1]]$relation==">=") {
          rule <- paste("<",saferound(myitem[[1]]$value,2))
        } else if (myitem[[1]]$relation=="<") {
          rule <- paste(">=",saferound(myitem[[1]]$value,2))
        } else if (myitem[[1]]$relation==">") {
          rule <- paste("<=",saferound(myitem[[1]]$value,2))
        } else if (myitem[[1]]$relation=="%in%") {
          rule <- paste("not (",paste(myitem[[1]]$value,collapse=" or "),")")
        } else 
        { rule <- "UNKNOWN" } 
        
      }
      if (is.na(str.matrix[i, varid])) {
        str.matrix[i,varid] <- rule
      } else {
        str.matrix[i,varid] <- paste(str.matrix[i,varid]," & ", rule)
      }
    }
    # fill variable names
    if (!is.null(added.param.cols)) {
      for (j in 1:length(added.param.cols)) {
        colid <- which(colnames(str.matrix)==added.param.cols[j])
        param <- rowdata[[i]][[2]][which(names(rowdata[[i]][[2]])==added.param.cols[j])]
        if (!is.null(round.param)) {
          param <- saferound(param, round.param)
        }
        str.matrix[i,colid] <- param 
      }
    }
    
  }
  
  ## prune empty columns?
  is.col.empty <- which(apply(str.matrix,2,function(x){all(is.na(x))}))
  if (length(is.col.empty)>0) {
    str.matrix <- str.matrix[,-is.col.empty]
  }
  
  # sort columns according to number of elements
  sortby <- apply(str.matrix,2,function(x){sum(!is.na(x))})
  if (!is.null(added.param.cols)) {
    remids <- (dim(str.matrix)[2]-length(added.param.cols)+1):(dim(str.matrix)[2])
    sortby[remids] <- sortby[remids]-999999 # bad style ^^ #TODO
  }
  sort.ix <- sort(sortby,index.return=TRUE,decreasing = TRUE)$ix
  str.matrix <- str.matrix[, sort.ix]
  
  
  str.matrix[is.na(str.matrix)]<-""
  
  
  return(str.matrix)
  
}
