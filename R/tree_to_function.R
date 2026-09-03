tree_to_function <- function(tree) {
  
  make_condition <- function(rule) {
    name <- rule$name
    relation <- rule$relation
    value <- rule$value
    
    # Use data[[name]] so column names do not need to be syntactic R names
    lhs <- call("[[", quote(data), name)
    
    # Build expression like data[["x"]] < 5
    call(relation, lhs, value)
  }
  
  make_body <- function(node) {
    is_terminal <- is.null(node$left_child) && is.null(node$right_child)
    
    if (is_terminal) {
      return(node$node_id)
    }
    
    condition <- make_condition(node$rule)
    
    # Rule FALSE -> left child
    # Rule TRUE  -> right child
    as.call(list(
      quote(`if`),
      condition,
      make_body(node$right_child),
      make_body(node$left_child)
    ))
  }
  
  body_expr <- make_body(tree)
  
  f <- function(data) NULL
  body(f) <- body_expr
  f
}

traverse_fast <- function(tree_f, dat) {
result <- vapply(seq_len(nrow(dat)), function(i) {
  tree_f(dat[i, , drop = FALSE])
}, FUN.VALUE = numeric(1))

}


