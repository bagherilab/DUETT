find_ND <- function(objective_values, level = 1, return_indices = F) {
  # this function returns the row indices corresponding to non dominated points in objective_value
  # This function assumes the objective functions are to be minimized
  # columns are objective function values and rows are different points
  
  if (nrow(objective_values) == 1) {
    return(1)
  }
  
  multi_order <- function(matrix_order) {
    n <- ncol(matrix_order)
    Order <- do.call(order, c(data.frame(matrix_order[, 1:n])))
    return(matrix_order[Order, ])
  }
  
  num_col = ncol(objective_values)
  objective_values = cbind(objective_values, 1:nrow(objective_values))
  
  # if all, loop as many rows as there are
  ifelse(level == "all", level_loop <- nrow(objective_values), level_loop <- level)
  
  nondom = list()
  for (n_level in 1:level_loop) {
    
    # if (n_level == 6) {browser()}
    if (nrow(objective_values) == 0) { # all are found
      break
    } else if (nrow(objective_values) == 1) { # only one row screws up multi_order
      nondom = c(nondom, objective_values[, num_col + 1])
    } else {
      ordered_values = multi_order(objective_values)
      nondom = c(nondom, list(ordered_values[which(!duplicated(cummin(ordered_values[,num_col]))), num_col+1]))
    }
    
    # remove current indices
    objective_values = objective_values[-which(objective_values[,num_col+1] %in% nondom[[n_level]]), , drop = F]
  }
  
  if (level == "all") {
    return(nondom)
  } else {
    return(nondom[[level]])
  }
  
}