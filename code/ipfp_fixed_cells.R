# Matt Moehr
# 2018-05-31
#
# Function to obtain IPF estimates of table cells given marginal sums and
# some cell values. Based on mipfp::Ipfp(). Original author: J. Barthelemy.

ipfp_fixed_cells <- function(seed, target.list, target.data, fixed_cells, 
                             print = FALSE, iter = 1000, tol = 1e-10, 
                             tol.margins = 1e-10, na.target = FALSE) {
  # Update an array using the iterative proportional fitting procedure, where
  # some cells of the array are fixed by the user.
  #
  # Author: Matt Moehr
  #  
  # Args:
  #   seed: The initial multi-dimensional array to be updated. Each cell must
  #         be non-negative.
  #   target.list: A list of the target margins provided in target.data. Each
  #                component of the list is an array whose cells indicates
  #                which dimension the corresponding margin relates to.
  #   target.data: A list containing the data of the target margins. Each
  #                component of the list is an array storing a margin.
  #                The list order must follow the one defined in target.list. 
  #                Note that the cells of the arrays must be non-negative, but
  #                can contains NA values.
  #   fixed_cells: A multi-dimensional array with the same dimensions as seed.
  #                If some cells of the table are known before hand they will
  #                be fixed to the values given. Assumed that NAs are the
  #                cells that should be fitted.
  #   print: Verbose parameter: if TRUE prints the current iteration number
  #          and the value of the stopping criterion.
  #   iter: The maximum number of iteration allowed; must be greater than 0.
  #   tol: If the maximum absolute difference between two iteration is lower
  #        than the value specified by tol, then ipfp has reached convergence
  #        (stopping criterion); must be greater than 0.
  #   tol.margins: The tolerance for margins consistency.
  #   na.target: If set to TRUE, allows the targets to have NA cells. In that
  #              case the margins consistency is not checked.
  #
  # Returns: A mipfp object consisting of a list whose elements are
  #   call: A call object in which all the specified arguments are given by
  #         their full names.
  #   method: The selected method for estimation.
  #   stp.crit: The final value of the stopping criterion.
  #   evol.stp.crit: Evolution of the stopping criterion over the iterations.
  #   conv: A boolean indicating whether the algorithm converged to a solution.
  #   error.margins: A list returning, for each margin, the absolute maximum 
  #                  deviation between the target and generated margin.
  
  
  # checking if a seed is provided
  if (is.null(seed) == TRUE) {
    stop('Error: no seed specified!')
  }
  
  # checking if target are provided
  if (is.null(target.data) == TRUE | is.null(target.data) == TRUE) {
    stop('Error: target.data and/or target.data not specified!')
  }
  
  # checking if NA in target cells if na.target is set to FALSE
  if (is.na(min(sapply(target.data, min))) == TRUE & na.target == FALSE)  {
    stop('Error: NA values present in the margins - use na.target = TRUE!')
  }
  
  # checking if NA in seed
  if (is.na(min(seed)) == TRUE) {
    stop('Error: NA values present in the seed!')
  }
  
  # checking non negativity condition for the seed and the target
  if (min(sapply(target.data, min), na.rm = na.target) < 0 | min(seed) < 0) {
    stop('Error: Target and Seed cells must be non-negative!')    
  }  
  
  if ( is.null(fixed_cells) == TRUE ) {
    stop('Error: no fixed cells specified. If you want to run IPF with all of the cells free to change, use mipfp::Ipfp() instead.')
  }
  
  # checking if fixed_cells and seed are same dimensions
  if ( nrow(seed) != nrow(fixed_cells) |
       ncol(seed) != ncol(fixed_cells) ) {
    stop('Error: dimensions of seed and fixed_cells are not the same.')
  }
  
  # checking the strict positiviy of tol and iter
  if (iter < 1 | tol <= 0.0) {
    stop('Error: tol and iter must be strictly positive!')
  }
  
  # checking if NA allowed and requesting the covariance matrices
  if (na.target == TRUE) { #& compute.cov == TRUE) {
    warning('Missing values allowed in the target margins.
            Computation of the covariance matrices set to FALSE!')
    compute.cov <- FALSE
  }
  
  # checking the margins consistency if no missing values in the targets
  error.margins <- TRUE  
  if (na.target == FALSE) {
    if (length(target.data) > 1) {
      for (m in 2:length(target.data)) {      
        if (abs(sum(target.data[[m-1]]) - sum(target.data[[m]])) > 
            tol.margins) {
          error.margins <- FALSE
          warning('Target not consistents - shifting to probabilities!
                  Check input data!\n')
          break
        }      
      }
      }
    } else {
      if (print == TRUE) {
        cat('NOTE: Missing values present in target cells. ')
        cat('Margins consistency not checked!\n')  
      }        
    }
  
  # if margins are not consistent, shifting from frequencies to probabilities
  if (error.margins == FALSE) {
    seed <- seed / sum(seed)
    for (m in 1:length(target.data)) {
      target.data[[m]] <- target.data[[m]] / sum(target.data[[m]])
    }
  }
  
  if (print == TRUE & error.margins == TRUE & na.target == FALSE) {
    cat('Margins consistency checked!\n')
  }
  
  # initial value is the seed
  result <- ifelse(is.na(fixed_cells), seed, fixed_cells)
  
  converged <- FALSE
  tmp.evol.stp.crit <- vector(mode="numeric", length = iter)
  
  ## create a marginal sum of the fixed cells
  temp.fixed = list();
  for ( j in 1:length(target.list) ) {
    # we only need to calc this once before starting iterations
    # we subtract out any fixed cells
    j_sum <- apply(fixed_cells, target.list[[j]], sum, na.rm = TRUE)
    temp.fixed[[j]] <- j_sum
  }
  
  # ipfp iterations
  for (i in 1:iter) {
    
    if (print == TRUE) {
      cat('... ITER', i, '\n')
    } 
    
    # saving previous iteration result (for testing convergence)
    result.temp <- result
    
    # loop over the constraints (ie the dimensions)
    # if you have a "regular" square table you will have 2 constraints
    for (j in 1:length(target.list)) {
      # ... extracting current margins      
      
      ## this adds up the units that we have so far attributed to cells
      unfixed_cells <- ifelse(is.na(fixed_cells),
                              result,
                              0
                              )
      
      if (print == TRUE) {
        cat('       top of unfixed cells:\n')
        print(unfixed_cells)
        cat('\n')
      }
      
      temp.sum <- apply(unfixed_cells, target.list[[j]], sum)
      
      # ... computation of the update factor, taking care of 0 and NA cells 
      ## TODO there is an error in the formulas, or just an edge case?
      ## when there are very few open cells it seems like they get stuck on zero
      ## the convergence criterion is met because there is very little change
      ## from iteration to iteration so it just stops and leaves a bunch of 
      ## people yet to be distributed to cells.
      temp.margin <- target.data[[j]] - temp.fixed[[j]]
      update.factor <- ifelse(temp.margin <= 0 | temp.sum <= 0, 
                              runif(1, .99, 1.01),
                              temp.margin / temp.sum
                              )
      if (na.target == TRUE) {
        update.factor[is.na(update.factor)] <- 1;
      }
      
      if (print == TRUE) {
        cat('       temp margin:', temp.margin, '\n')
        cat('       update factor:', update.factor, '\n')
      }
      
      # ... apply the update factor
      result <- base::sweep(result, target.list[[j]], update.factor, FUN = "*")
      
      # reset the fixed cells to their fixed values
      result <- ifelse(is.na(fixed_cells), result, fixed_cells)
    }
    
    # stopping criterion
    stp.crit <- max(abs(result - result.temp))
    tmp.evol.stp.crit[i] <- stp.crit
    if (stp.crit < tol) {
      converged <- TRUE
      if (print == TRUE) {
        cat('       stoping criterion:', stp.crit, '\n')
        cat('Convergence reached after', i, 'iterations!\n')
      } 
      break
    }
    
    if (print == TRUE) {
      cat('       stoping criterion:', stp.crit, '\n')
    }
    
  }
  
  # checking the convergence
  if (converged == FALSE) {
    warning('IPFP did not converged after ', iter, ' iteration(s)! 
            This migh be due to 0 cells in the seed, maximum number 
            of iteration too low or tolerance too small\n')
  }        
  
  # computing final max difference between generated and target margins
  diff.margins <- vector(mode = "numeric", length = length(target.list))
  if (na.target == FALSE) {
    for (j in 1:length(target.list)) {
      diff.margins[j] = max(abs(target.data[[j]] 
                                - apply(result, target.list[[j]], sum)))
      if (is.null(names(dimnames(seed))) == FALSE) {
        names(diff.margins)[j] <- paste(names(dimnames(seed))[target.list[[j]]],
                                        collapse = ".")
      }
    }
  }
  
  # storing the evolution of the stopping criterion
  evol.stp.crit <- tmp.evol.stp.crit[1:i]
  
  # computing the proportions
  result.prop <- result / sum(result)
  
  # gathering the results in a list
  results.list <- list("x.hat" = result, "p.hat" = result.prop, 
                       "conv" = converged, "error.margins" = diff.margins, 
                       "evol.stp.crit" = evol.stp.crit)
  
  # adding the method applied
  results.list$method <- "ipfp"
  
  # adding the calling expression
  results.list$call <- match.call()
  
  # updating the class attribute
  class(results.list) <- c("list", "mipfp")      
  
  # returning the result
  return(results.list)
  
  }