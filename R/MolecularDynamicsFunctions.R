#Target Functions.
#' Local Ripley's Target.
#'
#' Calculates the localised, linearised Ripley's K function for each point.
#'
#' @param points A matrix or data frame with two columns: x and y coordinates of each point.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param rmax Numeric value, the maximum radius for each K function to be calculated at.
#' @param nrval Numeric value, the number of equally-spaced radial values for each K function to be calculated over.
#' @return A list of the local K functions, one for each row (point) in points.
#' @export
ripley_target <- function(points, ROI, rmax = 200, nrval = 20){
  #Calculate r values.
  R <- (1:nrval) * (rmax / nrval)
  #Calculate distance matrix.
  distance_matrix <- toroidal_dist(points, ROI)
  #Get volume and number of points.
  volume <- prod(ROI)
  number_of_points <- nrow(points)
  lambda <- volume/(number_of_points * (number_of_points - 1))
  
  #Calculate local K for each point.
  Ks <- lambda * do.call(cbind, lapply(R, function(r){
    return(rowSums(distance_matrix <= r))
  }))
  return(lapply(1:number_of_points, function(i){
    return(cbind(R, Ks[i,]))
  }))
}

#' Euclidean Local Ripley's Target
#'
#' Calculates the localised, linearised Ripley's K function for each point using Euclidean distance, slightly faster.
#'
#' @param points A matrix or data frame with two columns: x and y coordinates of each point.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param rmax Numeric value, the maximum radius for each K function to be calculated at.
#' @param nrval Numeric value, the number of equally-spaced radial values for each K function to be calculated over.
#' @return A list of the local K functions, one for each row (point) in points.
#' @export
ripley_target_euclidean <- function(points, ROI, rmax = 200, nrval = 20){
  #Calculate r values.
  R <- (1:nrval) * (rmax / nrval)
  #Calculate distance matrix.
  distance_matrix <- as.matrix(Rfast::Dist(points))
  #Get volume and number of points.
  volume <- prod(ROI)
  number_of_points <- nrow(points)
  lambda <- volume/number_of_points
  
  #Calculate local K for each point.
  Ks <- matrix(0, nrow = nrval, ncol = number_of_points)
  for(i in 1:length(R)){
    Ks[i,] <- colSums(distance_matrix <= R[i]) - 1
  }
  Ks <- lambda * Ks
  targets <- vector("list", number_of_points)
  for(i in 1:number_of_points){
    targets[[i]] <- cbind(R, Ks[,i])
  }
  return(targets)
}

#' Main Euclidean Local Ripley's Target
#'
#' Calculates the localised, linearised Ripley's K function for each point using Euclidean distance, slightly faster. Uses alternative calculation for computational efficiency. Used in main simulator.
#'
#' @param points A matrix or data frame with two columns: x and y coordinates of each point.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param R A vector of radial values for which to calculate the K function over.
#' @param lambda Numeric value, used to weight the K function. Calculated automatically, should not be altered.
#' @param number_of_points Numeric value, the number of data points. Calculated automatically if not given.
#' @return A list of the local K functions, one for each row (point) in points.
#' @export
ripley_target_euclidean_main <- function(points, ROI, R, lambda, number_of_points){
  #Calculate distance matrix.
  distance_matrix <- as.matrix(Rfast::Dist(points))
  
  #Calculate local K for each point.
  n <- nrow(points)
  Ks <- matrix(0, nrow = length(R), ncol = n)
  for(i in 1:length(R)){
    Ks[i,] <- colSums(distance_matrix <= R[i]) - 1
  }
  l <- prod(ROI) / n
  Ks <- l * Ks
  targets <- vector("list", n)
  for(i in 1:n){
    targets[[i]] <- cbind(R, Ks[,i])
  }
  return(targets)
}

#' Main Euclidean Local Ripley's Target for Multiple Populations
#'
#' Calculates the localised, linearised Ripley's K function for each point of a given population compared to all other populations.
#'
#' @param points A matrix or data frame with two columns: x and y coordinates of each point.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param R A vector of radial values for which to calculate the K function over.
#' @param lambda Numeric value, used to weight the K function. Calculated automatically, should not be altered.
#' @param distances A distance matrix between all points.
#' @return A list of the local K functions, one for each row (point) in points.
#' @export
ripley_target_pop <- function(points, ROI, R, lambda, distances){
  #Get K function for each point of each unique point type.
  uniques <- unique(points[,ncol(points)])
  point_targets <- vector("list", length(uniques))
  for(j in 1:length(uniques)){
    u <- uniques[j]
    number_of_points <- sum(points[,ncol(points)] == u)
    #Calculate local K for each point.
    Ks <- matrix(0, nrow = length(R), ncol = number_of_points)
    distance_matrix <- distances[, points[,ncol(points)] == u]
    for(i in 1:length(R)){
      Ks[i,] <- colSums(distance_matrix <= R[i])
    }
    l <- prod(ROI) / number_of_points
    Ks <- l * Ks
    targets <- vector("list", number_of_points)
    for(i in 1:number_of_points){
      targets[[i]] <- cbind(R, Ks[,i])
    }
    point_targets[[j]] <- targets
  }
  return(point_targets)
}

#Aggregate Functions.
#' Default Aggregate.
#'
#' Does nothing, returns targets as list. Typically used for debugging.
#'
#' @param target List of targets, as output by target functions.
#' @return List of targets, as output by target functions.
#' @export
default_aggregate <- function(target){
  return(target)
}

#' Ripley Aggregate.
#'
#' Takes average of second column, converts list of localised Ripley's K functions into global K function.
#'
#' @param target List of targets, as output by target functions.
#' @return Global K function derived from mean of all targets.
#' @export
ripley_aggregate <- function(target){
  K <- Reduce("+", target) / length(target)
  colnames(K) <- c("r", "K")
  return(K)
}

#' Ripley Aggregate for Multiple Populations
#'
#' Takes average of second column for all targets. Used for multiple populations cases.
#'
#' @param target List of targets, as output by target functions.
#' @return List of global K functions, one for each population.
#' @export
ripley_aggregate_pop <- function(targets){
  targets <- lapply(targets, function(target){
    K <- Reduce("+", target) / length(target)
    colnames(K) <- c("r", "K")
    return(K)
  })
  return(targets)
}

#' Ripley H Aggregate.
#'
#' Converts list of localised Ripley's K functions into global H function.
#'
#' @param target List of targets, as output by target functions.
#' @return Global H function over all points.
#' @export
ripley_H_aggregate <- function(target){
  K <- Reduce("+", target) / length(target)
  K[,2] <- sqrt(K[,2]/pi) - K[,1]
  colnames(K) <- c("r", "H")
  return(K)
}

#' Ripley H Aggregate for Multiple Populations
#'
#' Calculates global H function for for all targets. Used for multiple populations cases.
#'
#' @param target List of targets, as output by target functions.
#' @return List of global H functions, one for each population.
#' @export
ripley_H_aggregate_pop <- function(targets){
  targets <- lapply(targets, function(target){
    K <- Reduce("+", target) / length(target)
    K[,2] <- sqrt(K[,2]/pi) - K[,1]
    colnames(K) <- c("r", "H")
    return(K)
  })
  return(targets)
}

#Error Functions.
#' Pointwise Ratiometric Error
#'
#' Calculates average ratiometric error of each point across a 2D curve. Compares each point to a distinct local target.
#'
#' @param target A list of targets, as output by target functions.
#' @param actual A list of actual point statistics, taken points input to target functions.
#' @return A vector of error values, one for each point.
#' @export
pointwise_ratiometric_error <- function(target, actual){
  d <- ncol(actual[[1]])
  n <- nrow(actual[[1]])
  errors <- rep(0, length(actual))
  for(i in 1:length(actual)){
    a <- abs(target[[i]][,d] - actual[[i]][,d])/(target[[i]][,d] + actual[[i]][,d])
    a[is.na(a)] <- 0
    errors[i] <- sum(a)/n
  }
  return(errors)
}

#' Pointwise Ratiometric Error Main.
#'
#' Calculates average ratiometric error of each point across a 2D curve, slightly faster. Compares each point to a distinct local target. Used in simulator function.
#'
#' @param target A list of targets, as output by target functions.
#' @param actual A list of actual point statistics, taken points input to target functions.
#' @param n Number of points, the length of actual.
#' @return  A vector of error values, one for each point.
#' @export
pointwise_ratiometric_error_main <- function(target, actual, n){
  errors <- rep(0, length(actual))
  for(i in 1:length(actual)){
    a <- abs(target[[i]][,2] - actual[[i]][,2])/(target[[i]][,2] + actual[[i]][,2])
    a[is.na(a)] <- 0
    errors[i] <- sum(a)/n
  }
  return(errors)
}

#' Pointwise Ratiometric Error Main Global.
#'
#' Calculates average ratiometric error of each point across a 2D curve. Used in simulator function.
#'
#' @param target A target, aggregated from output of a target function.
#' @param actual A list of actual point statistics, taken points input to target functions.
#' @param n Number of points, the length of actual.
#' @param num Placeholder argument, currently unused.
#' @return A vector of error values, one for each point.
#' @export
pointwise_ratiometric_error_main_global <- function(target, actual, n, num){
  R <- target[,1]
  errors <- rep(0, length(actual))
  for(i in 1:length(actual)){
    a <- actual[[i]][,2]
    e <- rep(0, length(a))
    csr <- pi * R ** 2
    und <- a < csr
    e[und] <- 1
    b <- abs(target[,2] - a)/(target[,2] + a)
    b[is.na(b)] <- 0
    ove <- a > target[,2]
    e[ove] <- b[ove]
    d <- 1 - (a - csr) / (target[,2] - csr)
    d[is.na(d)] <- 0
    e[!und & !ove] <- d[!und & !ove]
    errors[i] <- sum(e)/n
  }
  return(errors)
}

#' Pointwise Ratiometric Error Multiple Populations
#'
#' Calculates average ratiometric error of each point across a 2D curve for each point population.
#'
#' @param targets A list of targets, as output by target functions. One for each population.
#' @param actuals A list of actual point statistics, taken points input to target functions.
#' @param n Number of points, the length of actual.
#' @return A vector of error values, one for each point.
#' @export
pointwise_ratiometric_error_pop <- function(targets, actuals, n){
  errors <- lapply(1:length(targets), function(j){
    target <- targets[[j]]
    actual <- actuals[[j]]
    error <- rep(0, length(actual))
    for(i in 1:length(actual)){
      a <- abs(target[,2] - actual[[i]][,2])/(target[,2] + actual[[i]][,2])
      a[is.na(a)] <- 0
      error[i] <- sum(a)/n
    }
    return(error)
  })
  return(errors)
}

#' Preferential Ratiometric Error
#'
#' Calculates the ratiometric error of each point across multiple 2D curves and finds the error which is smallest.
#'
#' @param targets A list of targets, as output by target functions.
#' @param actual A list of actual point statistics, taken points input to target functions.
#' @param n Number of points, the length of actual.
#' @return A vector of error values, one for each point.
#' @export
preferential_ratiometric_error_main <- function(targets, actual, n){
  overall_errors <- vector("list", length(targets))
  for(j in 1:length(targets)){
    target <- targets[[j]]
    R <- target[,1]
    errors <- rep(0, length(actual))
    for(i in 1:length(actual)){
      a <- actual[[i]][,2]
      e <- rep(0, length(a))
      csr <- pi * R ** 2
      und <- a < csr
      e[und] <- 1
      b <- abs(target[,2] - a)/(target[,2] + a)
      b[is.na(b)] <- 0
      ove <- a > target[,2]
      e[ove] <- b[ove]
      d <- 1 - (a - csr) / (target[,2] - csr)
      d[is.na(d)] <- 0
      e[!und & !ove] <- d[!und & !ove]
      errors[i] <- sum(e)/n
    }
    overall_errors[[j]] <- errors
  }
  #Get minimums for each column.
  return(Rfast::colMins(do.call(rbind, overall_errors), value = TRUE))
}

#' Preferential Ratiometric Error All
#'
#' Calculates the ratiometric error of each point across multiple 2D curves. Returns all errors for tracking changes.
#'
#' @param targets A list of targets, as output by target functions.
#' @param actual A list of actual point statistics, taken points input to target functions.
#' @param n Number of points, the length of actual.
#' @return A vector of error values, one for each point.
#' @export
preferential_ratiometric_error_all <- function(targets, actual, n){
  overall_errors <- vector("list", length(targets))
  for(j in 1:length(targets)){
    target <- targets[[j]]
    R <- target[,1]
    errors <- rep(0, length(actual))
    for(i in 1:length(actual)){
      a <- actual[[i]][,2]
      e <- rep(0, length(a))
      csr <- pi * R ** 2
      und <- a < csr
      e[und] <- 1
      b <- abs(target[,2] - a)/(target[,2] + a)
      b[is.na(b)] <- 0
      ove <- a > target[,2]
      e[ove] <- b[ove]
      d <- 1 - (a - csr) / (target[,2] - csr)
      d[is.na(d)] <- 0
      e[!und & !ove] <- d[!und & !ove]
      errors[i] <- sum(e)/n
    }
    overall_errors[[j]] <- errors
  }
  #Get minimums for each column.
  return(do.call(cbind, overall_errors))
}

#' Preferential Ratiometric Error All from Zero
#'
#' Calculates the ratiometric error of each point across multiple 2D curves. Returns all errors for tracking changes. Baseline at 0, useful for CSR targets.
#'
#' @param targets A list of targets, as output by target functions.
#' @param actual A list of actual point statistics, taken points input to target functions.
#' @param n Number of points, the length of actual.
#' @return A vector of error values, one for each point.
#' @export
preferential_ratiometric_error_all_from_zero <- function(targets, actual, n){
  overall_errors <- vector("list", length(targets))
  for(j in 1:length(targets)){
    target <- targets[[j]]
    errors <- rep(0, length(actual))
    for(i in 1:length(actual)){
      a <- abs(target[,2] - actual[[i]][,2])/(target[,2] + actual[[i]][,2])
      a[is.na(a)] <- 0
      errors[i] <- sum(a)/n
    }
    overall_errors[[j]] <- errors
  }
  #Get minimums for each column.
  return(do.call(cbind, overall_errors))
}

#Velocity Functions.
#' Default Velocity.
#'
#' No movement, returns 0.
#'
#' @param D_min Minimum step size.
#' @param D_max Maximum step size.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param error A vector of error values, as output by error function.
#' @return Offsets for each point (in this case, 0).
#' @export
default_velocity_2D <- function(D_min, D_max, ROI, error){
  return(matrix(0, nrow = length(error), ncol = 2))
}

#Step Velocity - velocity is either minimum or maximum.
#' Step Velocity.
#'
#' Determines velocity at either minimum or maximum, depending on whether error surpasses a given threshold.
#'
#' @param D_min Minimum step size.
#' @param D_max Maximum step size.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param error A vector of error values, as output by error function.
#' @param threshold A numeric value. If a point's error is above the threshold, it will be offset with step size D_max, otherwise it will be offset with step size D_min.
#' @return Offsets for each point.
#' @export
step_velocity <- function(D_min, D_max, ROI, error, threshold = 0.5){
  #Apply linearly decreasing step size dependent on error.
  angle <- runif(n = length(error), min = 0, max = 2 * pi)
  step <- (D_max - D_min) * (error > threshold) + D_min
  return(cbind(step * cos(angle), step * sin(angle)))
}

#' Linear Velocity
#'
#' Calculates velocity, which scales linearly with error from minimum to maximum.
#'
#' @param D_min Minimum step size.
#' @param D_max Maximum step size.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param error A vector of error values, as output by error function.
#' @return Offsets for each point.
#' @export
linear_velocity <- function(D_min, D_max, ROI, error){
  #Apply linearly decreasing step size dependent on error.
  angle <- runif(n = length(error), min = 0, max = 2 * pi)
  step <- (D_max - D_min) * error + D_min
  return(cbind(step * cos(angle), step * sin(angle)))
}

#' Quadratic Velocity
#'
#' Calculates velocity, which scales quadratically with error from minimum to maximum.
#'
#' @param D_min Minimum step size.
#' @param D_max Maximum step size.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param error A vector of error values, as output by error function.
#' @return Offsets for each point.
#' @export
quadratic_velocity <- function(D_min, D_max, ROI, error){
  #Apply quadratically decreasing step size dependent on error.
  angle <- runif(n = length(error), min = 0, max = 2 * pi)
  step <- (D_max - D_min) * (error ** 2) + D_min
  return(cbind(step * cos(angle), step * sin(angle)))
}

#' Polynomial Velocity
#'
#' Calculates velocity, which scales with error, by some polynomial of factor n, from minimum to maximum.
#'
#' @param D_min Minimum step size.
#' @param D_max Maximum step size.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param error A vector of error values, as output by error function.
#' @param n Factor of polynomial used in velocity calculation.
#' @return Offsets for each point.
#' @export
polynomial_velocity <- function(D_min, D_max, ROI, error, n = 2){
  #Apply polynomially decreasing step size dependent on error.
  angle <- runif(n = length(error), min = 0, max = 2 * pi)
  step <- (D_max - D_min) * (error ** n) + D_min
  return(cbind(step * cos(angle), step * sin(angle)))
}

#' Polynomial Velocity Multiple Populations
#'
#' Calculates velocity, which scales with error, by some polynomial of factor n, from minimum to maximum. Used for multiple populations.
#'
#' @param D_min Minimum step size.
#' @param D_max Maximum step size.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param error A vector of error values, as output by error function.
#' @param n Factor of polynomial used in velocity calculation.
#' @return Offsets for each point.
#' @export
polynomial_velocity_pop <- function(clouds, D_min, D_max, ROI, errors, n = 2){
  #Apply polynomially decreasing step size dependent on error.
  clouds <- lapply(1:length(clouds), function(j){
    current_cloud <- clouds[[j]]
    error <- errors[[j]]
    angle <- runif(n = length(error), min = 0, max = 2 * pi)
    step <- (D_max - D_min) * (error ** n) + D_min
    current_cloud[,1:2] <- current_cloud[,1:2] + cbind(step * cos(angle), step * sin(angle))
    return(current_cloud)
  })
  return(clouds)
}

#' Basic Exponential Velocity Multiple Populations
#'
#' Calculates velocity, which scales exponentially with error from minimum to maximum.
#'
#' @param D_min Minimum step size.
#' @param D_max Maximum step size.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param error A vector of error values, as output by error function.
#' @return Offsets for each point.
#' @export
basic_exponential_velocity <- function(D_min, D_max, ROI, error){
  #Apply exponentially decreasing step size dependent on error.
  angle <- runif(n = length(error), min = 0, max = 2 * pi)
  step <- (D_max - D_min) * exp(1 - 1/(error ** 2)) + D_min
  return(cbind(step * cos(angle), step * sin(angle)))
}

#' Exponential Velocity
#'
#' Calculates velocity, which scales with error, both exponentially and by some polynomial of factor n, from minimum to maximum.
#'
#' @param D_min Minimum step size.
#' @param D_max Maximum step size.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param error A vector of error values, as output by error function.
#' @param n Factor of polynomial used in velocity calculation.
#' @return Offsets for each point.
#' @export
exponential_velocity <- function(D_min, D_max, ROI, error, n = 2){
  #Apply exponentially decreasing step size dependent on error.
  angle <- runif(n = length(error), min = 0, max = 2 * pi)
  step <- (D_max - D_min) * (exp(error ** n) - 1)/(exp(1) - 1) + D_min
  return(cbind(step * cos(angle), step * sin(angle)))
}

#Correction Functions.
#' Default Correction
#'
#' Does not correct points which escape boundary.
#'
#' @param points A matrix or data frame with two columns: x and y coordinates of each point.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @return New point coordinates after correction.
#' @export
default_correction <- function(points, ROI){
  return(points)
}

#' Toroidal Correction
#'
#' Wraps points around to other side.
#'
#' @param points A matrix or data frame with two columns: x and y coordinates of each point.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @return New point coordinates after correction.
#' @export
toroidal_correction <- function(points, ROI){
  for(i in 1:length(ROI)){
    points[points[,i] < 0,i] <- points[points[,i] < 0,i] + ROI[i]
    points[points[,i] > ROI[i],i] <- points[points[,i] > ROI[i],i] - ROI[i]
  }
  return(points)
}

#' Reflection Correction
#'
#' Causes points to rebound off of sides of ROI.
#'
#' @param points A matrix or data frame with two columns: x and y coordinates of each point.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @return New point coordinates after correction.
#' @export
reflection_correction <- function(points, ROI){
  for(i in 1:length(ROI)){
    points[points[,i] < 0,i] <- -points[points[,i] < 0,i]
    points[points[,i] > ROI[i],i] <- 2 * ROI[i] - points[points[,i] > ROI[i],i]
  }
  return(points)
}

#' Reflection Correction Multiple Populations.
#'
#' Causes points to rebound off of sides of ROI. Used in multiple populations cases.
#'
#' @param clouds A list of points data, where each data set is given as a matrix or data frame with two columns: x and y coordinates of each point.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @return New point coordinates after correction.
#' @export
reflection_correction_pop <- function(clouds, ROI){
  clouds <- lapply(1:length(clouds), function(j){
    points <- clouds[[j]]
    for(i in 1:length(ROI)){
      points[points[,i] < 0,i] <- -points[points[,i] < 0,i]
      points[points[,i] > ROI[i],i] <- 2 * ROI[i] - points[points[,i] > ROI[i],i]
    }
    return(points)
  })
  return(clouds)
}

#Simulator functions.
#' Global Ripley's Simulator
#'
#' Iteratively fits each point to the global Ripley's K function through Markov Chain Monte Carlo simulation.
#'
#' @param D_min Minimum step size.
#' @param D_max Maximum step size.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param times Number of time steps or iterations of the simulation.
#' @param point_cloud A matrix or data frame with two columns: x and y coordinates of each point. Used as a target distribution from which spatial statistics are drawn.
#' @param rmax Numeric value, the maximum radius for each K function to be calculated at.
#' @param nrval Numeric value, the number of equally-spaced radial values for each K function to be calculated over.
#' @param dimensions Dimensions of data set.
#' @param target Manual input for target function. Can be left as NULL as long as a target point cloud is provided. Length of each target must be equal to nrval.
#' @param number_of_points Number of points to use in simulation.
#' @param initial_distribution The initial frame of the simulation, used to specify pre-determined spatial organisation. If left NULL, a uniformly random distribution will be used.
#' @param output_target Boolean, determines whether spatial statistics will be output alongside point cloud at each frame. Useful for tracking changes in spatial statistics.
#' @return A molecular dynamics simulation. This takes the form of a list, containing a point cloud for each time frame. Will also contain spatial statistics at each time frame if output_target is TRUE.
#' @export
globalripleysim <- function(D_min = 0, D_max = 50, ROI = 1000,  times = 1000, point_cloud = NULL, rmax = 200, nrval = 20,
                            dimensions = 2, target = NULL, number_of_points = NULL, initial_distribution = NULL, output_target = TRUE){
  #Resolve discrepancies.
  #Get dimensions of ROI.
  if(is.matrix(point_cloud) || is.data.frame(point_cloud)){
    dimensions <- ncol(point_cloud)
    number_of_points <- nrow(point_cloud)
  }
  if(is.null(number_of_points)){
    stop("You must specify how many molecules to include in the simulation.")
  }
  if(length(ROI) > 1){
    dimensions <- length(ROI)
  } else {
    ROI <- rep(ROI, dimensions)
  }
  
  #Get R values.
  R <- (1:nrval) * (rmax / nrval)
  
  #Get volume.
  volume <- prod(ROI)
  
  #Check point cloud is given, otherwise use target.
  if(is.matrix(point_cloud) || is.data.frame(point_cloud)){
    lambda <- volume/number_of_points
    target <- ripley_aggregate(ripley_target_euclidean_main(point_cloud, ROI, R, lambda, number_of_points))
  } else if(is.null(target) || is.null(number_of_points)){
    stop("You must provide a target point cloud or define the target and number of points manually.")
  } else {
    lambda <- volume/number_of_points
  }
  
  #Get initial distribution.
  if(is.null(initial_distribution)){
    initial_distribution <- matrix(do.call(c, lapply(ROI, function(i){
      return(runif(number_of_points, min = 0, max = i))
    })), ncol = dimensions)
  }
  current_cloud <- initial_distribution
  number_of_points <- nrow(current_cloud)
  
  #Repeat for all times.
  frames <- vector("list", times)
  if(output_target){
    targets <- vector("list", times)
    for(time in 1:times){
      current_target <- ripley_target_euclidean_main(current_cloud, ROI,  R, lambda, number_of_points)
      current_cloud <- reflection_correction(current_cloud + polynomial_velocity(D_min, D_max, ROI, pointwise_ratiometric_error_main_global(target, current_target, nrval, number_of_points)), ROI)
      frames[[time]] <- current_cloud
      targets[[time]] <- ripley_H_aggregate(current_target)
    }
    #Return frames.
    return(list(frames, targets))
  } else {
    for(time in 1:times){
      current_cloud <- reflection_correction(current_cloud + polynomial_velocity(D_min, D_max, ROI, pointwise_ratiometric_error_main_global(target, ripley_aggregate(ripley_target_euclidean_main(current_cloud, ROI,  R, lambda, number_of_points), nrval), nrval, number_of_points)), ROI)
      frames[[time]] <- current_cloud
    }
    #Return frames.
    return(frames)
  }
}

#' Ripley's Simulator with Distinct Time Points
#'
#' Iteratively fits each point to the global Ripley's K function, target function changes at distinct time points.
#'
#' @param D_min Minimum step size.
#' @param D_max Maximum step size.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param time_points A vector of time points in the simulation. Each time point represents a change in target function. The final value in this vector is the total simulation time.
#' @param point_cloud A matrix or data frame with two columns: x and y coordinates of each point. Used as a target distribution from which spatial statistics are drawn.
#' @param rmax Numeric value, the maximum radius for each K function to be calculated at.
#' @param nrval Numeric value, the number of equally-spaced radial values for each K function to be calculated over.
#' @param dimensions Dimensions of data set.
#' @param targets Manual input for target function. Can be left as NULL as long as a target point cloud is provided. Length of each target must be equal to nrval.
#' @param number_of_points Number of points to use in simulation.
#' @param initial_distribution The initial frame of the simulation, used to specify pre-determined spatial organisation. If left NULL, a uniformly random distribution will be used.
#' @param output_target Boolean, determines whether spatial statistics will be output alongside point cloud at each frame. Useful for tracking changes in spatial statistics.
#' @return A molecular dynamics simulation. This takes the form of a list, containing a point cloud for each time frame. Will also contain spatial statistics at each time frame if output_target is TRUE.
#' @export
distripleysim <- function(D_min = 0, D_max = 50, ROI = 1000,  time_points = 1000, point_clouds = NULL, rmax = 200, nrval = 20,
                          dimensions = 2, targets = NULL, number_of_points = NULL, initial_distribution = NULL){
  #Get initial distribution.
  if(is.null(number_of_points)){
    number_of_points <- nrow(point_clouds[[1]])
  }
  if(is.null(initial_distribution)){
    initial_distribution <- matrix(do.call(c, lapply(c(ROI, ROI), function(i){
      return(runif(number_of_points, min = 0, max = i))
    })), ncol = dimensions)
  }
  
  #Iterate over all time points.
  sims <- vector("list", length(time_points))
  for(i in 1:length(time_points)){
    #Get times.
    if(i > 1){
      times <- time_points[[i]] - time_points[[i - 1]]
    } else {
      times <- time_points[[i]]
    }
    #Feed to simulator.
    if(!is.null(point_clouds)){
      sims[[i]] <- globalripleysim(D_min, D_max, ROI, times, point_clouds[[i]], rmax, nrval, dimensions, target,
                                   number_of_points, initial_distribution, output_target = TRUE)
    } else if(!is.null(targets)) {
      sims[[i]] <- globalripleysim(D_min, D_max, ROI, times, point_cloud, rmax, nrval, dimensions, targets[[i]],
                                   number_of_points, initial_distribution, output_target = TRUE)
    } else {
      stop("Provide target distributions or functions.")
    }
    #Update initial distribution.
    initial_distribution <- sims[[i]][[1]][[length(sims[[i]][[1]])]]
  }
  #Combine all simulations and output.
  simulation <- list(vector("list", max(time_points)), vector("list", max(time_points)))
  time_points <- c(0, time_points)
  for(i in 1:(length(time_points) - 1)){
    simulation[[1]][(time_points[i] + 1):time_points[i + 1]] <- sims[[i]][[1]]
    simulation[[2]][(time_points[i] + 1):time_points[i + 1]] <- sims[[i]][[2]]
  }
  #Return simulation.
  return(simulation)
}

#' Ripley's Simulator with Simultaneous Targets
#'
#' Iteratively fits each point to one of multiple possible Ripley's functions (depending on which is closest). Induces a perturbation (randomly offsetting each point) at a specified time.
#'
#' @param D_min Minimum step size.
#' @param D_max Maximum step size.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param times Number of time steps or iterations of the simulation.
#' @param point_clouds A list of points data, where each data set is given as a matrix or data frame with two columns: x and y coordinates of each point. Used as target distributions from which spatial statistics are drawn.
#' @param rmax Numeric value, the maximum radius for each K function to be calculated at.
#' @param nrval Numeric value, the number of equally-spaced radial values for each K function to be calculated over.
#' @param dimensions Dimensions of data set.
#' @param targets Manual input for target functions. Can be left as NULL as long as target point clouds are provided. Length of each target must be equal to nrval.
#' @param number_of_points Number of points to use in simulation.
#' @param initial_distribution The initial frame of the simulation, used to specify pre-determined spatial organisation. If left NULL, a uniformly random distribution will be used.
#' @param output_target Boolean, determines whether spatial statistics will be output alongside point cloud at each frame. Useful for tracking changes in spatial statistics.
#' @param perturbation_times The time frame(s) at which the perturbation occurs. Leave as 0 for no perturbation.
#' @param perturbation_strength Size of perturbation offset.
#' @return A molecular dynamics simulation. This takes the form of a list, containing a point cloud for each time frame. Will also contain spatial statistics at each time frame if output_target is TRUE.
#' @export
simripleysim <- function(D_min = 0, D_max = 50, ROI = 1000,  times = 1000, point_clouds = NULL, rmax = 200, nrval = 20,
                         dimensions = 2, targets = NULL, number_of_points = NULL, initial_distribution = NULL, output_target = TRUE,
                         perturbation_times = 0, perturbation_strength = 50){
  #Resolve discrepancies.
  #Check list of point clouds.
  if(is.list(point_clouds)){
    if(length(point_clouds) > 1){
      dimensions <- ncol(point_clouds[[1]])
      for(i in 2:length(point_clouds)){
        if(dimensions != ncol(point_clouds[[i]])){
          stop("Point clouds must have same dimensions.")
        }
      }
    }
  }
  #Get dimensions of ROI.
  if(length(ROI) > 1){
    dimensions <- length(ROI)
  } else {
    ROI <- rep(ROI, dimensions)
  }
  
  #Get R values.
  R <- (1:nrval) * (rmax / nrval)
  
  #Get volume.
  volume <- prod(ROI)
  
  #Create simulation list.
  frames <- vector("list", times)
  
  #Get point targets.
  if(is.null(targets)){
    point_targets <- lapply(point_clouds, function(points){
      number_of_points <- nrow(points)
      lambda <- volume/number_of_points
      return(ripley_aggregate(ripley_target_euclidean_main(points, ROI, R, lambda, number_of_points)))
    })
  } else {
    point_targets <- targets
  }
  
  #Get parameters.
  if(is.null(number_of_points)){
    number_of_points <- nrow(point_clouds[[1]])
  }
  lambda <- volume/number_of_points
  
  #Get initial distribution.
  if(is.null(initial_distribution)){
    initial_distribution <- matrix(do.call(c, lapply(ROI, function(i){
      return(runif(number_of_points, min = 0, max = i))
    })), ncol = dimensions)
  }
  current_cloud <- initial_distribution
  number_of_points <- nrow(current_cloud)
  
  #Repeat for all times.
  if(output_target){
    targets <- vector("list", times)
    for(time in 1:times){
      current_target <- ripley_target_euclidean_main(current_cloud, ROI,  R, lambda, number_of_points)
      current_cloud <- reflection_correction(current_cloud + polynomial_velocity(D_min, D_max, ROI, preferential_ratiometric_error_main(point_targets, current_target, nrval)), ROI)
      frames[[time]] <- current_cloud
      targets[[time]] <- ripley_H_aggregate(current_target)
      if(max(perturbation_times == time) > 0){
        angle <- runif(n = number_of_points, min = 0, max = 2 * pi)
        current_cloud <- reflection_correction(current_cloud + cbind(perturbation_strength * cos(angle), perturbation_strength * sin(angle)), ROI)
      }
    }
    #Return frames.
    return(list(frames, targets))
  } else {
    for(time in 1:times){
      current_cloud <- reflection_correction(current_cloud + polynomial_velocity(D_min, D_max, ROI, preferential_ratiometric_error_main(point_targets, ripley_aggregate(ripley_target_euclidean_main(current_cloud, ROI,  R, lambda, number_of_points), nrval), nrval)), ROI)
      frames[[time]] <- current_cloud
      if(max(perturbation_times == time) > 0){
        angle <- runif(n = number_of_points, min = 0, max = 2 * pi)
        current_cloud <- reflection_correction(current_cloud + cbind(perturbation_strength * cos(angle), perturbation_strength * sin(angle)), ROI)
      }
    }
    #Return frames.
    return(frames)
  }
}

#' Simultaneous Targets with Squeeze Perturbation
#'
#' Iteratively fits each point to one of multiple possible Ripley's functions (depending on which is closest), immobilises points and their neighbours at a given time. Instantaneously induces clusters.
#'
#' @param D_min Minimum step size.
#' @param D_max Maximum step size.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param times Number of time steps or iterations of the simulation.
#' @param point_clouds A list of points data, where each data set is given as a matrix or data frame with two columns: x and y coordinates of each point. Used as target distributions from which spatial statistics are drawn.
#' @param rmax Numeric value, the maximum radius for each K function to be calculated at.
#' @param nrval Numeric value, the number of equally-spaced radial values for each K function to be calculated over.
#' @param dimensions Dimensions of data set.
#' @param targets Manual input for target functions. Can be left as NULL as long as target point clouds are provided. Length of each target must be equal to nrval.
#' @param number_of_points Number of points to use in simulation.
#' @param initial_distribution The initial frame of the simulation, used to specify pre-determined spatial organisation. If left NULL, a uniformly random distribution will be used.
#' @param output_target Boolean, determines whether spatial statistics will be output alongside point cloud at each frame. Useful for tracking changes in spatial statistics.
#' @param perturbation_time The time frame at which the perturbation occurs. Leave as 0 for no perturbation.
#' @param number_cross_linked_clusters Number of clusters to be created.
#' @param number_cross_linked Number of points in each induced cluster.
#' @param cross_linked_cluster_radius Radius of induced clusters.
#' @return A molecular dynamics simulation. This takes the form of a list, containing a point cloud for each time frame. Will also contain spatial statistics at each time frame if output_target is TRUE.
#' @export
squeezeripleysim <- function(D_min = 0, D_max = 50, ROI = 1000,  times = 1000, point_clouds = NULL, rmax = 200, nrval = 20,
                             dimensions = 2, targets = NULL, number_of_points = NULL, initial_distribution = NULL, output_target = TRUE,
                             perturbation_time = 500, number_cross_linked_clusters = 1, number_cross_linked = 10, cross_linked_cluster_radius = 5){
  #Run simultaneous simulator up until perturbation time.
  sim <- simripleysim(D_min, D_max, ROI, perturbation_time, point_clouds, rmax, nrval, dimensions, targets, number_of_points,
                      initial_distribution, output_target, perturbation_times = 0, perturbation_strength = 0)
  
  #Get point density in final frame.
  final <- sim[[1]][[perturbation_time]]
  distances <- as.matrix(Rfast::Dist(final))
  densities <- colSums(distances <= cross_linked_cluster_radius)
  
  #Create cross-linked clusters.
  added <- rep(FALSE, length(densities))
  for(i in 1:number_cross_linked_clusters){
    #Find densest point. If no densest point exists, picked randomly.
    densest <- which.max(densities)
    added[densest] <- TRUE
    maxallowed <- min(number_cross_linked - 1, length(densities) - 1)
    
    #Find closest neighbours.
    neighbours <- order(distances[, densest])[1:maxallowed]
    added[neighbours] <- TRUE
    
    #Move points to within given cross-linked cluster radius.
    r <- cross_linked_cluster_radius * sqrt(runif(maxallowed, 0, 1))
    theta <- runif(maxallowed, 0, 2 * pi)
    final[neighbours,] <- cbind(r * cos(theta) + final[densest, 1], r * sin(theta) + final[densest, 2])
    
    #Remove from circulation.
    densities[added] <- -1
  }
  
  #Continue simulations.
  #Check list of point clouds.
  if(is.list(point_clouds)){
    if(length(point_clouds) > 1){
      dimensions <- ncol(point_clouds[[1]])
      for(i in 2:length(point_clouds)){
        if(dimensions != ncol(point_clouds[[i]])){
          stop("Point clouds must have same dimensions.")
        }
      }
    }
  }
  #Get dimensions of ROI.
  if(length(ROI) > 1){
    dimensions <- length(ROI)
  } else {
    ROI <- rep(ROI, dimensions)
  }
  
  #Get R values.
  R <- (1:nrval) * (rmax / nrval)
  
  #Get volume.
  volume <- prod(ROI)
  number_of_points <- nrow(point_clouds[[1]])
  lambda <- volume/(number_of_points)
  
  #Create simulation list.
  frames <- sim[[1]]
  
  #Get point targets.
  if(is.null(targets)){
    point_targets <- lapply(point_clouds, function(points){
      return(ripley_aggregate(ripley_target_euclidean_main(points, ROI, R, lambda, number_of_points)))
    })
  } else {
    point_targets <- targets
  }
  
  #Get initial distribution.
  current_cloud <- final
  
  #Repeat for all times.
  if(output_target){
    targets <- sim[[2]]
    for(time in (perturbation_time + 1):times){
      current_target <- ripley_target_euclidean_main(current_cloud, ROI,  R, lambda, number_of_points)
      velocity <- polynomial_velocity(D_min, D_max, ROI, preferential_ratiometric_error_main(point_targets, current_target, nrval))
      velocity[added, ] <- c(0,0)
      current_cloud <- reflection_correction(current_cloud + velocity, ROI)
      frames[[time]] <- current_cloud
      targets[[time]] <- ripley_H_aggregate(current_target)
    }
    #Return frames.
    return(list(frames, targets))
  } else {
    for(time in 1:times){
      velocity <- polynomial_velocity(D_min, D_max, ROI, preferential_ratiometric_error_main(point_targets, current_target, nrval))
      velocity[added, ] <- c(0,0)
      current_cloud <- reflection_correction(current_cloud + velocity, ROI)
      frames[[time]] <- current_cloud
    }
    #Return frames.
    return(frames)
  }
}

#' Simultaneous Targets with Squeeze and Release
#'
#' Iteratively fits each point to one of multiple possible Ripley's functions (depending on which is closest), squeezes points together at a given time then releases.
#'
#' @param D_min Minimum step size.
#' @param D_max Maximum step size.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param times Number of time steps or iterations of the simulation.
#' @param point_clouds A list of points data, where each data set is given as a matrix or data frame with two columns: x and y coordinates of each point. Used as target distributions from which spatial statistics are drawn.
#' @param rmax Numeric value, the maximum radius for each K function to be calculated at.
#' @param nrval Numeric value, the number of equally-spaced radial values for each K function to be calculated over.
#' @param dimensions Dimensions of data set.
#' @param targets Manual input for target functions. Can be left as NULL as long as target point clouds are provided. Length of each target must be equal to nrval.
#' @param number_of_points Number of points to use in simulation.
#' @param initial_distribution The initial frame of the simulation, used to specify pre-determined spatial organisation. If left NULL, a uniformly random distribution will be used.
#' @param output_target Boolean, determines whether spatial statistics will be output alongside point cloud at each frame. Useful for tracking changes in spatial statistics.
#' @param perturbation_time The time frame at which the perturbation occurs. Leave as 0 for no perturbation.
#' @param number_cross_linked_clusters Number of clusters to be created.
#' @param number_cross_linked Number of points in each induced cluster.
#' @param cross_linked_cluster_radius Radius of induced clusters.
#' @return A molecular dynamics simulation. This takes the form of a list, containing a point cloud for each time frame. Will also contain spatial statistics at each time frame if output_target is TRUE.
#' @export
squeezereleaseripleysim <- function(D_min = 0, D_max = 50, ROI = 1000,  times = 1000, point_clouds = NULL, rmax = 200, nrval = 20,
                                    dimensions = 2, targets = NULL, number_of_points = NULL, initial_distribution = NULL, output_target = TRUE,
                                    perturbation_time = 500, number_cross_linked_clusters = 1, number_cross_linked = 10, cross_linked_cluster_radius = 5){
  #Run simultaneous simulator up until perturbation time.
  sim <- simripleysim(D_min, D_max, ROI, perturbation_time, point_clouds, rmax, nrval, dimensions, targets, number_of_points,
                      initial_distribution, output_target, perturbation_times = 0, perturbation_strength = 0)
  
  #Get point density in final frame.
  final <- sim[[1]][[perturbation_time]]
  distances <- as.matrix(Rfast::Dist(final))
  densities <- colSums(distances <= cross_linked_cluster_radius)
  
  #Create cross-linked clusters.
  added <- rep(FALSE, length(densities))
  for(i in 1:number_cross_linked_clusters){
    #Find densest point. If no densest point exists, picked randomly.
    densest <- which.max(densities)
    added[densest] <- TRUE
    maxallowed <- min(number_cross_linked - 1, length(densities) - 1)
    
    #Find closest neighbours.
    neighbours <- order(distances[, densest])[1:maxallowed]
    added[neighbours] <- TRUE
    
    #Move points to within given cross-linked cluster radius.
    r <- cross_linked_cluster_radius * sqrt(runif(maxallowed, 0, 1))
    theta <- runif(maxallowed, 0, 2 * pi)
    final[neighbours,] <- cbind(r * cos(theta) + final[densest, 1], r * sin(theta) + final[densest, 2])
    
    #Remove from circulation.
    densities[added] <- -1
  }
  
  #Continue simulations.
  #Check list of point clouds.
  if(is.list(point_clouds)){
    if(length(point_clouds) > 1){
      dimensions <- ncol(point_clouds[[1]])
      for(i in 2:length(point_clouds)){
        if(dimensions != ncol(point_clouds[[i]])){
          stop("Point clouds must have same dimensions.")
        }
      }
    }
  }
  #Get dimensions of ROI.
  if(length(ROI) > 1){
    dimensions <- length(ROI)
  } else {
    ROI <- rep(ROI, dimensions)
  }
  
  #Get R values.
  R <- (1:nrval) * (rmax / nrval)
  
  #Get volume.
  volume <- prod(ROI)
  
  #Create simulation list.
  frames <- sim[[1]]
  
  #Get point targets.
  if(is.null(targets)){
    point_targets <- lapply(point_clouds, function(points){
      number_of_points <- nrow(points)
      lambda <- volume/number_of_points
      return(ripley_aggregate(ripley_target_euclidean_main(points, ROI, R, lambda, number_of_points)))
    })
  } else {
    point_targets <- targets
  }
  
  #Update parameters.
  number_of_points <- nrow(final)
  lambda <- volume/number_of_points
  
  #Get initial distribution.
  current_cloud <- final
  
  #Repeat for all times.
  if(output_target){
    targets <- sim[[2]]
    for(time in (perturbation_time + 1):times){
      current_target <- ripley_target_euclidean_main(current_cloud, ROI,  R, lambda, number_of_points)
      velocity <- polynomial_velocity(D_min, D_max, ROI, preferential_ratiometric_error_main(point_targets, current_target, nrval))
      current_cloud <- reflection_correction(current_cloud + velocity, ROI)
      frames[[time]] <- current_cloud
      targets[[time]] <- ripley_H_aggregate(current_target)
    }
    #Return frames.
    return(list(frames, targets))
  } else {
    for(time in 1:times){
      velocity <- polynomial_velocity(D_min, D_max, ROI, preferential_ratiometric_error_main(point_targets, current_target, nrval))
      current_cloud <- reflection_correction(current_cloud + velocity, ROI)
      frames[[time]] <- current_cloud
    }
    #Return frames.
    return(frames)
  }
}

#' Simultaneous Targets with Speed Perturbation
#'
#' Iteratively fits each point to one of multiple possible Ripley's functions (depending on which is closest), perturbs points and changes the Dmin/Dmax at a given time.
#'
#' @param D_min A vector of minimum step sizes. Will switch to next available size after perturbation.
#' @param D_max A vector of maximum step sizes. Will switch to next available size after perturbation.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param times Number of time steps or iterations of the simulation.
#' @param point_clouds A list of points data, where each data set is given as a matrix or data frame with two columns: x and y coordinates of each point. Used as target distributions from which spatial statistics are drawn.
#' @param rmax Numeric value, the maximum radius for each K function to be calculated at.
#' @param nrval Numeric value, the number of equally-spaced radial values for each K function to be calculated over.
#' @param dimensions Dimensions of data set.
#' @param targets Manual input for target functions. Can be left as NULL as long as target point clouds are provided. Length of each target must be equal to nrval.
#' @param number_of_points Number of points to use in simulation.
#' @param initial_distribution The initial frame of the simulation, used to specify pre-determined spatial organisation. If left NULL, a uniformly random distribution will be used.
#' @param output_target Boolean, determines whether spatial statistics will be output alongside point cloud at each frame. Useful for tracking changes in spatial statistics.
#' @param perturbation_times The time frame(s) at which the perturbation occurs. Set as 0 for no perturbation.
#' @param perturbation_strength Size of perturbation offset.
#' @return A molecular dynamics simulation. This takes the form of a list, containing a point cloud for each time frame. Will also contain spatial statistics at each time frame if output_target is TRUE.
#' @export
speedripleysim <- function(D_min = 0, D_max = 50, ROI = 1000,  times = 1000, point_clouds = NULL, rmax = 200, nrval = 20,
                           dimensions = 2, targets = NULL, number_of_points = NULL, initial_distribution = NULL, output_target = TRUE,
                           perturbation_times = 500, perturbation_strength = 50){
  #Check if Dmin and Dmax are vectors.
  if(length(D_max) > 1){
    if(length(D_min) == 1){
      D_min <- rep(D_min, length(D_max))
    }
  } else if(length(D_min) > 1){
    if(length(D_max) == 1){
      D_max <- rep(D_max, length(D_min))
    }
  }
  
  #Resolve discrepancies.
  #Check list of point clouds.
  if(is.list(point_clouds)){
    if(length(point_clouds) > 1){
      dimensions <- ncol(point_clouds[[1]])
      for(i in 2:length(point_clouds)){
        if(dimensions != ncol(point_clouds[[i]])){
          stop("Point clouds must have same dimensions.")
        }
      }
    }
  }
  #Get dimensions of ROI.
  if(length(ROI) > 1){
    dimensions <- length(ROI)
  } else {
    ROI <- rep(ROI, dimensions)
  }
  
  #Get R values.
  R <- (1:nrval) * (rmax / nrval)
  
  #Get parameters.
  if(is.null(number_of_points)){
    number_of_points <- nrow(point_clouds[[1]])
  }
  volume <- prod(ROI)
  
  #Create simulation list.
  frames <- vector("list", times)
  
  #Get point targets.
  if(is.null(targets)){
    point_targets <- lapply(point_clouds, function(points){
      number_of_points <- nrow(point_clouds[[1]])
      lambda <- volume/number_of_points
      return(ripley_aggregate(ripley_target_euclidean_main(points, ROI, R, lambda, number_of_points)))
    })
  } else {
    point_targets <- targets
  }
  
  #Get initial distribution.
  if(is.null(initial_distribution)){
    initial_distribution <- matrix(do.call(c, lapply(ROI, function(i){
      return(runif(number_of_points, min = 0, max = i))
    })), ncol = dimensions)
  }
  current_cloud <- initial_distribution
  number_of_points <- nrow(current_cloud)
  
  #Iterate over all possible speeds.
  time_points <- c(0, perturbation_times, times)
  targets <- vector("list", times)
  for(i in 1:length(D_max)){
    #Get steps sizes.
    Dmin <- D_min[i]
    Dmax <- D_max[i]
    
    #Run simulation.
    #Repeat for all times.
    if(output_target){
      for(time in (time_points[i] + 1):time_points[i + 1]){
        current_target <- ripley_target_euclidean_main(current_cloud, ROI,  R, lambda, number_of_points)
        current_cloud <- reflection_correction(current_cloud + polynomial_velocity(Dmin, Dmax, ROI, preferential_ratiometric_error_main(point_targets, current_target, nrval)), ROI)
        frames[[time]] <- current_cloud
        targets[[time]] <- ripley_H_aggregate(current_target)
        if(max(perturbation_times == time) > 0){
          angle <- runif(n = number_of_points, min = 0, max = 2 * pi)
          current_cloud <- reflection_correction(current_cloud + cbind(perturbation_strength * cos(angle), perturbation_strength * sin(angle)), ROI)
        }
      }
      
    } else {
      for(time in time_points[i]:time_points[i + 1]){
        current_cloud <- reflection_correction(current_cloud + polynomial_velocity(Dmin, Dmax, ROI, preferential_ratiometric_error_main(point_targets, ripley_aggregate(ripley_target_euclidean_main(current_cloud, ROI,  R, lambda, number_of_points), nrval), nrval)), ROI)
        frames[[time]] <- current_cloud
        if(max(perturbation_times == time) > 0){
          angle <- runif(n = number_of_points, min = 0, max = 2 * pi)
          current_cloud <- reflection_correction(current_cloud + cbind(perturbation_strength * cos(angle), perturbation_strength * sin(angle)), ROI)
        }
      }
    }
    
  }
  #Get output.
  if(output_target){
    #Return frames.
    return(list(frames, targets))
  } else {
    #Return frames.
    return(frames)
  }
}

#' Ripley's Simulator with Multiple Population (Isolated)
#'
#' Iteratively fits each point to the global Ripley's K function of its given population.
#'
#' @param D_min Minimum step size.
#' @param D_max Maximum step size.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param times Number of time steps or iterations of the simulation.
#' @param point_clouds A list of points data, where each data set is given as a matrix or data frame with two columns: x coordinate and y coordinate of each point. Used as target distributions from which spatial statistics are drawn.
#' @param rmax Numeric value, the maximum radius for each K function to be calculated at.
#' @param nrval Numeric value, the number of equally-spaced radial values for each K function to be calculated over.
#' @param dimensions Dimensions of data set.
#' @param initial_distribution The initial frame of the simulation, used to specify pre-determined spatial organisation. If left NULL, a uniformly random distribution will be used.
#' @param output_target Boolean, determines whether spatial statistics will be output alongside point cloud at each frame. Useful for tracking changes in spatial statistics.
#' @return A molecular dynamics simulation. This takes the form of a list, containing a point cloud for each time frame. Will also contain spatial statistics at each time frame if output_target is TRUE.
#' @export
popripleysimisol <- function(D_min = 0, D_max = 50, ROI = 1000,  times = 1000, point_clouds = NULL, rmax = 200, nrval = 20,
                             dimensions = 2, initial_distribution = NULL, output_target = TRUE){
  #Run simulator on each point cloud.
  sims <- lapply(point_clouds, function(points){
    globalripleysim(D_min, D_max, ROI, times, points, rmax, nrval, dimensions, target,
                    number_of_points = NULL, initial_distribution, output_target = TRUE)
  })
  #Combine.
  simulation <- vector("list", 1 + length(sims))
  simulation[[1]] <- lapply(1:times, function(i){
    do.call(rbind, lapply(1:length(sims), function(j){
      return(cbind(sims[[j]][[1]][[i]], j))
    }))
  })
  simulation[2:length(simulation)] <- lapply(1:length(sims), function(i){
    return(sims[[i]][[2]])
  })
  return(simulation)
}

#' Ripley's Simulator with Multiple Populations
#'
#' Iteratively fits each point to the global Ripley's K function regardless of its population.
#'
#' @param D_min Minimum step size.
#' @param D_max Maximum step size.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param times Number of time steps or iterations of the simulation.
#' @param point_cloud A matrix or data frame with three columns: x coordinate, y coordinate, and population number of each point. Used as target distributions from which spatial statistics are drawn.
#' @param rmax Numeric value, the maximum radius for each K function to be calculated at.
#' @param nrval Numeric value, the number of equally-spaced radial values for each K function to be calculated over.
#' @param dimensions Dimensions of data set.
#' @param target Manual input for target function. Can be left as NULL as long as a target point cloud is provided. Length of each target must be equal to nrval.
#' @param initial_distribution The initial frame of the simulation, used to specify pre-determined spatial organisation. If left NULL, a uniformly random distribution will be used.
#' @param output_target Boolean, determines whether spatial statistics will be output alongside point cloud at each frame. Useful for tracking changes in spatial statistics.
#' @return A molecular dynamics simulation. This takes the form of a list, containing a point cloud for each time frame. Will also contain spatial statistics at each time frame if output_target is TRUE.
#' @export
popripleysim <- function(D_min = 0, D_max = 50, ROI = 1000,  times = 1000, point_cloud = NULL, rmax = 200, nrval = 20,
                         dimensions = 2, target = NULL, number_of_points = NULL, initial_distribution = NULL, output_target = TRUE){
  #Resolve discrepancies.
  #Get dimensions of ROI.
  if(is.matrix(point_cloud) || is.data.frame(point_cloud)){
    dimensions <- ncol(point_cloud) - 1
  }
  if(length(ROI) > 1){
    dimensions <- length(ROI)
  } else {
    ROI <- rep(ROI, dimensions)
  }
  
  #Get R values.
  R <- (1:nrval) * (rmax / nrval)
  
  #Get volume.
  volume <- prod(ROI)
  
  #Check point cloud is given, otherwise use target.
  if(is.matrix(point_cloud) || is.data.frame(point_cloud)){
    number_of_points <- nrow(point_cloud)
    lambda <- volume/number_of_points
    distances <- as.matrix(Rfast::Dist(point_cloud[,1:(ncol(point_cloud) - 1)]))
    point_targets <- ripley_aggregate_pop(ripley_target_pop(point_cloud, ROI, R, lambda, distances))
  } else if(is.null(target) || is.null(number_of_points)){
    stop("You must provide a target point cloud or define the target and number of points manually.")
  } else {
    lambda <- volume/number_of_points
  }
  
  #Get initial distribution.
  if(is.null(initial_distribution)){
    initial_distribution <- matrix(do.call(c, lapply(ROI, function(i){
      return(runif(number_of_points, min = 0, max = i))
    })), ncol = dimensions)
    current_cloud <- initial_distribution
    labels <- c()
    uniques <- unique(point_cloud[,ncol(point_cloud)])
    for(i in uniques){
      labels <- c(labels, rep(i, sum(point_cloud[,ncol(point_cloud)] == i)))
    }
    current_cloud <- cbind(current_cloud, labels)
  } else {
    current_cloud <- initial_distribution
    uniques <- unique(initial_distribution[,3])
  }
  
  #Separate point cloud into lists.
  clouds <- vector("list", length(uniques))
  for(i in 1:length(uniques)){
    clouds[[i]] <- current_cloud[current_cloud[,ncol(current_cloud)] == uniques[i],]
  }
  
  #Repeat for all times.
  frames <- vector("list", times)
  if(output_target){
    targets <- vector("list", times)
    for(time in 1:times){
      distances <- as.matrix(Rfast::Dist(current_cloud[,1:(ncol(current_cloud) - 1)]))
      current_target <- ripley_target_pop(current_cloud, ROI,  R, lambda, distances)
      clouds <- reflection_correction_pop(polynomial_velocity_pop(clouds, D_min, D_max, ROI, pointwise_ratiometric_error_pop(point_targets, current_target, nrval)), ROI)
      current_cloud <- do.call(rbind, clouds)
      frames[[time]] <- current_cloud
      targets[[time]] <- ripley_H_aggregate_pop(current_target)
    }
    #Return frames.
    return(list(frames, targets))
  } else {
    for(time in 1:times){
      current_cloud <- reflection_correction(current_cloud + polynomial_velocity(D_min, D_max, ROI, pointwise_ratiometric_error_main_global(target, ripley_aggregate(ripley_target_euclidean_main(current_cloud, ROI,  R, lambda, number_of_points), nrval), nrval, number_of_points)), ROI)
      frames[[time]] <- current_cloud
    }
    #Return frames.
    return(frames)
  }
}

#' Completely Spatially Random Simulator with Point Removal.
#'
#' Simulates CSR distributions moving with fixed random step size and removes points of a certain population at a given rate.
#'
#' @param step_size Fixed step size.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param times Number of time steps or iterations of the simulation.
#' @param populations Number of distinct point populations to consider.
#' @param initial_numbers Initial number of points in each population. Can specify for each population with a vector.
#' @param populations_to_remove A vector of population numbers. Points from these populations will be removed at their corresponding removal rates.
#' @param removal_rates A vector of removal rates, determines what random proportion of each population will be removed in each frame.
#' @param dimensions Dimensions of data set.
#' @return A molecular dynamics simulation. This takes the form of a list, containing a point cloud for each time frame. Will also contain spatial statistics at each time frame if output_target is TRUE.
#' @export
csrwithremoval <- function(step_size = 50, ROI = 1000, times = 100, populations = 3, initial_numbers = 1000, populations_to_remove = 1, removal_rates = 0.1, dimensions = 2){
  #Get ROI.
  if(length(ROI) == 1){
    ROI <- rep(ROI, dimensions)
  }
  
  #Get initial numbers.
  if(length(initial_numbers) == 1){
    initial_numbers <- rep(as.numeric(initial_numbers), populations)
  }
  
  #Get initial numbers.
  if(length(removal_rates) == 1){
    removal_rates <- rep(removal_rates, length(populations_to_remove))
  }
  
  #Get initial distribution.
  initial_distribution <- matrix(do.call(c, lapply(ROI, function(i){
    return(runif(sum(initial_numbers), min = 0, max = i))
  })), ncol = dimensions)
  current_cloud <- initial_distribution
  labels <- c()
  uniques <- 1:populations
  for(i in uniques){
    labels <- c(labels, rep(i, initial_numbers[i]))
  }
  current_cloud <- cbind(current_cloud, labels, 1:nrow(current_cloud))
  
  #Iterate over all times.
  frames <- vector("list", times)
  for(time in 1:times){
    #Move points.
    angle <- runif(n = nrow(current_cloud), min = 0, max = 2 * pi)
    current_cloud[,1:2] <- reflection_correction(current_cloud[,1:2] + cbind(step_size * cos(angle), step_size * sin(angle)), ROI)
    frames[[time]] <- current_cloud
    current_cloud[,4] <- 1:nrow(current_cloud)
    #Remove points.
    for(i in 1:length(populations_to_remove)){
      pts <- current_cloud[,3] == populations_to_remove[i]
      if(sum(pts) > 0){
        pts <- (1:nrow(current_cloud))[pts]
        pts <- pts[runif(length(pts), 0, 1) < removal_rates[i]]
        if(length(pts) > 0){
          current_cloud <- current_cloud[-pts, ]
        }
      }
    }
  }
  #Return frames.
  return(frames)
}

#Static Simulation Functions.
#' Simulate Circular Clusters.
#'
#' Simulates a static point cloud with circular clusters.
#'
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param number_of_clusters Integer, number of circular clusters to generate.
#' @param cluster_radius Numeric, radius of the clusters.
#' @param points_per_cluster Integer, the number of points in each cluster.
#' @param background_to_cluster_ratio Numeric value, the ratio of background points to cluster points. Rounded to nearest integer.
#' @return A matrix with two columns: the x and y coordinates of the point cloud. This point cloud will contain circular clusters.
#' @export
simulatecircularclusters <- function(ROI = 1000, number_of_clusters = 10, cluster_radius = 30, points_per_cluster = 20, background_to_cluster_ratio = 0.33){
  #Update ROI size.
  if(length(ROI) == 1){
    ROI <- rep(ROI, 2)
  }
  #Calculate outliers.
  outliers <- background_to_cluster_ratio * (points_per_cluster * number_of_clusters) / (1 - background_to_cluster_ratio)
  
  #Define cluster centres.
  centres <- cbind(runif(number_of_clusters, cluster_radius, ROI[1] - cluster_radius), runif(number_of_clusters, cluster_radius, ROI[2] - cluster_radius))
  
  #Create circular clusters.
  points <- do.call(rbind, lapply(1:nrow(centres), function(i){
    r <- cluster_radius * sqrt(runif(points_per_cluster, 0, 1))
    theta <- runif(points_per_cluster, 0, 2 * pi)
    return(cbind(r * cos(theta) + centres[i,1], r * sin(theta) + centres[i,2]))
  }))
  
  #Overlay outliers.
  points <- rbind(points, cbind(runif(outliers, 0, ROI[1]), runif(outliers, 0, ROI[2])))
  return(points)
}

#' Simulate Complete Spatially Random Clusters.
#'
#' Simulates a static point cloud with randomly placed points.
#'
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param number_of_points Integer, number of points in point cloud.
#' @return A matrix with two columns: the x and y coordinates of the point cloud. This point cloud will contain randomly placed points only.
#' @export
simulatecsr <- function(ROI, number_of_points){
  #Update ROI size.
  if(length(ROI) == 1){
    ROI <- rep(ROI, 2)
  }
  return(cbind(runif(number_of_points, 0, max = ROI[1]), runif(number_of_points, 0, max = ROI[2])))
}

#Auxiliary functions.
#' Ripley's K Function 
#'
#' Calculate the global Ripley's K function for a point cloud.
#'
#' @param points A matrix or data frame with two columns: x and y coordinates of each point.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param rmax Numeric value, the maximum radius for each K function to be calculated at.
#' @param nrval Numeric value, the number of equally-spaced radial values for each K function to be calculated over.
#' @return The global K function for the points data set.
#' @export
ripleyk <- function(points, ROI, rmax = 200, nrval = 20){
  if(length(ROI) == 1){
    ROI <- rep(ROI, 2)
  }
  return(ripley_aggregate(ripley_target_euclidean(points, ROI, rmax, nrval)))
}

#' K to H
#'
#' Converts Ripley's K function to a H function.
#'
#' @param K A matrix or data frame with two columns: r and K.
#' @return The global H function derived from K.
#' @export
KtoH <- function(K){
  K[,2] <- sqrt(K[,2] / pi) - K[,1]
  colnames(K) <- c("r", "H")
  return(K)
}

#' H to K
#'
#' Converts Ripley's H function back to a K function.
#'
#' @param H A matrix or data frame with two columns: r and H.
#' @return The global K function derived from H.
#' @export
HtoK <- function(H){
  H[,2] <- pi * (H[,2] + H[,1]) ** 2
  colnames(H) <- c("r", "K")
  return(H)
}

#' Population Changer
#'
#' Changes points from population A to population B when they come within a distance D of population C. Used to post-process multiple population simulations.
#'
#' @param simulation Input simulation.
#' @param A A vector of population numbers to change.
#' @param B A vector of population numbers to change A into.
#' @param C A vector of population numbers which bring about change from A to B.
#' @param D A vector of distances for which C brings about change.
#' @return A molecular dynamics simulation, with population interactions now recorded.
#' @export
population_changer <- function(simulation, A, B, C, D){
  #Change first population.
  #Get points.
  points <- simulation[[1]][[1]]
  #Get distances.
  distances <- as.matrix(Rfast::Dist(points[,1:2]))
  for(i in 1:length(A)){
    a <- A[i]
    b <- B[i]
    c <- C[i]
    d <- D[i]
    As <- points[,3] == a
    Cs <- points[,3] == c
    if(sum(As) != 0 && sum(Cs) != 0){
      inrange <- distances <= d
      inrange <- Rfast::colMaxs(inrange[Cs, ], value = TRUE)
      As_to_change <- inrange & As
      points[As_to_change, 3] <- b
      # points[((1:nrow(points))[points[,3] == a])[Rfast::colMaxs((distances <= d)[points[,3] == c, points[,3] == a], value = TRUE) == 1], 3] <- b
    }
  }
  simulation[[1]][[1]] <- points
  
  #Iterate over all future populations.
  for(j in 2:length(simulation[[1]])){
    #Get points.
    points <- simulation[[1]][[j]]
    #Get last labels.
    points[,3] <- simulation[[1]][[j - 1]][,3]
    #Get distances.
    distances <- as.matrix(Rfast::Dist(points[,1:2]))
    for(i in 1:length(A)){
      a <- A[i]
      b <- B[i]
      c <- C[i]
      d <- D[i]
      As <- points[,3] == a
      Cs <- points[,3] == c
      if(sum(As) != 0 && sum(Cs) != 0){
        inrange <- distances <= d
        inrange <- Rfast::colMaxs(inrange[Cs, ], value = TRUE)
        As_to_change <- inrange & As
        points[As_to_change, 3] <- b
        # points[((1:nrow(points))[points[,3] == a])[Rfast::colMaxs((distances <= d)[points[,3] == c, points[,3] == a], value = TRUE) == 1], 3] <- b
      }
    }
    simulation[[1]][[j]] <- points
  }
  return(simulation)
}

#' Population Changer
#'
#' Changes points from population A to population B when they come within a distance D of population C. Accounts for removed points. Used to post-process multiple population simulations.
#'
#' @param simulation Input simulation.
#' @param A A vector of population numbers to change.
#' @param B A vector of population numbers to change A into.
#' @param C A vector of population numbers which bring about change from A to B.
#' @param D A vector of distances for which C brings about change.
#' @return A molecular dynamics simulation, with population interactions now recorded.
#' @export
population_changer_with_removal <- function(simulation, A, B, C, D){
  #Change first population.
  #Get points.
  points <- simulation[[1]][[1]]
  #Get distances.
  distances <- as.matrix(Rfast::Dist(points[,1:2]))
  for(i in 1:length(A)){
    a <- A[i]
    b <- B[i]
    c <- C[i]
    d <- D[i]
    As <- points[,3] == a
    Cs <- points[,3] == c
    if(sum(As) != 0 && sum(Cs) != 0){
      inrange <- distances <= d
      inrange <- Rfast::colMaxs(inrange[Cs, ], value = TRUE)
      As_to_change <- inrange & As
      points[As_to_change, 3] <- b
    }
  }
  simulation[[1]][[1]] <- points
  
  #Iterate over all future populations.
  for(j in 2:length(simulation[[1]])){
    #Get points.
    points <- simulation[[1]][[j]]
    #Get last labels.
    points[,3] <- simulation[[1]][[j - 1]][points[,4], 3]
    #Get distances.
    distances <- as.matrix(Rfast::Dist(points[,1:2]))
    for(i in 1:length(A)){
      a <- A[i]
      b <- B[i]
      c <- C[i]
      d <- D[i]
      As <- points[,3] == a
      Cs <- points[,3] == c
      if(sum(As) != 0 && sum(Cs) != 0){
        inrange <- distances <= d
        if(sum(Cs) == 1){
          inrange <- inrange[Cs, ]
        } else {
          inrange <- Rfast::colMaxs(inrange[Cs, ], value = TRUE)
        }
        As_to_change <- inrange & As
        points[As_to_change, 3] <- b
      }
    }
    simulation[[1]][[j]] <- points
  }
  return(simulation)
}

#' Track Changes
#'
#' Tracks changes in number of points of population A over time.
#'
#' @param simulation Input simulation.
#' @param A A population number to track.
#' @param time_step A numeric value representing the length of one time frame.
#' @return A molecular dynamics simulation, with population interactions now recorded.
#' @export
track_changes <- function(simulation, A, time_step = 1){
  counts <- rep(0, length(simulation[[1]]))
  for(i in 1:length(simulation[[1]])){
    counts[i] <- sum(simulation[[1]][[i]][,3] == A)
  }
  return(cbind(1:length(simulation[[1]]) * time_step, counts))
}

#Plot functions.
#' Plot Point Cloud.
#'
#' Creates a nice plot of any input point cloud.
#'
#' @param points A matrix or data frame with two columns: x and y coordinates of each point. An additional third column for population type may also be given, this will be coloured automatically.
#' @param labels A vector of characters representing the x and y axis labels.
#' @param textsize Numeric, font size of axis text size.
#' @param titletextoffset Numeric, increment in font size for titles compared to axis text.
#' @param removetitles Boolean, determines whether axis text is removed.
#' @param pointsize Numeric, size of points in point cloud plot.
#' @param borderthickness Numeric, line thickness of plot border.
#' @param colours Vector of RGB colours representing colours to be used in case of multiple populations. Chosen automatically from a colourblind-friendly palette if not given.
#' @param repeatcolours Boolean, allows plot to repeat colours as many times as necessary to colour all populations. If FALSE, you will need to specify a number of colours equal to the number of different populations.
#' @param background Character or RGB colour. Background colour of plot.
#' @param xlimits Vector with lower and upper bound in x direction.
#' @param ylimits Vector with lower and upper bound in y direction.
#' @return A stylised ggplot of the point cloud.
#' @export
plotpoints <- function(points, labels = c("x", "y"), textsize = 16, titletextoffset = 0, removetitles = TRUE, pointsize = 1.25, borderthickness = 1, colours = NULL, repeatcolours = TRUE, background = "transparent", xlimits = NULL, ylimits = NULL){
  #Define colours.
  darkgrey <- rgb(64/255, 64/255, 64/255)
  
  #Get labels.
  xlab <- labels[1]
  ylab <- labels[2]
  #Set third column.
  if(ncol(points) == 2){
    points <- cbind(points, 0)
  }
  #Get limits.
  if(is.null(xlimits)){
    xlimits <- c(min(points[,1], na.rm = TRUE), max(points[,1], na.rm = TRUE))
  }
  if(is.null(ylimits)){
    ylimits <- c(min(points[,2], na.rm = TRUE), max(points[,2], na.rm = TRUE))
  }
  #Get colours.
  if(!is.null(colours)){
    if(length(colours) < length(unique(points[,3])) && !repeatcolours){
      stop("Number of colours must be equal to number of point types.")
    }
  } else if(length(unique(points[,3])) <= 6 || repeatcolours){
    colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255),
                rgb(115/255, 94/255, 243/255), rgb(248/255, 175/255, 0/255), rgb(101/255, 143/255, 255/255))
  }
  if(!is.null(colours) && repeatcolours){
    colours <- c(rgb(64/255, 64/255, 64/255), rep(colours, length.out = length(unique(points[,3])) - 1))
  }
  colnames(points) <- c("x", "y", "t")
  points <- as.data.frame(points)
  if(is.null(colours)){
    if(removetitles){
      ggplot2::ggplot(points, ggplot2::aes(x = x, y = y, color = as.factor(t))) +
        ggplot2::geom_point(size = pointsize) +
        ggplot2::labs(x = xlab, y = ylab) +
        ggplot2::scale_x_continuous(expand = c(0.01, 0.01), limits = xlimits) +
        ggplot2::scale_y_continuous(expand = c(0.01, 0.01), limits = ylimits) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = borderthickness),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    } else {
      ggplot2::ggplot(points, ggplot2::aes(x = x, y = y, color = as.factor(t))) +
        ggplot2::geom_point(size = pointsize) +
        ggplot2::labs(x = xlab, y = ylab) +
        ggplot2::scale_x_continuous(expand = c(0.01, 0.01), limits = xlimits) +
        ggplot2::scale_y_continuous(expand = c(0.01, 0.01), limits = ylimits) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = borderthickness),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    }
  } else {
    if(removetitles){
      ggplot2::ggplot(points, ggplot2::aes(x = x, y = y, color = as.factor(t))) +
        ggplot2::geom_point(size = pointsize) +
        ggplot2::labs(x = xlab, y = ylab) +
        ggplot2::scale_x_continuous(expand = c(0.01, 0.01), limits = xlimits) +
        ggplot2::scale_y_continuous(expand = c(0.01, 0.01), limits = ylimits) +
        ggplot2::scale_colour_manual(values = colours) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = borderthickness),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    } else {
      ggplot2::ggplot(points, ggplot2::aes(x = x, y = y, color = as.factor(t))) +
        ggplot2::geom_point(size = pointsize) +
        ggplot2::labs(x = xlab, y = ylab) +
        ggplot2::scale_x_continuous(expand = c(0.01, 0.01), limits = xlimits) +
        ggplot2::scale_y_continuous(expand = c(0.01, 0.01), limits = ylimits) +
        ggplot2::scale_colour_manual(values = colours) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = borderthickness),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       legend.position = "none", text = ggplot2::element_text(family = "serif"))
    }
  }
}

#' Plot Lines.
#'
#' Creates a nice plot of some lines.
#'
#' @param data A matrix or data frame. First column represents x coordinates. Each additional column represents y coordinates for a new line.
#' @param labels A vector of characters representing the x and y axis labels.
#' @param xlimits Vector with lower and upper bound in x direction.
#' @param ylimits Vector with lower and upper bound in y direction.
#' @param colours Vector of RGB colours representing colours to be used in case of multiple lines. Chosen automatically from a colourblind-friendly palette if not given.
#' @param textsize Numeric, font size of axis text size.
#' @param titletextoffset Numeric, increment in font size for titles compared to axis text.
#' @param removetitles Boolean, determines whether axis text is removed.
#' @param linewidth Numeric, thickness of lines in plot.
#' @param xpand Vector of scalar multipliers for additional space (expand) given in x direction.
#' @param ypand Vector of scalar multipliers for additional space (expand) given in y direction.
#' @param xpandadd Vector of numeric increments for additional space (expand) given in x direction.
#' @param ypandadd Vector of numeric increments for additional space (expand) given in y direction.
#' @param xangle Numeric, angle of x axis text.
#' @param xhjust Numeric, horizontal displacement of x axis text.
#' @param xvjust Numeric, vertical displacement of x axis text.
#' @param borderthickness Numeric, line thickness of plot border.
#' @param xpandmultiplier Numeric for automatically scaling xpand if not given.
#' @param linetype Linetype, see ggplot2 line types.
#' @param background Character or RGB colour. Background colour of plot.
#' @return A stylised ggplot of the lines.
#' @export
plotlines <- function(data, labels = c("x", "y"), xlimits = NULL, ylimits = NULL, colours = NULL, textsize = 20, titletextoffset = 4, removetitles = FALSE, linewidth = 1, xpand = NULL, ypand = c(0, 0), xpandadd = c(0, 0), ypandadd = c(0, 0), xangle = 0, xhjust = 0.5, xvjust = 0, borderthickness = 1, xpandmultiplier = 0.01, linetype = "solid", background = "transparent"){
  #Define colours.
  darkgrey <- rgb(64/255, 64/255, 64/255)
  
  #Convert to proper expands.
  if(is.null(xpand)){
    xpand <- c(max(floor(log10(abs(min(data[,1], na.rm = TRUE)))) + 1, 0) * xpandmultiplier, (floor(log10(abs(max(data[,1], na.rm = TRUE)))) + 1) * xpandmultiplier)
    if(!is.null(ylimits)){
      if(ylimits[1] == 0){
        xpand[1] <- 0
      }
    }
  }
  xpand <- ggplot2::expansion(mult = xpand, add = xpandadd)
  ypand <- ggplot2::expansion(mult = ypand, add = ypandadd)
  #Get colours.
  if(!is.null(colours)){
    if(length(colours) != ncol(data) - 1){
      stop("Number of colours must be equal to number of lines.")
    }
  } else if(ncol(data) <= 6){
    colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255), rgb(115/255, 94/255, 243/255),
                rgb(248/255, 175/255, 0/255), rgb(101/255, 143/255, 255/255))
    colours <- colours[1:(ncol(data) - 1)]
  }
  #Reshape data.
  data <- as.data.frame(data)
  colnames(data)[1] <- "x"
  if(nrow(data) < 3){
    return(NULL)
  }
  points <- tidyr::gather(data, key = "line", value = "y_value", -x)
  if(is.null(colours)){
    if(removetitles){
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x, y = y_value, color = line)) +
        ggplot2::geom_line(linewidth = linewidth, linetype = linetype) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                       text = ggplot2::element_text(family = "serif"))
    } else {
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x, y = y_value, color = line)) +
        ggplot2::geom_line(linewidth = linewidth, linetype = linetype) +
        ggplot2::labs(x = labels[1], y = labels[2]) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize, angle = xangle, hjust = xhjust, vjust = xvjust),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                       text = ggplot2::element_text(family = "serif"))
    }
  } else {
    if(removetitles){
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x, y = y_value, color = line)) +
        ggplot2::geom_line(linewidth = linewidth, linetype = linetype) +
        ggplot2::scale_colour_manual(values = colours) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                       text = ggplot2::element_text(family = "serif"))
    } else {
      p <- 
        ggplot2::ggplot(points, ggplot2::aes(x = x, y = y_value, color = line)) +
        ggplot2::geom_line(linewidth = linewidth, linetype = linetype) +
        ggplot2::labs(x = labels[1], y = labels[2]) +
        ggplot2::scale_colour_manual(values = colours) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = background), plot.background = ggplot2::element_rect(fill = background, color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = textsize + titletextoffset), axis.text.x = ggplot2::element_text(size = textsize, angle = xangle, hjust = xhjust, vjust = xvjust),
                       axis.title.y = ggplot2::element_text(size = textsize + titletextoffset),  axis.text.y = ggplot2::element_text(size = textsize),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = borderthickness), legend.position = "none",
                       text = ggplot2::element_text(family = "serif"))
    }
  }
  #Change limits.
  if(!is.null(xlimits)){
    p <- p + ggplot2::scale_x_continuous(limits = xlimits, expand = xpand)
  } else {
    p <- p + ggplot2::scale_x_continuous(expand = xpand)
  }
  if(!is.null(ylimits)){
    p <- p + ggplot2::scale_y_continuous(limits = ylimits, expand = ypand)
  } else {
    p <- p + ggplot2::scale_y_continuous(expand = ypand)
  }
  return(suppressWarnings(print(p)))
}

#Animation functions.
#' Animate Point Cloud.
#'
#' Produces an animation of point dynamics from a simulation.
#'
#' @param simulation Input simulation.
#' @param integer_times Boolean which determines whether times will be rounded to the nearest integer.
#' @param time_step A numeric value representing the length of one time frame.
#' @param time_unit Units of time (optional).
#' @param axis_text_size Font size of text on axes.
#' @return A gganimate object representing the animated point distribution.
#' @export
animate_points <- function(simulation, integer_times = TRUE, time_step = 1, time_unit = "ms", axis_text_size = 12){
  darkgrey <- rgb(64/255, 64/255, 64/255)
  if(length(simulation) == 2){
    simulation <- simulation[[1]]
  }
  all_data <- as.data.frame(do.call(rbind, lapply(1:length(simulation), function(i){
    return(cbind(simulation[[i]], i))
  })))
  colnames(all_data) <- c("x", "y", "t")
  p <- ggplot2::ggplot(all_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(size = 2, color = darkgrey) +
    ggplot2::labs(x = "x", y = "y") +
    ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
    ggplot2::scale_y_continuous(expand = c(0.01, 0.01)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent"), plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
                   axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                   legend.position = "none", text = ggplot2::element_text(family = "serif", size = 16))
  if(integer_times){
    p + gganimate::transition_time(as.integer(t)) + ggplot2::labs(title = paste("Time: {frame_time * time_step}", time_unit, sep = ""))
  } else {
    p + gganimate::transition_time(t) + ggplot2::labs(title = paste("Time: {frame_time * time_step}", time_unit, sep = ""))
  }
}

#' Animate Multiple Populations.
#'
#' Produces an animation of population dynamics from a simulation.
#'
#' @param simulation Input simulation.
#' @param integer_times Boolean which determines whether times will be rounded to the nearest integer.
#' @param time_step A numeric value representing the length of one time frame.
#' @param time_unit Units of time (optional).
#' @param labels A vector of characters representing the name of each population type. Will be displayed in the animation legend if not NULL.
#' @param text_size Text size of labels in legend.
#' @param axis_text_size Font size of text on axes.
#' @param point_size Size of each point in the simulation.
#' @return A gganimate object representing the animated point distribution, including multiple populations.
#' @export
animate_populations <- function(simulation, integer_times = TRUE, time_step = 1, time_unit = "ms", labels = NULL, text_size = 16, axis_text_size = 12, point_size = 2, legendposition = "right"){
  ibmcols <- c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255), rgb(115/255, 94/255, 243/255), rgb(248/255, 175/255, 0/255), rgb(101/255, 143/255, 255/255))
  if(is.list(simulation[[1]])){
    simulation <- simulation[[1]]
  }
  all_data <- as.data.frame(do.call(rbind, lapply(1:length(simulation), function(i){
    return(cbind(simulation[[i]][,1:3], i))
  })))
  colnames(all_data) <- c("x", "y", "p", "t")
  if(!is.null(labels)){
    for(i in 1:length(labels)){
      all_data[all_data[,3] == i,3] <- labels[i]
    }
  }
  colours <- rep(ibmcols, length.out = length(unique(all_data[,3])))
  p <- ggplot2::ggplot(all_data, ggplot2::aes(x = x, y = y, colour = as.factor(p))) +
    ggplot2::geom_point(size = point_size) +
    ggplot2::labs(x = "x", y = "y") +
    ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
    ggplot2::scale_y_continuous(expand = c(0.01, 0.01)) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent"), plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
                   axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),  axis.text.y = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 16),
                   legend.position = legendposition, text = ggplot2::element_text(family = "serif", size = 16))
    # ggplot2::theme(panel.background = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
    #                panel.grid.minor = ggplot2::element_blank(), legend.title = ggplot2::element_blank(),
    #                panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
    #                legend.text = ggplot2::element_text(size = text_size), axis.text.x = ggplot2::element_text(size = axis_text_size),
    #                axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(),
    #                axis.text.y = ggplot2::element_text(size = axis_text_size))
  if(integer_times){
    p + gganimate::transition_time(as.integer(t)) + ggplot2::labs(title = paste("Time: {frame_time * time_step}", time_unit, sep = ""))
  } else {
    p + gganimate::transition_time(t) + ggplot2::labs(title = paste("Time: {frame_time * time_step}", time_unit, sep = ""))
  }
}

#' Animate Target Function.
#'
#' Animates the actual Ripley's K function of the point distribution in the simulation, compared to a target curve.
#'
#' @param simulation Input simulation.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param target The target curve.
#' @param rmax Numeric value, the maximum radius for each K function to be calculated at.
#' @param nrval Numeric value, the number of equally-spaced radial values for each K function to be calculated over.
#' @param integer_times Boolean which determines whether times will be rounded to the nearest integer.
#' @param colours Colours for target and actual curve respectively.
#' @param labels Vector of characters representing axis labels.
#' @param time_step A numeric value representing the length of one time frame.
#' @param time_unit Units of time (optional).
#' @param axis_text_size Font size of text on axes.
#' @param title_text_offset Increment in font size for axis titles.
#' @return A gganimate object representing the animated K function and target statistic.
#' @export
animate_target <- function(simulation, ROI, target, rmax = 200, nrval = 20, integer_times = TRUE, colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255)), labels = c("r", "K(r)"), time_step = 1, time_unit = "ms", axis_text_size = 16, title_text_offset = 4, legendposition = "right"){
  if(length(ROI) == 1){
    ROI <- rep(ROI, 2)
  }
  targets <- simulation[[2]]
  all_data <- as.data.frame(do.call(rbind, lapply(1:length(targets), function(i){
    return(cbind(target, HtoK(targets[[i]])[,2], i))
  })))
  colnames(all_data) <- c("x", "Target", "Actual", "t")
  cols <- c("Target" = colours[1], "Actual" = colours[2])
  p <- ggplot2::ggplot(all_data, ggplot2::aes(x = x)) +
    ggplot2::geom_line(ggplot2::aes(y = Actual, colour = "Actual"), linewidth = 1) +
    ggplot2::geom_line(ggplot2::aes(y = Target, colour = "Target"), linewidth = 1) +
    ggplot2::labs(x = labels[1], y = labels[2]) +
    ggplot2::scale_colour_manual(name = "Functions", values = cols) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
    ggplot2::scale_y_continuous(expand = c(0.01, 0.01)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent"), plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black", linewidth = 1), text = ggplot2::element_text(family = "serif", size = 16),
                   legend.position = legendposition)
  if(integer_times){
    p + gganimate::transition_time(as.integer(t)) + ggplot2::labs(title = paste("Time: {frame_time * time_step}", time_unit, sep = ""))
  } else {
    p + gganimate::transition_time(t) + ggplot2::labs(title = paste("Time: {frame_time * time_step}", time_unit, sep = ""))
  }
}

#' Animate H Function from Points.
#'
#' Animates the actual Ripley's H function fitting to other H function(s) from a given point cloud or clouds.
#'
#' @param simulation Input simulation.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param point_clouds A list of points data, where each data set is given as a matrix or data frame with two columns: x coordinate and y coordinate. Used as target distributions from which spatial statistics are drawn.
#' @param rmax Numeric value, the maximum radius for each Ripley's function to be calculated at.
#' @param nrval Numeric value, the number of equally-spaced radial values for each Ripley's function to be calculated over.
#' @param integer_times Boolean which determines whether times will be rounded to the nearest integer.
#' @param colours Colours for target and actual curve respectively.
#' @param labels Vector of three characters with first entry representing x axis label, the second representing the name of the target function and the third representing the name of the actual functon.
#' @param time_step A numeric value representing the length of one time frame.
#' @param time_unit Units of time (optional).
#' @param time_points Times at which target function was changed. For discrete time points case only, otherwise leave NULL.
#' @return A gganimate object representing the animated H function and target statistic.
#' @export
animate_H_from_points <- function(simulation, ROI, point_clouds, rmax = 200, nrval = 20, integer_times = TRUE, colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255)), labels = c("r", "Target", "Actual"), time_step = 1, time_unit = "ms", time_points = NULL, legendposition = "right"){
  #Update ROI.
  if(length(ROI) == 1){
    ROI <- rep(ROI, 2) 
  }
  #Create point clouds list.
  if(is.matrix(point_clouds) || is.data.frame(point_clouds)){
    point_clouds <- list(point_clouds)
  }
  if(length(point_clouds) > 1){
    #Multiple K functions case.
    target <- lapply(point_clouds, function(points){
      return(ripley_H_aggregate(ripley_target_euclidean(points, ROI, rmax = rmax, nrval = nrval)))
    })
    targets <- simulation[[2]]
    if(is.null(time_points)){
      #Simultaneous functions case.
      R <- target[[1]][,1]
      target <- do.call(cbind, lapply(target, function(targ){
        return(targ[,2])
      }))
      all_data <- as.data.frame(do.call(rbind, lapply(1:length(targets), function(i){
        return(cbind(R, target, targets[[i]][,2], i))
      })))
      colnames(all_data)[1] <- "x"
      all_data <- cbind(tidyr::gather(all_data[,1:(ncol(all_data) - 1)], key = "line", value = "y_value", -x), all_data[,ncol(all_data)])
      colnames(all_data)[ncol(all_data)] <- "t"
      ibmcols <- c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255), rgb(115/255, 94/255, 243/255),
                   rgb(248/255, 175/255, 0/255), rgb(101/255, 143/255, 255/255))
      colours <- rep(ibmcols, length.out = length(unique(all_data$line)))
      p <- 
        ggplot2::ggplot(all_data, ggplot2::aes(x = x, y = y_value, color = line)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::scale_colour_manual(values = colours) +
        ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
        ggplot2::scale_y_continuous(expand = c(0.01, 0.01)) +
        ggplot2::labs(x = "r", y = "H(r)") +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent"), plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = 1), text = ggplot2::element_text(family = "serif", size = 16),
                       legend.position = legendposition)
      if(integer_times){
        p + gganimate::transition_time(as.integer(t)) + ggplot2::labs(title = paste("Time: {frame_time * time_step}", time_unit, sep = ""))
      } else {
        p + gganimate::transition_time(t) + ggplot2::labs(title = paste("Time: {frame_time * time_step}", time_unit, sep = ""))
      }
    } else {
      #Distinct time points case.
      time_points <- c(0, time_points)
      
      #Separate out simulations.
      all_data <- do.call(rbind, lapply(1:(length(time_points) - 1), function(j){
        return(as.data.frame(do.call(rbind, lapply((time_points[j] + 1):time_points[j + 1], function(i){
          return(cbind(target[[j]], targets[[i]][,2], i))
        }))))
      }))
      colnames(all_data) <- c("x", "Target", "Actual", "t")
      cols <- c("Target" = colours[1], "Actual" = colours[2])
      p <- ggplot2::ggplot(all_data, ggplot2::aes(x = x)) +
        ggplot2::geom_line(ggplot2::aes(y = Actual, colour = "Actual"), linewidth = 1) +
        ggplot2::geom_line(ggplot2::aes(y = Target, colour = "Target"), linewidth = 1) +
        ggplot2::labs(x = labels[1], y = "H(r)") +
        ggplot2::scale_colour_manual(name = "Functions", values = cols) +
        ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
        ggplot2::scale_y_continuous(expand = c(0.01, 0.01)) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent"), plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = 1), text = ggplot2::element_text(family = "serif", size = 16),
                       legend.position = legendposition)
      if(integer_times){
        p + gganimate::transition_time(as.integer(t)) + ggplot2::labs(title = paste("Time: {frame_time * time_step}", time_unit, sep = ""))
      } else {
        p + gganimate::transition_time(t) + ggplot2::labs(title = paste("Time: {frame_time * time_step}", time_unit, sep = ""))
      }
    }
  } else {
    #Standard plot case.
    target <- ripley_H_aggregate(ripley_target_euclidean(point_clouds[[1]], ROI, rmax = rmax, nrval = nrval))
    targets <- simulation[[2]]
    all_data <- as.data.frame(do.call(rbind, lapply(1:length(targets), function(i){
      return(cbind(target, targets[[i]][,2], i))
    })))
    colnames(all_data) <- c("x", "Target", "Actual", "t")
    cols <- c("Target" = colours[1], "Actual" = colours[2])
    p <- ggplot2::ggplot(all_data, ggplot2::aes(x = x)) +
      ggplot2::geom_line(ggplot2::aes(y = Actual, colour = "Actual"), size = 1) +
      ggplot2::geom_line(ggplot2::aes(y = Target, colour = "Target"), size = 1) +
      ggplot2::labs(x = "r", y = "H(r)") +
      ggplot2::scale_colour_manual(name = "Functions", values = cols) +
      ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
      ggplot2::scale_y_continuous(expand = c(0.01, 0.01)) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent"), plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                     panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(colour = "black", linewidth = 1), text = ggplot2::element_text(family = "serif", size = 16),
                     legend.position = legendposition)
    if(integer_times){
      p + gganimate::transition_time(as.integer(t)) + ggplot2::labs(title = paste("Time: {frame_time * time_step}", time_unit, sep = ""))
    } else {
      p + gganimate::transition_time(t) + ggplot2::labs(title = paste("Time: {frame_time * time_step}", time_unit, sep = ""))
    }
  }
}

#' Animate K Function from Points.
#'
#' Animates the actual Ripley's K function fitting to other K function(s) from a given point cloud or clouds.
#'
#' @param simulation Input simulation.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param point_clouds A list of points data, where each data set is given as a matrix or data frame with two columns: x coordinate and y coordinate. Used as target distributions from which spatial statistics are drawn.
#' @param rmax Numeric value, the maximum radius for each Ripley's function to be calculated at.
#' @param nrval Numeric value, the number of equally-spaced radial values for each Ripley's function to be calculated over.
#' @param integer_times Boolean which determines whether times will be rounded to the nearest integer.
#' @param colours Colours for target and actual curve respectively.
#' @param labels Vector of three characters with first entry representing x axis label, the second representing the name of the target function and the third representing the name of the actual functon.
#' @param time_step A numeric value representing the length of one time frame.
#' @param time_unit Units of time (optional).
#' @param time_points Times at which target function was changed. For discrete time points case only, otherwise leave NULL.
#' @return A gganimate object representing the animated K function and target statistic.
#' @export
animate_K_from_points <- function(simulation, ROI, point_clouds, rmax = 200, nrval = 20, integer_times = TRUE, colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255)), labels = c("r", "Target", "Actual"), time_step = 1, time_unit = "ms", time_points = NULL, legendposition = "right"){
  #Update ROI.
  if(length(ROI) == 1){
   ROI <- rep(ROI, 2) 
  }
  #Create point clouds list.
  if(is.matrix(point_clouds) || is.data.frame(point_clouds)){
    point_clouds <- list(point_clouds)
  }
  if(length(point_clouds) > 1){
    #Multiple K functions case.
    target <- lapply(point_clouds, function(points){
      return(ripley_aggregate(ripley_target_euclidean(points, ROI, rmax = rmax, nrval = nrval)))
    })
    targets <- simulation[[2]]
    if(is.null(time_points)){
      #Simultaneous functions case.
      R <- target[[1]][,1]
      target <- do.call(cbind, lapply(target, function(targ){
        return(targ[,2])
      }))
      all_data <- as.data.frame(do.call(rbind, lapply(1:length(targets), function(i){
        return(cbind(R, target, pi * (targets[[i]][,2] + targets[[i]][,1]) ** 2, i))
      })))
      colnames(all_data)[1] <- "x"
      all_data <- cbind(tidyr::gather(all_data[,1:(ncol(all_data) - 1)], key = "line", value = "y_value", -x), all_data[,ncol(all_data)])
      colnames(all_data)[ncol(all_data)] <- "t"
      p <- 
        ggplot2::ggplot(all_data, ggplot2::aes(x = x, y = y_value, color = line)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
        ggplot2::scale_y_continuous(expand = c(0.01, 0.01)) +
        ggplot2::labs(x = "r", y = "K(r)") +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent"), plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = 1), text = ggplot2::element_text(family = "serif", size = 16),
                       legend.position = legendposition)
      if(integer_times){
        p + gganimate::transition_time(as.integer(t)) + ggplot2::labs(title = paste("Time: {frame_time * time_step}", time_unit, sep = ""))
      } else {
        p + gganimate::transition_time(t) + ggplot2::labs(title = paste("Time: {frame_time * time_step}", time_unit, sep = ""))
      }
    } else {
      #Distinct time points case.
      time_points <- c(0, time_points)
      
      #Separate out simulations.
      all_data <- do.call(rbind, lapply(1:(length(time_points) - 1), function(j){
        return(as.data.frame(do.call(rbind, lapply((time_points[j] + 1):time_points[j + 1], function(i){
          return(cbind(target[[j]], targets[[i]][,2], i))
        }))))
      }))
      colnames(all_data) <- c("x", "Target", "Actual", "t")
      cols <- c("Target" = colours[1], "Actual" = colours[2])
      p <- ggplot2::ggplot(all_data, ggplot2::aes(x = x)) +
        ggplot2::geom_line(ggplot2::aes(y = Actual, colour = "Actual"), linewidth = 1) +
        ggplot2::geom_line(ggplot2::aes(y = Target, colour = "Target"), linewidth = 1) +
        ggplot2::labs(x = "r", y = "K(r)") +
        ggplot2::scale_colour_manual(name = "Functions", values = cols) +
        ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
        ggplot2::scale_y_continuous(expand = c(0.01, 0.01)) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent"), plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                       panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black", linewidth = 1), text = ggplot2::element_text(family = "serif", size = 16),
                       legend.position = legendposition)
      if(integer_times){
        p + gganimate::transition_time(as.integer(t)) + ggplot2::labs(title = paste("Time: {frame_time * time_step}", time_unit, sep = ""))
      } else {
        p + gganimate::transition_time(t) + ggplot2::labs(title = paste("Time: {frame_time * time_step}", time_unit, sep = ""))
      }
    }
  } else {
    #Standard plot case.
    target <- ripley_aggregate(ripley_target_euclidean(point_clouds[[1]], ROI, rmax = rmax, nrval = nrval))
    targets <- simulation[[2]]
    all_data <- as.data.frame(do.call(rbind, lapply(1:length(targets), function(i){
      return(cbind(target, pi * (targets[[i]][,2] + targets[[i]][,1]) ** 2, i))
    })))
    colnames(all_data) <- c("x", "Target", "Actual", "t")
    cols <- c("Target" = colours[1], "Actual" = colours[2])
    p <- ggplot2::ggplot(all_data, ggplot2::aes(x = x)) +
      ggplot2::geom_line(ggplot2::aes(y = Actual, colour = "Actual"), linewidth = 1) +
      ggplot2::geom_line(ggplot2::aes(y = Target, colour = "Target"), linewidth = 1) +
      ggplot2::labs(x = "r", y = "K(r)") +
      ggplot2::scale_colour_manual(name = "Functions", values = cols) +
      ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
      ggplot2::scale_y_continuous(expand = c(0.01, 0.01)) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent"), plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                     panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(colour = "black", linewidth = 1), text = ggplot2::element_text(family = "serif", size = 16),
                     legend.position = legendposition)
    if(integer_times){
      p + gganimate::transition_time(as.integer(t)) + ggplot2::labs(title = paste("Time: {frame_time * time_step}", time_unit, sep = ""))
    } else {
      p + gganimate::transition_time(t) + ggplot2::labs(title = paste("Time: {frame_time * time_step}", time_unit, sep = ""))
    }
  }
}

#' Animate H Function of a Population.
#'
#' Animates the actual Ripley's H function fitting to another H function for one population in a multiple populations simulation.
#'
#' @param simulation Input simulation.
#' @param ROI A vector of two values representing the width and height of the region of interest, respectively.
#' @param point_cloud A matrix or data frame with two columns: x coordinate and y coordinate. Used as target distribution from which spatial statistics are drawn.
#' @param population_number The index of the population to be extracted and analysed.
#' @param rmax Numeric value, the maximum radius for each Ripley's function to be calculated at.
#' @param nrval Numeric value, the number of equally-spaced radial values for each Ripley's function to be calculated over.
#' @param integer_times Boolean which determines whether times will be rounded to the nearest integer.
#' @param colours Colours for target and actual curve respectively.
#' @param labels Vector of three characters with first entry representing x axis label, the second representing the name of the target function and the third representing the name of the actual functon.
#' @param time_step A numeric value representing the length of one time frame.
#' @param time_unit Units of time (optional).
#' @return A gganimate object representing the animated H function and target statistic.
#' @export
animate_H_from_populations <- function(simulation, ROI, point_cloud, population_number, rmax = 200, nrval = 20, integer_times = TRUE, colours = c(rgb(212/255, 30/255, 125/255), rgb(245/255, 93/255, 0/255)), labels = c("r", "Target", "Actual"), time_step = 1, time_unit = "ms"){
  #Get targets.
  targets <- lapply(simulation[[2]], function(tar){
    return(tar[[population_number]])
  })
  simulation <- list(simulation[[1]], targets)
  #Get original target for that population.
  volume <- prod(ROI)
  number_of_points <- nrow(point_cloud)
  lambda <- volume/(number_of_points)
  distances <- as.matrix(Rfast::Dist(point_cloud[,1:(ncol(point_cloud) - 1)]))
  target <- ripley_H_aggregate_pop(ripley_target_pop(point_cloud, ROI, R, lambda, distances))[[population_number]]
  #Feed into animator.
  animate_target(simulation, ROI, target, rmax, nrval, integer_times, colours, labels, time_step, time_unit)
}