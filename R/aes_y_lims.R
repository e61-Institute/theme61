#' Get aesthetic y-axis limits and tick marks.
#'
#' These functions are used to produce aesthetic y-axis limits and tick spacing
#'
#' @param y_val Numeric. Number for which we are going
#' @param type String. Are we looking for the next smallest or next largest
#' aesthetic value.
#' @rdname e61_aes_limits
#' @export

# Get an aesthetic number for setting y-axis scaling defaults
get_aes_num <- function(y_val, type = c("next_largest", "next_smallest")) {

  # set the adjustment factor based on whether we are looking at a value above or below 1
  if (y_val > 0) adj <- 1 else adj <- -1

  aes_y_points <- data.frame(points = c(10, 12, 15, 16, 20, 25, 30, 40, 50, 60, 75, 80, 100))

  order_mag <- ceiling(log10(adj * y_val))
  aes_y_points <- aes_y_points %>% dplyr::mutate(points_adj = adj * points)
  aes_y_points <- aes_y_points %>% dplyr::mutate(points_diff = points_adj - (y_val / 10 ^ (order_mag - 2)))

  if(type == "next_smallest") aes_y_points <- aes_y_points %>% dplyr::mutate(points_diff = -1 * points_diff)

  if (y_val > 0) {
    aes_y_points <- aes_y_points %>%
      dplyr::filter(points_diff > 0) %>%
      dplyr::mutate(points_diff == min(points_diff))

  } else {

    aes_y_points <- aes_y_points %>%
      dplyr::filter(points_diff < 0) %>%
      dplyr::mutate(points_diff == max(points_diff))
  }

  # take the smallest value that is greater than 0
  y_aes_adj <- adj * aes_y_points$points[1]
  y_aes <- aes_y_points$points[1]

  return(y_aes_adj * 10 ^ (order_mag - 2))
}

# Aesthetic tick s for a given pair of numbers

#' @param min_y_val Double. Minimum y-axis value.
#' @param max_y_val Double. Maximum y-axis value.
#' @rdname e61_aes_limits
#' @export

get_aes_ticks <- function(min_y_val, max_y_val){

  if(min_y_val == max_y_val) return(NULL)

  # check the argments are correctly ordered, otherwise adjust
  if(max_y_val < min_y_val) {
    temp_bot <- min_y_val
    min_y_val <- max_y_val
    max_y_val <- temp_bot
  }

  # differences between points by the number of ticks to include
  aes_y_points <- list(
    five_point = c(10, 15, 25, 50, 75, 100),
    four_point = c(12, 16, 20, 40, 60, 80),
    three_point = c(30, 45, 90)
  )

  # the maximum size of the tick spacing - set to max value (100 as we adjust based on the order of magnitude)
  max_size <- 100

  # determine the difference between the two points
  if (min_y_val <= 0 & max_y_val >= 0){
    diff <- max_y_val + abs(min_y_val)

    # both not equal to 0 - gap spans 0
    if(min_y_val != 0 & max_y_val != 0){
      min_val <- min(c(max_y_val, abs(min_y_val)))
      max_val <- min(c(max_y_val, abs(min_y_val)))

      max_size <- min_val

      # top equal to 0 and bot below
    } else if(min_y_val < 0){
      max_size <- abs(min_y_val) / 2

      # bot equal to 0 and top above
    } else {
      max_size <- max_y_val / 2
    }

  } else if (min_y_val < 0 & max_y_val < 0) {

    diff <- abs(min_y_val) - abs(max_y_val)

    max_size <- diff / 2

  } else {
    diff <- max_y_val - min_y_val

    max_size <- diff / 2
  }

  # adjust the order of magnitude of the difference if it is less than 10 or > 100
  order_mag <- ceiling(log10(diff))

  aes_y_points <- lapply(aes_y_points, function(x) x * 10 ^ (order_mag - 2))

  order_mag_max <- ceiling(log10(max_size))

  # if the difference is a factor of 5, use five ticks
  if (any(unlist(lapply(aes_y_points$five_point, near, diff)))) {
    band_val <- diff / 5

    # if a factor of 4
  } else if (any(unlist(lapply(aes_y_points$four_point, near, diff)))) {
    band_val <- diff / 4

    # else use a factor of 3
  } else if (any(unlist(lapply(aes_y_points$three_point, near, diff)))) {
    band_val <- diff / 3

    # Rule 1 - If the difference is not in any of the lists, then it isn't aesthetic and we should try the max size to begin with
  } else {

    # But first check the max value is aesthetic itself - it should be most of the time
    aes_y_points <- aes_y_points %>% unlist()

    # adjust the order of magnitude if necessary
    if(!any(unlist(lapply(aes_y_points, near, max_size / 10 ^ (order_mag_max - 2))))) {
      return(NULL)

    } else {
      band_val <- max_size
    }
  }

  # Rule 2 - If the band size is larger than the distance from the smallest value to 0, then it will not be aesthetic!!
  if(band_val > max_size) {
    band_val <- max_size
  }

  # Adjust orders of magnitude so that modular arithmetic still works
  adj_max_size <- max_size / 10 ^ (order_mag_max - 2)
  adj_band_val <- band_val / 10 ^ (order_mag_max - 2)

  adj_min_y_val <- min_y_val / 10 ^ (order_mag_max - 2)
  adj_max_y_val <- max_y_val / 10 ^ (order_mag_max - 2)

  # Rule 3 - If both the top and bottom value are not factors of the band size then it will not be aesthetic!! (if they are either side of 0)
  if(min_y_val < 0 & max_y_val > 0){
    if(adj_max_size %% adj_band_val != 0){

      if(adj_max_size %% 2 == 0) {
        band_val <- max_size / 2

      } else if(adj_max_size %% 3 == 0){
        band_val <- max_size / 3

      } else {
        band_val <- max_size
      }
    }

    adj_band_val <- band_val / 10 ^ (order_mag_max - 2)

    if(adj_max_y_val %% adj_band_val != 0 | adj_min_y_val %% adj_band_val != 0) {
      return(NULL)
    }
  }

  # Rule 4 - there should not be more than 7 ticks (if the numbers are either side of 0 we may be forced to use this)
  if(diff / band_val > 7) {
    return(NULL)
  }

  return(band_val)
}

# Return aesthetic number pairs for a given pair of y-axis numbers
#' @param y_val_1 Double. Minimum y-axis value.
#' @param y_val_2 Double. Maximum y-axis value.
#' @rdname e61_aes_limits
#' @export

get_aes_pair <- function(y_val_1, y_val_2){

  # 1 - Start with the larger value - get an aesthic number for that one first
  if(abs(y_val_1) > abs(y_val_2)){
    largest_val <- y_val_1
    smallest_val <- y_val_2

  } else {
    largest_val <- y_val_2
    smallest_val <- y_val_1
  }

  aes_largest_val <- get_aes_num(largest_val, type = "next_largest")

  # 2 - Then find all the aesthetic pairs
  aes_y_points <- c(10, 12, 15, 16, 20, 25, 30, 40, 50, 60, 75, 80, 100)

  # adjust to the right order of magnitude for the smallest value
  order_mag_large <- ceiling(log10(abs(aes_largest_val)))
  order_mag_small <- ceiling(log10(abs(smallest_val)))

  # If the numbers are more than one order of magnitude different, they'll never make a good pair
  if(order_mag_large > order_mag_small + 1){
    order_mag <- order_mag_large - 1

  } else {
    order_mag <- order_mag_small
  }

  aes_y_points <- aes_y_points * 10 ^ (order_mag - 2)

  # multiply by -1 if we're looking for a negative aesthetic pair
  if(smallest_val < 0) aes_y_points <- -1 * aes_y_points

  aes_pairs <- c()

  for(i in seq_along(aes_y_points)){

    test_val <- aes_y_points[i]

    ticks <- get_aes_ticks(test_val, aes_largest_val)

    if(!is.null(ticks)) aes_pairs <- c(aes_pairs, test_val)
  }

  if(is.null(aes_pairs)) return(list(0, aes_largest_val))

  temp_data <- expand.grid(aes_large = aes_largest_val, aes_small = aes_pairs)

  # 3 - get the pair that is closest to the aesthetic value of the smaller number
  # For the aesthetic number, if they are both positive or both negative we want the value just below the smallest number
  if((y_val_1 < 0 & y_val_2 < 0) | (y_val_1 > 0 & y_val_2 > 0)){
    aes_smallest_val <- get_aes_num(smallest_val, type = "next_smallest")

    # adjust for the smallest aesthetic value
    ret_smallest_val <- temp_data %>%
      mutate(diff = aes_small - aes_smallest_val) %>%
      filter(diff <= 0, abs(aes_small) <= abs(smallest_val)) %>%
      slice_max(diff, n = 1, with_ties = F) %>%
      pull(aes_small)

  } else {
    aes_smallest_val <- get_aes_num(smallest_val, type = "next_largest")

    # adjust for the smallest aesthetic value
    ret_smallest_val <- temp_data %>%
      mutate(diff = aes_small - aes_smallest_val) %>%
      filter(diff <= 0) %>%
      slice_max(diff, n = 1, with_ties = F) %>%
      pull(aes_small)
  }

  if (length(ret_smallest_val) == 0) ret_smallest_val <- 0

  return(list(ret_smallest_val, aes_largest_val))
}

# Return aesthetic limits given a min and max y-axis values

#' @param min_y_val Double. Minimum y-axis value.
#' @param max_y_val Double. Maximum y-axis value.
#' @param from_zero Logical. Should the limits start at zero or just below the
#' minimum value?
#' @rdname e61_aes_limits
#' @export

get_aes_limits <- function(min_y_val, max_y_val, from_zero = F){

  if(is.null(min_y_val) | is.null(max_y_val)){
    stop("Y-axis limits could not be determined. Please check your y-axis variable is numeric.")
  }

  if(max_y_val < min_y_val){
    temp <- min_y_val
    min_y_val <- max_y_val
    max_y_val <- temp
  }

  # If they are the same, return a scale from 0 to the value
  if(min_y_val == max_y_val){

    aes_num <- get_aes_num(y_val = min_y_val, type = "next_largest")

  # if we want to scale from from_zero then only use one value for the limits
  } else if(from_zero){

    if(min_y_val < 0){
      limits <- list(get_aes_num(min_y_val, type = "next_largest"), 0)

    } else {
      limits <- list(0, get_aes_num(max_y_val, type = "next_largest"))
    }

  # otherwise just get an aesthetic pair (not equal and not a bar chart)
  } else {

    limits <- get_aes_pair(min_y_val, max_y_val)

    temp <- unlist(limits)
    limits <- list(min(temp), max(temp))
  }

  # Get the aestehtic tick spacing from the aesthetic pair
  tick_spacing <- get_aes_ticks(min_y_val = limits[[1]], max_y_val = limits[[2]])

  return(list(limits[[1]], limits[[2]], tick_spacing))
}
