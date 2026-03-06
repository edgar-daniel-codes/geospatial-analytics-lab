# distance_utils.R

## Geospatial Analytics Lab
## Distance Utils 
## By Edgar Daniel

# This scripts contains multiple auxiliary functions to handle both euclidian and  
# spheric distances from geo-spatial problems. 
# 


# Set the seed to ensure reproducibility.
set.seed(999)

### ----------------------------------------------------------------------------
### Required Libraries  --------------------------------------------------------


### Functions  -----------------------------------------------------------------


min_manhattan_to_df2 <- function(df1,
                                 df2,
                                 id_col,
                                 x_col_df1,
                                 y_col_df1,
                                 x_col_df2,
                                 y_col_df2,
                                 min_buffer,
                                 max_buffer) {
  
  ###
  # This function takes two different dataframes df1 and df2, both containinig 
  # projected points in the euclidian space and for each unique element in df1
  # finds out the minimum distance to any other point in df2, that is at most 
  # max_buffer units apart. 
  #   - df1,
  #   - df2,
  #   - id_col,
  #   - x_col_df1,
  #   - y_col_df1,
  #   - x_col_df2,
  #   - y_col_df2,
  #   - min_buffer,
  #   - max_bufferher there are waiting times 
  # Outputs:
  # df: DataFrame with ID identifier from df1 with its minimum distance value
  ### 
  
  # Input Validation
  stopifnot(
    "df1 must be a data frame"          = is.data.frame(df1),
    "df2 must be a data frame"          = is.data.frame(df2),
    "id_col not found in df1"           = id_col      %in% names(df1),
    "x_col_df1 not found in df1"        = x_col_df1  %in% names(df1),
    "y_col_df1 not found in df1"        = y_col_df1  %in% names(df1),
    "x_col_df2 not found in df2"        = x_col_df2  %in% names(df2),
    "y_col_df2 not found in df2"        = y_col_df2  %in% names(df2),
    "min_buffer must be a positive number" = is.numeric(min_buffer) && min_buffer > 0,
    "max_buffer must be a positive number" = is.numeric(max_buffer) && max_buffer > 0,
    "min_buffer must be <= max_buffer"  = min_buffer <= max_buffer
  )
  
  #  Early exit on empty inputs
  if (nrow(df1) == 0L) {
    warning("df1 has no rows; returning empty data frame.")
    return(data.frame(CVEGEO = character(0), min_dist = numeric(0)))
  }
  if (nrow(df2) == 0L) {
    warning("df2 has no rows; all distances will be NA.")
  }
  
  # Coordinate extraction
  id_vals <- df1[[id_col]]
  x1 <- suppressWarnings(as.numeric(as.character(df1[[x_col_df1]])))
  y1 <- suppressWarnings(as.numeric(as.character(df1[[y_col_df1]])))
  
  # Warn if coercion produced NAs
  if (anyNA(x1)) warning(sprintf("%d NA(s) produced coercing '%s' to numeric.", sum(is.na(x1)), x_col_df1))
  if (anyNA(y1)) warning(sprintf("%d NA(s) produced coercing '%s' to numeric.", sum(is.na(y1)), y_col_df1))
  
  out <- rep(NA_real_, length(id_vals))
  
  ## Auxiliar functions 
  
  # Compute min Manhattan distance within a candidate set  
  min_manhattan <- function(df_candidates, x_, y_) {
    df_candidates |>
      mutate(dist_m = abs(x_ - .data[[x_col_df2]]) +
               abs(y_ - .data[[y_col_df2]])) |>
      pull(dist_m) |>
      min(na.rm = TRUE)
  }
  
  # Filter df2 to a bounding box ────────────────────────────────── 
  bbox_filter <- function(buffer) {
    df2 |>
      filter(
        abs(.data[[x_col_df2]] - x_) <= buffer,
        abs(.data[[y_col_df2]] - y_) <= buffer
      )
  }
  
  # Main
  for (i in seq_along(id_vals)) {
    
    # Skip rows where coordinates could not be parsed
    if (is.na(x1[i]) || is.na(y1[i])) next
    
    x_ <- x1[i]
    y_ <- y1[i]
    
    result <- tryCatch({
      
      df_bbox <- bbox_filter(min_buffer)
      
      if (nrow(df_bbox) > 0) {
        min_manhattan(df_bbox, x_, y_)
      } else {
        df_bbox <- bbox_filter(max_buffer)
        if (nrow(df_bbox) > 0) min_manhattan(df_bbox, x_, y_) else NA_real_
      }
      
    }, error = function(e) {
      warning(sprintf("Error at row %d (id = %s): %s", i, as.character(id_vals[i]), conditionMessage(e)))
      NA_real_
    })
    
    out[i] <- result
  }
  
  # Output 
  data.frame(
    id_col   = id_vals,
    min_dist = out
  )
}



