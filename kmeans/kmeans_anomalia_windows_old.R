
kmeans_windows_detected_optimized <- function(name_Dataset, dataset, windows, output_dir = getwd(), save_file = TRUE) {
  # kmeans_windows_detected_optimized Function Description
  #
  # This R function serves as a wrapper for the hanct_kmeans algorithm, designed to detect sequences
  # within a time series dataset. It returns a data frame containing the detected sequences for each
  # series in the dataset. The resulting data frame includes the following columns:
  #
  # - name_Dataset: The name of the input dataset.
  # - serie: The name of the time series column being analyzed.
  # - i: An identifier for the detected sequence.
  # - windows: The number of windows used to detect the sequence.
  # - idx1, idx2, ..., idxN: The indices corresponding to the detected sequence elements.
  #
  # Optionally, the function can save the resulting data frame as an RDS file in a specified output directory,
  # based on the value of the `save_file` parameter.
  #
  # Parameters:
  # - name_Dataset: A character string representing the name of the dataset.
  # - dataset: The input dataset containing time series data.
  # - windows: An integer specifying the number of windows used for sequence detection.
  # - output_dir: (Optional) A character string specifying the directory where the RDS file
  #   will be saved. The default value is the current working directory (getwd()).
  # - save_file: (Optional) A logical value indicating whether to save the resulting data frame as an
  #   RDS file. If set to TRUE (default), the data frame is saved; if set to FALSE, it won't be saved.
  #
  # Example Usage:
  # To apply the function and save the result as "gecco_sample_windows_5.RDS":
  # result <- kmeans_windows_detected_optimized('gecco_sample', gecco_sample, 5,
  #                                             '/Users/myaccount/Desktop/Teste_folder', save_file = TRUE)
  #
  # To apply the function without saving the result as an RDS file:
  # result <- kmeans_windows_detected_optimized('gecco_sample', gecco_sample, 5,
  #                                             '/Users/myaccount/Desktop/Teste_folder', save_file = FALSE)
  #
  # The result can be accessed as "result" and contains the detected sequences in a structured format.
  #
  # Note:
  # The 'event' column is automatically skipped during processing to avoid being treated as a series.
  # The function handles cases where there are no valid sequences and pads shorter sequences with NA values.
  
  colnames_list <- colnames(dataset)
  
  results <- list()
  
  for (serie in colnames_list) {
    if (serie == 'event') {
      next  # Skip processing the 'event' column
    }
    
    tryCatch({
      
      model <- hanct_kmeans(seq = windows)
      fitted_model <- fit(model, dataset[, serie])
      detection <- detect(fitted_model, dataset[, serie])
      seq_window <- detection %>% filter(event == TRUE)
      
      seq_lengths <- seq_window$seqlen
      seq_indices <- seq_window$idx
      
      if (length(seq_lengths) == 0) {
        # Handle the case where there are no valid sequences
        df <- data.frame(
          name_Dataset = character(0),
          serie = character(0),
          i = integer(0),
          windows = integer(0)
        )
        for (i in 1:windows) {
          df[paste0("idx", i)] <- integer(0)
        }
      } else {
        max_seq_length <- max(seq_lengths)
        
        # Pad the shorter sequences with NA values to make them all the same length
        idx_columns <- lapply(1:windows, function(i) {
          if (i <= max_seq_length) {
            seq_indices + (i - 1)
          } else {
            rep(NA, length(seq_indices))
          }
        })
        
        col_names <- paste0("idx", 1:windows)
        
        df <- data.frame(
          name_Dataset = rep(name_Dataset, length(seq_lengths)),
          serie = rep(serie, length(seq_lengths)),
          i = 1:length(seq_lengths),
          windows = rep(windows, length(seq_lengths)),
          setNames(data.frame(idx_columns), col_names)
        )
      }
      
      results[[serie]] <- df
    }, error = function(e) {
      cat(paste("Error processing serie", serie, ": ", e$message, "\n"))
      # You can choose to skip this serie and continue the loop or take other actions.
    })
  }
  
  final_df <- do.call(rbind, results)
  
  if (save_file) {
    # Generate the filename based on name_Dataset and windows
    file_name <- paste0(name_Dataset, "_windows_", windows, ".RDS")
    
    # Construct the full filepath
    file_path <- file.path(output_dir, file_name)
    
    # Save the result as an RDS file
    saveRDS(final_df, file = file_path)
  }
  
  return(final_df)
}
