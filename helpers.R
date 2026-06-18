# ---- UI ----
scroll_box <- function(..., height = "800px", collapsed = FALSE) {
  box(...,
      collapsible = TRUE,
      collapsed = collapsed,
      style = paste0(
        "overflow-y: auto;", 
        "max-height: ", height
      ))
}
nav_buttons <- function(prev_id = NULL, next_id = NULL,
                        prev_label = "Previous", next_label = "Next") {
  div(style = "
    position: sticky;
    bottom: 0;
    width: 100%;
    background-color:rgba(255,255,255,0);
    padding: 10px 30px;
    border-top: 1px solid #ddd;
    display: flex;
    justify-content: flex-end;
    gap: 20px;
    z-index: 1000;
  ",
    if (!is.null(prev_id)) actionButton(
      prev_id, prev_label, icon = icon("arrow-left"),
      style = "font-size:18px; padding:10px 20px; background-color:#6c757d; color:white;"
    ),
    if (!is.null(next_id)) actionButton(
      next_id, next_label, icon = icon("arrow-right"),
      style = "font-size:18px; padding:10px 20px; background-color:#007bff; color:white;"
    )
  )
}
# ---- Filtering ----
detect_feature_outliers <- function(df,
                                    method = c("IQR", "Z-score", "MAD", "Missing rate"), 
                                    na_threshold = 0.2,
                                    outlier_prop_threshold = 0.1,
                                    iqr_multiplier = 1.5) {
  method <- match.arg(method)
  outlier_features <- c()
  
  for (feature in colnames(df)) {
    values <- df[[feature]]
    
    if (method == "Missing") {
      na_prop <- mean(is.na(values))
      if (na_prop > na_threshold) {
        outlier_features <- c(outlier_features, feature)
      }
      
    } else if (method == "IQR") {
      q1 <- quantile(values, 0.25, na.rm = TRUE)
      q3 <- quantile(values, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower <- q1 - iqr_multiplier * iqr
      upper <- q3 + iqr_multiplier * iqr
      outlier_prop <- mean(values < lower | values > upper, na.rm = TRUE)
      if (outlier_prop > outlier_prop_threshold) {
        outlier_features <- c(outlier_features, feature)
      }
      
    } else if (method == "Z-score") {
      z_scores <- scale(values)
      outlier_prop <- mean(abs(z_scores) > 3, na.rm = TRUE)
      if (outlier_prop > outlier_prop_threshold) {
        outlier_features <- c(outlier_features, feature)
      }
      
    } else if (method == "MAD") {
      med <- median(values, na.rm = TRUE)
      mad_val <- mad(values, constant = 1, na.rm = TRUE)
      outlier_prop <- mean(abs(values - med) > (3 * mad_val), na.rm = TRUE)
      if (outlier_prop > outlier_prop_threshold) {
        outlier_features <- c(outlier_features, feature)
      }
    }
  }
  
  return(outlier_features)
}

# ---- Imputation ----
plot_missing_heatmap_fast <- function(data, show_feature = TRUE, top_n = 40) {
  # Logical missing matrix
  mat <- is.na(as.matrix(data))
  rownames(mat) <- rownames(data)  # samples
  colnames(mat) <- colnames(data)  # lipids
  
  # Count missing per lipid
  missing_counts <- colSums(mat)
  
  # Select top_n lipids
  top_features <- names(sort(missing_counts, decreasing = TRUE))[1:min(top_n, ncol(mat))]
  mat <- mat[, top_features, drop = FALSE]
  
  # Convert to numeric (0/1)
  mat_num <- 1 * mat   # keep same orientation: samples × lipids
  
  # Hover text matrix, same dimension as mat_num
  hover_text <- matrix(
    paste0(
      "Sample: ", rep(rownames(mat), times = ncol(mat)), "<br>",
      "Lipid: ", rep(colnames(mat), each = nrow(mat)), "<br>",
      "Missing: ", as.vector(mat)
    ),
    nrow = nrow(mat), ncol = ncol(mat), byrow = FALSE
  )
  
  # Plotly heatmap
  p <- plot_ly(
    x = colnames(mat),          
    y = rownames(mat),          
    z = mat_num,
    type = "heatmap",
    text = hover_text,
    hoverinfo = "text",
    colorscale = list(list(0, "white"), list(1, "red")),
    showscale = FALSE,
    zmin = 0,
    zmax = 1
  ) %>%
    layout( 
      title = paste("Missing Value Heatmap (Top", length(top_features), "Features)"),
      xaxis = list(title = "Lipids", showticklabels = show_feature,tickangle = 45, automargin = TRUE),
      yaxis = list(title = "Samples", automargin = TRUE)
    )
  
  return(p)
}

detect_group_missing_threshold <- function(df, metadata, group_var, threshold = 0.95) {
  # df: data frame with samples as rows and lipids as columns
  # metadata: data frame with samples as rows and group info
  # group_var: column name in metadata that defines groups
  # threshold: fraction of missing values to flag a lipid (default = 0.95)
  
  groups <- unique(metadata[[group_var]])
  result_list <- list()
  for (lipid in colnames(df)) {
    group_missing <- sapply(groups, function(g) {
      samples_in_group <- rownames(metadata[metadata[[group_var]] == g, ,drop = F])
      sub_values <- df[samples_in_group, lipid, drop = TRUE]
      frac_missing <- sum(is.na(sub_values)) / length(sub_values)
      return(frac_missing >= threshold)
    })
    names(group_missing) <- groups  # make sure names are set
    if (any(group_missing)) {
      result_list[[lipid]] <- names(group_missing[group_missing == TRUE])
    }
  }
  
  return(result_list)
}

# drop all-NA columns with notification
drop_all_na_columns_df <- function(df, numeric_cols) {
  
  numeric_cols <- numeric_cols[!is.na(numeric_cols)]
  numeric_cols <- intersect(numeric_cols, colnames(df))
  
  if (length(numeric_cols) == 0) return(df)
  
  # find 100% NA
  all_na_cols <- names(
    which(sapply(df[, numeric_cols, drop = FALSE], function(x) all(is.na(x))))
  )

  if (length(all_na_cols) > 0) {
    
    shown <- if (length(all_na_cols) <= 10) {
      paste(all_na_cols, collapse = ", ")
    } else {
      paste(paste(all_na_cols[1:10], collapse = ", "), "...")
    }
    
    msg <- paste0(
      "These columns contain 100% missing values and were automatically removed: ",
      shown
    )
    
    showNotification(msg, type = "warning", duration = 10)
    
    df <- df[, setdiff(colnames(df), all_na_cols), drop = FALSE]
  }
  
  df
}



impute_missing_values <- function(data, 
                                  method = c("knn-featurewise",
                                                   "knn-samplewise", 
                                                   "mean", 
                                                   "median", 
                                                   "LoD 1/5 minimum value",
                                                    "LoD 1/2 minimum value"), 
                                  k = 5) {
  method <- match.arg(method)
  original_rownames <- rownames(data)  # Save rownames
  
  dt <- as.data.table(data)
  
  numeric_cols <- names(dt)[sapply(dt, is.numeric)]

  if (method %in% c("mean","median","LoD 1/5 minimum value","LoD 1/2 minimum value")) {
  if (method == "mean") {
    
    dt[, (numeric_cols) := lapply(.SD, function(x) {
      replace(x, is.na(x), mean(x, na.rm = TRUE))
    }), .SDcols = numeric_cols]
    
  } else if (method == "median") {
    dt[, (numeric_cols) := lapply(.SD, function(x) {
      replace(x, is.na(x), median(x, na.rm = TRUE))
    }), .SDcols = numeric_cols]
    
  } else if (method == "LoD 1/5 minimum value") {
    dt[, (numeric_cols) := lapply(.SD, function(x) {
      replace(x, is.na(x), min(x, na.rm = TRUE) / 5)
    }), .SDcols = numeric_cols]
  }
  else if (method == "LoD 1/2 minimum value") {
    dt[, (numeric_cols) := lapply(.SD, function(x) {
      replace(x, is.na(x), min(x, na.rm = TRUE) / 2)
    }), .SDcols = numeric_cols]
  }
    result <- as.data.frame(dt)
    rownames(result) <- original_rownames
    return(result)
  }
  
  # 2. KNN FEATUREWISE  → check missing % per FEATURE (column)
  if (method == "knn-featurewise") {
    
    # missing % per column (per feature)
    col_missing <- sapply(dt[, ..numeric_cols], function(x) mean(is.na(x)))
    bad_cols <- names(col_missing[col_missing > 0.80])
    good_cols <- setdiff(numeric_cols, bad_cols)

    
    if (length(bad_cols) > 0) {
      showNotification(
        paste("These features have >80% missing values and will be LoD-imputed (min/5) instead of KNN:", 
              paste(bad_cols, collapse = ", ")),
        type = "warning", duration = 10
      )
      
      dt[, (bad_cols) := lapply(.SD, function(x) {
        if (all(is.na(x))) return(x)   # avoid min() error
        lod <- min(x, na.rm = TRUE) / 5
        replace(x, is.na(x), lod)
      }), .SDcols = bad_cols]
    }
    
    # KNN only on remaining columns
    if (length(good_cols) > 0) {
      df <- as.data.frame(dt)
      mat <- t(as.matrix(df[, good_cols, drop = FALSE]))
      imputed <- impute::impute.knn(mat, k = k)
      
      df[, good_cols] <- as.data.frame(t(imputed$data))
    }
    rownames(df) <- original_rownames
    return(df)
  }
  
  # 3. KNN SAMPLEWISE → check missing % per SAMPLE (row)
  if (method == "knn-samplewise") {
    
    # missing % per row (per sample)
    row_missing <- apply(dt[, ..numeric_cols], 1, function(x) mean(is.na(x)))
    bad_rows <- names(row_missing[row_missing > 0.80])
    good_rows <- setdiff(rownames(dt), bad_rows)
    
    # Check missing % per feature (column)
    col_missing <- sapply(dt[, ..numeric_cols], function(x) mean(is.na(x)))
    bad_cols <- names(col_missing[col_missing > 0.80])
    
    # Step A: LoD-impute sparse samples
    if (length(bad_rows) > 0) {
      showNotification(
        paste("These samples have >80% missing values and will be LoD-imputed (min/5) instead of KNN:", 
              paste(bad_rows, collapse = ", ")),
        type = "warning", duration = 10
      )
      
      for (r in bad_rows) {
        row_vec <- unlist(dt[r, ..numeric_cols])
        if (!all(is.na(row_vec))) {
          lod <- min(row_vec, na.rm = TRUE) / 5
          row_vec[is.na(row_vec)] <- lod
        }
        dt[r, (numeric_cols) := as.list(row_vec)]
      }
    }
    
    # Step B: LoD-impute sparse features (avoid column >80% error)
    if (length(bad_cols) > 0) {
      showNotification(
        paste("These features have >80% missing and will be LoD-imputed (min/5) instead of KNN:", 
              paste(bad_cols, collapse = ", ")),
        type = "warning", duration = 10
      )
      
      dt[, (bad_cols) := lapply(.SD, function(x) {
        if (all(is.na(x))) return(x)
        lod <- min(x, na.rm = TRUE) / 5
        replace(x, is.na(x), lod)
      }), .SDcols = bad_cols]
    }
    
    # Step C: KNN only on the good rows
    if (length(good_rows) > 0) {
      df <- as.data.frame(dt)
      mat <- as.matrix(df[good_rows, numeric_cols, drop = FALSE])
      imputed <- impute::impute.knn(mat, k = k)
      
      df[good_rows, numeric_cols] <- as.data.frame(imputed$data)
    }
    rownames(df) <- original_rownames
    return(df)
  }
}

# ---- Parsing ----
clean_names <- function(names) {
  # Step 0: Remove common internal standard keywords early (case-insensitive)
  names <- stringr::str_remove_all(
    names,
    "(?i)(\\bIS\\b|\\bISTD\\b|\\binternal standard\\b|\\bIS[_-]?|\\bISTD[_-]?|internal standard[_-]?)"
  )
  
  # Step 1: Remove the parentheses and replace the first parentheses with a space
  names <- gsub("\\(", " ", names)
  names <- gsub("\\)", "", names)
  
  # remove all the spaces at the end of the string
  names <- gsub("\\s+$", "", names)
  # remove all the spaces at front
  names <- gsub("^\\s+", "", names)
  names <- toupper(names)
  return(names)
}

detect_chain <- function(name){
  # Extract chain info (after lipid class)
  chains <- stringr::str_extract_all(name, "[0-9]+:[0-9]+")[[1]]
  # return True if any chain info is found
  return(length(chains) > 0)
}


lipid_lookup <- readRDS("www/lipid_lookup.rds")


parse_name <- function(names, IS=F){
  # step 1: identify the lipid class by separating the blank
  # step 2: identify whether it has one/two chain information,by identifying the slash
  # step 3: identify the chain information by separating the colon for each chain
  
  
  parse_lipid <- function(name,IS=F) {
    # Identify lipid class (first part before space or digit)
    # Extract carbon and unsaturation numbers
    parse_chain <- function(chain) {
      if (!is.na(chain)) {
        as.numeric(stringr::str_split(chain, ":")[[1]])
      } else {
        c(NA, NA)
      }
    }
    
    if (!detect_chain(name) & IS == F) {
      #name trim white space and to lower
      name <- tolower(gsub("\\s+$", "", name))
      name <- gsub("^\\s+", "", name)
      
      
      result <- lipid_lookup[[name]]
      name_2 <- if (!is.null(result)) result else name
    } else {
      name_2 <- name
    }
    
    
    # remove O- p-, if theres is any in name_2
    # adding this code above is to clearly identify the lipids when O- is at front.
    name_class <- gsub("O-|P-", "", name_2)
    name_class <- clean_names(name_class)
    name_class <- gsub("O-|P-", "", name_2)
    name_class <- clean_names(name_class)
    lipid_class <- stringr::str_extract(name_class, "^[A-Z-]+")
    if (is.na(lipid_class)) {
      lipid_class <- name_class
    }
    lipid_classes_with_number <- c(
      "HEX4CER", "HEX3CER", "HEX2CER",
      "PIP3","PIP2",
      "GM3", "GM2", "GM1",
      "GD3", "GD2", "GD1"
    )
    
    match_idx <- which(stringr::str_detect(name_class, lipid_classes_with_number))[1]
    
    if (!is.na(match_idx)) {
      lipid_class <- lipid_classes_with_number[match_idx]
    }
    
    
    if (grepl("O-", name_2)) {
      lipid_class <- paste(lipid_class, "O-")
    } else if (grepl("P-", name_2)) {
      lipid_class <- paste(lipid_class, "P-")
    }
    
    if (lipid_class =="FFA"){
      lipid_class = "FA"
    }
    
    if (lipid_class == "TAG"){
      lipid_class = "TG"
    }
    
    if (lipid_class == "DAG"){
      lipid_class = "DG"
    }
    if (lipid_class == "MAG"){
      lipid_class = "MG"
    }
    if (lipid_class == "CER") {
      lipid_class = "Cer"
    }
    
    if (lipid_class %in% c("LCER", "LACCER")) {
      lipid_class = "LacCer"
    }
    
    if (lipid_class %in% c("HCER", "HEXCER")) {
      lipid_class = "HexCer"
    }
    
    if (lipid_class %in% c("DCER", "DHCER")){
      lipid_class = "dhCer"
    }
    
    if (lipid_class == "COA"){
      lipid_class = "CoA"
    }
    
    if (IS) {
      return(data.frame(
        Name = name,
        `Lipid class` = lipid_class,
        `Chain1` = NA, `Chain1 unsaturation` = NA,
        `Chain2` = NA, `Chain2 unsaturation` = NA,
        `Chain3` = NA, `Chain3 unsaturation` = NA,
        `Total carbon` = NA,
        `Total unsaturation` = NA
      ))
    }
    # Lipid structures like TG 52:3 (FA 16:0) only have total carbon and unsaturation, 
    # but no specific chain info for all three acyl chains.
    if (!is.na(lipid_class) && lipid_class == "TG" && grepl("FA", name_2)){
      chains <- stringr::str_extract_all(name_2, "[0-9]+:[0-9]+")[[1]]
      total_chain <- chains[1]
      if (length(chains) < 2) {
        sn1_split <- c(NA, NA)
      } else {
        sn1_split <- parse_chain(chains[2])
      }
      total_chain_split <- parse_chain(total_chain)
      table <- data.frame(
        `Name` = name_2,
        `Lipid class` = lipid_class,
        `Chain1` = sn1_split[1],
        `Chain1 unsaturation` = sn1_split[2],
        `Chain2` = NA,
        `Chain2 unsaturation` = NA,
        `Chain3` = NA,
        `Chain3 unsaturation` = NA,
        `Total carbon` = total_chain_split[1],
        `Total unsaturation` = total_chain_split[2]
      )
      
    }
    else if (!is.na(lipid_class) 
             && (lipid_class == "SM" | lipid_class == "Cer" | lipid_class == "HexCer" | lipid_class == "LacCer")){
      chains <- stringr::str_extract_all(name_2, "[0-9]+:[0-9]+")[[1]]
      chain1 <- parse_chain(chains[1])
      if (length(chains) >= 2) {
        chain2 <- parse_chain(chains[2])
        total_c <- chain1[1] + chain2[1]
        total_db <- chain1[2] + chain2[2]
      }
      else{ 
        chain2 <- c(NA, NA)
        if (chain1[1] >= 30) {
          if (chain1[2] >= 1){
            total_c <- chain1[1]
            total_db <- chain1[2] 
            chain1[1] <- 18
            chain1[2] <- 1
            chain2[1] <- total_c - 18 
            chain2[2] <- total_db - 1
            }
          else {
            total_c <- chain1[1]
            total_db <- chain1[2] 
            chain1[1] <- NA
            chain1[2] <- NA
            }
        }
        else{
          chain2[1] <- chain1[1]
          chain2[2] <- chain1[2] 
          chain1[1] <- 18
          chain1[2] <- 1
          total_c <- chain1[1] + chain2[1]
          total_db <- chain1[2] + chain2[2]
        }
      }
      table <- data.frame(
        `Name` = name_2,
        `Lipid class` = lipid_class,
        `Chain1` = chain1[1],
        `Chain1 unsaturation` = chain1[2],
        `Chain2` = chain2[1],
        `Chain2 unsaturation` = chain2[2],
        `Chain3` = NA,
        `Chain3 unsaturation` = NA,
        `Total carbon` = total_c,
        `Total unsaturation` = total_db
      )
     
      
    }
    
    else if (!is.na(lipid_class) 
            && (lipid_class == "dhCer")){
      chains <- stringr::str_extract_all(name_2, "[0-9]+:[0-9]+")[[1]]
      chain1 <- parse_chain(chains[1])
      if (length(chains) >= 2) {
        chain2 <- parse_chain(chains[2])
        total_c <- chain1[1] + chain2[1]
        total_db <- chain1[2] + chain2[2]
      }
      else{ 
        chain2 <- c(NA, NA)
        if (chain1[1] >= 30) {
          total_c <- chain1[1]
          total_db <- chain1[2] 
          chain1[1] <- 18
          chain1[2] <- 0
          chain2[1] <- total_c - 18 
          chain2[2] <- total_db - 0
        }
        else{
          chain2[1] <- chain1[1]
          chain2[2] <- chain1[2] 
          chain1[1] <- 18
          chain1[2] <- 0
          total_c <- chain1[1] + chain2[1]
          total_db <- chain1[2] + chain2[2]
        }
      }
      table <- data.frame(
        `Name` = name_2,
        `Lipid class` = lipid_class,
        `Chain1` = chain1[1],
        `Chain1 unsaturation` = chain1[2],
        `Chain2` = chain2[1],
        `Chain2 unsaturation` = chain2[2],
        `Chain3` = NA,
        `Chain3 unsaturation` = NA,
        `Total carbon` = total_c,
        `Total unsaturation` = total_db
      )
    }
    else if (!is.na(lipid_class) 
             && 
             (lipid_class %in% c("DG","PC","PC O-","PC P-","PE", "PE O-","PE P-","PI","PS","PG","PA","TG","TG O-","TG P-")))
              {
      # Extract chain info (after lipid class)
      chains <- stringr::str_extract_all(name_2, "[0-9]+:[0-9]+")[[1]]
      
      chain1 <- parse_chain(chains[1])
      
      # only total chain is reported, no specific sn1/sn2 info
      if (length(chains) == 1){
        
        total_c <- chain1[1] 
        total_db <- chain1[2]
        chain1 <-  c(NA, NA)
        chain2 <- c(NA, NA)
        chain3 <- c(NA,NA)
        table <- data.frame(
          Name = name,
          `Lipid class` = lipid_class,
          `Chain1` = chain1[1],
          `Chain1 unsaturation` = chain1[2],
          `Chain2` = chain2[1],
          `Chain2 unsaturation` = chain2[2],
          `Chain3` = chain3[1],
          `Chain3 unsaturation` = chain3[2],
          `Total carbon` = total_c,
          `Total unsaturation` = total_db
        ) 
      }
      else{
        chain1 <- parse_chain(chains[1])
        chain2 <- if (length(chains) >= 2) parse_chain(chains[2]) else c(NA, NA)
        chain3 <- if (length(chains) >= 3) parse_chain(chains[3]) else c(NA, NA)
        
        table <- data.frame(
          Name = name,
          `Lipid class` = lipid_class,
          `Chain1` = chain1[1],
          `Chain1 unsaturation` = chain1[2],
          `Chain2` = chain2[1],
          `Chain2 unsaturation` = chain2[2],
          `Chain3` = chain3[1],
          `Chain3 unsaturation` = chain3[2],
          `Total carbon` = sum(chain1[1], chain2[1], chain3[1],na.rm = TRUE),
          `Total unsaturation` = sum(chain1[2], chain2[2],chain3[2],na.rm = TRUE)
        ) 
      }
 
    }
    else{
      
      
      # Extract chain info (after lipid class)
      chains <- stringr::str_extract_all(name_2, "[0-9]+:[0-9]+")[[1]]
      
      chain1 <- parse_chain(chains[1])
      chain2 <- if (length(chains) >= 2) parse_chain(chains[2]) else c(NA, NA)
      chain3 <- if (length(chains) >= 3) parse_chain(chains[3]) else c(NA, NA)
      
      if (length(chains) == 1){
        chain2 <- c(NA, NA)
        chain3 <- c(NA,NA)
      }
      else if(length(chains)==2){
        chain3 <- c(NA,NA)
      }
      
      table <- data.frame(
        Name = name,
        `Lipid class` = lipid_class,
        `Chain1` = chain1[1],
        `Chain1 unsaturation` = chain1[2],
        `Chain2` = chain2[1],
        `Chain2 unsaturation` = chain2[2],
        `Chain3` = chain3[1],
        `Chain3 unsaturation` = chain3[2],
        `Total carbon` = sum(chain1[1], chain2[1], chain3[1],na.rm = TRUE),
        `Total unsaturation` = sum(chain1[2], chain2[2],chain3[2],na.rm = TRUE)
      )}
  }
  
  # Apply function to all names
  result <- do.call(rbind, lapply(names, parse_lipid,IS=IS))
  # remove the duplicate rows
  result <- result[!duplicated(result), ]
  
  # initialize clean_name with NA
  result$Clean.Name <- NA
  
  # rule for TAG
  tag_idx <- (result$Lipid.class == "TG" & grepl("FA", result$Name))
  
  result$Clean.Name[tag_idx] <- paste0(result$Lipid.class[tag_idx], " ",
                                       result$Total.carbon[tag_idx], ":",
                                       result$Total.unsaturation[tag_idx])
  # rules for only total available idx
  total_available_idx <- (is.na(result$Chain1)&is.na(result$Chain2)&is.na(result$Chain3))
  result$Clean.Name[total_available_idx] <- paste0(result$Lipid.class[total_available_idx], " ",
                                       result$Total.carbon[total_available_idx], ":",
                                       result$Total.unsaturation[total_available_idx])
  
  # rule for non-TAG
  non_tag_idx <- !tag_idx & !total_available_idx
  idx <- non_tag_idx
  result$Clean.Name[idx] <- paste0(result$Lipid.class[idx], " ",
                                   result$Chain1[idx], ":", result$Chain1.unsaturation[idx])
  
  has_sn2 <- !is.na(result$Chain2[idx])
  result$Clean.Name[idx][has_sn2] <- paste0(result$Clean.Name[idx][has_sn2], "/",
                                            result$Chain2[idx][has_sn2], ":",
                                            result$Chain2.unsaturation[idx][has_sn2])
  # repeat for Chain3
  has_sn3 <- !is.na(result$Chain3[idx])
  result$Clean.Name[idx][has_sn3] <- paste0(result$Clean.Name[idx][has_sn3], "/",
                                            result$Chain3[idx][has_sn3], ":",
                                            result$Chain3.unsaturation[idx][has_sn3])
  
  return(result)
}

# ---- Combine ----
insert_combine_rule_ui <- function(id,parsedData_df) {
  insertUI(
    selector = "#CombineButton",
    where = "beforeBegin",
    ui = tags$div(
      id = paste0("combine_rule_", id),
      fluidRow(
        column(5,
               selectInput(paste0("lipid_class_", id),
                           "Select Lipid Class:",
                           choices = c(
                             "Combine all duplicated lipids based on clean names in the parse table",
                             sort(unique(parsedData_df$Lipid.class))
               ))
        ),
        column(3,
               selectInput(paste0("combine_method_", id),
                           "Combine Method:",
                           choices = c("mean", "median", "sum", "max","min"),
                           selected = "sum")
        ),
        column(2,
               actionButton(paste0("remove_rule_", id), 
                            label = HTML('<i class="fa fa-times" style="color:white;"></i>'), 
                            class = "btn-danger", style = "margin-top: 25px;")
        )
      )
    )
  )
}


create_lipid_boxplot <- function(sample_data, parse_table) {
  # Load required libraries
  require(ggplot2)
  require(tidyr)
  require(dplyr)
  
  # Assign lipid classes based on parse_table
  sample_class <- parse_table$Lipid_class
  sample_class <- sample_class[match(colnames(sample_data), parse_table$name)]
  
  # Merge class info with sample data
  sample_with_class <- rbind(sample_class, sample_data)
  rownames(sample_with_class) <- c("Lipid_class", rownames(sample_with_class)[-1])
  
  # Transpose and convert to dataframe
  data <- as.data.frame(t(sample_with_class))
  data$Lipid_class <- as.factor(data$Lipid_class)
  
  # Reshape to long format
  data_long <- data %>%
    tidyr::pivot_longer(
      cols = -Lipid_class,
      names_to = "Sample_Type",
      values_to = "Value"
    ) %>%
    dplyr::select(Lipid_class, Value)
  
  # Clean data
  data_long <- na.omit(data_long)
  
  # Split values by lipid class
  split_set <- split(data_long$Value, data_long$Lipid_class)
  
  # Convert to numeric and create plotting dataframe
  df <- data.frame(
    value = as.numeric(unlist(split_set, use.names = FALSE)),
    group = rep(names(split_set), times = lengths(split_set)))
  
  n_classes <- length(unique(df$group))
  # Use Polychrome package (install first)
  color_palette <- unname(Polychrome::green.armytage.colors(n_classes))
  # Create plot with colored boxes
  ggplot(df, aes(x = group, y = log2(value), fill = group)) +
    
    geom_boxplot() +
    labs(x = "Lipid Class", y = "log2(Value)", title = "Lipid Class Boxplot") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(
      name = "Lipid Class",
      values = color_palette  # Use custom palette
    ) 
  
}
detect_duplication <- function(parse_data){
  # find duplicated clean names and return corresponding Name(s)
  duplicated_names <- parse_data$Name[duplicated(parse_data$Clean.Name) | 
                                        duplicated(parse_data$Clean.Name, fromLast = TRUE)]
  
  # sort the duplicated_names
  duplicated_names <- sort(duplicated_names)
  count <- length(duplicated_names)
  message <- paste(duplicated_names, collapse = ", ")
  message <- paste("<b>",
                   count,
                   "potential duplicates or same lipids with different adduct are</b>:<br>",
                   message
                   )
  return(message)
}


combine_duplicated_lipids <- function(dataset, merged_function_string) {
  # Convert string input to function if needed
  merged_function <- match.fun(merged_function_string)
  # Check for duplicated column names
  if (!any(duplicated(colnames(dataset)))) {
    message_no <- "No duplicated column names found."
    return(list(data = dataset, message = message_no))
  }
  
  # Get original column names and unique names in order of first appearance
  original_names <- colnames(dataset)
  duplicated_names <- original_names[duplicated(original_names)]
  unique_ordered <- original_names[!duplicated(original_names)]

  
  # Split columns by name and apply the merged function to duplicates
  merged_cols <- lapply(split.default(dataset, original_names), function(col_group) {
    res <- if (ncol(col_group) > 1) {
      apply(col_group, 1, function(x) merged_function(x, na.rm = TRUE))
    } else {
      col_group[, 1]
    }
    
    res <- as.vector(res)
    names(res) <- rownames(col_group)
    res
  })

  # Create new dataset with ordered columns
  new_dataset <- as.data.frame(merged_cols[unique_ordered])
  colnames(new_dataset) <- unique_ordered
  if (length(unique(duplicated_names)) < 15) {
    dup_message <- paste("Duplicated columns found and merged using", 
                         merged_function_string, ":",
                         paste(unique(duplicated_names), collapse = ", "))
  }
  else{
  # Build message
  dup_message <- paste("Duplicated columns found and merged using", 
                       merged_function_string, ":",
                       paste(unique(duplicated_names)[1:10], collapse = ", "),
                        "...")
  }
  
  return(list(data = new_dataset, message = dup_message))
}

# combine the duplicated lipids based on the total carbon and total unsaturation
swap_clean_name <- function(lipid_class,dataset, lipid_df_samples) {
  #check the columnnames of dataset is the same sequence as the Name column in lipid_df_samples
  if (!all(colnames(dataset) == lipid_df_samples$Name)) {
    stop("Column names of dataset do not match the Name column in parsing table.")
  }
  # fetch all the lipid that match the lipid_class
  # rename their name with paste0(lipid_class,' ',Total.carbon,Total.unsaturation)
  if (nrow(lipid_df_samples[lipid_df_samples$Lipid.class == lipid_class, ]) == 0) {
    stop(paste("No lipids found for lipid class:", lipid_class))
  }
  idx <- lipid_df_samples$Lipid.class == lipid_class
  lipid_df_samples$Name[idx] <- lipid_df_samples$Clean.Name[idx]
  
  # rename the dataset column names with the new lipid names
  # only selected class get a new clean name
  colnames(dataset) <- lipid_df_samples$Name
 
  return(list(data = dataset, lipid_df_samples = lipid_df_samples))
}


# ---- IS normalization ----
# Find best match for each lipid class
find_best_match <- function(is_classes, sample_classes) {
  # Ensure inputs are not empty
  if (length(is_classes) == 0 || length(sample_classes) == 0) {
    return(data.frame(Lipid_Class_Sample = character(0), Internal_standard = character(0), stringsAsFactors = FALSE))
  }
  
  # Create a dataframe with sample_classes as the first column
  result_df <- data.frame(Lipid_Class_Sample = sample_classes, Internal_standard = NA, stringsAsFactors = FALSE)
  
  # Function to find the best match for a single sample class
  find_match <- function(is_classes, sample_class) {
    sample_class_upper <- sample_class
    is_classes_upper <- is_classes
    
    # Special case: if sample_class is "FFA" and no IS class contains "FFA"
    if (sample_class_upper == "FFA" && !any(grepl("FFA", is_classes_upper))) {
      return("TAG")
    }
    
    # Check for exact match (case insensitive)
    exact_matches <- which(is_classes_upper == sample_class_upper)
    if (length(exact_matches) > 0) {
      return(is_classes[exact_matches[1]])
    }
    
    # Check for longest partial match
    matching_classes <- sapply(is_classes_upper, function(is_class) {
      if (isTRUE(grepl(is_class, sample_class_upper))) {
        return(nchar(is_class))
      } else {
        return(0)
      }
    })
    
    # If any partial matches found, return the one with the longest match
    if (any(matching_classes > 0)) {
      best_match_index <- which.max(matching_classes)
      return(is_classes[best_match_index])
    }
    
    # No match found
    return(NA)
  }
  
  
  # Apply the matching function to each sample class
  result_df$Internal_standard <- sapply(result_df$Lipid_Class_Sample, find_match, is_classes = is_classes)
  
  return(result_df)
}

normalize_by_internal_standard <- function(area_matrix, lipid_df_samples, 
                                           IS_matrix) {
  # Before Using this function, ensure that: 
  #   the colnames of the input are the lipid identifiers
  area_matrix <- t(area_matrix)
  IS_matrix <- t(IS_matrix)
  # Ensure area_matrix has row names
  if (is.null(rownames(area_matrix))) {
    stop("area_matrix must have row names corresponding to lipid identifiers.")
  }
  
  # Extract internal standard class stubs
  istd_names <- rownames(IS_matrix)
  
  # remove the internal standard from the lipid names
  lipid_names <- rownames(area_matrix)
  
  # Initialize normalized_matrix outside the loop to accumulate changes
  normalized_matrix <- area_matrix
  
  # Iterate over each internal standard
  for (istd_name in istd_names) {
    print(paste("Processing internal standard:", istd_name))
    
    lipid_class_stub <- lipid_df_samples$Internal_Standard
    
    istd_class_stub <- parse_name(istd_name)$Lipid.class
    # Find indices of lipids matching the class
    
    class_indices <- which(lipid_class_stub == istd_class_stub)

    
    if (length(class_indices) == 0) {
      warning(paste("No lipids are normalized based on the internal standard:", istd_name))
      next
    }
    
    # Extract internal standard intensity
    istd_intensity <- IS_matrix[rownames(IS_matrix)==istd_name, ]
    
    # Check for zeros or NAs in internal standard intensity
    if (any(is.na(istd_intensity))) {
      warning(paste("NA values found in internal standard intensity for:", istd_name))
    }
    if (any(istd_intensity == 0)) {
      warning(paste("Zero values found in internal standard intensity for:", istd_name))
    }
    
    if (length(istd_intensity) != ncol(area_matrix)) {
      stop(paste("Length of istd_intensity (", length(istd_intensity), 
                 ") does not match number of columns in area_matrix (", 
                 ncol(area_matrix), ")."))
    }
    normalized_matrix[class_indices, ] <- sweep(area_matrix[class_indices, , drop = FALSE], 
                                                2, 
                                                istd_intensity, 
                                                "/")
    
  }
  
  return(as.data.frame(t(normalized_matrix)))
}



lipid_class_normalization <- function(normalized_matrix, lipid_df_samples, method = c("sum", "median", "mean")) {
  method <- match.arg(method)  # ensure only valid options

  normalized_matrix <- t(normalized_matrix)
  
  # Perform aggregation by lipid class
  agg_class_matrix <- aggregate(normalized_matrix, 
                                by = list(lipid_df_samples$Lipid.class), 
                                FUN = method)
  
  # Count lipid classes
  lipid_class_count <- table(lipid_df_samples$Lipid.class)
  lipid_class_count_1 <- names(lipid_class_count[lipid_class_count == 1])
  
  # add a warning
  if (length(lipid_class_count_1) > 0) {
    showNotification(
      paste0(
        "⚠️ Lipid classes with only one lipid detected: ",
        paste(lipid_class_count_1, collapse = ", "),
        ". Lipid class normalization for these classes is skipped to avoid distortion of the data."
      ),
      type = "warning",
      duration = 20
    )
  }
  
  # Avoid dividing by the original value for singleton lipid classes (set to 1)
  agg_class_matrix[agg_class_matrix$Group.1 %in% lipid_class_count_1, -1] <- 1
  
  # Make lipid name → class dictionary
  lipid_class_dict <- setNames(lipid_df_samples$Lipid.class, lipid_df_samples$Name)
  # Class → value vector dict
  class_value_dict <- setNames(as.list(as.data.frame(t(agg_class_matrix[, -1]))), agg_class_matrix[, 1])
  
  # Normalize
  for (lipid in rownames(normalized_matrix)) {
    Lipid.class <- lipid_class_dict[[lipid]]
    class_values <- class_value_dict[[Lipid.class]]
    normalized_matrix[lipid, ] <- normalized_matrix[lipid, ] / class_values
  }
  
  return(as.data.frame(t(normalized_matrix)))
}

# ---- Normalization Plan ----
sample_normalize <- function(mat, method = c("sum", "mean", "median")) {
  method <- match.arg(method)  # restrict to allowed options
  
  # Choose the row-wise normalization factor
  row_stats <- switch(method,
                      sum = rowSums(mat),
                      mean = rowMeans(mat),
                      median = apply(mat, 1, median))
  
  # Normalize each row
  mat <- sweep(mat, 1, row_stats, FUN = "/")
  return(as.data.frame(mat))
}

feature_normalize <- function(mat, method = c("sum", "mean", "median")) {
  method <- match.arg(method)  # restrict to allowed options
  
  # Choose the column-wise normalization factor
  col_stats <- switch(method,
                      sum = colSums(mat),
                      mean = colMeans(mat),
                      median = apply(mat, 2, median))
  
  # Normalize each column
  mat <- sweep(mat, 2, col_stats, FUN = "/")
  return(as.data.frame(mat))
}

quantile_normalize <- function(mat) {
  # MetaboAnalyst expects: features in rows, samples in columns
  # Your input: samples in rows, features in columns
  # => transpose before and after
  
  # Transpose so features = rows, samples = cols
  mat_t <- t(mat)
  
  # Run preprocessCore quantile normalization
  mat_qn <- preprocessCore::normalize.quantiles(as.matrix(mat_t))
  
  # Transpose back to original orientation
  mat_qn <- t(mat_qn)
  
  # Restore row and column names
  rownames(mat_qn) <- rownames(mat)
  colnames(mat_qn) <- colnames(mat)
  
  return(as.data.frame(mat_qn))
}


logit_transform <- function(x) {
  x <- as.data.frame(x)   # ensure it's a data frame
  x <- t(x)               # transpose for per-column operations
  
  epsilon <- 1e-6
  rows_corrected <- c()
  
  # Check each row individually
  for (row in seq_len(nrow(x))) {
    if (any(x[row, ] <= 0 | x[row, ] >= 1, na.rm = TRUE)) {
      rows_corrected <- c(rows_corrected, rownames(x)[row])
      
      # Min-max normalize ONLY this row
      min_val <- min(x[row, ], na.rm = TRUE)
      max_val <- max(x[row, ], na.rm = TRUE)
      
      if (max_val != min_val) {
        x[row, ] <- (x[row, ] - min_val) / (max_val - min_val)
      } else {
        # If all values are identical, set to 0.5 to avoid division by zero
        x[row, ] <- rep(0.5, ncol(x))
      }
    }
  }
  
  # Notify user if any correction was made
  if (length(rows_corrected) > 0) {
    if (length(rows_corrected) <= 10)
    message <-      paste0(
      "⚠️ Out-of-range values for logit transformation detected in ",
      length(rows_corrected), " lipid(s): ",
      paste(rows_corrected, collapse = ", "),
      ". Min-max normalization was applied to those lipids before logit transformation.
        It is probabily because these lipids are the only lipid in their lipid class."
    )
    else if (length(rows_corrected) > 10){
      message <-      paste0(
        "⚠️ Out-of-range values for logit transformation detected in ",
        length(rows_corrected), " lipid(s): ",
        paste(rows_corrected[1:10], collapse = ", "),
        "... Min-max normalization was applied to those lipids before logit transformation.
        It is probabily because these lipids are the only lipid in their lipid class."
      )
    }
    
    showNotification(
      message,
      type = "warning",
      duration = 20
    )
  }
  
  # Clip to avoid 0 and 1 exactly
  x_adj <- pmin(pmax(x, epsilon), 1 - epsilon)
  
  # Apply logit transformation row-wise
  result <- qlogis(x_adj)
  
  return(as.data.frame(t(result)))
}


cuberoot <- function(x) {
  rnames <- rownames(x)
  cnames <- colnames(x)
  x <- as.matrix(x)
  x <- sign(x) * abs(x)^(1/3)
  df <- as.data.frame(x)
  rownames(df) <- rnames
  colnames(df) <- cnames
  return(df)
}


# Mean Centering
mean_center <- function(df) {
  scale(df, center = TRUE, scale = FALSE)
}

# Auto Scaling (Z-score)
auto_scale <- function(df) {
  scale(df, center = TRUE, scale = TRUE)
}

pareto_scale <- function(df) {
  apply(df, 2, function(x) {
    (x - mean(x, na.rm=TRUE)) / sqrt(sd(x, na.rm=TRUE))
  })
}

# Range Scaling
range_scale <- function(df) {
  means <- colMeans(df, na.rm = TRUE)
  ranges <- apply(df, 2, function(x) diff(range(x, na.rm = TRUE)))
  scale(df, center = means, scale = ranges)
}

no_scale <- function(df) {
  df
}

# ---- Plots ----
boxplot_lipid_class <- function(dat, lipid_df_samples,
                                Title =  "Boxplot of Lipid Class Averages Across Samples") {
  dat <- t(dat)
  dat <- as.data.frame(dat)
  
  # dat:samples x lipid (rownames = sample names , colnames =lipid names)
  # lipid_df_samples: lipid annotation table (Name = lipid name, Lipid.class = class)
  # Match lipid info to data rows
  lipid_df_samples <- lipid_df_samples[match(rownames(dat), lipid_df_samples$Name), ]
  dat$Lipid_Class <- lipid_df_samples$Lipid.class
  # Group by lipid class and calculate average intensity per sample
  averaged_by_class <- dat %>%
    group_by(Lipid_Class) %>%
    summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop")
  
  # Convert to long format for ggplot
  df_long <- averaged_by_class %>%
    pivot_longer(
      cols = -Lipid_Class,
      names_to = "Sample",
      values_to = "Mean_Value"
    )
  
  # Generate a palette with enough colors for all lipid classes
  n_classes <- length(unique(df_long$Lipid_Class))
  custom_colors <- colorRampPalette(brewer.pal(12, "Set3"))(n_classes)
  
  # Plot
  ggplot(df_long, aes(x = Lipid_Class, y = Mean_Value, fill = Lipid_Class)) +
    geom_boxplot() +
    labs(x = "Lipid Class", y = "Average Intensity",
         title = Title) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = custom_colors)
}


create_boxplot <- function(df_p, main_title = "Boxplot of Variables") {
  # Convert data to long format
  
  df_long <- stack(df_p)
  
  # Create the boxplot
  temp_plot <- ggplot(df_long, aes(x = ind, y = values)) +
    geom_boxplot(fill = "#2D89C8", color = "black", outlier.shape = NA) +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust = 1)) +
    labs(x = NULL, y = "Value", title = main_title)
  
  return(temp_plot)
}

create_pca_plot <- function(data, groups, 
                            title = "PCA Plot", 
                            width = 800, 
                            center_m = F,
                            scale = F,
                            height = 600,
                            point_size = 3) {
  # if any column has na values. remove that column
  data <- data[, colSums(is.na(data)) == 0]
  
  # Perform PCA
  pca_res <- tryCatch({prcomp(data, center = center_m, scale. = scale)},
                      error = function(e) {
                        showNotification(
                          Paste0("Error in PCA: ",e$message,"Please check if your data has enough features (columns) without NA values for PCA analysis."),
                          type = "error",
                          duration = 10
                        )
                        stop(e)
                      })
  
  # Calculate percentage of variance explained
  variance_explained <- pca_res$sdev^2 / sum(pca_res$sdev^2) * 100
  pc1_var <- round(variance_explained[1], 2)
  pc2_var <- round(variance_explained[2], 2)
  
  # Create dataframe with first two principal components and groups
  pca_df <- as.data.frame(pca_res$x[, 1:2])
 
  pca_df$Group <- factor(groups)
  pca_df$Sample <- rownames(pca_df)   # keep sample IDs
  # Create the plot
  p <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Group)) +
    stat_ellipse(aes(fill = Group), 
                 geom = "polygon", 
                 alpha = 0.2, 
                 color = NA, 
                 type = "norm",
                 show.legend = FALSE) +
    geom_point(aes(text =  paste0("Sample: ", Sample, "<br>Group: ", Group)), size = point_size) +   # label attached here
    theme_minimal() +
    labs(
      title = title, 
      x = paste0("PC1 (", pc1_var, "% variance)"),
      y = paste0("PC2 (", pc2_var, "% variance)")
    )+
    guides(fill = "none")   
  
  p
}

create_pca_plot_3d <- function(data, groups, 
                               title = "3D PCA Plot", 
                               point_size = 5) {
  # Perform PCA
  pca_res <- prcomp(data, center = F, scale. = F)
  
  # Calculate percentage of variance explained
  variance_explained <- pca_res$sdev^2 / sum(pca_res$sdev^2) * 100
  pc1_var <- round(variance_explained[1], 2)
  pc2_var <- round(variance_explained[2], 2)
  pc3_var <- round(variance_explained[3], 2)
  
  # Create dataframe with first three principal components and groups
  pca_df <- as.data.frame(pca_res$x[, 1:3])
  colnames(pca_df) <- c("PC1", "PC2", "PC3")
  pca_df$Group <- factor(groups)
  pca_df$Sample <- rownames(pca_df)
  
  # Create interactive 3D scatter plot
  fig <- plot_ly(
    data = pca_df,
    x = ~PC1,
    y = ~PC2,
    z = ~PC3,
    color = ~Group,
    colors = "Set1",
    text = ~paste("Sample:", Sample,
                  "<br>Group:", Group,
                  "<br>PC1:", round(PC1, 2),
                  "<br>PC2:", round(PC2, 2),
                  "<br>PC3:", round(PC3, 2)),
    hoverinfo = "text",
    marker = list(size = point_size)
  ) %>%
    add_markers() %>%
    layout(
      title = title,
      scene = list(
        xaxis = list(title = paste0("PC1 (", pc1_var, "%)")),
        yaxis = list(title = paste0("PC2 (", pc2_var, "%)")),
        zaxis = list(title = paste0("PC3 (", pc3_var, "%)"))
      )
    )
  
  return(fig)
}


create_pie_plot <- function(dat,lipid_df_samples) {
  dat <- t(dat)
  dat <- as.data.frame(dat)
  # If dat has negative values, show message instead of pie chart
  if (any(dat < 0, na.rm = TRUE)) {
    plotly_pie <- plot_ly() %>%
      layout(
        title = "Lipid Composition",
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        showlegend = FALSE,
        annotations = list(
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          text = "Pie chart not available for negative values",
          showarrow = FALSE,
          font = list(size = 15)
        ),
        # Remove margins so the text is centered nicely
        margin = list(l = 0, r = 0, t = 60, b = 0)
      )
    return(plotly_pie)
  }
  
  # Match lipid info to data rows
  match_idx <- match(rownames(dat), lipid_df_samples$Name)
  
  lipid_df_samples <- lipid_df_samples[match_idx, ]
  dat$Lipid_Class <- lipid_df_samples$Lipid.class
  
  # Group by lipid class and calculate SUM intensity across all samples
  summed_by_class <- dat %>%
    group_by(Lipid_Class) %>%
    summarise(Total_Intensity = sum(across(where(is.numeric)), na.rm = TRUE), .groups = "drop")
  # Generate a palette with enough colors
  n_classes <- length(unique(summed_by_class$Lipid_Class))
  custom_colors <- colorRampPalette(brewer.pal(12, "Set3"))(n_classes)
  
  plotly_pie <- plot_ly(
    data = summed_by_class,
    labels = ~Lipid_Class,
    values = ~Total_Intensity,
    type = 'pie',
    textposition = 'inside',
    textinfo = 'label+percent',
    marker = list(colors = custom_colors, line = list(color = 'white', width = 1)),
    hovertemplate = paste(
      "<b>Lipid Class:</b> %{label}<br>",
      "<b>Total Intensity:</b> %{value:.4f}<br>",
      "<b>Percentage:</b> %{percent}<extra></extra>"
    )
  ) %>%
    layout(title = "Lipid Composition")
  
  return(plotly_pie)
}

create_lipid_bar_plot <- function(lipid_df_sample){
  lipid_df_sample <- lipid_df_sample %>%
    group_by(Lipid.class) %>%
    summarise(Count = n_distinct(Name))%>%
    arrange(desc(Count)) %>%
    mutate(Lipid.class = forcats::fct_reorder(Lipid.class, Count, .desc = TRUE))
  
  #add a sentence of showing the total lipids
  title_0 <- paste0("Total Unique Lipids: ", sum(lipid_df_sample$Count))
  bar_plot <- ggplot(lipid_df_sample, 
                     aes(x = Lipid.class,
                         y = Count, 
                         fill = Lipid.class,
                         text = paste0("Lipid Class: ", Lipid.class, "<br>Count: ", Count))
  ) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = paste0("Number of Unique Lipids per Lipid Class <br><sup>",title_0,"</sup>"),
         x = "Lipid Class",
         y = "Number of Unique Lipids") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  return(bar_plot)
}

plot_group_distribution <- function(df,
                                    group_variable,
                                    title = "Group Distribution") {
  
  if (!group_variable %in% colnames(df)) {
    stop(paste("Column not found:", group_variable))
  }
  
  p <- ggplot(df, aes(x = .data[[group_variable]])) +
    geom_bar(fill = "#E64B35FF", alpha = 0.8) +
    labs(
      title = title,
      x = "Groups",
      y = "Number of Samples"
    ) +
    #show the number
    geom_text(
      stat = "count",
      aes(label = paste0("n = ", after_stat(count))),
      vjust = -0.4,
      size = 4
    ) +
    theme_minimal(base_size = 14)
  return(p)
}

plot_volcano <- function(volcano_data,Title, alpha = 0.05, fold_threshold = 0.5,adj = TRUE) {
  
  
  if (adj){
    volcano_data$neglog10_p_value <- -log10(volcano_data$Adjusted_P_value)
    ylab <- "-log10(Adjusted p-value)"
  }else{
    volcano_data$neglog10_p_value <- -log10(volcano_data$P_value)
    ylab <- "-log10(p-value)"
  }
  
  volcano_data$Significant <- ifelse(volcano_data$neglog10_p_value > -log10(alpha), "Significant", "Not Significant")
  volcano_data$Category <- with(volcano_data, ifelse(Significant == "Significant"
                                                     & (fold_change <= -fold_threshold
                                                        | fold_change >= fold_threshold),
                                                     "Significant & Beyond Threshold",
                                                     ifelse(Significant == "Significant", "Significant", "Not Significant")))
  
  p <- ggplot(volcano_data, 
              aes(x = fold_change, y = neglog10_p_value, color = Category,
                  text = paste0(
                    "Name: ", Name,
                    "<br>Fold Change: ", round(fold_change, 3),
                    "<br>-log10(p): ", round(neglog10_p_value, 3),
                    "<br>p-value: ", signif(P_value, 3)
                  ))) +
    geom_point(alpha = 0.8, size = 1.2) +
    scale_color_manual(values = c("Not Significant" = "grey", "Significant & Beyond Threshold" = "red","Significant" = "black")) +
    theme_classic() +
    theme(legend.position = "none")+
    labs(title = Title,
         x = "Fold Change",
         y = ylab) +
    geom_hline(yintercept = -log10(alpha), linetype = "dashed", color = "blue") +
    geom_vline(xintercept = c(-fold_threshold, fold_threshold), linetype = "dashed", color = "black") +
    annotate("text", x = fold_threshold, y = 0, 
             label = paste0("+", fold_threshold), 
             hjust = -0.1, vjust = -0.5, color = "black", size = 3) +
    annotate("text", x = -fold_threshold, y = 0, 
             label = paste0("-", fold_threshold), 
             hjust = 1.1, vjust = -0.5, color = "black", size = 3) +
    geom_text(
      data = subset(volcano_data, Category == "Significant & Beyond Threshold"),
      aes(label = Name),
      size = 3, color = "black")
  
  p
}

plot_lipid_comparison <- function(lipid_class,
                                  lipid_df,        
                                  metdat,          
                                  lipid_df_sample, 
                                  group_variable,
                                  colors = c("lightblue","pink","indianred3","turquoise4","seagreen"),
                                  plot_type = c("boxplot", "violin plot"),
                                  add_stats = TRUE,
                                  stats_method = c("student", "welch", "wilcox"),
                                  show_jitter = T,
                                  double_bond_range = NULL,
                                  total_carbon_range = NULL,
                                  double_bond_range2=NULL,
                                  total_carbon_range2 = NULL) {
  
  plot_type   <- match.arg(plot_type)
  stats_method <- match.arg(stats_method)
  lipid_df <- as.data.frame(lipid_df)
  metdat2 <- metdat %>%
    tibble::rownames_to_column("Sample")   
  metdat2 <- metdat2[, c("Sample", group_variable), drop = FALSE]
  
  # ---- Step 1: Wide to Long format ----
  df_long <- lipid_df %>%
    tibble::rownames_to_column("Sample") %>%
    tidyr::pivot_longer(-Sample, names_to = "Lipid", values_to = "Value")
  
  # ---- Step 2: join metadata ----
  df_long <- dplyr::left_join(df_long,
                              metdat2,
                              by = "Sample")
  
  # ---- Step 3: Add Lipid annotation ----
  df_long <- dplyr::left_join(df_long,
                              lipid_df_sample[, c("Name", "Lipid.class","Total.carbon","Total.unsaturation")],
                              by = c("Lipid" = "Name"))
  
  
  # ---- Step 4: select class ----
  df_long <- df_long %>% dplyr::filter(Lipid.class == lipid_class)
  title_add <- ''
  # Filter by double bond if provided
  if (!is.null(double_bond_range)) {
    df_long_db <- df_long %>%
      dplyr::filter(Total.unsaturation >= double_bond_range[1],
                    Total.unsaturation <= double_bond_range[2])
    if ( double_bond_range[1] ==  double_bond_range[2]){
      title_add <-  paste0(title_add," double bond =", double_bond_range[1])
    }
    else{
      title_add <-  paste0(title_add," ",double_bond_range[1],"<= double bond <=", double_bond_range[2])
    }
    if (!is.null(double_bond_range2)){
      
      df_long_db1 <- df_long %>%
        dplyr::filter(Total.unsaturation >= double_bond_range2[1],
                      Total.unsaturation <= double_bond_range2[2])
      df_long_db <- rbind(df_long_db, df_long_db1)
      if ( double_bond_range2[1] ==  double_bond_range2[2]){
        title_add <-  paste0(title_add," double bond =", double_bond_range2[1])
      }
      else{
        title_add <-  paste0(title_add," ",double_bond_range2[1],"<= double bond <=", double_bond_range2[2])
      }
    }
    df_long <- df_long_db
  }
  
  # Filter by total carbon if provided
  if (!is.null(total_carbon_range)) {
    df_long_c <- df_long %>%
      dplyr::filter(Total.carbon >= total_carbon_range[1],
                    Total.carbon <= total_carbon_range[2])
    if ( total_carbon_range[1] ==  total_carbon_range[2]){
      title_add <-  paste0(title_add," total carbon =", total_carbon_range[1])
    }
    else{
      title_add <-  paste0(title_add," ",total_carbon_range[1],"<= total carbon number <=", total_carbon_range[2])
    }
    
    if (!is.null(total_carbon_range2)){
      df_long_c1 <- df_long %>%
        dplyr::filter(Total.carbon >= total_carbon_range2[1],
                      Total.carbon <= total_carbon_range2[2])
      df_long_c <- rbind(df_long_c, df_long_c1)
      if ( total_carbon_range2[1] ==  total_carbon_range2[2]){
        title_add <-  paste0(title_add," total carbon =", total_carbon_range2[1])
      }
      else{
        title_add <-  paste0(title_add," ",total_carbon_range2[1],"<= total carbon number <=", total_carbon_range2[2])
      }
    }
    df_long <- df_long_c
  }
  
  # ✅ Check if the data is empty after filtering
  if (nrow(df_long) == 0) {
    showNotification(
      "No data points match the selected criteria. Please adjust the setting.",
      type = "error",
      duration = 6
    )
    return(NULL)  # stop the function early
  }
  
  # ---- Step 5: color ----
  df_long[[group_variable]] <- factor(df_long[[group_variable]])
  n_groups <- length(levels(df_long[[group_variable]]))
  if (length(colors) < n_groups) {
    colors <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(n_groups)
  }
  group_colors <- setNames(colors, levels(df_long[[group_variable]]))
  
  # ---- Step 6: Base plot ----
  p <- ggplot(df_long, aes(x = .data[[group_variable]], y = Value, fill = .data[[group_variable]]))
  
  if (plot_type == "boxplot") {
    p <- p + geom_boxplot(outlier.shape = NA, alpha = 0.7) +
      stat_summary(fun = median, geom = "text",
                   aes(label = round(after_stat(y), 3)), vjust = -1.2) 
      if (show_jitter) {
        p <- p + geom_jitter(
          aes(fill = .data[[group_variable]],
              text = paste0("Sample: ", Sample, "<br>Lipid: ", Lipid)),
          width = 0.2, alpha = 0.2, size = 0.8
        )
      }
  } else if (plot_type == "violin plot") {
    p <- p + geom_violin(trim = FALSE, alpha = 0.7) +
      stat_summary(fun = median, geom = "point", size = 2, color = "black") +
      stat_summary(fun = median, geom = "text",
                   aes(label = round(after_stat(y), 3)), vjust = -1.2, color = "black")
      if (show_jitter) {
        p <- p + geom_jitter(
          aes(fill = .data[[group_variable]],
              text = paste0("Sample: ", Sample, "<br>Lipid: ", Lipid)),
          width = 0.2, alpha = 0.2, size = 0.8
        )
      }
  }
  
  # ---- Step 7: statistical test ----
  if (add_stats && length(levels(df_long[[group_variable]])) == 2) {
    if (stats_method == "student") {
      p <- p + ggpubr::stat_compare_means(method = "t.test",
                                          method.args = list(var.equal = TRUE),
                                          label = "p.format")
    } else if (stats_method == "welch") {
      p <- p + ggpubr::stat_compare_means(method = "t.test",
                                          method.args = list(var.equal = FALSE),
                                          label = "p.format")
    } else if (stats_method == "wilcox") {
      p <- p + ggpubr::stat_compare_means(method = "wilcox.test",
                                          label = "p.format")
    }
  }
  test_method <- ifelse(stats_method == "student", "Student's t-test",
                        ifelse(stats_method == "welch", "Welch's t-test",
                               ifelse(stats_method == "wilcox", "Wilcoxon test", NA)))
  p <- p +
    scale_fill_manual(values = group_colors) +
    labs(title = paste("Comparison of", lipid_class, "lipids between groups with", 
                       test_method,"\n",
                       title_add),
         x = "Group", y = "Expression", fill = "Group") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10))
  return(p)
}

#inidividual comparison
plot_indi_lipid_comparison <- function(lipid_name,
                                       lipid_df,        
                                       metdat,          
                                       lipid_df_sample, 
                                       group_variable,
                                       colors = c("lightblue","pink","indianred3","turquoise4","seagreen"),
                                       plot_type = c("boxplot", "violin plot"),
                                       add_stats = TRUE,
                                       stats_method = c("student", "welch", "wilcox"),
                                       show_jitter = T) {
  
  plot_type   <- match.arg(plot_type)
  stats_method <- match.arg(stats_method)
  lipid_df <- as.data.frame(lipid_df)
  metdat2 <- metdat %>%
    tibble::rownames_to_column("Sample")   
  metdat2 <- metdat2[, c("Sample", group_variable), drop = FALSE]
  
  # ---- Step 1: wide to long ----
  df_long <- lipid_df %>%
    tibble::rownames_to_column("Sample") %>%
    tidyr::pivot_longer(-Sample, names_to = "Lipid", values_to = "Value")
  
  # ---- Step 2: join metadata----
  df_long <- dplyr::left_join(df_long,
                              metdat2,
                              by = "Sample")
  
  # ---- Step 3: Add Lipid annotation ----
  df_long <- dplyr::left_join(df_long,
                              lipid_df_sample[, c("Name","Lipid.class")],
                              by = c("Lipid" = "Name"))
  
  
  # ---- Step 4: select class ----
  df_long <- df_long %>% dplyr::filter(Lipid == lipid_name)
  
  
  # ---- Step 5: color ----
  df_long[[group_variable]] <- factor(df_long[[group_variable]])
  n_groups <- length(levels(df_long[[group_variable]]))
  if (length(colors) < n_groups) {
    colors <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(n_groups)
  }
  group_colors <- setNames(colors, levels(df_long[[group_variable]]))
  
  # ---- Step 6: Base plot ----
  p <- ggplot(df_long, aes(x = .data[[group_variable]], y = Value, fill = .data[[group_variable]]))
  
  if (plot_type == "boxplot") {
    p <- p + geom_boxplot(outlier.shape = NA, alpha = 0.7) +
      stat_summary(fun = median, geom = "text",
                   aes(label = round(after_stat(y), 3)), vjust = -1.2) 

    if (show_jitter) {
      p <- p + geom_jitter(
        aes(fill = .data[[group_variable]],
            text = paste0("Sample: ", Sample, "<br>Lipid: ", Lipid)),
        width = 0.2, alpha = 0.2, size = 0.8
      )
    }
  } else if (plot_type == "violin plot") {
    p <- p + geom_violin(trim = FALSE, alpha = 0.7) +
      stat_summary(fun = median, geom = "point", size = 2, color = "black") +
      stat_summary(fun = median, geom = "text",
                   aes(label = round(after_stat(y), 3)), vjust = -1.2, color = "black")
    if (show_jitter) {
                     p <- p + geom_jitter(
                       aes(fill = .data[[group_variable]],
                           text = paste0("Sample: ", Sample, "<br>Lipid: ", Lipid)),
                       width = 0.2, alpha = 0.2, size = 0.8
                     )
                   }
  }
  
  # ---- Step 7: Statistical testing ----
  if (add_stats && length(levels(df_long[[group_variable]])) == 2) {
    if (stats_method == "student") {
      p <- p + ggpubr::stat_compare_means(method = "t.test",
                                          method.args = list(var.equal = TRUE),
                                          label = "p.format")
    } else if (stats_method == "welch") {
      p <- p + ggpubr::stat_compare_means(method = "t.test",
                                          method.args = list(var.equal = FALSE),
                                          label = "p.format")
    } else if (stats_method == "wilcox") {
      p <- p + ggpubr::stat_compare_means(method = "wilcox.test",
                                          label = "p.format")
    }
  }
  test_method <- ifelse(stats_method == "student", "Student's t-test",
                        ifelse(stats_method == "welch", "Welch's t-test",
                               ifelse(stats_method == "wilcox", "Wilcoxon test", NA)))
  p <- p +
    scale_fill_manual(values = group_colors) +
    labs(title = paste("Comparison of", lipid_name, " between groups with", test_method),
         x = "Group", y = "Expression", fill = "Group") +
    theme_minimal()
  
  return(p)
}

# ---- Create DMLH ----
create_LCHplot_single <- function(label_text = TRUE, 
                                  stub, 
                                  group_selection,
                                  lipid_df_samples,
                                  range_min = -1, 
                                  range_max =1,
                                  font_size =2.2,
                                  title_size = 15,
                                  text_x_size = 12,
                                  text_y_size = 12,
                                  title_x_size = 16,
                                  title_y_size = 16,
                                  legend_title_size=14,
                                  legend_text_size=12,
                                  sequence = "Based on total carbon number",
                                  color_scheme = list(low = "blue", mid = "white", high = "red", na = "grey90")) {
  
  double_chain <- unique(lipid_df_samples$Lipid.class[!is.na(lipid_df_samples$Chain2)])
  
  single_chain <- setdiff(
    unique(lipid_df_samples$Lipid.class),
    double_chain
  )

  # determine whether stub is in single chain or double chain
  if (stub %in% single_chain ) {
    df_subset_base <- subset(lipid_df_samples, Lipid.class == stub)
    df_subset <- df_subset_base[, c("Total.carbon","Total.unsaturation", group_selection)]
    
    # Remove rows where group_selection column is NA/NaN
    df_subset <- df_subset[complete.cases(df_subset), ]
    
    # if there is duplication, get the average of the group_selection
    # If nothing left after filtering, skip
    if (nrow(df_subset) == 0) {
      message("Skipping ", stub, ": no complete data for ", group_selection)
      return(NULL)
    }
    df_subset <- aggregate(. ~ Total.carbon + Total.unsaturation, data = df_subset, FUN = mean)
    
    df_subset$mean_label <- round(df_subset[[group_selection]], 4)
    df_subset$mean_label[is.na(df_subset$mean_label)] <- ""
    
    x_breaks <- seq(min(df_subset$Total.carbon), max(df_subset$Total.carbon), by = 1)
    y_breaks <- seq(min(df_subset$Total.unsaturation), max(df_subset$Total.unsaturation), by = 1)
    x_limits <- c(min(df_subset$Total.carbon) - 1, max(df_subset$Total.carbon) + 1)
    y_limits <- c(min(df_subset$Total.unsaturation) - 1, max(df_subset$Total.unsaturation) + 1)
    
    df_subset[[group_selection]] <- as.numeric(df_subset[[group_selection]])
    out_of_range <- df_subset[[group_selection]] < range_min | df_subset[[group_selection]] > range_max
    
    if (any(out_of_range, na.rm = TRUE)) {
      showNotification(
        paste0("Values out of range in ", stub, " for ", group_selection,
               ": ", paste(df_subset[[group_selection]][out_of_range], collapse = ", ")),
        type = "warning")
    }
    df_subset[[group_selection]] <- pmax(
      pmin(df_subset[[group_selection]], range_max),
      range_min
    )
    
    p <- ggplot(df_subset, aes(x = Total.carbon, y = Total.unsaturation, fill = .data[[group_selection]])) +
      geom_tile(color = "white", width = 1, height = 1) +
      scale_fill_gradient2(
        low = color_scheme$low,
        mid = color_scheme$mid,
        high = color_scheme$high,
        midpoint = (range_max - range_min)/2 + range_min,
        limits = c(range_min, range_max),
        na.value = color_scheme$na
      ) +
      scale_x_continuous(breaks = x_breaks, limits = x_limits, expand = c(0, 0)) +
      scale_y_continuous(breaks = y_breaks, limits = y_limits, expand = c(0, 0)) +
      coord_fixed() +
      labs(title = paste("Heatmap of", group_selection, "for", stub),
           x = "Total Carbon Length", y = "Total unsaturation", fill = group_selection) +
      theme_bw()+
      theme(
        plot.title = element_text(size = title_size, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(size = text_x_size, angle = 0, vjust = 0.5),  # x-axis tick labels
        axis.text.y = element_text(size = text_y_size),                          # y-axis tick labels
        axis.title.x = element_text(size = title_x_size, face = "bold"),          
        axis.title.y = element_text(size = title_y_size, face = "bold"),
        legend.title = element_text(size = legend_title_size, face = "bold"),
        legend.text  = element_text(size = legend_text_size)
      )
    
    if (label_text) {
      p <- p + geom_text(aes(label = mean_label), color = "black", size = font_size)
    }
    
  }
  else if (stub %in% double_chain ) {
    df_subset<- subset(lipid_df_samples, Lipid.class == stub)
    df_subset$mean_label[is.na(df_subset[[group_selection]])] <- ""
    if (is.numeric(df_subset[[group_selection]])) {
      df_subset$mean_label <- round(df_subset[[group_selection]], 4)
    } else {
      df_subset$mean_label <- df_subset[[group_selection]]  # keep as-is
    }
    df_subset$x_var <- paste0(df_subset$Chain1, ":", df_subset$Chain1.unsaturation, "\n",df_subset$Chain2, ":", df_subset$Chain2.unsaturation,"\n", df_subset$Total.carbon)
    y_breaks <- seq(min(df_subset$Total.unsaturation), max(df_subset$Total.unsaturation), by = 1)
    y_limits <- c(min(df_subset$Total.unsaturation) - 1, max(df_subset$Total.unsaturation) + 1)
    if (sequence == "Based on Chain1 carbon number"){
      df_subset$x_order <- df_subset$Chain1*100 + df_subset$Chain1.unsaturation
    }
    else if (sequence == "Based on total carbon number"){
      df_subset$x_order <- df_subset$Total.carbon
    }
    else if ( sequence == "Based on Chain2 carbon number"){
      df_subset$x_order <- df_subset$Chain2 *100 + df_subset$Chain2.unsaturation
    }
    df_subset$x_var <- factor(df_subset$x_var, levels = unique(df_subset$x_var[order(df_subset$x_order)]))
    df_subset$y_var <- factor(df_subset$Total.unsaturation, levels = sort(unique(df_subset$Total.unsaturation)))
    df_subset[[group_selection]] <- as.numeric(df_subset[[group_selection]])
    out_of_range <- df_subset[[group_selection]] < range_min | df_subset[[group_selection]] > range_max
    if (any(out_of_range, na.rm = TRUE)) {
      showNotification(
        paste0("Values out of range in ", stub, " for ", group_selection,
               ": ", paste(df_subset[[group_selection]][out_of_range], collapse = ", ")),
        type = "warning")
    }
    df_subset[[group_selection]] <- pmax(
      pmin(df_subset[[group_selection]], range_max),
      range_min
    )
    p <- ggplot(df_subset, aes(x = x_var, y = y_var, fill =  .data[[group_selection]])) +
      geom_tile(color = "white", width = 1, height = 1) +
      scale_fill_gradient2(
        low = color_scheme$low,
        mid = color_scheme$mid,
        high = color_scheme$high,
        midpoint = (range_max - range_min)/2 + range_min,
        limits = c(range_min, range_max),
        na.value = color_scheme$na
      )+
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      coord_fixed() +
      labs(title = paste("Heatmap of", group_selection, "for", stub),
           x = "Chain1 \n Chain2 \n Total Carbon Length", y = "Total Unsaturation", fill = group_selection) +
      theme_bw()+
      theme(
        plot.title = element_text(size = title_size, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = text_x_size, angle = 0, vjust = 0.5),  # x-axis tick labels
        axis.text.y = element_text(size = text_y_size),                          # y-axis tick labels
        axis.title.x = element_text(size = title_x_size, face = "bold"),          # optional: x-axis title
        axis.title.y = element_text(size = title_y_size, face = "bold")           # optional: y-axis title
      )
    
    if (label_text) {
      p <- p + geom_text(aes(label = mean_label), color = "black", size = font_size)
    }
    
  }
  else{
    warning("This specific lipid class is not in the data.")  # console warning
    showNotification(
      "This specific lipid class is not in the data.",
      type = "warning",  
      duration = 5      
    )
  }
  
  return(p)
}



# ---- Correlation Heatmap ----
correlation_cal_adj <- function(numeric_data, control_z = F,
                                lower_point=0,upper_point=0){
  # Calculate the correlation matrix
  correlation_matrix <- cor(numeric_data)
  raw_correlation <- correlation_matrix
  if (control_z) {
    # apply fisher z score to the correlation matrix
    n <- nrow(numeric_data)
    # Sample size
    
    z_matrix <- atanh(correlation_matrix) # Fisher's z-transformation (atanh is the inverse of tanh)
    
    # Step 3: Calculate the p-values
    # Compute the standard error of Fisher's z
    se <- 1 / sqrt(n - 3)
    
    # Use the z-scores to calculate p-values
    p_matrix <- 2 * pnorm(-abs(z_matrix / se))
    
    # Step 4: Adjust the p-values
    upper_tri_pvals <- p_matrix[upper.tri(p_matrix)]
    
    # Adjust p-values for multiple testing
    p_adj_upper <- p.adjust(upper_tri_pvals, method = "BH")
    
    # Create a new matrix for adjusted p-values
    p_adj_matrix <- matrix(NA, nrow = nrow(p_matrix), ncol = ncol(p_matrix))
    
    # Fill the upper triangle with adjusted p-values
    p_adj_matrix[upper.tri(p_adj_matrix)] <- p_adj_upper
    
    # Mirror the upper triangle to the lower triangle
    p_adj_matrix[lower.tri(p_adj_matrix)] <- t(p_adj_matrix)[lower.tri(p_adj_matrix)]
    
    
    # Step 5: Filter for significant correlations (e.g., p < 0.05)
    significance_level <- 0.05
    correlation_matrix[p_adj_matrix >= significance_level] <- NA  # Set non-significant correlations to NA
    correlation_matrix[correlation_matrix >= lower_point & correlation_matrix <= upper_point] <- NA
    
    # rownames(p_adj_matrix) <- colnames(p_adj_matrix) <- rownames(correlation_matrix)
    return (correlation_matrix)
  }
  else {
    raw_correlation[raw_correlation >= lower_point & raw_correlation <= upper_point] <- NA
    return(raw_correlation)}
}



correlation_heatmap_plotly <- function(df,color_setting,group_name) {
  if (color_setting == "RWB"){
    #RdYIBu
    defined_colorscale = list(
      list(0, "steelblue"),
      list(0.5, "#E8E8E8"),
      list(1, "red4"))
  }
  else if (color_setting == "BGY"){
    defined_colorscale = list(
      list(0, "yellow"),
      list(0.5, "green"),
      list(1, "blue"))
  }
  else if (color_setting == "OWB"){
    defined_colorscale = list(
      list(0, "navy"),
      list(0.5, "#E8E8E8"),
      list(1, "darkorange")
    )
  }
  else if (color_setting == "YWB") {
    defined_colorscale = list(
      list(0, "#00008B"),
      list(0.5, "#E8E8E8"),
      list(1, "#FFD700")
    )
  }
  else if (color_setting == "greyscale"){
    defined_colorscale = list(
      list(0, "#E8E8E8"),
      list(1, "black")
    )
  }
  else if (color_setting == "viridis"){
    viridis_colors <- viridis::viridis(256)
    defined_colorscale <- lapply(seq_along(viridis_colors), function(i) {
      list((i-1)/(length(viridis_colors)-1), viridis_colors[i])
    })
  }
  else if (color_setting == "heat"){
    heat_colors <- heat.colors(256)
    defined_colorscale <- lapply(seq_along(heat_colors), function(i) {
      list((i-1)/(length(heat_colors)-1), heat_colors[i])
    })
  }
  
  else {
    stop("Invalid color setting. ")
  }
  
  
  # Ensure the correlation matrix is numeric
  mat <- as.matrix(df)
  
  # Row and column names
  rownames(mat) <- rownames(df)
  colnames(mat) <- colnames(df)
  
  # Plotly heatmap
  p <- plot_ly(
    x = colnames(mat),
    y = rownames(mat),
    z = mat,
    type = "heatmap",
    colorscale = defined_colorscale,
    zmid = 0,          # midpoint at 0
    showscale = TRUE    # show colorbar
  ) %>%
    layout(
      title = paste("Correlation Heatmap for",group_name,"group")
    )
  
  return(p)
}

# ---- Statistical testing ----
split_group_labels <- function(dat, metadata, group_col = 2) {
  # Extract unique group labels from the specified column
  group_labels <- metadata[, group_col]
  dat <- as.data.frame(dat)
  dat <- cbind(group_labels, dat)
  # Split data by group
  split_data <- split(dat, group_labels)
  split_data <- lapply(split_data, function(x) x[, -1, drop = FALSE])
  return(split_data)
}
safe_t_test <- function(x, y,variance_p) {
  x <- na.omit(x)
  y <- na.omit(y)
  
  if (length(x) < 2 || length(y) < 2) {
    return(data.frame(p_value = NA, warning = "Too few observations"))
  }
  
  if (sd(x) == 0 || sd(y) == 0) {
    return(data.frame(p_value = NA, warning = "Zero variance"))
  }
  
  t_res <- tryCatch({
    t.test(x, y, var.equal = variance_p)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(t_res)) {
    return(data.frame(p_value = NA, warning = "t.test error"))
  }
  
  return(data.frame(p_value = t_res$p.value, warning = NA))
}

t_test_lipids <- function(group1_df, group2_df,variance_p=T) {
  result <- data.frame()
  
  for (lipid in intersect(colnames(group1_df), colnames(group2_df))) {
    x <- group1_df[[lipid]]
    y <- group2_df[[lipid]]
    
    test <- safe_t_test(x, y,variance_p)
    result <- rbind(result, data.frame(Name = lipid, test))
  }
  # Add adjusted p-values using BH method
  result$p_value_adjusted <- p.adjust(result$p_value, method = "BH")
  # put the p_value_adjusted in the third column
  result <- result[, c("Name", "p_value", "p_value_adjusted", "warning")]
  # rank the lipids based on p_value
  result <- result[order(result$p_value), ]
  #rename colums
  colnames(result) <- c("Name", "P_value", "Adjusted_P_value", "Warning")
  return(result)
}

run_oneway_anova <- function(expr_data, group_vector, p_adjust_method = "BH") {
  stopifnot(nrow(expr_data) == length(group_vector))
  group_vector <- as.factor(group_vector)
  
  anova_results <- lapply(seq_len(ncol(expr_data)), function(i) {
    feature <- expr_data[, i]
    # Remove NAs
    valid_idx <- !is.na(feature) & !is.na(group_vector)
    feature_clean <- feature[valid_idx]
    group_clean <- group_vector[valid_idx]
    
    if (length(unique(group_clean)) < 2 || length(unique(feature_clean)) < 2) {
      return(list(F_value = NA, P_value = NA))
    }
    
    fit <- aov(feature_clean ~ group_clean)
    smry <- summary(fit)[[1]]
    list(
      F_value = smry[["F value"]][1],
      P_value = smry[["Pr(>F)"]][1]
    )
  })

  result <- do.call(rbind, lapply(seq_along(anova_results), function(i) {
    cbind(
      Name = colnames(expr_data)[i],
      F_value = anova_results[[i]]$F_value,
      P_value = anova_results[[i]]$P_value
    )
  }))
  
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  result$F_value <- as.numeric(result$F_value)
  result$P_value <- as.numeric(result$P_value)
  result$Adjusted_P_value <- p.adjust(result$P_value, method = p_adjust_method)
  
  return(result)

}


calculate_group_means <- function(split_data, lipid_df_samples = NULL) {
  # samples x lipids
  # Calculate mean for each group
  group_means <- lapply(split_data, function(x) colMeans(x, na.rm = TRUE))
  mean_df <- as.data.frame(group_means)
  
  mean_df$Name <- rownames(mean_df)
  
  # add the column name as the sequence of unique group labels
  colnames(mean_df) <- c(paste0("Mean_group_", names(group_means)), "Name")
  
  # Merge with lipid_df_samples if provided
  if (!is.null(lipid_df_samples)) {
    merged <- merge(lipid_df_samples, mean_df, by = "Name", all.x = TRUE)
    return(merged)
  } else {
    return(mean_df)
  }
}

append_mean_dataframe <- function(df, lipid_df_samples) {
  # Ensure mean_df has a 'Name' column
  if (!"Name" %in% colnames(lipid_df_samples)) {
    stop("mean_df must have a 'Name' column.")
  }
  # Remove the second to the eighth columns of the lipid_df_samples
  lipid_df_samples <- lipid_df_samples[, -c(2:11)]
  # Merge df with lipid_df_samples based on 'Name'
  merged_df <- merge( df,lipid_df_samples, by = "Name", all.x = TRUE)
  merged_df <- merged_df[order(merged_df$Adjusted_P_value), ]
  return(merged_df)
}


DSPC <- function(X, lambda = NULL, FDR = "BH") {
  library(glasso)
  # Convert to matrix
  X <- as.matrix(X)
  
  # ️Remove columns with NA/NaN/Inf
  bad_cols <- colnames(X)[apply(X, 2, function(v) any(!is.finite(v)))]
  if (length(bad_cols) > 0) {
    showNotification(
      paste0("Removed columns containing NA/NaN/Inf for DSPC analysis: ", paste(bad_cols, collapse = ", ")),
      type = "warning", duration = 6
    )
    X <- X[, !colnames(X) %in% bad_cols, drop = FALSE]
  }
  
  # Remove zero variance columns
  zero_var_cols <- colnames(X)[apply(X, 2, sd, na.rm = TRUE) == 0]
  if (length(zero_var_cols) > 0) {
    showNotification(
      paste0("Removed zero-variance columns for DSPC analysis: ", paste(zero_var_cols, collapse = ", ")),
      type = "warning", duration = 6
    )
    X <- X[, !colnames(X) %in% zero_var_cols, drop = FALSE]
  }
  
  if (ncol(X) < 2) {
    showNotification("Not enough valid variables left after filtering. DSPC stopped.", 
                     type = "error", duration = NULL)
    return(NULL)
  }
  # Standardize data (required in paper)
  # Standardize data (DSPC expects z-score standardized variables)
  col_means <- apply(X, 2, mean, na.rm = TRUE)
  col_sds   <- apply(X, 2, sd,   na.rm = TRUE)
  
  needs_z <- any(abs(col_means) > 1e-6) || any(abs(col_sds - 1) > 1e-6)

  if (needs_z) {
    X <- scale(X, center = TRUE, scale = TRUE)
    # showNotification(
    #   "DSPC requires standardized variables. Z-score scaling was applied automatically for the group of data .",
    #   type = "message", duration = 6
    # )
  } else {
    # Optional: stay silent, or use a debug-level message
    # showNotification("DSPC input already standardized; no additional scaling applied.",
    #                  type = "message", duration = 4)
  }

  
  n <- nrow(X)
  p <- ncol(X)
  vars <- colnames(X)
  
  # Empirical covariance
  S <- crossprod(X) / n
  
  # Automatic lambda if not given (recommended use CV or EBIC externally)
  if (is.null(lambda)) {
    lambda <- sqrt(log(p)/n)
  }
  
  # Sparse precision estimator (Glasso)
  glasso_fit <- glasso(S, rho = lambda, penalize.diagonal = FALSE)
  Theta_hat <- glasso_fit$wi
  Sigma_hat <- glasso_fit$w   # implied covariance
  
  # De-biasing
  temp <- S - Sigma_hat
  Theta_tilde <- Theta_hat + Theta_hat %*% temp %*% Theta_hat
  
  # Variance estimator
  sigma_hat2 <- matrix(0, p, p)
  for (i in 1:p) {
    for (j in 1:p) {
      sigma_hat2[i, j] <- Theta_hat[i,j]^2 + Theta_hat[i,i] * Theta_hat[j,j]
    }
  }
  
  # Test statistics
  Z <- sqrt(n) * Theta_tilde / sqrt(sigma_hat2)
  
  # Two-sided p-values
  pvals <- 2 * pnorm(abs(Z), lower.tail = FALSE)
  
  # Extract upper triangle (no diagonal, no duplicates)
  idx <- which(upper.tri(Z), arr.ind = TRUE)
  
  i_name <- vars[idx[,1]]
  j_name <- vars[idx[,2]]
  
  # Partial correlations using de-biased Theta
  rho <- Theta_tilde / sqrt(diag(Theta_tilde) %o% diag(Theta_tilde))
  
  partial_corr <- rho[idx]
  pvalue <- pvals[idx]
  
  # FDR correction
  qvalue <- p.adjust(pvalue, method = FDR)
  
  # Construct edge list
  result <- data.frame(
    Var1 = i_name,
    Var2 = j_name,
    correlation = partial_corr,
    # Z = Z[idx],
    pvalue = pvalue,
    qvalue = qvalue
  )
  
  return(result)
}

cor_mat_to_long <- function(cor_mat, remove_diag = TRUE, remove_duplicates = TRUE) {
  # Check input
  if (!is.matrix(cor_mat)) stop("Input must be a matrix.")
  if (nrow(cor_mat) != ncol(cor_mat)) stop("Matrix must be square.")
  
  # Convert to data frame with rownames preserved
  df <- as.data.frame(cor_mat)
  df$Var1 <- rownames(df)
  
  # Reshape to long format
  long_df <- reshape2::melt(df, id.vars = "Var1", variable.name = "Var2", value.name = "correlation")
  
  # Optionally remove diagonal
  if (remove_diag) {
    long_df <- long_df[long_df$Var1 != long_df$Var2, ]
  }
  
  # Optionally remove symmetric duplicates (keep upper or lower triangle only)
  if (remove_duplicates) {
    long_df <- long_df[!duplicated(t(apply(long_df[, 1:2], 1, sort))), ]
  }
  
  # Reorder columns for readability
  long_df <- long_df[, c("Var1", "Var2", "correlation")]
  rownames(long_df) <- NULL
  
  return(long_df)
}


get_subnetworks <- function(long_df, max_cluster_size = 20, min_nodes = 5) {
  library(igraph)
  library(leidenAlg)
  
  # Input check
  req <- c("Var1", "Var2", "correlation")
  if (!all(req %in% colnames(long_df))){
    # stop("Input must contain columns: Var1, Var2, correlation")
    print(long_df)
    showNotification(
      paste0("Input must contain columns: Var1, Var2, correlation"),
      type = "error", duration = 6
    )
    return(NULL)
    }
  
  # Clean & prepare edges
  long_df$Var1 <- trimws(as.character(long_df$Var1))
  long_df$Var2 <- trimws(as.character(long_df$Var2))
  
  edges <- long_df %>%
    filter(!is.na(correlation), correlation != 0) %>%
    rename(from = Var1, to = Var2, weight = correlation)
  
  # Node list
  node_ids <- unique(c(edges$from, edges$to))
  nodes <- data.frame(id = node_ids, stringsAsFactors = FALSE)
  
  # Build graph
  g <- graph_from_data_frame(edges, directed = FALSE)
  E(g)$weight <- abs(edges$weight)
  
  # Auto-tune Leiden resolution
  find_res <- function(g, max_sz) {
    for (r in seq(0.1, 100, 0.1)) {
      cl <- leiden.community(g, resolution = r)
      if (all(table(cl$membership) <= max_sz)) return(cl)
    }
    stop("Failed to find valid Leiden resolution.")
  }
  
  clusters <- find_res(g, max_cluster_size)
  memberships <- clusters$membership
  
  # Assign cluster
  nodes$cluster <- memberships[match(nodes$id, names(memberships))]
  
  # Print summary
  cat("\n===== Leiden Subnetwork Summary =====\n")
  print(table(nodes$cluster))
  cat("\nTotal subnetworks:", length(unique(nodes$cluster)), "\n")
  
  # Split into subnetworks
  subnet_list <- list()
  cluster_ids <- sort(unique(nodes$cluster))
  summary_df <- data.frame()  # will store cluster & node count
  for (cl in cluster_ids) {
    sub_nodes <- nodes$id[nodes$cluster == cl]
    
    # Skip clusters smaller than threshold
    if (length(sub_nodes) < min_nodes) next 
    
    sub_edges <- edges %>% filter(from %in% sub_nodes & to %in% sub_nodes)
    
    subnet_list[[paste0("Cluster_", cl)]] <- list(
      nodes = data.frame(id = sub_nodes),
      edges = sub_edges
    )
    
    summary_df <- rbind(summary_df,
                        data.frame(
                          cluster_id = paste0("Cluster_", cl),
                          n_nodes = length(sub_nodes),
                          stringsAsFactors = FALSE
                        ))
  }
  
  return(list(subnetworks = subnet_list, summary = summary_df))
}

plot_subnetwork <- function(subnet,title = NULL) {
  library(visNetwork)
  library(dplyr)
  library(scales)
  
  nodes <- subnet$nodes
  edges <- subnet$edges
  
  edges <- edges %>%
    mutate(
      color = ifelse(weight > 0, "#E75480", "#3FA9F5"),
      width = rescale(abs(weight), to = c(0.8, 5)),
      title = paste0("Correlation = ", round(weight, 3))
    )
  ledges <- data.frame(color = c("#E75480", "#3FA9F5"),
                       label = c("Positive correlation", "Negative correlation"))
  
  visNetwork(nodes, edges,main = title) %>%
    visEdges(smooth = FALSE) %>%
    # visLegend(addNodes = data.frame(
    #   label = c("Positive correlation", "Negative correlation"),
    #   color = c("#E75480", "#3FA9F5"),
    #   shape = c("dot", "dot"),
    #   size  = c(6, 6)
    #   )
    # )%>%
    visNodes(
      shape = "dot",          # always circular
      size = 15,              # fixed size
      fixed = list(
        x = FALSE,            # allow dragging horizontally
        y = FALSE             # allow dragging vertically
      )
    ) %>%
    visOptions(
      highlightNearest = list(
        enabled = TRUE,
        degree = 1,
        hover = FALSE,
        hideColor = "rgba(200,200,200,0.3)"
      ),
      nodesIdSelection = TRUE
    ) %>%
    visInteraction(
      dragNodes = TRUE,
      dragView  = TRUE,
      zoomView  = TRUE,
      selectable = TRUE
    ) %>%
    visPhysics(enabled = FALSE) %>%     # fully disable physics
    visLayout(randomSeed = 2926)%>%
    visExport(
      type = "png",                # png / jpg / svg / gif
      name = "DSPC_Network",       # filename (without extension)
      style = ""
  )
}
run_pls_model <- function(
    X, Y,
    n_perm = 200,
    n_cv = 7,
    scale_method = "none",
    seed = 2926
) {
  library(ropls)
  # stopifnot(is.matrix(X) || is.data.frame(X))
  # stopifnot(length(Y) == nrow(X))
  # Validation
  if (!(is.matrix(X) || is.data.frame(X))) {
    shiny::showNotification(
      "Input X must be a matrix or data.frame.",
      type = "error", duration = 6
    )
    return(NULL)
  }
  
  if (length(Y) != nrow(X)) {
    shiny::showNotification(
      "The length of Y does not match the number of samples in X.",
      type = "error", duration = 6
    )
    return(NULL)
  }
  
  set.seed(seed)
  
  X <- as.matrix(X)
  Y <- factor(Y)
  keep <- is.finite(rowSums(X)) & !is.na(Y)
  X <- X[keep, , drop = FALSE]
  Y <- droplevels(Y[keep])
  
  # remove bad columns: NA/Inf or zero variance
  good_cols <- apply(X, 2, function(v) all(is.finite(v)) && sd(v) > 0)
  X <- X[, good_cols, drop = FALSE]
  model <- tryCatch({
    opls(
      X,
      Y,
      predI = 1,
      orthoI = 0,
      scaleC = scale_method,
      permI = n_perm,
      crossvalI = n_cv,fig.pdfC = "none"
    )
  }, error = function(e) {
    shiny::showNotification(
      paste("PLS-DA error:", e$message),
      type = "error", duration = 10
    )
    return(NULL)
  })
  
  return(model)
}

run_opls_model <- function(
    X, Y,
    n_perm = 200,
    n_cv = 7,
    scale_method = "none",
    seed = 2926
) {
  library(ropls)
  # stopifnot(is.matrix(X) || is.data.frame(X))
  # stopifnot(length(Y) == nrow(X))
  # Validation 
  if (!(is.matrix(X) || is.data.frame(X))) {
    shiny::showNotification(
      "Input X must be a matrix or data.frame.",
      type = "error", duration = 6
    )
    return(NULL)
  }
  
  if (length(Y) != nrow(X)) {
    shiny::showNotification(
      "The length of Y does not match the number of samples in X.",
      type = "error", duration = 6
    )
    return(NULL)
  }
  
  set.seed(seed)
  
  X <- as.matrix(X)
  Y <- factor(Y)
  keep <- is.finite(rowSums(X)) & !is.na(Y)
  X <- X[keep, , drop = FALSE]
  Y <- droplevels(Y[keep])
  
  # remove bad columns: NA/Inf or zero variance
  good_cols <- apply(X, 2, function(v) all(is.finite(v)) && sd(v) > 0)
  X <- X[, good_cols, drop = FALSE]
  model <- tryCatch({
    opls(
      X,
      Y,
      predI = 1,
      orthoI = NA,
      scaleC = scale_method,
      permI = n_perm,
      crossvalI = n_cv,fig.pdfC = "none"
    )
  }, error = function(e) {
    shiny::showNotification(
      paste("OPLS-DA error:", e$message),
      type = "error", duration = 10
    )
    return(NULL)
  })
  
  return(model)
}
plot_opls_vip <- function(opls_model,
                          color_scheme = "viridis",
                          title ="OPLS-DA VIP Plot") {
  
  vip <- getVipVn(opls_model)
  
  load <- as.data.frame(opls_model@loadingMN)
  load$Lipid <- rownames(load)
  load$VIP <- vip[load$Lipid]
  
  p <- ggplot(load, aes(
    x = p1,
    y = VIP,
    color = VIP,
    text = paste0(
      "Lipid: ", Lipid,
      "<br>p1 loading: ", round(p1, 4),
      "<br>VIP score: ", round(VIP, 4)
    )
  )) +
    geom_point(size = 2) +
    theme_classic() +
    labs(
      x = "Loading (p1)",
      y = "VIP Score",
      title = title
    )
  
  # Add color scale depending on the user-selected scheme
  p <- p + switch(
    color_scheme,
    
    "viridis" = scale_color_viridis_c(),
    "cividis" = scale_color_viridis_c(option = "cividis"),
    "plasma" = scale_color_viridis_c(option = "plasma"),
    "magma"  = scale_color_viridis_c(option = "magma"),
    "inferno" = scale_color_viridis_c(option = "inferno"),
    "Teal gradient" = scale_color_gradientn(colors = c("#01665E", "#5AB4AC", "#C7EAE5")),
    "Purple Pink Orange gradient" = scale_color_gradientn(colors = c("#5E4FA2", "#9E0142", "#F46D43", "#FEE08B")),
    'Mint Teal Blue gradient'= scale_color_gradientn(colors = c("#E0F3DB", "#A8DDB5", "#43A2CA", "#0868AC")),
    
    # default if user gives unknown option
    scale_color_viridis_c()
  )
  
  return(p)
}

run_RF <- function(X, Y,
                   data_partition = 0.6,
                   n_tree = 500,
                   seed = 2926) {
  library(caret)
  # --- Validation ---
  if (!(is.matrix(X) || is.data.frame(X))) {
    showNotification(
      "Input X must be a matrix or data.frame.",
      type = "error", duration = 6
    )
    return(NULL)
  }
  
  if (length(Y) != nrow(X)) {
    showNotification(
      "The length of Y does not match the number of samples in X.",
      type = "error", duration = 6
    )
    return(NULL)
  }
  
  if (data_partition <= 0 || data_partition >= 1) {
    showNotification(
      "data_partition must be between 0 and 1.",
      type = "error", duration = 6
    )
    return(NULL)
  }
  
  # --- Setup ---
  set.seed(seed)
  
  X <- as.data.frame(X)
  y <- factor(Y)
  
  # --- Train / Test split ---
  train_idx <- caret::createDataPartition(
    y, p = data_partition, list = FALSE
  )
  
  X_train <- X[train_idx, , drop = FALSE]
  X_test  <- X[-train_idx, , drop = FALSE]
  
  y_train <- y[train_idx]
  y_test  <- y[-train_idx]
  
  # --- Fit Random Forest ---
  rf_model <- randomForest::randomForest(
    x = X_train,
    y = y_train,
    ntree = n_tree,
    mtry  = floor(sqrt(ncol(X))),
    importance = TRUE
  )
  
  # --- Prediction ---
  pred <- predict(rf_model, X_test)
  
  # --- Confusion Matrix ---
  cm <- caret::confusionMatrix(pred, y_test)
  
  # --- Feature importance ---
  imp <- randomForest::importance(rf_model)
  
  importance_df <- data.frame(
    feature = rownames(imp),
    MeanDecreaseGini = imp[, "MeanDecreaseGini"],
    MeanDecreaseAccuracy = imp[, "MeanDecreaseAccuracy"],
    row.names = NULL
  )
  
  importance_df <- importance_df[
    order(-importance_df$MeanDecreaseGini),
  ]
  
  # --- Return everything cleanly ---
  return(list(
    model = rf_model,
    confusion_matrix = cm,
    importance = importance_df,
    train_index = train_idx,
    params = list(
      data_partition = data_partition,
      n_tree = n_tree,
      seed = seed
    )
  ))
}

plot_rf_importance <- function(importance_df,
                               top_n = 20,
                               color_scheme = "viridis",
                               title = NULL) {
  
  library(ggplot2)
  library(plotly)
  
  if (is.null(title)) {
    title <- paste("Top", top_n, "Features by Random Forest Importance")
  }
  # --- helper function ---
  make_plot <- function(df, value_col, x_label) {
    df <- df[order(-df[[value_col]]),,drop = F]
    df <- head(df, top_n)
    
    p <- ggplot(
      df,
      aes(
        x = .data[[value_col]],
        y = reorder(feature, .data[[value_col]]),
        color = .data[[value_col]],
        text = paste0(
          "Feature: ", feature,
          "<br>Mean Decrease Accuracy: ", round(MeanDecreaseAccuracy, 4),
          "<br>Mean Decrease Gini: ", round(MeanDecreaseGini, 4)
        )
      )
    ) +
      geom_point(size = 3) +
      guides(color = "none") +
      theme_bw() +
      theme(
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank()
      ) +
      labs(
        x = x_label,
        y = NULL,
        color = x_label,
        title = title
      )
    
    
    p + switch(
      color_scheme,
      "viridis"  = scale_color_viridis_c(),
      "cividis"  = scale_color_viridis_c(option = "cividis"),
      "plasma"   = scale_color_viridis_c(option = "plasma"),
      "magma"    = scale_color_viridis_c(option = "magma"),
      "inferno"  = scale_color_viridis_c(option = "inferno"),
      "Teal gradient" =
        scale_color_gradientn(colors = c("#01665E", "#5AB4AC", "#C7EAE5")),
      "Purple Pink Orange gradient" =
        scale_color_gradientn(colors = c("#5E4FA2", "#9E0142", "#F46D43", "#FEE08B")),
      "Mint Teal Blue gradient" =
        scale_color_gradientn(colors = c("#E0F3DB", "#A8DDB5", "#43A2CA", "#0868AC")),
      scale_color_viridis_c()
    )
  }
  
  p1 <- make_plot(importance_df, "MeanDecreaseGini", "Mean Decrease Gini")
  p2 <- make_plot(importance_df, "MeanDecreaseAccuracy", "Mean Decrease Accuracy")
  p1_plt <- ggplotly(p1, tooltip = "text")
  p2_plt <- ggplotly(p2, tooltip = "text")
  
  p <- subplot(
    p1_plt,
    p2_plt,
    nrows = 1,
    shareX = FALSE,
    shareY = FALSE,
    titleX = T,
    titleY = FALSE
  )
  
  p
}

create_hcl_heatmaply <- function(expr_mat,
                             metadat = NULL,
                             group_var = NA,
                             parse_table = NULL,             # "none", "row", "column"
                             cluster_rows = FALSE,
                             cluster_cols = FALSE,
                             colors = NULL) {
  showNotification("Please wait patiently before the heatmap result is ready...",
                   type = "message",duration = 15)
  # Ensure numeric matrix
  expr_mat <- as.matrix(expr_mat)
  mode(expr_mat) <- "numeric"
  ann_row <- data.frame(Group = metadat[[group_var]])
  rownames(ann_row) <- rownames(metadat)
  
  ann_col <- data.frame(Lipid.class = parse_table$Lipid.class)
  rownames(ann_col) <- parse_table$Name
  
  
  if (is.null(colors)) {
    colors <- colorRampPalette(
      c("#2166AC", "#F7F7F7", "#B2182B")
    )(256)
  }
  
  
  heatmaply::heatmaply(
    expr_mat,
    scale = "none",
    Rowv = cluster_rows,
    Colv = cluster_cols,
    colors = colors,
    row_side_colors = ann_row,
    col_side_colors = ann_col,
    showticklabels = c(F, F),  # x then y
    row_dend_left = T,
    xlab = "Lipids",
    ylab = "Samples"
  )
}


# ---- Save ----
# save_plot_safe <- function(plot_obj, path, width=8, height=6, dpi=300) {
#   try({
#     if (!is.null(plot_obj)) {
# 
#       grDevices::png(path, width=width, height=height, units="in", res=dpi)
#       print(plot_obj)
#       grDevices::dev.off()
#     }
#   }, silent = TRUE)
# }

save_plot_safe <- function(plot_obj, path, width = 8, height = 6, dpi = 300) {
  try({
    if (!is.null(plot_obj)) {
      grDevices::png(path, width = width, height = height, units = "in", res = dpi)
      on.exit(grDevices::dev.off(), add = TRUE)
      print(plot_obj)
    }
  }, silent = TRUE)
}

save_opls_plot_safe <- function(model_obj, typeVc, path, width = 8, height = 6, dpi = 300) {
  tryCatch({
    if (is.null(model_obj)) return(invisible(NULL))
    
    grDevices::png(path, width = width, height = height, units = "in", res = dpi)
    on.exit(grDevices::dev.off(), add = TRUE)
    
    plot(model_obj, typeVc = typeVc)
  }, error = function(e) {
    message("Failed to save ", basename(path), ": ", e$message)
  })
}

save_LCH_heatmaps_to_dir <- function(out_dir,
                                     parsed_tbl,
                                     ngroup,
                                     input) {
  req(parsed_tbl)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  classes <- unique(parsed_tbl$Lipid.class)
  groups <- colnames(parsed_tbl)[(ncol(parsed_tbl) - ngroup + 1):ncol(parsed_tbl)]
  
  out_paths <- c()
  color_scheme <- switch(input$color_code_LCH,
                         "RWB"   = list(low = "blue", mid = "white", high = "red", na = "grey90"),
                         "BGY"       = list(low = "#08306B", mid = "#41B6C4", high = "#FFFFB2", na = "grey90"),
                         "OWB"       = list(low = "#E66101", mid = "white", high = "#0C2C84", na = "grey90"),
                         "greyscale" = list(low = "grey90", mid = "grey70", high = "black", na = "grey95"),
                         "viridis"   = list(low = viridis::viridis(3)[1], mid = viridis::viridis(3)[2], high = viridis::viridis(3)[3], na = "grey90"),
                         "heat"      = list(low = "yellow", mid = "orange", high = "red", na = "grey90")
  )
  for (class in classes) {
    for (group in groups) {
      
      plot <- create_LCHplot_single(
        label_text = input$label_text_checkbox,
        stub = class,
        group_selection = group,
        lipid_df_samples = parsed_tbl,
        range_min = input$heatmap_min,
        range_max = input$heatmap_max,
        font_size = input$label_text_size,
        title_size = input$label_title_size,
        text_x_size = input$label_x_size,
        text_y_size = input$label_y_size,
        title_x_size = input$title_x_size,
        title_y_size = input$title_y_size,
        sequence = input$sequence_heatmap,
        legend_title_size = input$legend_title_size,
        legend_text_size = input$legend_text_size,
        color_scheme = color_scheme
      )
      
      # Avoid illegal characters in filenames by replacing them with underscores
      safe_class <- gsub("[^A-Za-z0-9_\\-]+", "_", class)
      safe_group <- gsub("[^A-Za-z0-9_\\-]+", "_", group)
      
      path <- file.path(out_dir, paste0("Differential_Mean_Lipid_Heatmap_", safe_class, "_", safe_group, ".png"))
      
      ggsave(path, plot = plot, width = input$width_LCH, height = input$height_LCH, dpi = input$DPI_LCH)
      
      out_paths <- c(out_paths, path)
    }
  }
  
  out_paths
}

