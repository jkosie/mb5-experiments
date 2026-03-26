# ============================================================
# FULL 768-ORDER GENERATOR + CHECKER
# 48 labs x 16 CSVs per lab = 768 total CSVs
# Each CSV contains the full 12-trial structure.
# ============================================================

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

capitalize_first <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

split_and_reverse <- function(lst) {
  if (length(lst) %% 2 != 0) {
    stop("List length must be even to split evenly in half.")
  }
  mid <- length(lst) / 2
  c(rev(lst[1:mid]), rev(lst[(mid + 1):length(lst)]))
}

handle_reverse_ordering <- function(trial_list_element, reverse_order_block, reverse_order_list) {
  if (reverse_order_block == "base_rev") {
    if (reverse_order_list == "rev") {
      return(rev(split_and_reverse(trial_list_element)))
    } else {
      return(split_and_reverse(trial_list_element))
    }
  } else {
    if (reverse_order_list == "rev") {
      return(rev(trial_list_element))
    } else {
      return(trial_list_element)
    }
  }
}

count_or_zero <- function(tab, key) {
  if (key %in% names(tab)) as.integer(tab[[key]]) else 0L
}

# ------------------------------------------------------------
# Global stimulus / design dictionaries
# ------------------------------------------------------------

fribble_dict <- list(
  set_1 = list(
    c("Tripod", "Diamond"),
    c("SimsDiamond", "Cylinder"),
    c("Pyramid", "Bowtie"),
    c("Pacman", "Arrow"),
    c("Crinkle", "Pumpkin"),
    c("Bowl", "Gumdrop")
  ),
  set_2 = list(
    c("Pacman", "Cylinder"),
    c("Bowtie", "Arrow"),
    c("Tripod", "Bowl"),
    c("Crinkle", "Diamond"),
    c("Pyramid", "Gumdrop"),
    c("SimsDiamond", "Pumpkin")
  )
)

fractal_dict <- list(
  set_1 = list(
    c("pair1_a", "pair1_b"),
    c("pair2_a", "pair2_b"),
    c("pair3_a", "pair3_b"),
    c("pair4_a", "pair4_b"),
    c("pair5_a", "pair5_b"),
    c("pair6_a", "pair6_b")
  )
)

complexity_order_dict <- list(
  order_1 = c(
    "simple", "complex", "complex", "simple", "complex", "simple",
    "simple", "complex", "complex", "simple", "simple", "complex"
  ),
  order_2 = c(
    "complex", "simple", "simple", "complex", "simple", "complex",
    "complex", "simple", "simple", "complex", "complex", "simple"
  )
)

test_familiar_location_dict <- list(
  loc_order_1 = c(
    "left", "right", "right", "left", "left", "right",
    "left", "right", "left", "left", "right", "right"
  ),
  loc_order_2 = c(
    "right", "left", "left", "right", "right", "left",
    "right", "left", "right", "right", "left", "left"
  )
)

# Three lab-level time-pair groups; each lab gets one group.
familiarization_time_pairs <- list(
  list(c(5, 10, 15), c(10, 15, 5)),
  list(c(5, 15, 10), c(15, 10, 5)),
  list(c(10, 5, 15), c(15, 5, 10))
)

# ------------------------------------------------------------
# Build one full 12-trial CSV
# ------------------------------------------------------------

build_trial_df <- function(
    order_id,
    lab_number,
    fribble_set,
    complexity_order,
    reverse_order_block,
    reverse_order_list,
    time_pair_group,
    stimulus_type_order,
    familiar_role,
    location_order,
    time_order_index,
    rel_image_path = "stimuli/images/",
    img_ext = ".png",
    test_time = 5) {
  
  trial_num <- 12
  trial_num_set <- 6
  fractal_set <- "set_1"
  
  cur_complexity_list <- complexity_order_dict[[complexity_order]]
  cur_test_familiar_location_list <- test_familiar_location_dict[[location_order]]
  
  cur_complexity_list_1 <- cur_complexity_list[1:trial_num_set]
  cur_complexity_list_2 <- cur_complexity_list[(trial_num_set + 1):trial_num]
  
  cur_familiarization_time_set <- familiarization_time_pairs[[time_pair_group]][[time_order_index]]
  fam_time_1 <- cur_familiarization_time_set[1]
  fam_time_2 <- cur_familiarization_time_set[2]
  fam_time_3 <- cur_familiarization_time_set[3]
  
  fam_time_list <- c(
    fam_time_2, fam_time_3, fam_time_1,
    fam_time_1, fam_time_3, fam_time_2,
    fam_time_1, fam_time_3, fam_time_2,
    fam_time_3, fam_time_2, fam_time_1
  )
  
  cur_familiar_fribble_items <- sapply(
    fribble_dict[[fribble_set]],
    function(pair) pair[familiar_role + 1]
  )
  cur_familiar_fractal_items <- sapply(
    fractal_dict[[fractal_set]],
    function(pair) pair[familiar_role + 1]
  )
  
  cur_novel_role <- ifelse(familiar_role == 0, 1, 0)
  
  cur_novel_fribble_items <- sapply(
    fribble_dict[[fribble_set]],
    function(pair) pair[cur_novel_role + 1]
  )
  cur_novel_fractal_items <- sapply(
    fractal_dict[[fractal_set]],
    function(pair) pair[cur_novel_role + 1]
  )
  
  if (stimulus_type_order == "fribble_first") {
    cur_familiar_items <- c(cur_familiar_fribble_items, cur_familiar_fractal_items)
    cur_novel_items <- c(cur_novel_fribble_items, cur_novel_fractal_items)
    
    cur_familiar_fribble_images <- paste0(
      cur_familiar_fribble_items, "_", capitalize_first(cur_complexity_list_1)
    )
    cur_familiar_fractal_images <- paste0(
      "fractal_", cur_complexity_list_2, "_", cur_familiar_fractal_items, "_bright"
    )
    
    cur_novel_fribble_images <- paste0(
      cur_novel_fribble_items, "_", capitalize_first(cur_complexity_list_1)
    )
    cur_novel_fractal_images <- paste0(
      "fractal_", cur_complexity_list_2, "_", cur_novel_fractal_items, "_bright"
    )
    
    cur_familiar_images <- c(cur_familiar_fribble_images, cur_familiar_fractal_images)
    cur_novel_images <- c(cur_novel_fribble_images, cur_novel_fractal_images)
    
  } else {
    cur_familiar_items <- c(cur_familiar_fractal_items, cur_familiar_fribble_items)
    cur_novel_items <- c(cur_novel_fractal_items, cur_novel_fribble_items)
    
    cur_familiar_fractal_images <- paste0(
      "fractal_", cur_complexity_list_1, "_", cur_familiar_fractal_items, "_bright"
    )
    cur_familiar_fribble_images <- paste0(
      cur_familiar_fribble_items, "_", capitalize_first(cur_complexity_list_2)
    )
    
    cur_novel_fractal_images <- paste0(
      "fractal_", cur_complexity_list_1, "_", cur_novel_fractal_items, "_bright"
    )
    cur_novel_fribble_images <- paste0(
      cur_novel_fribble_items, "_", capitalize_first(cur_complexity_list_2)
    )
    
    cur_familiar_images <- c(cur_familiar_fractal_images, cur_familiar_fribble_images)
    cur_novel_images <- c(cur_novel_fractal_images, cur_novel_fribble_images)
  }
  
  ordered_cur_complexity_list <- handle_reverse_ordering(
    cur_complexity_list, reverse_order_block, reverse_order_list
  )
  
  ordered_cur_test_familiar_location_list <- handle_reverse_ordering(
    cur_test_familiar_location_list, reverse_order_block, reverse_order_list
  )
  
  switched_ordered_cur_test_familiar_location_list <- ifelse(
    ordered_cur_test_familiar_location_list == "left",
    "right",
    "left"
  )
  
  ordered_fam_time_list <- handle_reverse_ordering(
    fam_time_list, reverse_order_block, reverse_order_list
  )
  
  timeout_ordered_fam_time_list <- ordered_fam_time_list * 2
  
  ordered_cur_familiar_items <- handle_reverse_ordering(
    cur_familiar_items, reverse_order_block, reverse_order_list
  )
  
  ordered_cur_novel_items <- handle_reverse_ordering(
    cur_novel_items, reverse_order_block, reverse_order_list
  )
  
  ordered_cur_familiar_images <- handle_reverse_ordering(
    cur_familiar_images, reverse_order_block, reverse_order_list
  )
  
  ordered_cur_novel_images <- handle_reverse_ordering(
    cur_novel_images, reverse_order_block, reverse_order_list
  )
  
  ordered_cur_familiar_images_path <- file.path(
    rel_image_path, paste0(ordered_cur_familiar_images, img_ext)
  )
  
  ordered_cur_novel_images_path <- file.path(
    rel_image_path, paste0(ordered_cur_novel_images, img_ext)
  )
  
  left_image_path_1 <- ifelse(
    ordered_cur_test_familiar_location_list == "left",
    ordered_cur_familiar_images_path,
    ordered_cur_novel_images_path
  )
  
  right_image_path_1 <- ifelse(
    ordered_cur_test_familiar_location_list == "right",
    ordered_cur_familiar_images_path,
    ordered_cur_novel_images_path
  )
  
  left_image_path_2 <- ifelse(
    switched_ordered_cur_test_familiar_location_list == "left",
    ordered_cur_familiar_images_path,
    ordered_cur_novel_images_path
  )
  
  right_image_path_2 <- ifelse(
    switched_ordered_cur_test_familiar_location_list == "right",
    ordered_cur_familiar_images_path,
    ordered_cur_novel_images_path
  )
  
  data.frame(
    lab_number = rep(lab_number, trial_num),
    order_id = rep(order_id, trial_num),
    trial_number = 1:trial_num,
    fribble_set = rep(fribble_set, trial_num),
    fractal_set = rep(fractal_set, trial_num),
    complexity_order = rep(complexity_order, trial_num),
    reverse_order_block = rep(reverse_order_block, trial_num),
    reverse_order_list = rep(reverse_order_list, trial_num),
    time_pair_group = rep(time_pair_group, trial_num),
    time_order_index = rep(time_order_index, trial_num),
    stimulus_type_order = rep(stimulus_type_order, trial_num),
    familiar_role = rep(familiar_role, trial_num),
    location_order = rep(location_order, trial_num),
    familiar_stimulus = ordered_cur_familiar_images,
    novel_stimulus = ordered_cur_novel_images,
    familiar_stimulus_item = ordered_cur_familiar_items,
    novel_stimulus_item = ordered_cur_novel_items,
    complexity_condition = ordered_cur_complexity_list,
    familiarization_time = ordered_fam_time_list,
    familiar_stimulus_path = ordered_cur_familiar_images_path,
    novel_stimulus_path = ordered_cur_novel_images_path,
    familiar_location_1 = ordered_cur_test_familiar_location_list,
    familiar_location_2 = switched_ordered_cur_test_familiar_location_list,
    left_image_path_1 = left_image_path_1,
    right_image_path_1 = right_image_path_1,
    left_image_path_2 = left_image_path_2,
    right_image_path_2 = right_image_path_2,
    test_time_1 = rep(test_time, trial_num),
    test_time_2 = rep(test_time, trial_num),
    familiarization_time_timeout = timeout_ordered_fam_time_list,
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Generator: writes all 768 CSVs
# ------------------------------------------------------------

generate_trials <- function() {
  output_dir <- file.path(getwd(), "trial_lists_768")
  
  # Use this if you want to start from a completely clean folder.
  # unlink(output_dir, recursive = TRUE)
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 48 lab-level templates:
  # 2 fribble sets x 2 complexity orders x 2 block reversals x 2 list reversals x 3 time-pair groups
  lab_templates <- expand.grid(
    fribble_set = c("set_1", "set_2"),
    complexity_order = c("order_1", "order_2"),
    reverse_order_block = c("base_block", "base_rev"),
    reverse_order_list = c("base", "rev"),
    time_pair_group = 1:3,
    stringsAsFactors = FALSE
  )
  
  if (nrow(lab_templates) != 48) {
    stop("Lab template count is not 48. Check the design.")
  }
  
  # 16 within-lab conditions:
  # 2 stimulus orders x 2 familiar roles x 2 location orders x 2 time orders
  lab_conditions <- expand.grid(
    stimulus_type_order = c("fribble_first", "fractal_first"),
    familiar_role = c(0, 1),
    location_order = c("loc_order_1", "loc_order_2"),
    time_order_index = c(1, 2),
    stringsAsFactors = FALSE
  )
  
  if (nrow(lab_conditions) != 16) {
    stop("Within-lab condition count is not 16. Check the design.")
  }
  
  generated_files <- character(0)
  global_order_id <- 1
  all_manifest_rows <- list()
  
  for (lab_idx in seq_len(nrow(lab_templates))) {
    lab_number <- lab_idx
    lab_spec <- lab_templates[lab_idx, ]
    
    lab_dir <- file.path(output_dir, sprintf("lab_%02d", lab_number))
    dir.create(lab_dir, recursive = TRUE, showWarnings = FALSE)
    
    lab_manifest <- data.frame(
      order_id = integer(0),
      lab_number = integer(0),
      fribble_set = character(0),
      complexity_order = character(0),
      reverse_order_block = character(0),
      reverse_order_list = character(0),
      time_pair_group = integer(0),
      stimulus_type_order = character(0),
      familiar_role = integer(0),
      location_order = character(0),
      time_order_index = integer(0),
      file_name = character(0),
      stringsAsFactors = FALSE
    )
    
    for (cond_idx in seq_len(nrow(lab_conditions))) {
      cond <- lab_conditions[cond_idx, ]
      
      cur_df <- build_trial_df(
        order_id = global_order_id,
        lab_number = lab_number,
        fribble_set = lab_spec$fribble_set,
        complexity_order = lab_spec$complexity_order,
        reverse_order_block = lab_spec$reverse_order_block,
        reverse_order_list = lab_spec$reverse_order_list,
        time_pair_group = as.integer(lab_spec$time_pair_group),
        stimulus_type_order = cond$stimulus_type_order,
        familiar_role = as.integer(cond$familiar_role),
        location_order = cond$location_order,
        time_order_index = as.integer(cond$time_order_index)
      )
      
      file_name <- sprintf("order_%03d.csv", global_order_id)
      output_path <- file.path(lab_dir, file_name)
      write.csv(cur_df, output_path, row.names = FALSE)
      
      generated_files <- c(generated_files, output_path)
      
      lab_manifest <- rbind(
        lab_manifest,
        data.frame(
          order_id = global_order_id,
          lab_number = lab_number,
          fribble_set = lab_spec$fribble_set,
          complexity_order = lab_spec$complexity_order,
          reverse_order_block = lab_spec$reverse_order_block,
          reverse_order_list = lab_spec$reverse_order_list,
          time_pair_group = as.integer(lab_spec$time_pair_group),
          stimulus_type_order = cond$stimulus_type_order,
          familiar_role = as.integer(cond$familiar_role),
          location_order = cond$location_order,
          time_order_index = as.integer(cond$time_order_index),
          file_name = file_name,
          stringsAsFactors = FALSE
        )
      )
      
      global_order_id <- global_order_id + 1
    }
    
    write.csv(lab_manifest, file.path(lab_dir, "lab_manifest.csv"), row.names = FALSE)
    all_manifest_rows[[length(all_manifest_rows) + 1]] <- lab_manifest
  }
  
  write.csv(
    do.call(rbind, all_manifest_rows),
    file.path(output_dir, "all_order_assignment.csv"),
    row.names = FALSE
  )
  
  cat("Generated", length(generated_files), "CSV files in", output_dir, "\n")
  invisible(generated_files)
}

# ------------------------------------------------------------
# Checker: verifies every lab folder
# ------------------------------------------------------------

check_lab_folder <- function(lab_dir) {
  files <- sort(list.files(lab_dir, pattern = "^order_[0-9]{3}\\.csv$", full.names = TRUE))
  
  if (length(files) == 0) {
    return(list(
      summary = data.frame(
        lab_folder = basename(lab_dir),
        n_files = 0,
        files_ok = FALSE,
        stimulus_balance_ok = FALSE,
        familiar_role_balance_ok = FALSE,
        location_balance_ok = FALSE,
        time_balance_ok = FALSE,
        within_file_trial_balance_ok = FALSE,
        item_role_balance_ok = FALSE,
        overall_ok = FALSE,
        notes = "No CSV files found",
        stringsAsFactors = FALSE
      ),
      files = data.frame(),
      items = data.frame()
    ))
  }
  
  dfs <- lapply(files, function(f) read.csv(f, stringsAsFactors = FALSE))
  
  file_summary <- do.call(
    rbind,
    lapply(seq_along(dfs), function(i) {
      df <- dfs[[i]]
      
      data.frame(
        lab_folder = basename(lab_dir),
        file = basename(files[i]),
        order_id = unique(df$order_id)[1],
        stimulus_type_order = unique(df$stimulus_type_order)[1],
        familiar_role = unique(df$familiar_role)[1],
        location_order = unique(df$location_order)[1],
        time_order_index = unique(df$time_order_index)[1],
        loc_left = count_or_zero(table(df$familiar_location_1), "left"),
        loc_right = count_or_zero(table(df$familiar_location_1), "right"),
        time_5 = count_or_zero(table(df$familiarization_time), "5"),
        time_10 = count_or_zero(table(df$familiarization_time), "10"),
        time_15 = count_or_zero(table(df$familiarization_time), "15"),
        comp_simple = count_or_zero(table(df$complexity_condition), "simple"),
        comp_complex = count_or_zero(table(df$complexity_condition), "complex"),
        stringsAsFactors = FALSE
      )
    })
  )
  
  n_files <- nrow(file_summary)
  files_ok <- (n_files == 16)
  
  stim_tab <- table(file_summary$stimulus_type_order)
  stimulus_balance_ok <- all(c("fribble_first", "fractal_first") %in% names(stim_tab)) &&
    stim_tab[["fribble_first"]] == 8 &&
    stim_tab[["fractal_first"]] == 8
  
  role_tab <- table(file_summary$familiar_role)
  familiar_role_balance_ok <- all(c("0", "1") %in% names(role_tab)) &&
    role_tab[["0"]] == 8 &&
    role_tab[["1"]] == 8
  
  loc_order_tab <- table(file_summary$location_order)
  location_balance_ok <- all(c("loc_order_1", "loc_order_2") %in% names(loc_order_tab)) &&
    loc_order_tab[["loc_order_1"]] == 8 &&
    loc_order_tab[["loc_order_2"]] == 8
  
  time_order_tab <- table(file_summary$time_order_index)
  time_balance_ok <- all(c("1", "2") %in% names(time_order_tab)) &&
    time_order_tab[["1"]] == 8 &&
    time_order_tab[["2"]] == 8
  
  within_file_trial_balance_ok <- all(
    file_summary$loc_left == 6,
    file_summary$loc_right == 6,
    file_summary$time_5 == 4,
    file_summary$time_10 == 4,
    file_summary$time_15 == 4,
    file_summary$comp_simple == 6,
    file_summary$comp_complex == 6
  )
  
  all_familiar_items <- unlist(lapply(dfs, function(df) df$familiar_stimulus_item))
  all_novel_items <- unlist(lapply(dfs, function(df) df$novel_stimulus_item))
  all_items <- sort(unique(c(all_familiar_items, all_novel_items)))
  
  familiar_counts <- table(all_familiar_items)
  novel_counts <- table(all_novel_items)
  
  item_detail <- data.frame(
    lab_folder = basename(lab_dir),
    item = all_items,
    familiar_count = as.integer(familiar_counts[all_items]),
    novel_count = as.integer(novel_counts[all_items]),
    stringsAsFactors = FALSE
  )
  
  item_detail$familiar_count[is.na(item_detail$familiar_count)] <- 0
  item_detail$novel_count[is.na(item_detail$novel_count)] <- 0
  item_detail$both_roles_present <- item_detail$familiar_count > 0 & item_detail$novel_count > 0
  
  item_role_balance_ok <- all(item_detail$both_roles_present)
  
  overall_ok <- files_ok &&
    stimulus_balance_ok &&
    familiar_role_balance_ok &&
    location_balance_ok &&
    time_balance_ok &&
    within_file_trial_balance_ok &&
    item_role_balance_ok
  
  notes <- c()
  if (!files_ok) notes <- c(notes, paste0("Expected 16 files, found ", n_files))
  if (!stimulus_balance_ok) notes <- c(notes, "Stimulus order not balanced 8/8")
  if (!familiar_role_balance_ok) notes <- c(notes, "Familiar role not balanced 8/8")
  if (!location_balance_ok) notes <- c(notes, "Location order not balanced 8/8")
  if (!time_balance_ok) notes <- c(notes, "Time-order index not balanced 8/8")
  if (!within_file_trial_balance_ok) notes <- c(notes, "One or more files failed internal trial-balance checks")
  if (!item_role_balance_ok) {
    bad_items <- item_detail$item[!item_detail$both_roles_present]
    notes <- c(notes, paste0("Items missing one role: ", paste(bad_items, collapse = ", ")))
  }
  
  if (length(notes) == 0) notes <- "OK" else notes <- paste(notes, collapse = "; ")
  
  summary <- data.frame(
    lab_folder = basename(lab_dir),
    n_files = n_files,
    files_ok = files_ok,
    stimulus_balance_ok = stimulus_balance_ok,
    familiar_role_balance_ok = familiar_role_balance_ok,
    location_balance_ok = location_balance_ok,
    time_balance_ok = time_balance_ok,
    within_file_trial_balance_ok = within_file_trial_balance_ok,
    item_role_balance_ok = item_role_balance_ok,
    overall_ok = overall_ok,
    notes = notes,
    stringsAsFactors = FALSE
  )
  
  list(summary = summary, files = file_summary, items = item_detail)
}

check_all_labs <- function() {
  root_dir <- file.path(getwd(), "trial_lists_768")
  lab_report_file <- file.path(root_dir, "lab_balance_report.csv")
  file_report_file <- file.path(root_dir, "lab_file_detail_report.csv")
  item_report_file <- file.path(root_dir, "lab_item_detail_report.csv")
  
  lab_dirs <- sort(list.files(root_dir, pattern = "^lab_[0-9]{2}$", full.names = TRUE))
  if (length(lab_dirs) == 0) {
    stop(paste0("No lab folders found in: ", root_dir))
  }
  
  summaries <- list()
  file_details <- list()
  item_details <- list()
  
  for (lab_dir in lab_dirs) {
    result <- check_lab_folder(lab_dir)
    summaries[[length(summaries) + 1]] <- result$summary
    file_details[[length(file_details) + 1]] <- result$files
    item_details[[length(item_details) + 1]] <- result$items
  }
  
  lab_report <- do.call(rbind, summaries)
  file_report <- do.call(rbind, file_details)
  item_report <- do.call(rbind, item_details)
  
  write.csv(lab_report, lab_report_file, row.names = FALSE)
  write.csv(file_report, file_report_file, row.names = FALSE)
  write.csv(item_report, item_report_file, row.names = FALSE)
  
  print(lab_report)
  
  cat("\nOverall pass rate:", sum(lab_report$overall_ok), "of", nrow(lab_report), "labs passed\n")
  cat("Lab summary written to:", lab_report_file, "\n")
  cat("File-level report written to:", file_report_file, "\n")
  cat("Item-level report written to:", item_report_file, "\n")
  
  failed_items <- item_report[!item_report$both_roles_present, ]
  if (nrow(failed_items) > 0) {
    cat("\nItems missing one role in at least one lab:\n")
    print(failed_items)
  } else {
    cat("\nAll items appeared as both familiar and novel in every lab.\n")
  }
  
  invisible(list(
    lab_report = lab_report,
    file_report = file_report,
    item_report = item_report
  ))
}

# ------------------------------------------------------------
# Run generator, then checker
# ------------------------------------------------------------

generate_trials()
check_all_labs()