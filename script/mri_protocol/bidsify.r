library(jsonlite)
library(dplyr)
library(readxl)
library(lubridate)
library(writexl)

##### Define paths #####
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript script.R </path/to/imagine_data/input_data/<gene_site> -b <batch date>")
}
gene_path <- args[1]  # <path>/imagine_data/input_data/<gene_site>
batch_idx <- which(args == "-b")

batch <- if (length(batch_idx) > 0) args[batch_idx + 1] else NULL

base_dir <- dirname(dirname(gene_path)) 
gene_site <- basename(gene_path)
gene <- sub("_.*$", "", gene_site)

nifti_dir <- file.path(gene_path, "NIFTI")

# Create bids folder
bids_dir <- file.path(base_dir, "share_data", gene_site, "BIDS")
dir.create(bids_dir, recursive = TRUE, showWarnings=FALSE)

# Define subs as list of subject codes from batch list file, with path to nifti dir
batch_list_file <- file.path(gene_path, paste0(gene, "_id_list_", batch, ".txt"))
sub_list <- readLines(batch_list_file)
subs <- file.path(nifti_dir, paste0("sub-", sub_list)) 

# Read in spreadsheet for DOBs
excel_filename <- paste0(gene, "_pt_identifiable_DONOTSHARE.xlsx")
excel_file <- file.path(gene_path, excel_filename)
pt_data <- read_excel(excel_file)

# Track duplicated output file names and name them to a different run later on
run_tracker <- list() 

# Create log file
log_file <- file.path(bids_dir, paste0(gene, "_bidsify_log_", batch, ".txt"))
cat("Pipeline started at", format(Sys.time()), "\n", file = log_file)

# Create an age spreadsheet
ages_file <- file.path(gene_path, paste0(gene, "_age_", batch, ".xlsx"))
# edit csv only if it is empty; if it is filled in, then read it
file_is_new <- !file.exists(ages_file) || nrow(read_excel(ages_file)) == 0
ages_df <- if (file_is_new) {
  data.frame(
    scan = character(),
    subject_code = character(),
    session_number = character(),
    age_at_scan = character(),
    stringsAsFactors = FALSE
  )
} else {
  read_excel(ages_file)
}


# CHECKS: all selected subs' niftis have (1) corresponding json, (2) date in nifti file name where needed
skipped_subs <- c()

nii_files <- unlist(lapply(
  subs,
  list.files,
  pattern = "\\.nii\\.gz$",
  full.names = TRUE,
  recursive = TRUE
))
json_files <- sub("\\.nii\\.gz$", ".json", nii_files)
has_14_digits <- grepl("\\d{14}", basename(nii_files))

missing_json <- nii_files[!file.exists(json_files)]
if (length(missing_json) > 0) {
  warning(
    paste(
      "Missing JSON for: \n",
      paste(basename(missing_json), collapse = "\n"),
      "\nProceeding without JSON for these files; information may be inaccurate."
    )
  )
}

if (file_is_new) {
  missing_date <- nii_files[!has_14_digits]
  if (length(missing_date) > 0) {
    stop(
      paste(
        "Missing date (14-digit number) for: \n",
        paste(basename(missing_date), collapse = "\n"),
        "\nPlease ensure every scan has a date (14-digit number) in the file name before running this script."
      )
    )
  }
}


##### Loop over dirs for each sub #####
for (sub in subs) {
  
  subname <- basename(sub)
  cat("Processing:", subname, "\n")
  cat(subname, ": START\n", file = log_file, append = TRUE)
  subcode <- sub("^sub-", "", subname)

  nifti_files <- list.files(sub, pattern="\\.nii\\.gz$", full.names = TRUE) # define as .nii files 
  if (length(nifti_files) == 0) {
    cat(subname, ": NO FILES\n", file = log_file, append = TRUE)
    skipped_subs <- c(skipped_subs, subname)
    next
  }

  cat(subname, ": PROCESSING ", length(nifti_files), " files\n", file = log_file, append = TRUE)
  
  
  # Extract acquisition time from corresponding JSONs - this is to order them so that run number is based on acq time
  acq_times <- sapply(nifti_files, function(f) {
    json_file <- sub("\\.nii\\.gz$", ".json", f)
    if (file.exists(json_file)) {
      json_data <- tryCatch(fromJSON(json_file), error=function(e) NULL)
      json_data$AcquisitionTime %||% json_data$AcquisitionDateTime %||% ""
    } else ""
  })
  # Order nifti files by acq time
  nifti_files <- nifti_files[order(acq_times)]

  
  ### Set up date-session mapping ### 
  if (file_is_new) {
    if (!subcode %in% pt_data$subject_code) {
      cat(subname, ": Skipped - not found in spreadsheet: pt_identifiable_DONOTSHARE.xlsx\n")
      cat(subname, ": SKIP - NOT IN SPREADSHEET\n", file = log_file, append = TRUE)
      skipped_subs <- c(skipped_subs, subname)
      next 
    }
    dates <- sapply(nifti_files, function(f) {
      fname <- basename(f)
      num_part <- regmatches(fname, gregexpr("\\d{14}", fname))[[1]][1]
      substr(num_part, 1, 8)
    })
    unique_dates <- sort(unique(dates))   # unique dates sorted chronologically
    ses_lookup <- setNames(seq_along(unique_dates), unique_dates)   # for each sub, map dates to a session code
  } else ""


  ### Loop over each .nii file per sub dir ###
  for (file in nifti_files) {
    cat("  ", "PROCESSING NIFTI\n", file = log_file, append = TRUE)

    # Extract each section of the file name as a string
    filename <- basename(file)
    filename_plain <- gsub("\\.(nii\\.gz|nii|json)$", "", filename) # get filename
    filename_lower <- tolower(filename_plain) # lowercase everything
    parts <- strsplit(filename_lower, "_")[[1]] # split filename by underscore

    # Read in corresponding json files
    json_file <- sub("\\.nii\\.gz$|\\.nii$", ".json", file) 
    json_data <- NULL
    if (file.exists(json_file)) {
        json_data <- tryCatch(
            fromJSON(json_file),
            error = function(e) {
                message("Error - Skipping JSON: ", json_file)
                return(NULL)
            }
        )
    }

    ######## EXTRACT SESSION (based on corresponding date) #############
    if (file_is_new) { # if using our dicom to nifti to bids
      num_part <- regmatches(filename_lower, gregexpr("\\d{14}", filename_lower))[[1]]
      if (length(num_part) == 0) {
        stop("No date found in filename: ", filename_lower)
        }
      date <- substr(num_part,1,8) #first 8 numbers of num_part is the date
      ses_code <- sprintf("%02d", ses_lookup[[date]]) # find the session code corresponding to date
      #cat("ses:", ses_code, "date:", date, "\n") # date corresponds with a session (ex. ses-01)

      # read in DOB from spreadsheet and calculate age at scan date; format as -XXyXXmXXd
      row <- pt_data[pt_data$subject_code == subcode, ]
      dob_string <- row$date_of_birth[1]
      dob <- as.Date(dob_string, format = "%Y-%m-%d")
      if (is.na(dob)) {
        stop(paste("No matching ID found for", subname))
      }
      scan_date <- as.Date(date, format = "%Y%m%d")
      age <- interval(dob, scan_date)
      p <- as.period(age)
      age_in_days <- floor(as.numeric(p, "days"))
      age_ymd <- if (age_in_days < 100) {
           paste0("00y00m", sprintf("%02dd", age_in_days))
      } else {
          sprintf("%02dy%02dm%02dd", p$year, p$month, p$day)
      } 

    } else { # if an existing age_<batch>.csv is provided 
      match_row <- ages_df[trimws(ages_df$scan)==filename,]
      if (nrow(match_row) == 0) {
        stop("No matching scan file found in ages_file for: ", filename)
      }
      ses_code <- sprintf("%02d", as.numeric(trimws(as.character(match_row$session_number[1]))))
    }


    ######## EXTRACT SCAN TYPE / SUFFIX ########

    # First by file name, if possible
    if (any(grepl("t1|t1w|mprage|mp rage|mp2rage|brain volume", parts[-1]))) {
      suffix <- "T1w"
    } else if (any(grepl("flair|dark-fluid", parts[-1]))) {
      suffix <- "FLAIR"
    } else if  (any(grepl("t2|t2w", parts[-1]))) {
      suffix <- "T2w"
    
    # Then by JSON
    } else if (!is.null(json_data)) {
        series_desc <- tolower(json_data$SeriesDescription %||% "")
        study_desc <- tolower(json_data$StudyDescription %||% "")

        meta_text <- paste(series_desc, study_desc)     # combine into one string

      if (grepl("t1|t1w", meta_text)) {
        suffix <- "T1w"
      } else if (grepl("flair|dark-fluid", meta_text)) {
        suffix <- "FLAIR"
      } else if (grepl("t2|t2w", meta_text)) {
        suffix <- "T2w"
      } else {
        suffix <- "unknown"}
    
    } else {
      suffix <- "unknown"} 
    
    print(suffix)

    ######## EXTRACT ACQUISITION #############

    # read json
    if (!is.null(json_data)) {
      acq_type <- tolower(json_data$MRAcquisitionType %||% "")
      series_desc <- tolower(json_data$SeriesDescription %||% "")
      contrast <- tolower(json_data$ContrastBolusIngredient %||% "")
      fieldstrength <- tolower(json_data$MagneticFieldStrength %||% "") 

      # read field strength as acq
      if (grepl("1.5", fieldstrength))  {
        acq <- "-15T"
      } else if (grepl("3|3.0", fieldstrength)) {
        acq <- "-3T"
      } else if (grepl("7|7.0", fieldstrength)) {
        acq <- "-7T"
      } else {
        acq <- ""}

      # If 2d scan, add plane and 2d tag to acq- 
      acq_text <- paste(acq_type, series_desc)
      if (grepl("2d", acq_type)) {
              if (any(grepl("tra|ax|axial|transverse", acq_text))) {
                plane <- "-ax2d"
              } else if (any(grepl("cor|coronal", acq_text))) { 
                plane <- "-cor2d"
              } else if (any(grepl("sag|sagittal", acq_text))) {
                plane <- "-sag2d"
              } else {
                plane <- "-2d"}    
          } else {
            plane <- ""}
    
      # add contrast tag if there is contrast
      if (grepl("gadolinium|gad|contrast", contrast)) {
        ce <- "-gad"
      } else {
        ce <- ""} 

    }
    acq_term <- ""
    ce_term <- ""
    if (nzchar(acq) || nzchar(plane)) {  # if any of these terms exist, then add to acq_term
      acq_term <- paste0("_acq", acq, plane) }
    if (nzchar(ce)) {  
      ce_term <- paste0("_ce", ce) }
    

    ###### Defining the file extensions to be reattached ######
    if (grepl("\\.nii\\.gz$", file)) {
      ext <- "nii.gz"
    } else if (grepl("\\.json$", file)) {
      ext <- "json"
    }


    ####### ADDING RUN: if there already is a file with the same name in run_tracker, +1 to run #######

    # Set base name as name so far WITHOUT run and ext
    base_name <- paste0(
      subname, "_",
      "ses-", ses_code, #age_format,
      acq_term, ce_term, "_", 
      suffix
      )

    # If base_name already exists in run_tracker (repeated name), then +1 to run. Otherwise, run=01
    if (ext == "nii.gz") {   # this is to make sure run doesn't +1 for corresponding json/nifti pairs ; should all be .nii.gz anyway
      if (!base_name %in% names(run_tracker)) {
        run_tracker[[base_name]] <- 1
      } else {
        run_tracker[[base_name]] <- run_tracker[[base_name]] + 1
      }
    }
    run <- sprintf("%02d", run_tracker[[base_name]]) # convert to 1 to 01 etc.


    ##### Stitch together the new file name ######
    new_name <- paste0(
      subname, "_",
      "ses-", ses_code, #age_format,
      acq_term, ce_term,
      "_run-", run,
      "_", suffix, 
      ".", ext
    )

    # Making the new directory paths
    anat_dir <- file.path(
      bids_dir, subname, paste0("ses-", ses_code), "anat") 
    dir.create(anat_dir, recursive = TRUE, showWarnings=FALSE) #create dirs to anat for each ses
      
    new_path <- file.path(anat_dir, new_name)


    # Prevent duplication if rerunning script
    if (file.exists(new_path)) {
      cat("File path already exists. Skipping:", new_name, "\n") 
      next
    }

    # add row to ages.csv
    if (file_is_new) {
      #cat(new_name, ",", subcode, ",", ses_code, ",", age_ymd, "\n", file = ages_file, append = TRUE)
      new_row <- data.frame(
      scan = new_name,
      subject_code = subcode,
      session_number = ses_code,
      age_at_scan = age_ymd,
      stringsAsFactors = FALSE
      )
      ages_df <- rbind(ages_df, new_row)
    } else { # if it already exists, replace scan name with new name
      match_scan <- which(trimws(as.character(ages_df$scan)) == filename)
      ages_df$scan[match_scan[1]] <- new_name
  }
    


    # Print the old and new file names
    cat("OLD:", file, "\n")
    cat("NEW:", new_path, "\n\n") 
    cat("  ", basename(new_path), "\n", file = log_file, append = TRUE)


    # Copy files into the anat folders as renamed name
    file.copy(
      from = file,
      to   = new_path
    )

    ### RENAME JSONS according to corresponding nifti names: if json_file exists (as defined in beginning of loop), replace .nii.gz with .json ###
    if (file.exists(json_file)) { 
      json_new_name <- sub("\\.nii\\.gz$", ".json", new_name)
      json_new_path <- file.path(anat_dir, json_new_name)
      file.copy(from = json_file, to = json_new_path)
    }
  }
  cat(subname, ": DONE\n", file = log_file, append = TRUE)
}

write_xlsx(ages_df, ages_file) # create ages spreadsheet if empty, and replace scan file names
final_ages_path <- file.path(bids_dir, paste0(gene, "_age_", batch, ".xlsx"))
copy_success <- file.copy(ages_file, final_ages_path, overwrite = TRUE)
if (!copy_success) {
  warning("Failed to copy age spreadsheet to BIDS folder: ", final_ages_path, ". Please manually copy it over before the next step.")
}

bids_dirs <- list.files(bids_dir, full.names = TRUE)
bids_dirs <- bids_dirs[file.info(bids_dirs)$isdir]
basename_bids_dirs <- bids_dirs[basename(bids_dirs) %in% basename(subs)]
nifti_dirs <-list.files(nifti_dir, full.names = TRUE) 
nifti_dirs <- nifti_dirs[file.info(nifti_dirs)$isdir]

cat("Bidsifying completed.", length(basename_bids_dirs),"out of", length(subs), "subjects successfully renamed and copied to:", bids_dir, "\n")
cat ("Subjects skipped:", skipped_subs, "\n")
cat("Completed at:", format(Sys.time()), "\n", file = log_file, append = TRUE)