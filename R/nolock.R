#'* ---------------------------------------------------------------------------*
#'*                  Append 'WITH (nolock)' to 'SQL' Queries                   *
#'* ---------------------------------------------------------------------------*

crayon <- rstudioapi <- utils <- stringr <- NULL

nolock <- function(query = NULL) {
  pacman::p_load(crayon, rstudioapi, utils, stringr)

  #'* Loading data *

  message(crayon::blue$bold("Please enter a valid 'SQL' query into the opened temporal file..."))

  temp_nolock_path <- tempfile(pattern = "nolock_", fileext = ".sql")
  # write("", file = temp_nolock_path)

  # ... rest of the function ...

  if (is.null(query)) {
    if (interactive()) {
      utils::file.edit(temp_nolock_path)
      if (file.exists(temp_nolock_path)) {
        sql_code <- readLines(temp_nolock_path)
      } else {
        stop("File not found or was cancelled.")
      }
    } else {
      # Non-interactive mode.
      message("Running in non-interactive mode. No file editing possible.")
      return(invisible(NULL))
    }
  } else {
    sql_code <- query
  }

  if (length(grep("SELECT|FROM|WHERE|JOIN|UPDATE|DELETE", sql_code, ignore.case = TRUE)) == 0) {
    stop("\nInvalid or empty 'SQL' query.")
  }

  if (length(sql_code) == 0 || all(sql_code == "")) {
    return(invisible(NULL))
  } else {
    sql_code <- paste(unlist(sql_code), collapse = " \r\n")

    #' * Splitting the SQL query into fragments *

    sql_code <- stringr::str_split(sql_code, "(?<=[Jj][Oo][Ii][Nn]\\s|[Ff][Rr][Oo][Mm]\\s)")[[1]]

    # Function to merge fragments ending with "next from" (considering case sensitivity) with the next fragment.
    merge_next_from <- function(sql_code) {
      i <- 1
      while (i < length(sql_code)) {
        # Checking if the fragment ends with "next from", considering case sensitivity.
        if (grepl("[nN][eE][xX][tT]\\s*[fF][rR][oO][mM]\\s*$", sql_code[i])) {
          # Combining the fragment with the next one and removing the next fragment.
          sql_code[i] <- paste0(sql_code[i], sql_code[i + 1])
          sql_code <- sql_code[-(i + 1)]
        } else {
          i <- i + 1
        }
      }
      return(sql_code)
    }
    sql_code <- merge_next_from(sql_code)

    #'* Pattern to find the table name and alias *

    omit_words <- c( # | ommited words
      "left", "right", "full", "outer", "inner", "cross", "natural", "join", "where",
      "group", "having", "order", "limit", "fetch", "union", "except", "pivot", "unpivot",
      "values", "sample", "qualify", "start", "changes", "window", "intersect", "using",
      "drop", "into", "with", "then", "null", "not", "is", "IS", "when", "truncate",
      "select", "exec", "use", "drop", "if", "on", "declare", "set", "create", "delete",
      "while", "for", "user", "option", "cursor"
    )
    for (i in 1:length(omit_words)) {
      omit_words[i] <- gsub("(.)", "[\\1\\U\\1]", omit_words[i], perl = TRUE)
      omit_words[i] <- paste0("|", omit_words[i], "\\b")
    }
    omit_words_s <- c("select", "create", "declare", "delete", "is", "IS") # | ommited words after 'WITH'
    for (i in 1:length(omit_words_s)) {
      omit_words_s[i] <- gsub("(.)", "[\\1\\U\\1]", omit_words_s[i], perl = TRUE)
      omit_words_s[i] <- paste0("|\\s*?", omit_words_s[i], "\\b")
    }

    regex_pattern_cut <- paste0(
      "^", "(?!\\s*?[wW][iI][tT][hH]\\b", paste(omit_words_s, collapse = ""),
      ")\\s*?", "(\\w+\\.)*\\w+", "(\\s*[aA][sS])?",
      "(\\s*(?![oO][nN]\\b", paste(omit_words, collapse = ""),
      ")\\w+)?", "\\s?", "\\K", "(?!.*?\\b[wW][iI][tT][hH]\\b)",
      "(?!\\s*\\()"
    )

    regex_pattern_match <- paste0(
      "^", "(?!\\s*?[wW][iI][tT][hH]", paste(omit_words_s, collapse = ""),
      ")\\s*?([\\w\\[\\].]+\\.)*", "([\\w\\[\\].]+\\.)*[\\w\\[\\]]+(\\s*[aA][sS])?",
      "(\\s*(?![oO][nN]\\b", "(?!.*?\\b[wW][iI][tT][hH]\\b)",
      paste(omit_words, collapse = ""), ")\\w+)?", "\\s?",
      "(?!.*?\\b[wW][iI][tT][hH]\\b)"
    )

    rm(i, omit_words)

    #'* Splitting the text by names and aliases *

    SQL_PATTERN_MATCH <- na.omit(stringr::str_extract(sql_code, regex_pattern_match))

    # Remove strings that end with a digit
    SQL_PATTERN_MATCH <- SQL_PATTERN_MATCH[!grepl("^(.*\\d)\\s*$", SQL_PATTERN_MATCH)]

    pattern <- "^\\s*\\w+(\\s+\\w+)?\\s*$"
    SQL_PATTERN_MATCH <- SQL_PATTERN_MATCH[!grepl(pattern, SQL_PATTERN_MATCH)]

    # Match table names with aliases, reset position, then split the text.
    unlist(strsplit(sql_code, regex_pattern_cut, perl = TRUE))

    sql_code <- unlist(strsplit(sql_code, regex_pattern_cut, perl = TRUE))

    #'* Merging the text *

    # Matrix indicating whether there's a match to the regex_pattern.
    regex_match <- cbind(sql_code, numeric(length(sql_code)))

    for (i in 1:length(sql_code)) {
      regex_match[i, 2] <- !is.na(match(sql_code[i], SQL_PATTERN_MATCH))
    }

    # Cleaning: removing spaces before 'WITH (nolock)' if they exist.
    for (i in 1:nrow(regex_match)) {
      if (regex_match[i, 2] == TRUE) {
        if (stringr::str_sub(regex_match[i, 1], start = -1) == " ") {
          regex_match[i, 1] <- substr(regex_match[i, 1], 1, nchar(regex_match[i, 1]) - 1)
        }
      } else {
        NULL
      }
    }

    for (i in 1:nrow(regex_match)) {
      if (regex_match[i, 2] == TRUE) {
        regex_match[i, 1] <- paste0(regex_match[i, 1], " WITH (nolock) ")
      } else {
        NULL
      }
    }

    sql_code <- paste(regex_match[, 1], collapse = "")
    rm(i, regex_pattern_cut, regex_pattern_match, regex_match)

    #'* Saving the operation result to a file *'

    # sql_code <- paste0("/* ('Cmd+Ctrl+L') in 'DataGrip' to reformat: */ \r\n", sql_code)
    writeLines(sql_code, temp_nolock_path)

    if (interactive() && is.null(query)) {
      if (rstudioapi::isAvailable()) {
        rstudioapi::navigateToFile(temp_nolock_path)
      } else {
        file.edit(temp_nolock_path)
      }
    } else {
      message(sql_code)
    }

    #'* Copying the result to the clipboard *'

    result <- paste(readLines(temp_nolock_path), collapse = " \r\n")
    if (Sys.info()["sysname"] == "Windows") {
      writeLines(result, pipe("clip.exe"))
    } else if (Sys.info()["sysname"] == "Darwin") {
      system(paste("echo", shQuote(result), "| pbcopy"))
    } else {
      message("Unsupported operating system.")
    }


    #' * Print the changes made: *'

    message_header <- paste0(
      "\n",
      crayon::black$bold(sprintf("\nNumber of tables modified: %d\n", length(SQL_PATTERN_MATCH))),
      crayon::black(paste0(rep("\u2500", 85), collapse = ""))
    )
    message(message_header)

    # cleaning: replace more than one space with a single space
    SQL_PATTERN_MATCH <- stringr::str_replace_all(SQL_PATTERN_MATCH, "\\s+", " ")
    SQL_PATTERN_MATCH <- stringr::str_trim(SQL_PATTERN_MATCH)

    if (length(SQL_PATTERN_MATCH) > 0) {
      # Determine the length of the longest string in SQL_PATTERN_MATCH
      max_length <- max(nchar(gsub("\\h+", " ", SQL_PATTERN_MATCH, perl = TRUE))) + 2

      # Loop through the table names and print the formatted string
      for (table_name in SQL_PATTERN_MATCH) {
        formatted_string <- gsub("\\h+", " ", table_name, perl = TRUE)
        combined_string <- paste0(
          crayon::blue(formatted_string),
          crayon::yellow(" WITH "),
          crayon::yellow$bold("(nolock)")
        )

        formatted_message <- sprintf(
          crayon::black(paste0("%-", max_length, "s", "   ", "%-", max_length + 14, "s")),
          formatted_string,
          combined_string
        )

        message(formatted_message) # This will print the formatted message using message()
      }
    }

    message_footer <- paste0(crayon::black(paste0(rep("\u2500", 85), collapse = "")))
    message(message_footer)
  }
}
