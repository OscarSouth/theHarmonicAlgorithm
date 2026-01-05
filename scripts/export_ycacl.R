#!/usr/bin/env Rscript
# Export YCACL slices into a normalized CSV that downstream Haskell code can consume.
# The exporter performs the following musical transforms:
#   1. Limits the corpus to a curated composer list (edit allowed_composers below).
#   2. Parses every note token, preserving accidentals and octave signs so spellings such
#      as B-1 vs B--1 remain distinct.
#   3. Orders pitch classes by absolute register (low → high), deduplicating the upper
#      voices to focus the Markov model on harmonic content rather than voicing doublings.
#   4. Captures the true low voice (fundamental) for each slice; the output now carries
#      this as a pitch-class integer (0–11) so bass-centric analytics can key off it later.
#   5. Filters out slices with less than three unique pitch classes or more than max_voices
#      (default 7) to bias toward harmonically meaningful events.
suppressPackageStartupMessages(library(tidyverse))

args <- commandArgs(trailingOnly = TRUE)
source_dir <- if (length(args) >= 1) args[[1]] else file.path("..", "musicdata", "YCACL")
metadata_path <- if (length(args) >= 2) args[[2]] else file.path("..", "musicdata", "YCAC-metadata.csv", "YCAC-metadata.csv")
output_path <- if (length(args) >= 3) args[[3]] else file.path(".", "data", "ycacl_sequences.csv")
max_voices <- if (length(args) >= 4) as.integer(args[[4]]) else 7

allowed_composers <- c()
# allowed_composers <- c("debussy", "stravinsky")

normalize_composer <- function(name) {
  name %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "")
}

if (!dir.exists(source_dir)) {
  stop(sprintf("YCACL source directory not found: %s", source_dir))
}

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

load_metadata <- function(path) {
  tryCatch({
    meta <- read_csv(path, show_col_types = FALSE)
    if (!all(c("File", "Composer") %in% names(meta))) {
      return(tibble())
    }
    meta %>%
      mutate(file_key = tools::file_path_sans_ext(basename(File))) %>%
      select(file_key, composer = Composer)
  }, error = function(e) tibble())
}

metadata <- load_metadata(metadata_path)
metadata_map <- if (nrow(metadata) > 0) metadata else tibble()

noteNameToPitchClass <- function(pitch) {
  pcMap <- list(
    "C" = 0, "C#" = 1, "D-" = 1, "Db" = 1,
    "D" = 2, "D#" = 3, "E-" = 3, "Eb" = 3,
    "E" = 4, "E#" = 5, "F-" = 4, "Fb" = 4,
    "F" = 5, "F#" = 6, "G-" = 6, "Gb" = 6,
    "G" = 7, "G#" = 8, "A-" = 8, "Ab" = 8,
    "A" = 9, "A#" = 10, "B-" = 10, "Bb" = 10,
    "B" = 11, "B#" = 0, "C-" = 11, "Cb" = 11
  )
  pcMap[[pitch]]
}

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0 || all(is.na(lhs))) {
    rhs
  } else {
    lhs
  }
}

formatFundamental <- function(symbol, register) {
  # Keeps the accidental spelling and octave sign so downstream debugging can see
  # which literal token we interpreted as the fundamental.
  suffix <- if (register == 0) "0" else as.character(register)
  paste0(symbol, suffix)
}

# Parse a single note token (e.g., "B--1"). YCACL frequently lists notes from high → low,
# so we need to parse both accidentals and register hints before we can resort the stack.
# Tokens can encode three semantics inside their hyphen runs:
#   - Accidentals (single hyphen immediately after the letter → flat)
#   - Negative octave markers (hyphen immediately before the numeric run)
#   - A mix of both (e.g., "B--1" = Bb in octave −1)
# This function preserves the accidental in the symbol while mapping the trailing hyphens
# onto octave signs when appropriate. The heuristic is:
#   * If there are ≥2 trailing hyphens or the token already contains an accidental before
#     the numeric run, treat one hyphen as the octave sign and the rest as accidentals.
#   * Otherwise, assume a positive octave.
# These rules ensure `B-1` (Bb in +1) remains distinct from `B--1` (Bb in −1).
parseNoteToken <- function(token) {
  token <- str_trim(token)
  if (token == "" || !str_detect(token, "^[A-Ga-g]")) {
    return(NULL)
  }

  letter <- str_to_upper(str_sub(token, 1, 1))
  rest <- str_sub(token, 2)
  rest <- rest %>%
    str_replace_all("♭", "-") %>%
    str_replace_all("♯", "#") %>%
    str_replace_all("b", "-")

  digits_match <- str_match(rest, "(\\d+)$")

  if (is.na(digits_match[, 2])) {
    prefix <- rest
    register <- 0
  } else {
    digits <- digits_match[, 2]
    prefix <- str_sub(rest, 1, nchar(rest) - nchar(digits))
    hyphen_run <- str_match(prefix, "(-+)$")[, 2]
    hyphen_count <- ifelse(is.na(hyphen_run), 0, str_length(hyphen_run))
    prefix_without_run <- if (hyphen_count > 0) {
      str_sub(prefix, 1, nchar(prefix) - hyphen_count)
    } else {
      prefix
    }

    use_register_sign <- FALSE
    if (hyphen_count > 0) {
      use_register_sign <- hyphen_count > 1 || str_length(prefix_without_run) > 0
    }

    register_sign <- if (use_register_sign) -1 else 1
    remaining_hyphens <- if (use_register_sign) max(hyphen_count - 1, 0) else hyphen_count
    prefix <- paste0(prefix_without_run, str_dup("-", remaining_hyphens))
    register <- register_sign * as.integer(digits)
  }

  symbol <- paste0(letter, prefix)
  pc <- noteNameToPitchClass(symbol)

  if (is.null(pc)) {
    return(NULL)
  }

  tibble(
    pc = pc,
    register = register,
    absolute = register * 12 + pc,
    fundamental = formatFundamental(symbol, register)
  )
}

chooseFundamentalRow <- function(df) {
  # Skip extremely low doublings when the same pitch class reappears in higher octaves.
  for (idx in seq_len(nrow(df))) {
    candidate <- df[idx, ]
    same_pc <- df[df$pc == candidate$pc, , drop = FALSE]
    higher_same_pc <- same_pc[same_pc$register > candidate$register, , drop = FALSE]

    # Treat as a doubling if the bass register is sub-zero and a matching pitch exists
    # within the next two octaves (<= 24 semitones above). This captures pedals like
    # A--1 stacked under A-1 / A1 but keeps legitimate low fundamentals elsewhere.
    if (candidate$register <= -1 && nrow(higher_same_pc) > 0) {
      nearest <- min(higher_same_pc$register - candidate$register)
      if (nearest <= 2) {
        next
      }
    }

    return(candidate)
  }

  # Fallback: if every pitch class is duplicated with no clear candidate, return the
  # absolute lowest voice so we at least stay deterministic.
  df[1, , drop = FALSE]
}

parsePitchBundle <- function(noteString) {
  tokens <- str_split(str_squish(noteString), "\\s+")[[1]]
  parsed <- map(tokens, parseNoteToken) %>% compact()

  if (length(parsed) == 0) {
    return(list(
      ordered = numeric(0),
      fundamental_pc = NA_real_,
      fundamental_note = NA_character_
    ))
  }

  # Resort by absolute pitch so the first row is always the lowest voice even if the
  # source listed notes from soprano → bass.
  df <- bind_rows(parsed) %>% arrange(absolute)

  fundamental_row <- chooseFundamentalRow(df)

  list(
    ordered = removeDupsFromRight(df$pc),
    fundamental_pc = fundamental_row$pc,
    fundamental_note = fundamental_row$fundamental
  )
}

removeDupsFromRight <- function(vec) {
  result <- c()
  seen <- c()
  for (val in rev(vec)) {
    if (!(val %in% seen)) {
      result <- c(val, result)
      seen <- c(seen, val)
    }
  }
  result
}

csv_files <- list.files(path = source_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
if (length(csv_files) == 0) {
  stop(sprintf("No YCACL CSV files found under %s", source_dir))
}

build_piece_id <- function(path) {
  tools::file_path_sans_ext(basename(path))
}

deduce_composer <- function(path) {
  key <- build_piece_id(path)
  if (nrow(metadata_map) > 0) {
    row <- metadata_map %>% filter(file_key == key) %>% slice_head(n = 1)
    if (nrow(row) == 1) {
      return(row$composer[[1]])
    }
  }
  basename(dirname(path))
}

process_file <- function(file_path) {
  piece_id <- build_piece_id(file_path)
  composer <- deduce_composer(file_path)
  suppressWarnings({
    df <- read_csv(
      file_path,
      col_names = c("offset", "noteNames"),
      col_types = cols(
        offset = col_character(),
        noteNames = col_character()
      )
    )
  })
  df %>%
    mutate(piece = piece_id, composer = composer) %>%
    select(composer, piece, noteNames)
}

message(sprintf("Scanning %d YCACL files ...", length(csv_files)))
raw_data <- map_dfr(csv_files, process_file) %>%
  mutate(composer_key = normalize_composer(composer))

if (length(allowed_composers) > 0) {
  raw_data <- raw_data %>%
    filter(composer_key %in% allowed_composers)
}

raw_data <- raw_data %>% select(-composer_key)

cleaned <- raw_data %>%
  filter(str_count(noteNames, " ") >= 2) %>%
  mutate(parsed = map(noteNames, parsePitchBundle)) %>%
  mutate(
    ordered = map(parsed, "ordered"),
    fundamental_note = map_chr(parsed, ~ .x$fundamental_note %||% NA_character_),
    fundamental_pc = map_dbl(parsed, ~ .x$fundamental_pc %||% NA_real_)
  ) %>%
  # Keep progressions that still contain ≥3 distinct pitch classes after deduping doublings;
  # this is the minimal information required to infer functional harmony.
  filter(map_lgl(ordered, ~ length(.x) >= 3)) %>%
  filter(map_lgl(ordered, ~ length(.x) <= max_voices)) %>%
  # `fundamental_pc` only goes NA when the parser cannot identify a low voice (e.g.,
  # corrupt token). This guard prevents us from emitting malformed rows.
  filter(!is.na(fundamental_pc))

if (nrow(cleaned) == 0) {
  stop("No valid YCACL records after filtering")
}

prepared <- cleaned %>%
  group_by(composer, piece) %>%
  mutate(order = row_number()) %>%
  arrange(order, .by_group = TRUE) %>%
  ungroup() %>%
  mutate(pitches = map_chr(ordered, ~ paste(.x, collapse = " "))) %>%
  mutate(fundamental = as.integer(floor(fundamental_pc))) %>%
  select(composer, piece, order, pitches, fundamental)

write_csv(prepared, output_path)
message(sprintf("YCACL export complete: %s (%d rows)", output_path, nrow(prepared)))
