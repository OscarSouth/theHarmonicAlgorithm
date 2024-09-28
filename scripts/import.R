library(tidyverse)

bach <- read_csv(
  "/home/oscarsouth/.stack/global-project/data/jsbach_chorals_harmony.data",
  col_names = c(
    "seq", "event",
    "0", "1", "2", "3", "4", "5",
    "6", "7", "8", "9", "10", "11",
    "fund", "acc", "label"
  ), cols(
    seq = col_character(),
    event = col_integer(),
    `0` = col_character(),
    `1` = col_character(),
    `2` = col_character(),
    `3` = col_character(),
    `4` = col_character(),
    `5` = col_character(),
    `6` = col_character(),
    `7` = col_character(),
    `8` = col_character(),
    `9` = col_character(),
    `10` = col_character(),
    `11` = col_character(),
    fund = col_character(),
    acc = col_integer(),
    label = col_character()
  )
)

bach <-
  bach %>%
  select(seq, event, fund, acc, label) %>%
  add_column(pitch = bach %>%
               select(`0`:`11`) %>%
               t() %>%
               as.data.frame() %>%
               unname() %>%
               map(function(x) str_which(x, "YES")-1)
  ) %>%
  select(pitch, fund)

# bachMatrix <<-
#   reduce(bach$pitch,
#          rbind,
#          matrix(,0,bach$pitch %>%
#                   map(length) %>%
#                   rapply(c) %>%
#                   max()
#          )
#   ) %>%
#   unname()
# 
# bachFund <<- bach$fund

noteNameToPitchClass <- function(pitch) {
  pcMap <- list(
    "C"  = 0, "C#" = 1, "D-" = 1, "Db" = 1,
    "D"  = 2, "D#" = 3, "E-" = 3, "Eb" = 3,
    "E"  = 4, "E#" = 5, "F-" = 4, "Fb" = 4,
    "F"  = 5, "F#" = 6, "G-" = 6, "Gb" = 6,
    "G"  = 7, "G#" = 8, "A-" = 8, "Ab" = 8,
    "A"  = 9, "A#" = 10, "B-" = 10, "Bb" = 10,
    "B"  = 11, "B#" = 0, "C-" = 11, "Cb" = 11
  )
  return(pcMap[[pitch]])
}

pitchClassToNoteNameSharp <- function(pitch) {
  pc = (pitch %% 12) %>% as.character()
  noteMap <- list(
    "0" = "C", "1" = "C#",  "2" = "D",
    "3" = "D#", "4" = "E",  "5" = "F",
    "6" = "F#", "7" = "G",  "8" = "G#",
    "9" = "A", "10" = "A#", "11" = "B"
  )
  return(noteMap[[pc]])
}

parseAndOrderPitches <- function(noteString) {
  notes <- unlist(strsplit(noteString, " "))
  noteNames <- gsub("[0-9]", "", notes)
  registers <- as.integer(gsub("[^0-9]", "", notes))
  pitchClasses <- sapply(noteNames, noteNameToPitchClass)
  pcRegister <- pitchClasses + (registers * 12)
  sortedPitchClasses <- pitchClasses[order(pcRegister)] %>% unname()
  return(sortedPitchClasses)
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
  return(result)
}

# main_path <- "/home/oscarsouth/Desktop/musicdata/YCACL"
# 
# csv_files <- list.files(path = main_path, pattern = "\\.csv$", 
#                         recursive = TRUE, full.names = TRUE)
# 
# read_and_select <- function(file) {
#   read_csv(file, col_names = c("offset", "noteNames")) %>%
#     select(offset, noteNames)
# }
# 
# data <- map_dfr(csv_files, read_and_select)

data <- read_csv("/home/oscarsouth/.stack/global-project/data/YCACL.csv")

#data <- data[1:10000,]#keep only first 10000 rows

data <-
  data %>%
  filter(str_count(noteNames, " ") >= 2)

data$noteNames <-
  data$noteNames %>%
  str_replace_all("--", "-") %>%
  str_replace_all("#-", "#")

data$orderedPitches <-
  data$noteNames %>%
  lapply(parseAndOrderPitches) %>%
  lapply(removeDupsFromRight)

data <- data[lengths(data$orderedPitches) >= 3, ]

data$fundamental <-
  data$orderedPitches %>%
  lapply(function(pitches) {
    pitchClassToNoteNameSharp(pitches[1])
  }) %>%
  unlist()

data <-
  data %>%
  select(orderedPitches, fundamental)

# bach <-
#   bach %>%
#   select(pitch, fund)

colnames(bach) <- colnames(data)

data <- rbind(data, bach)
data <- rbind(data, bach)
data <- rbind(data, bach)
data <- rbind(data, bach)
data <- rbind(data, bach)
data <- rbind(data, bach)
data <- rbind(data, bach)
data <- rbind(data, bach)
data <- rbind(data, bach)
data <- rbind(data, bach)

max_length <- max(sapply(data$orderedPitches, length))
pitchMatrix <- matrix(NA, nrow = length(data$orderedPitches), 
                          ncol = max_length)
for (i in seq_along(data$orderedPitches)) {
  row_data <- data$orderedPitches[[i]]
  pitchMatrix[i, 1:length(row_data)] <- row_data
  
  if (length(row_data) < max_length) {
    pitchMatrix[i, (length(row_data) + 1):max_length] <- 
      rep(row_data, length.out = max_length - length(row_data))
  }
}

pitchMatrix <- unname(pitchMatrix)

fund <- unlist(data$fundamental)

write.csv(pitchMatrix, file = 
  "/home/oscarsouth/.stack/global-project/data/pitchMatrix.csv", 
  row.names = FALSE)

write.csv(fund, file = 
  "/home/oscarsouth/.stack/global-project/data/fund.csv", 
  row.names = FALSE)





















# main_path <- "/home/oscarsouth/Desktop/musicdata/YCACL"
# 
# csv_files <- list.files(path = main_path, pattern = "\\.csv$", 
#                         recursive = TRUE, full.names = TRUE)
# 
# read_and_select <- function(file) {
#   read_csv(file, col_names = c("offset", "noteNames")) %>%
#     select(offset, noteNames)
# }
# 
# data <- map_dfr(csv_files, read_and_select)
# 
# write_csv(data, "/home/oscarsouth/.stack/global-project/data/YCACL.csv")



