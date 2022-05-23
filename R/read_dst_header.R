# TODO:
#       validate - if e.g. warnings return a flag
#                  possibly also date checking
#       add t2
#

#' Read header data, normally from .DAT file
#'
#' @description The data header in .DAT file containing a detailed description
#' of how the *.DAT file is configured. This feature is used by SeaStar when graphically
# representing the data, and is practical if the user wants to import the data into other
# applications or databases.
#'
#' @param fil file name, including path
#' @param long boolean(default TRUE). Use FALSE better intend to read in lots of files
#'
#' @return a tibble with header data and detail file information
#' @export
#'
#' @examples
#' library(staroddi)
#' header <- read_dst_header(system.file("demos/1M9380.DAT", package = "staroddi"))
#' header

read_dst_header <- function(fil, long = TRUE) {

  file_info <- fs::file_info(fil)

  RAW <- readLines(fil, encoding = "latin1")
  n.comments <- stringr::str_detect(RAW, "^#") |> sum()
  # need to know the minimum number of comments from Sigmar
  if(n.comments == 0 | !stringr::str_detect(RAW[1], "Date-time:")) return(NULL)

  n.data <- length(RAW) - n.comments
  if(n.data == 0) return(NULL)

  t1 <- RAW[n.comments + 1] |> stringr::str_split("\t", simplify = TRUE)
  t1 <- t1[1,2]
  # skip for now - generates error in some reading - check upstream
  #t2 <- RAW[length(RAW)] |> stringr::str_split("\t", simplify = TRUE)
  #t2 <- t2[1,2]

  meta <-
    RAW[1:n.comments] |>
    # should really not be needed, but check e.g. "/u2/merki/gogn/DSTmilli-gogn/M1088/1M1088.DAT"
    unique() |>
    stringr::str_replace("#", "") |>
    stringr::str_split("\t")

  res <-
    tibble::tibble(var = purrr::map_chr(meta, 2),
                   val = purrr::map_chr(meta, 3)) |>
    dplyr::bind_rows(tibble::tribble(~var, ~val,
                                     "n", as.character(n.data),
                                     "t1", t1))

  # The Date def has most often two elements
  #  TODO: improve this
  names(meta) <- purrr::map_chr(meta, 2)
  x <- purrr::pluck(meta, "Date def.:")[4]
  if(!is.null(x)) {
    res <-
      res |>
      dplyr::mutate(val = ifelse(var == "Date def.:",
                                 paste0(val, x),
                                 val))
  }

  if(long) {

  res <-
    res |>
    dplyr::mutate(comment = dplyr::case_when(var == "Field separation:" & val == "0" ~ "tab",
                                             var == "Field separation:" & val == "0" ~ "space",
                                             var == "Decimal point:" & val == "0" ~ ",",
                                             var == "Decimal point:" & val == "1" ~ ".",
                                             var == "Channel 1:" ~ tolower(stringr::str_sub(val, 1, 5)),
                                             var == "Channel 2:" ~ tolower(stringr::str_sub(val, 1, 5)),
                                             var == "Channel 3:" ~ tolower(stringr::str_sub(val, 1, 5)),
                                             var == "Channel 4:" ~ tolower(stringr::str_sub(val, 1, 5)),
                                             var == "Channel 5:" ~ tolower(stringr::str_sub(val, 1, 5)),
                                             TRUE ~ NA_character_)) |>
    dplyr::bind_rows(file_info |>
                       dplyr::mutate_all(as.character) |>
                       tidyr::gather(var, val))

  } else {
    res <-
      res |>
      # to keep the order
      dplyr::mutate(var = forcats::as_factor(var)) |>
      tidyr::spread(var, val, convert = TRUE)
    # NOTE
      #  seems to be that the format in the Date-time: can be of the form
      #   mdy_hms but that the actual data can be of the form
      #   dmy_hms
      # see e.g.: "/u2/merki/gogn/Hrognkelsi/M14613/1M14613.DAT"
      # here try both
    dtm <- res$`Date-time:`
    dtm2 <- c(lubridate::dmy_hms(dtm), lubridate::mdy_hms(dtm))
    dtm2 <- dtm2[!is.na(dtm2)]
    dtm2 <- dtm2[1]
    res$`Date-time:` <- dtm2
    res <-
      res |>
      dplyr::bind_cols(file_info)
  }

  return(res)

}


# # https://raw.githubusercontent.com/wiki/aodn/imos-toolbox/documents/Instruments/Star_ODDI/StarmonT.pdf

#
# The *.DAT file contains a data header, which is a detailed description of how the
# *.DAT file is configured. This feature is used by SeaStar |> when graphically
# representing the data, and is practical if the user wants to import the data into other
# applications or databases. Most of these descriptive items are derived from the ...
#
# Each header item is contained in one line, and all header lines start with a #
# (bookmark) and a number. Then follows a description of the header item, and then 1-4
# directives, all separated by tabs. Eventually a comment trails the directives, preceded
# by a semicolon (;).
#
# The following is a description of the directives contained in the data header.
# # #0 Date-time: 1
# #    The date and time of that particular*.DAT file creation.
# time.creation <- pluck(meta, "Date-time:", 3)
# # #1 Recorder: 1
# #    The recorder and sequence number, f. example 12T0801.
# #    Used for confirmation and as a graph header.
# recorder <- pluck(meta, "Recorder:", 3)
# # #2 File type: 1
# #    Describes file column structure, mainly if Date and Time are joined (0-3).
# #       0: Result file Number Date Time Channels 1-3
# #       1: Result file Number Date & Time Channels 1-3
# #       2: Binary file Number Binary Channels 1-3
# filetype <- pluck(meta, "File type:", 3) |> as.integer()
# # #3 Columns: 1
# #    Total number of columns (3-6).
# ncol <- pluck(meta, "Columns:", 3) |> as.integer()
# # #4 Channels: 1
# #    Number of measurement parameters (1-3).
# channels <- pluck(meta, "Channels:", 3) |> as.integer()
# # #5 Field separation: 1
# # Separation between columns (0,1).
# # 0: Tab
# # 1: space
# sep <- pluck(meta, "Field separation:", 3) |> as.integer()
# # #6 Decimal point: 1
# # (0,1)
# # 0: Comma
# # 1: Dot
# decimal <- pluck(meta, "Decimal point:", 3) |> as.integer()
# # #7 Date def.: 1
# # Date format (0,1)
# # 0: dd mm yy
# # 1: mm dd yy
# dformat <- pluck(meta, "Time def.:", 3) |> as.integer()
# # #8 Time def.: 1
# # Time separation (0,1)
# # 0: Colon (:)
# # 1: Dot (.)
# tformat <- pluck(meta, "Time def.:", 3) |> as.integer()
# # #9 Channel 1: 4
# #    Set as left axis, normally temperature. The four directives are:
# #     Axis header, unit:    Text
# #     Column header, unit:  Text
# #     Number of decimals:   (0-3)
# #     Axis direction        (1,2)
# #       1: Ascending
# #       2: Descending
# ch1 <- pluck(meta, "Channel 1:")
# ch1.what <- ch1[3]
# # #10 Channel 2: 4
# #     Set as right axis, normally pressure. The four directives are:
# #     Axis header, unit:    Text
# #     Column header, unit:  Text
# #     Number of decimals:   (0-3)
# #     Axis direction        (1,2)
# #       1: Ascending
# #       2: Descending
# ch2 <- pluck(meta, "Channel 2:")
# ch2.what <- ch2[3]
# # #11 Re-conversion: 1
# # Definition on conversion / Extra header in graph (0,1)
# # 0: Original conversion
# # 1: Reconverted
#
# # #12 No temperature correction: 1
# # Only in re-conversion, temperature correction in pressure calculations, is used
# # as extra header information (0,1).
# # 0: Normal correction
# # 1: Correction was disabled
#
# # #13 Pressure offset correction:2
# # In pressure calculation, a zero offset can be adjusted. The two directives are:7
# # Applied: (0,1)
# # 0: No adjustment was made
# # 1: A zero offset was adjusted.
# # Adjustment value: An integer value in mbar
#
# # #14 Channel 3: 4
# #     Set as extra right axis, normally salinity. The four directives are:
# #     Axis header, unit:    Text
# #     Column header, unit:  Text
# #     Number of decimals:   (0-3)
# #     Axis direction        (1,2)
# #       1: Ascending
# #       2: Descending
# ch3 <- pluck(meta, "Channel 3:")
# ch3.what <- ch3[3]

# fil <- "/u2/merki/gogn/DSTmilli-gogn/C1244/4C1244.DAT"

