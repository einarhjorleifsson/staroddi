#' Read profile data from StarOddi
#'
#' @param fil Name of the file to read
#'
#' @return A tibble with metadata stored in attributes
#' @export
#'
#' @examples
#' library(staroddi)
#' dst <- read_dst(system.file("demos/1M9380.DAT", package="staroddi"))
#' dst
#' attributes(dst)$header
read_dst <- function(fil) {

  not_all_na <- function(x) any(!is.na(x))

  lu <-
    tibble::tribble(~letter, ~audkenni,
                    "C", "ISL.CENTI.",
                    "G", "ISL.GPS.",
                    "M", "ISL.MILLI.",
                    "R", "ISL.RAF.")

  header <- read_dst_header(fil)

  n.comments <-
    fil |>
    readLines(n = 30) |>
    stringr::str_detect("^#") |>
    sum()


  delim <- header |> dplyr::filter(var == "Field separation:") |> dplyr::pull(comment)
  if(length(delim) == 0) {
    delim <- "tab"
  }

  dttm <- header |> dplyr::filter(var == "Date def.:") |> dplyr::pull(val)
  dttm <- stringr::str_sub(dttm, 1, 1)
  if(length(dttm) == 0) {
    dttm <- "0"
  }

  ncol <- header |> dplyr::filter(var == "Columns:") |> dplyr::pull(val)
  if(length(ncol) == 0) ncol <- 4
  if(ncol == "4") cn <- c(".rid", "time", "temp", "depth")
  if(ncol == "5") cn <- c(".rid", "date", "time", "temp", "depth")


  DATA <-
    readr::read_delim(fil,
               na = c("____"),
               col_names = cn,
               col_types = "c",
               delim = "\t",
               skip = n.comments,
               show_col_types = FALSE,
               guess_max = 1e5) |>
    dplyr::mutate(temp = as.numeric(stringr::str_replace(temp, ",", ".")),
           depth = as.numeric(stringr::str_replace(depth, ",", ".")))
  if(ncol == 5) {
    DATA <-
      DATA |>
      dplyr::mutate(time = paste(date, time)) |>
      dplyr::select(-date)
  }

  if(dttm == "0") {
    DATA <-
      DATA |>
      dplyr::mutate(time = utf8::utf8_encode(time),
                    time = lubridate::dmy_hms(time))
  } else {
    DATA <- DATA |> dplyr::mutate(time = lubridate::mdy_hms(time))
  }

  attributes(DATA)$header <- header

  return(DATA)

}
