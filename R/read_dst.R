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

  delim <- header |> dplyr::filter(var == "Field separation:") |> dplyr::pull(comment)
  delim <- ifelse(delim == "tab", "\t", " ")
  decimal <- header |> dplyr::filter(var == "Decimal point:") |> dplyr::pull(comment)
  dttm <- header |> dplyr::filter(var == "Date def.:") |> dplyr::pull(val)
  dttm <- stringr::str_sub(dttm, 1, 1)
  ncol <- header |> dplyr::filter(var == "Columns:") |> dplyr::pull(val)
  recorder <- header |> dplyr::filter(var == "Recorder:") |> dplyr::pull(val)

  if(ncol != "4") stop("Check header setting")
  if(ncol == "4") cn <- c(".rid", "time", "temp", "depth")


  DATA <-
    utils::read.table(fil,
                      header = FALSE,
                      col.names = cn,
                      sep = delim,
                      dec = decimal,
                      na.strings = "____") |>
    tibble::as_tibble()

  if(dttm == "0") {
    DATA <- DATA |> dplyr::mutate(time = lubridate::dmy_hms(time))
  } else {
    DATA <- DATA |> dplyr::mutate(time = lubridate::mdy_hms(time))
  }

  DATA <-
    DATA |>
    dplyr::mutate(depth = -depth,
                  recorder = recorder)
  attributes(DATA)$header <- header

  return(DATA)

}
