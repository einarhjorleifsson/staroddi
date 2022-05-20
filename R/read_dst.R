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
#' attributes(dst)$meta
read_dst <- function(fil) {

  not_all_na <- function(x) any(!is.na(x))

  lu <-
    tibble::tribble(~letter, ~audkenni,
                    "C", "ISL.CENTI.",
                    "G", "ISL.GPS.",
                    "M", "ISL.MILLI.",
                    "R", "ISL.RAF.")

  RAW <- readLines(fil, encoding = "latin1")
  comments <- stringr::str_detect(RAW, "^#")
  n.comments = sum(comments)

  META <-
    tibble::tibble(x = RAW[comments]) |>
    dplyr::mutate(x = iconv(x, "latin1", "UTF-8")) |>
    dplyr::mutate(x = stringr::str_replace(x, "#", "")) |>
    tidyr::separate(x, into = c("id", "var", "val"), sep = "\t", extra = "merge") |>
    dplyr::mutate(val = stringr::str_replace_all(val, "\t", " "))
  n.channels <- META |> dplyr::filter(var == "Channels:") |> dplyr::pull(val) |> as.integer()
  ch1 <- META |> dplyr::filter(var == "Channel 1:") |> dplyr::pull(val)
  ch2 <- META |> dplyr::filter(var == "Channel 2:") |> dplyr::pull(val)
  recorder <- META |> dplyr::filter(var == "Recorder:") |> dplyr::pull(val)
  utgafa <- stringr::str_sub(recorder, 1, 1) |> as.integer()
  numer <- stringr::str_sub(recorder, 2)
  letter2 <- stringr::str_extract(numer, "[a-zA-Z]+") |> stringr::str_to_upper()
  audkenni <-
    lu |>
    dplyr::filter(letter == letter2) |>
    dplyr::pull(audkenni)

  DATA <-
    readr::read_delim(I(RAW[!comments]),
                      na = c("____"),
                      delim = "\t",
                      skip_empty_rows = TRUE,
                      comment = "#",
                      col_names = FALSE,
                      col_types = "ic??ccc",
    ) |>
    dplyr::select(where(not_all_na)) |>
    dplyr::rename(.rid = 1,
                  time = 2,
                  temp = 3,
                  depth = 4) |>
    dplyr::mutate(time = lubridate::dmy_hms(time),
                  depth = -depth,
                  dst_id = recorder,
                  audkenni = audkenni,
                  utgafa = utgafa)

  attributes(DATA)$meta <- META
  attributes(DATA)$file <- fil

  return(DATA)

}


read_dst2 <- function(fil) {

  not_all_na <- function(x) any(!is.na(x))

  RAW <- readLines(fil, encoding = "latin1")
  comments <- stringr::str_detect(RAW, "^#")
  n.comments = sum(comments)

  # if header
  if(n.comments >= 1) {
    META <-
      tibble::tibble(x = RAW[comments]) |>
      dplyr::mutate(x = iconv(x, "latin1", "UTF-8")) |>
      tidyr::separate(x, into = c("id", "var", "val"), sep = "\t", extra = "merge") |>
      dplyr::mutate(val = stringr::str_replace_all(val, "\t", " "))

    vakning <-  META |> dplyr::filter(var %in% c("Recorder", "Recorder:")) |> dplyr::pull(val) |> stringr::str_replace(" DST milli ", "M") |> stringr::str_trim()

    if(length(vakning) > 1) vakning <- vakning[1]

    dstid_source <- "Recorder"
    if(length(vakning) == 0) {
      vakning <- basename(fil |> stringr::str_replace(".DAT", ""))
      dstid_source <- "filename"
    }

    if(stringr::str_sub(vakning, 1, 7) == "SeaStar") {
      vakning <- META |> dplyr::filter(stringr::str_sub(var, 1, 5) == "Chart") |> dplyr::pull(var) |> stringr::str_trim()
      vakning <- stringr::str_replace(vakning, "DAT", "") |> stringr::str_trim()
      dstid_source <- "Chart"
    }

    META <-
      META |>
      dplyr::mutate(dst_id = vakning |> stringr::str_trim(),
                    dstid_source = dstid_source,
                    path = fil,
                    n = n.comments)


  } else {
    # if no header
    dstid_source <- "filename"
    META <-
      tibble::tibble(dst_id = basename(fil),
                     dstid_source = dstid_source,
                     path = fil,
                     n = n.comments) |>
      dplyr::mutate(dst_id = stringr::str_replace(dst_id, ".DAT", ""),
                    dst_id = stringr::str_replace(dst_id, "^new", ""),
                    dst_id = stringr::str_replace(dst_id, "-ii", ""))
  }

  if(length(RAW[!comments]) == 0) {
    DATA <-
      tibble::tibble(x = "no records",
                     dst_id = META$dst_id[1] |> stringr::str_trim())
  } else {
    if(!any(stringr::str_detect(RAW[!comments], "Filename:"))) {

      DATA <-
        readr::read_delim(I(RAW[!comments]), delim = "\t", skip_empty_rows = TRUE, comment = "#", col_names = FALSE, col_types = "ccccccc") |>
        dplyr::select(where(not_all_na))

      DATA <-
        DATA |>
        dplyr::mutate(ncol = ncol(DATA),
                      dst_id = META$dst_id[1]) |>
        dplyr::select(dst_id, ncol, dplyr::everything())
    } else {
      DATA <-
        tibble::tibble(raw = RAW[!comments],
                       dst_id = META$dst_id[1]) |>
        dplyr::mutate(raw = ifelse(raw == "", NA_character_, raw)) |>
        dplyr::filter(!is.na(raw))
    }
  }

  DATA <-
    DATA |>
    dplyr::mutate(path = fil)

  # some checks
  ncol.meta <-
    META |>
    dplyr::filter(var == "Columns:") |>
    dplyr::pull(val)

  if(ncol.meta != DATA$ncol |> unique())  {
    stop("you fucked up")
  } else {
    DATA <-
      DATA |>
      dplyr::rename(.rid = 3,
                    time = 4,
                    Temperature = 5,
                    Depth = 6)
  }



  attributes(DATA)$meta <- META

  return(DATA)

}
