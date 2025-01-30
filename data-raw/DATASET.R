## code to prepare `DATASET` dataset goes here

#usethis::use_data(DATASET, overwrite = TRUE)


# ------------------------------------------------------------------------------
# external files
fs::file_copy("/u2/merki/gogn/Hlynur-Ólafsvík/1M9380.DAT",
              "inst/staroddi/1M9380.DAT")
dst <- read_dst(system.file("demos/1M9380.DAT", package="staroddi"))
attributes(dst)$header |> knitr::kable()
