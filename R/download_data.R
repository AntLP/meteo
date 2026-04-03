# R/download_data.R

library(rvest)
library(httr)

# Filter a character vector of link hrefs to QC climate daily CSV filenames only.
parse_csv_filenames <- function(links) {
  links[grepl("^climate_daily_QC_.+_\\d{4}_P1D\\.csv$", links)]
}

# Fetch the ECCC directory listing and return a vector of CSV filenames.
get_csv_list <- function(base_url) {
  page <- read_html(base_url)
  links <- html_attr(html_elements(page, "a"), "href")
  parse_csv_filenames(links)
}

# Download all QC climate daily CSVs that are not already in data_dir.
# Warns (does not stop) on per-file failures.
download_qc_data <- function(
  base_url = "https://dd.weather.gc.ca/today/climate/observations/daily/csv/QC/",
  data_dir = "data"
) {
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  message("Récupération de la liste des fichiers...")
  csv_files <- get_csv_list(base_url)
  message(sprintf("Trouvé %d fichier(s) CSV.", length(csv_files)))

  downloaded <- 0L
  skipped <- 0L

  for (fname in csv_files) {
    dest <- file.path(data_dir, fname)
    if (file.exists(dest)) {
      skipped <- skipped + 1L
      next
    }

    tryCatch(
      {
        url <- paste0(base_url, fname)
        download.file(url, dest, quiet = TRUE, mode = "wb")
        downloaded <- downloaded + 1L
      },
      error = function(e) {
        warning(sprintf(
          "Échec du téléchargement : %s\n%s",
          fname,
          conditionMessage(e)
        ))
      }
    )
  }

  message(sprintf(
    "Terminé : %d téléchargé(s), %d ignoré(s) (déjà présents).",
    downloaded,
    skipped
  ))
  invisible(NULL)
}
