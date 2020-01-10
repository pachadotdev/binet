library(dplyr)
library(data.table)

gln_txt <- "dev/translations.txt"
url <- "http://language.media.mit.edu/data/public/unesco_langlang_20120722_iso639-3.txt"

if (!file.exists(gln_txt)) {
  try(download.file(url, gln_txt))
}

translations <- fread(gln_txt) %>%
  as_tibble()

save(translations, file = "data/translations.rda", compress = "xz")
