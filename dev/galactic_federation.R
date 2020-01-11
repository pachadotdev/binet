library(readxl)
library(tidyr)

galactic_federation <- read_excel("dev/galactic_federation.xlsx", range = "A2:M11")

galactic_federation <- galactic_federation %>%
  gather(product, export_value, -planet)

save(galactic_federation, file = "data/galactic_federation.rda")
