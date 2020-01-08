# data downloaded from in gz
# I had to use 7-Zip on Windows
# https://amp.pharm.mssm.edu/static/hdfs/harmonizome/data/omim/gene_attribute_edges.txt.gz

omim <- data.table::fread("dev/gene_attribute_edges.txt") %>%
  as_tibble() %>%
  filter(weight != "weight") %>%
  mutate(weight = as.integer(weight))

# genes_in_hdn_fig1 <- c("AR", "ATM", "BRCA1", "BRCA2", "CDH1",
#                        "GARS", "HEXB", "KRAS", "LMNA", "MSH2",
#                        "PIK3CA", "TP53", "MAD1L1", "RAD54L", "VAPB",
#                        "CHEK2", "BSCL2", "ALS2", "BRIP1")
#
# omim <- omim %>%
#   filter(source %in% genes_in_hdn_fig1)

save(omim, file = "data/omim.rda", compress = "xz")
