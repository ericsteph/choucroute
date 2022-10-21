
library(swissPCI)

name_flr <- "data"

# Step0: Find and set up the current url_basis

documentsource <- "https://www.bfs.admin.ch/bfs/it/home/statistiche/prezzi/indice-nazionale-prezzi-consumo.assetdetail.23527374.html"

url_basis <- get_url_swissPCI(documentsource)

# Step1: Get the dataset from the web to a local file
get_swissPCI(url_basis = url_basis, name_flr = name_flr)


# Step2: Get the INDEX_m data (data from the sheet "Index_m")
# d <- swissPCI::crea_d(name_flr = name_flr, language.x = c("Italian", "PosTxt_I"))


# Step3: save the PCI data in two folder, .rds file in "cubi esterni" e .xlsx file in "data"
name_flr <- c("data", "cubi esterni")
get_all_PCI(name_flr = name_flr, language.x = c("French", "PosTxt_F"))