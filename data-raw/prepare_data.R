# Read the combined dataset (income, housing, capital gains)
housepack <- readRDS("data-raw/housepack.rds")

# Filter by Year >= 1985 to align with CGT introduction
income_pc <- housepack$income_pc[housepack$income_pc$Year >= 1985, ]
detached_full <- housepack$detached_full[housepack$detached_full$Year >= 1985, ]
cgt_full <- housepack$cgt_full[housepack$cgt_full$Year >= 1985, ]

# Save datasets as .rda files for package use
usethis::use_data(income_pc, detached_full, cgt_full, overwrite = TRUE)
