## code to prepare `alspac_toy_data` dataset

library(ALSPAC.helpR)

# Read in the file
data <- load_alspac(
  lower.case = TRUE,
  keep.value.labels = TRUE,
  load.metadata = FALSE)

set.seed(1996)

alspac_toy_data <- data.frame('cidb1234' = sort(sample(data$cidb4195, 500, replace=TRUE)),
                              'qlet' = sample(data$qlet, 500, replace=TRUE))
# any(duplicated(sub_data$cidb1234)) TRUE

for (c in sort(sample(names(data)[-c(1,2)], 498))) {
  alspac_toy_data[,c] <- sample(data[,c], 500, replace=TRUE)
}

# haven::write_sav(alspac_toy_data, './inst/tutorial/alspac_toy_data.sav')

usethis::use_data(alspac_toy_data, overwrite = TRUE)

# Check non-ASCII ======================================
# for (x in names(alspac_toy_data)) {
#   v = alspac_toy_data[,x][!is.na(alspac_toy_data[,x])]
#   if(any(stringr::str_detect(v, "[^[:ascii:]]"))) {
#     print(x)
#   }
# }
# n8131 has some £59 labels...
