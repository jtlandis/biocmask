## code to prepare `se_simple` dataset goes here

set.seed(12345)
df_simple <- S4Vectors::DataFrame(
  signal = sample(1:100, 8),
  condition = S4Vectors::Rle(rep(c("cntrl", "drug"), each = 4)),
  group = factor(sample(c("A", "B", "C"), 8, replace = TRUE))
)
rownames(df_simple) <- paste0("row_", seq_len(nrow(df_simple)))
usethis::use_data(df_simple, overwrite = TRUE)
