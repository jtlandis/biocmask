test_top_env <- new_bioc_top_env()
test_bot_env <- new_bioc_bot_env(parent = test_top_env, context = "default")

dummy_list <- list(tag = "hello world", value = 42)

df_simple_mask <- function() {
  m1 <- biocmask$new(
    df_simple,
    .indices = NULL,
    .env_top = test_top_env,
    .env_bot = test_bot_env
  )
  m2 <- biocmask$new(
    dummy_list,
    .indices = NULL,
    .env_top = test_top_env,
    .env_bot = test_bot_env
  )
  biocmask_manager$new(
    df_simple,
    .masks = list(default = m1, metadata = m2),
    .ctx_env = test_bot_env
  )
}

df_simple_grouped_mask <- function() {
  m1 <- biocmask$new(
    df_simple,
    .indices = list(c(1, 3, 5, 7), c(2, 4, 6, 8)),
    .env_top = test_top_env,
    .env_bot = test_bot_env
  )
  m2 <- biocmask$new(
    dummy_list,
    .indices = NULL,
    .env_top = test_top_env,
    .env_bot = test_bot_env
  )
  biocmask_manager$new(
    df_simple,
    .masks = list(default = m1, metadata = m2),
    .ctx_env = test_bot_env
  )
}
