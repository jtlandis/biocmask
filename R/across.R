

expand_across <- function(quo, ctx, mask) {
  
  browser()
  if (!quo_is_call(quo, "across", ns = c("", "dplyr"))) {
    return(list(quo))
  }
  env <- quo_get_env(quo)
  expr <- match.call(definition = dplyr::across,
                     call = quo_get_expr(quo),
                     expand.dots = FALSE,
                     envir = env)
  
  
  
  
}