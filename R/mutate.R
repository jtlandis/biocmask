
#' @importFrom dplyr mutate

#' Mutate a SummarizedExperiment object under an data mask
#' @param .data a SummarizedExperiment object
#' @param ... expressions
#' @value SummarizedExperiment object
#' @export
mutate.SummarizedExperiment <- function(.data, ...) {
  
  # browser()
  
  .env <- rlang::caller_env()
  .mask <- TidySEMaskManager$new(.data, .env, "mutate")
  poke_ctx_local("SE:::mask_manager", .mask)
  poke_ctx_local("SE:::dplyr_function", "mutate")
  poke_ctx_local("SE:::caller_env", .env)
  quos <- rlang::enquos(..., .named = TRUE)
  nms <- names(quos)
  for (k in seq_along(quos)) {
    quo <- quos[[k]]
    name <- nms[k]
    .mask$eval_mutate_assays(quo, name)
  }
  .mask$finalize_mutate_data(.data)
  
}

# mutate_SE_expr <- function(dot, name, mask) {
#   
#   quos <- expand_SE_context(dot)
#   if (!attr(quos, "specified")) {
#     mask$eval_mutate_assays(dot, name)
#     return(invisible(NULL))
#   }
#   eval_fn <- switch(attr(quos, "context"),
#                     `SE:::assays` = mask$eval_mutate_assays,
#                     `SE:::rows` = mask$eval_mutate_rows,
#                     `SE:::cols` = mask$eval_mutate_cols)
#   nms <- names(quos)
#   for (i in seq_along(quos)) {
#     eval_fn(quos[[i]], nms[i])
#   }
#   invisible(NULL)
#   
# }
# 
# expand_SE_context <- function(quo) {
#   env <- quo_get_env(quo)
#   if (!quo_is_call(quo, name = c("rows", "cols", "assays"))) {
#     return(
#       structure(
#         list(quo),
#         context = "SE:::assays",
#         specified = FALSE
#       )
#     )
#   } else {
#     expr <- quo_get_expr(quo)
#     symbl <- expr[[1]]
#     
#     ctx <- if (identical(symbl, quote(assays))) {
#       "SE:::assays"
#     } else if(identical(symbl, quote(rows))) {
#       "SE:::rows"
#     } else {
#       "SE:::cols"
#     }
#     dots <- lapply(expr[-1], new_quosure, env = env)
#     nms <- names(dots)
#     if (is.null(nms) || any(nchar(nms)==0)) {
#       cli::cli_abort("issue in context {ctx}. all arguments must be named")
#     }
#     attr(dots, "context") <- ctx
#     attr(dots, "specified") <- TRUE
#   }
#   dots
# }
# 
# 
# se1 <- se
# se2 <- se
# se3 <- se
# 
# system.time()
# 
# system.time({
#  
# })
# 
# system.time({
#   
# })
# 
# fast_ <- function(se) {
#   assays(se)[['counts']] <- .tmp <- round(assay(se3, "NumReads"), 0)
#   rowData(se)$sum <- rowSums(.tmp)
#   colData(se)$sum <- colSums(.tmp)
#   se
# }
# f1 <- function(se) {
#   mutate(se, 
#          counts = round(NumReads, 0),
#          rows(
#            sum = rowSums(.assay$counts)
#          ),
#          cols(
#            sum = colSums(.assay$counts)
#          ))
# }
# f2 <- function(se) {
#   mutate(se, 
#          counts = round(NumReads, 0),
#          rows(
#            sum = vapply(counts, sum, numeric(1))
#          ),
#          cols(
#            sum = vapply(counts, sum, numeric(1))
#          ))
# }
# 
# bench::mark(
#   fastest = fast_(se),
#   my_mutate = f1(se),
#   slow_mut = f2(se), check = F
# )
# 
# assay(obj, 1) <- as.matrix(assay(obj, 1))
# bench::mark(
#   current = mutate(obj, new_counts = case_when(condition=="untreated" ~ counts + 10, T ~ counts)),
#   mine = mutate_SummarizedExperiment(obj, new_counts = {counts <- as.vector(counts);case_when(condition=="untreated" ~ counts + 10, T ~ counts)}),
#   check = F
# )
# 
# # eval_mask <- function(mask) {
# #   env <- rlang::caller_env()
# #   bot <- env(
# #     env,
# #     assays = function(...) {
# #       browser()
# #       quos <- rlang::enquos(...)
# #       quo_names <-  names(quos)
# #       assay_env <- mask$env_assay
# #       assay_mask <- mask$mask_assay
# #       for(k in seq_along(quos)) {
# #         quo <- quos[[k]]
# #         assay_env[[quo_names[k]]] <- rlang::eval_tidy(quo, data = assay_mask)
# #       }
# #     },
# #     rows = function(...) {
# #       browser()
# #       quos <- rlang::enquos(...)
# #       quo_names <-  names(quos)
# #       row_env <- mask$env_row
# #       row_mask <- mask$mask_row
# #       for(k in seq_along(quos)) {
# #         quo <- quos[[k]]
# #         row_env[[quo_names[k]]] <- rlang::eval_tidy(quo, data = row_mask)
# #       }
# #     },
# #     cols = function(...) {
# #       browser()
# #       quos <- rlang::enquos(...)
# #       quo_names <-  names(quos)
# #       col_env <- mask$env_col
# #       col_mask <- mask$mask_col
# #       for(k in seq_along(quos)) {
# #         quo <- quos[[k]]
# #         col_env[[quo_names[k]]] <- rlang::eval_tidy(quo, data = col_mask)
# #       }
# #     }
# #   )
# #   rlang::new_data_mask(bot, top = env)
# # }
# 
# 
# 
