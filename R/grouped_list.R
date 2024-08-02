
# library(vctrs)
# 
# 
# new_grouped_lst <- function(x, keys) {
#   x1 <- list_of(!!!x[[1]])
# 
#   vctrs::new_list_of(x, ptype = vec_ptype(x1), class = "vctrs_grouped_list", .key = keys)
# }
# # .out <- vec_chop(1:10, list(1:2,3:5, 6:9, 10)) |> list() |> new_grouped_lst(keys = tibble(condition = c("cntrl","drug")))
# 
# vec_ptype_abbr.vctrs_grouped_list <- function(x, ...) {
#   paste0("grpd<",
#          vec_ptype_abbr(attr(attr(x, "ptype"), "ptype"))
#          ,"[", nrow(attr(x, ".key")),"]>")
# }

case_key <- function(x, ...) {
  if (!inherits(x, "vctrs_grouped_list")) {
    cli::cli_abort(
      "x is not a {.cls vctrs_grouped_list}"
    )
  }
  group_keys <- attr(x, ".key")
  env <- rlang::current_env()
  dots <- rlang::list2(...)
  n_args <- length(dots)
  pairs <- purrr::map(dots, \(x) {
    list(lhs = rlang::f_lhs(x), rhs = rlang::f_rhs(x))
  })
  lhs_eval <- vector("list", n_args)
  rhs_eval <- vector("list", n_args)
  for (i in seq_along(dots)) {
    pair <- pairs[[i]]
    lhs_eval[[i]] <- rlang::eval_tidy(pair$lhs, env = env)
    rhs_eval[[i]] <- rlang::eval_tidy(pair$rhs, data = group_keys)
  }
  .size <- vctrs::vec_size_common(!!!lhs_eval)
  conditions <- vctrs::vec_recycle_common(!!!lhs_eval, .size = .size)
  seen <- logical(.size)
  out <- vector('list', .size)
  for (i in seq_along(conditions)) {
    cond <- conditions[[i]]
    selection <- rhs_eval[[i]]
    adding <- (!seen) & cond
    out[adding] <- purrr::map2(x[adding], list(selection), ~.x[.y])
    seen <- seen | adding
    if (all(seen)) break
  }

  if (all(purrr::map_int(out, length)==1L)) {
    out <- unlist(out, recursive = FALSE)
  }
  out

}


# pillar_shaft.vctrs_grouped_list <- function(x, ...) {
#   out <- map_chr(
#     x,
#     ~ sprintf("[%s]",
#               paste0(pillar::style_subtle(map_chr(.x, size_sum)),
#                      collapse = ","))
#   )
#   pillar::new_pillar_shaft_simple(out, align = "left", min_width = 10, shorten = "back")
# }
# 
# # tibble(y = 1, z = 10, a = "A", x = .out)
# 
# # rowd <- rowData(se) |> as_tibble(rownames = ".features")
# # col_data_chop <- colData(se) |> as_tibble(rownames = ".samples") |> tidyr::chop(-condition)
# # rowd$samples <- list()
# #
# # setClass("vctrs_grouped_list", contains = "list")
# #
# # setMethod(lapply, signature(X = "vctrs_grouped_list"),
# #           function(X, FUN, ...) {
# #             base::lapply(
# #               X,
# #               function(x, ...) {
# #                 base::lapply(X = x, FUN = FUN, ...)
# #               },
# #               ...
# #             ) |>
# #               new_list_of(ptype = vctrs::vec_ptype(X[[1]]), class = "vctrs_grouped_list", .key = attr(X, ".key"))
# #           })
# 
