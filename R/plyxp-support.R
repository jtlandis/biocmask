#' @include utils.R

into_dimlist <- function(assay_ind) {
  list(
    biocmask::unreplicate(assay_ind$rows),
    biocmask::unreplicate(assay_ind$cols)
  )
}

chop_assays_outer <- function(obj, .ind) {
  dimlist <- into_dimlist(.ind)
  chop_dims_outer(obj, dimlist)
}

chop_dims_outer <- function(obj, .dims) {
  # is_missing <- vapply(.dims, rlang::is_missing, FUN.VALUE = logical(1))
  n <- length(.dims)
  nlens <- lengths(.dims)
  out_size <- Reduce(`*`, nlens, right = TRUE, accumulate = 1L)

  curr_dim <- n
  obj_slice <- NULL
  objs <- out <- vector("list", out_size[[1L]])
  objs[[1L]] <- out[[1L]] <- obj
  n_obj <- 1L
  slice_expr <- expr(.slice)
  dim_args <- vec_rep(list(rlang::missing_arg()), n)
  while (curr_dim > 0) {
    .slices <- .dims[[curr_dim]]

    i_seq <- seq_len(nlens[curr_dim])
    nn <- length(i_seq)
    n_out <- out_size[curr_dim]
    out_seq <- seq_len(n_out)

    if (!is_missing(.slices)) {
      dim_args[[curr_dim]] <- slice_expr
      e <- inject(expr(obj_slice[!!!dim_args, drop = FALSE]))
      # out <- vector("list", out_size[curr_dim])
      for (j in seq_len(n_obj)) {
        obj_slice <- .subset2(objs, j)
        shift <- (j - 1L) * nn
        for (i in i_seq) {
          .slice <- .subset2(.slices, i)
          out[[shift + i]] <- eval(e)
        }
      }
      dim_args[[curr_dim]] <- missing_arg()
    }

    n_obj <- n_out
    curr_dim <- curr_dim - 1L
    objs[out_seq] <- out[out_seq]
  }
  out
}

#' @export
mat_index <- function(rows_ind, cols_ind, nrows) {
  shift <- (cols_ind - 1L) * nrows
  vctrs::vec_rep(rows_ind, length(cols_ind)) +
    vctrs::vec_rep_each(shift, length(rows_ind))
}


#' @title `biocmask` for SummarizedExperiment `assays()`
#' @name BiocDataMask-assays
#' @description
#' A more specialized version of the biocmask R6 object for the
#' assays list object. This includes chopping and unchopping
#' of matrix like objects.
#' @return an object inheriting [`biocmask`][biocmask::BiocDataMask].
#' @noRd
biocmask_assay <- R6::R6Class(
  "biocmask_assay",
  inherit = biocmask,
  cloneable = FALSE,
  public = list(
    #' @description
    #' Create a biocmask from `.data`. `.data` is chopped by
    #' `.indices`, and environments are built from `.env`
    #'
    #' @param .data a named list like object to create a mask
    #' @param .indices the indices that will be used to chop `.data`
    #' @param .env_bot an environment that the resulting mask will be built
    #' from.
    #' @param .env_top an environment that `.env_bot` inherits from
    #' @param .nrow,.ncol the number of rows and columns of each element of
    #' `.data` respectively
    initialize = function(.data,
                          .indices,
                          .env_bot,
                          .env_top = .env_bot,
                          .nrow,
                          .ncol) {
      super$initialize(
        .data,
        .indices = .indices,
        .env_bot = .env_bot,
        .env_top = .env_top
      )
      env_bind(
        private$env_current_group_info,
        .nrow = .nrow,
        .ncol = .ncol
      )
      private$.nrow <- .nrow
      private$.ncol <- .ncol
    },
    #' @description
    #' unchop data within the mask, returns a matrix
    #' @param name name of binding to retrieve and unchop
    unchop = function(name) {
      data <- self$get_chop(name)
      if (is.null(data)) {
        return(NULL)
      }
      unchopped <- if (is.null(private$.indices)) {
        .subset2(data, 1L)
      } else {
        bioc_unchop(
          x = lapply(data, as.vector),
          ptype = as.vector(private$.ptype[[name]]),
          indices = private$.indices
        )
      }
      matrix(
        unchopped,
        nrow = private$.nrow,
        ncol = private$.ncol
      )
    }
  ),
  private = list(
    get_chop_fun = function() {
      .indices <- private$.indices
      if (is.null(.indices)) {
        # private$.e <- private$.env_col_chop <- private$env_data_chop
        return(function(name) {
          name <- enexpr(name)
          expr(list(!!name))
        })
      } else {
        # .indices would have been created by get_group_indices()
        #
        # type <- attr(.indices, "type")
        private$.ngroups <- length(.indices)
        fun <- function(name) {
          name <- enexpr(name)
          expr(chop_assays_outer(!!name, .indices))
        }
        return(fun)
      }
    },
    .nrow = NULL,
    .ncol = NULL
  )
)

#' @export
expand_groups3 <- function(rows = NULL, cols = NULL, obj) {
  nr <- nrow(obj)
  nc <- ncol(obj)
  row_ind <- rows %||% IndexGrouping(indices = list(seq_len(nr)))
  col_ind <- cols %||% IndexGrouping(indices = list(seq_len(nc)))
  nr_grps <- NROW(row_ind)
  nc_grps <- NROW(col_ind)
  row_ind <- Replicated(row_ind, nc_grps)
  col_ind <- Replicated(col_ind, nr_grps, each = TRUE)
  if (nr_grps > 1 && nc_grps > 1) {
    rows <- row_ind
    cols <- col_ind
  } else {
    if (!is.null(rows)) rows <- S4Vectors::I(rows)
    if (!is.null(cols)) cols <- S4Vectors::I(cols)
  }
  indices <- map2(
    as.vector(row_ind)@indices,
    as.vector(col_ind)@indices,
    mat_index,
    nrows = nr
  )

  IndexGrouping(
    indices = indices,
    rows = rows,
    cols = cols
  )
}

into_dimlist <- function(ind) {
  list(
    unreplicate(ind$rows)@indices,
    unreplicate(ind$cols)@indices
  )
}

# large_se <- SummarizedExperiment::SummarizedExperiment(
#   assays = list(data = matrix(rnorm(1e6), nrow = 1000, ncol = 1000))
# ) |>
#   plyxp::new_plyxp() |>
#   plyxp::mutate(
#     rows(row_grp = sample(1L:100L, 1000, TRUE)),
#     cols(col_grp = sample(1L:100L, 1000, TRUE))
#   )

# row_grps <- SummarizedExperiment::rowData(large_se) |> biocmask:::as_index_grouping()
# col_grps <- SummarizedExperiment::colData(large_se) |> biocmask:::as_index_grouping()
# assay_grps <- expand_groups3(row_grps, col_grps, obj = large_se)


# large_mat <- SummarizedExperiment::assay(large_se)
# bench::mark(
#   chop = biocmask:::chop_mat(large_mat, assay_grps),
#   chop_outer = biocmask:::chop_dims_outer(large_mat, assay_grps |>
#  biocmask:::into_dimlist()))
###   rust_outer = biocmask:::chop_matrix_(large_mat, assay_grps |> into_dimlist())
# )


# small_se <- SummarizedExperiment::SummarizedExperiment(
#   assays = list(data = matrix(1:12, nrow = 3, ncol = 4))
# ) |>
#   plyxp::new_plyxp() |>
#   plyxp::mutate(
#     rows(row_grp = c(2, 2, 1)),
#     cols(col_grp = c(2, 1, 1, 2))
#   )


# ind <- biocmask:::expand_groups3(
#   SummarizedExperiment::rowData(small_se) |> biocmask:::as_index_grouping(),
#   SummarizedExperiment::colData(small_se) |> biocmask:::as_index_grouping(),
#   obj = small_se
# )

# small_mat <- SummarizedExperiment::assay(small_se)
# bench::mark(
#   chop = biocmask:::chop_mat(small_mat, ind),
#   chop_outer = biocmask:::chop_dims_outer(small_mat, ind |> into_dimlist()),
#   rust_outer = biocmask:::chop_matrix_(small_mat, ind |> into_dimlist())
# )

# expanded <- expand_groups2(groups$row_groups, groups$col_groups)
# out <- map2(
#   expanded[[".rows::.indices"]],
#   expanded[[".cols::.indices"]],
#   .f = function(row, col, n) {
#     mat_index(row, col, nrows = n)
#   }, n = nrow(obj)
# )
# attr(out, "plyxp:::row_chop_ind") <- expanded[[".rows::.indices"]]
# attr(out, "plyxp:::col_chop_ind") <- expanded[[".cols::.indices"]]
# attr(out, "type") <- attr(groups, "type")

# if (interactive()) {
#   # obj <- group_by(plyxp::se_simple, rows(direction), cols(condition))
#   obj <- dplyr::select(plyxp::se_simple, rows(direction), cols(condition))
#   row_grps <- SummarizedExperiment::rowData(obj) |> as_index_grouping()
#   col_grps <- SummarizedExperiment::colData(obj) |> as_index_grouping()
#   assay_grps <- expand_groups3(row_grps, col_grps, obj = plyxp::se_simple)
#   top_env <- new_bioc_top_env(unreplicate = unreplicate)
#   bot_assay_env <- new_bioc_bot_env(context = "assays", parent = top_env)

#   mask_assay <- biocmask_assay$new(
#     assays(plyxp::se_simple),
#     assay_grps,
#     .nrow = nrow(obj),
#     .ncol = ncol(obj),
#     .env_bot = rlang::env(bot_assay_env,
#       chop_assays_outer = chop_assays_outer
#     ),
#     .env_top = top_env
#   )

#   mask_rows <- biocmask$new(
#     rowData(plyxp::se_simple),
#     row_grps,
#     .env_bot = new_bioc_bot_env(context = "rows", parent = top_env),
#     .env_top = top_env
#   )

#   mm <- biocmask_manager$new(
#     .data = plyxp::se(plyxp::se_simple),
#     .masks = list(assays = mask_assay, rows = mask_rows)
#   )

#   view_assays_from_rows <- new_view_spec(
#     ctx = "assays", from = "rows",
#     .col_ind = unreplicate(.ctx$.indices$cols),
#     .ncol_groups = length(.col_ind),
#     .row_ind = unreplicate(.ctx$.indices$rows),
#     .ordered_unchop = base::order(vctrs::list_unchop(base::as.list(.col_ind))),
#     .nrow_groups = length(.row_ind),
#     mapper = ~ ((seq_len(.ncol_groups) - 1L) * .nrow_groups
#     ) + .x,
#     asis = ~ {
#       if (.ncol_groups < 2) {
#         return(.subset2(.x, 1L))
#       }
#       .x <- do.call("cbind", .x)
#       .x[, .ordered_unchop, drop = FALSE]
#     },
#     reshape = ~ lapply(seq_len(base::nrow(.x)), \(y) .x[y, , drop = FALSE])
#   )

#   view_rows_from_assays <- new_view_spec(
#     ctx = "rows", from = "assays",
#     .col_ind = unreplicate(.indices$cols),
#     .col_sizes = vctrs::vec_rep_each(
#       base::lengths(base::as.list(.col_ind)), .nrow_groups
#     ),
#     .ncol_groups = length(.col_ind),
#     .row_ind = unreplicate(.indices$rows),
#     .nrow_groups = length(.row_ind),
#     mapper = ~ ((.x - 1L) %% .nrow_groups) + 1L,
#     reshape = ~ bioc_rep(.x, times = .col_sizes[[.i]])
#   )

#   mm$link_ctx(view_assays_from_rows)
#   mm$link_ctx(view_rows_from_assays)
#   mm$link_ctx(new_view_spec("assays", "assays"), "data")
#   mm$link_ctx(new_view_spec("rows", "rows"), "data")

#   mask <- mm$ctx_mask$environments@env_current_group_info
#   mask$`biocmask:::ctx:::group_id` <- 5L
#   mm$views$rows$reshape_access$direction

#   mm$views$rows$asis_access$direction

#   mm$extended$rows$asis_access$counts
#   mm$extended$rows$reshape_access$counts

#   # v <- link_view(
#   #   mm,
#   #   view_assays_from_rows
#   # )
#   # v$top_env$.__cache_map__.
# }
