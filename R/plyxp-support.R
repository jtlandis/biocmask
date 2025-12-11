# expand_groups2 <- function(.rows, .cols) {
#   names(.rows) <- sprintf(".rows::%s", names(.rows))
#   names(.cols) <- sprintf(".cols::%s", names(.cols))
#   .nrow <- nrow(.rows)
#   .ncol <- nrow(.cols)
#   .rows <- map(.rows, vec_rep, times = .ncol)
#   .cols <- map(.cols, vec_rep_each, times = .nrow)
#   out <- c(.rows, .cols)
#   n <- .nrow * .ncol
#   out[[".nrows"]] <- map_int(out[[".rows::.indices"]], length)
#   out[[".ncols"]] <- map_int(out[[".cols::.indices"]], length)
#   attr(out, "row.names") <- c(NA_integer_, -n)
#   class(out) <- c("tbl_df", "tbl", "data.frame")

#   # due to this ordering here, I had introduced an unexpected
#   # column-wise ordering of assays. I have changed it and commented
#   # it out and also revered to the original intent of row-wise ordering.
#   # o <- order(
#   #   out[[".cols::.indices_group_id"]],
#   #   out[[".rows::.indices_group_id"]]
#   # )
#   # out <- out[o,]
#   out$.group_id <- seq_len(n)
#   out
# }

#' @export
mat_index <- function(rows_ind, cols_ind, nrows) {
  shift <- (cols_ind - 1L) * nrows
  vctrs::vec_rep(rows_ind, length(cols_ind)) +
    vctrs::vec_rep_each(shift, length(rows_ind))
}

# is_grouped_rows <- function(.groups) {
#   !is_empty(.groups$row_groups)
# }

# is_grouped_cols <- function(.groups) {
#   !is_empty(.groups$col_groups)
# }


vec_chop_assays <- function(.data, .indices) {
  map2(
    as.vector(.indices$rows)@indices,
    as.vector(.indices$cols)@indices,
    function(.x, .y, .data) .data[.x, .y, drop = FALSE],
    .data = .data
  )
}

# vec_chop_assays_row <- function(.data, .indices) {
#   map(as.vector(.indices$rows)@indices,
#     function(.i, .data) .data[.i, , drop = FALSE],
#     .data = .data
#   )
# }

# vec_chop_assays_col <- function(.data, .indices) {
#   map(as.vector(.indices$cols)@indices,
#     function(.i, .data) .data[, .i, drop = FALSE],
#     .data = .data
#   )
# }

# chop_dims_with_rep <- function(obj, .dims) {
#   n <- length(.dims)
#   nlens <- unname(lengths(.dims))
#   out_size <- Reduce(`*`, nlens, right = TRUE, accumulate = 1L)

#   times <- 1L
#   for (i in rev(seq_len(n))) {
#     dim <- .subset2(.dims, i)
#     if (is_missing(dim)) {
#       dim <- list(rlang::missing_arg())
#     }
#     size <- nlens[i]
#     for (j in seq_len(n - i)) {
#       .dims[[i + j]] <- Replicated(.dims[[i + j]], times = size, each = TRUE)
#     }
#     .dims[[i]] <- Replicated(dim, times = times)
#     times <- out_size[i]
#   }
#   dim_args <- lapply(.dims, as.vector) |> lapply(as.list)
#   dot_args <- rlang::syms(sprintf("..%i", seq_along(dim_args)))
#   inject(
#     base::mapply(\(..., obj) obj[!!!dot_args, drop = FALSE],
#       !!!dim_args,
#       MoreArgs = list(obj = obj),
#       SIMPLIFY = FALSE
#     )
#   )
# }

#' @export
chop_dims_outer <- function(obj, .dims) {
  # browser()
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
chop_mat <- function(obj, ind) {
  # browser()
  # vecs <- lapply(ind@indices, function(i, obj) obj[i], obj = obj)
  vecs <- vctrs::vec_chop(as.vector(obj), indices = ind@indices)
  .rows <- ind$rows
  unreplicate(.rows, TRUE) <- lengths(unreplicate(.rows, TRUE)@indices)
  .cols <- ind$cols
  unreplicate(.cols, TRUE) <- lengths(unreplicate(.cols, TRUE)@indices)
  dimnms <- dimnames(obj)
  if (is.null(dimnms)) {
    pmap(
      list(
        vecs,
        as.vector(.rows),
        as.vector(.cols)
      ),
      function(data,
               nrow, ncol) {
        dim(data) <- c(nrow, ncol)
        data
      }
    )
  } else {
    rownm <- ind$rows
    if (is.null(dimnms[[1L]])) {
      rownm <- vector("list", length(rownm))
    } else {
      unreplicate(rownm) <- bioc_chop(
        dimnms[[1]],
        indices = unreplicate(rownm, TRUE)@indices
      )
    }

    colnm <- ind$cols
    if (is.null(dimnms[[2L]])) {
      colnm <- vector("list", length(colnm))
    } else {
      unreplicate(colnm) <- bioc_chop(
        dimnms[[2]],
        indices = unreplicate(colnm, TRUE)@indices
      )
    }
    pmap(
      list(
        vecs,
        as.vector(.rows),
        as.vector(.cols),
        as.vector(rownm),
        as.vector(colnm)
      ),
      function(data,
               nrow, ncol,
               rownm, colnm) {
        dim(data) <- c(nrow, ncol)
        dimnames(data) <- list(rownm, colnm)
        data
      }
    )
  }
}

chop_mat2 <- function(obj, ind) {
  # vecs <- lapply(ind@indices, function(i, obj) obj[i], obj = obj)
  vctrs::vec_chop(as.vector(obj), indices = ind@indices)
}

#' @export
chop_dims_outer2 <- function(obj, .dims) {
  # is_missing <- vapply(.dims, rlang::is_missing, FUN.VALUE = logical(1))
  n <- length(.dims)
  nlens <- lengths(.dims)
  out_size <- Reduce(`*`, nlens, right = TRUE, accumulate = 1L)

  curr_dim <- n
  obj_slice <- NULL
  objs <- out <- vector("list", out_size[[1L]])
  objs[[1L]] <- obj
  n_obj <- 1L
  slice_expr <- expr(.slice)
  dim_args <- vec_rep(list(rlang::missing_arg()), n)
  while (curr_dim > 0) {
    .slices <- .dims[[curr_dim]]

    i_seq <- seq_len(nlens[curr_dim])
    nn <- length(i_seq)
    n_out <- out_size[curr_dim]
    # out_seq <- seq_len(n_out)

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
    if (curr_dim == 0) {
      return(out)
    }
    # objs[out_seq] <- out[out_seq]
    #
    .slices <- .dims[[curr_dim]]

    i_seq <- seq_len(nlens[curr_dim])
    nn <- length(i_seq)
    n_out <- out_size[curr_dim]
    # out_seq <- seq_len(n_out)

    if (!is_missing(.slices)) {
      dim_args[[curr_dim]] <- slice_expr
      e <- inject(expr(obj_slice[!!!dim_args, drop = FALSE]))
      # out <- vector("list", out_size[curr_dim])
      for (j in seq_len(n_obj)) {
        obj_slice <- .subset2(out, j)
        shift <- (j - 1L) * nn
        for (i in i_seq) {
          .slice <- .subset2(.slices, i)
          objs[[shift + i]] <- eval(e)
        }
      }
      dim_args[[curr_dim]] <- missing_arg()
    }

    n_obj <- n_out
    curr_dim <- curr_dim - 1L
  }
  objs
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
        type <- paste0(names(S4Vectors::mcols(.indices)), collapse = "")
        private$.ngroups <- NROW(.indices)
        fun <- switch(type,
          rowscols = function(name) {
            name <- enexpr(name)
            expr(vec_chop_assays(!!name, .indices))
          },
          rows = function(name) {
            name <- enexpr(name)
            expr(vec_chop_assays_row(!!name, .indices))
          },
          cols = function(name) {
            name <- enexpr(name)
            expr(vec_chop_assays_col(!!name, .indices))
          }
        )
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


# if (interactive()) {
#   # obj <- group_by(plyxp::se_simple, rows(direction), cols(condition))
#   obj <- dplyr::select(plyxp::se_simple, rows(direction), cols(condition))
#   row_grps <- SummarizedExperiment::rowData(obj) |> as_index_grouping()
#   col_grps <- SummarizedExperiment::colData(obj) |> as_index_grouping()
#   assay_grps <- expand_groups3(row_grps, col_grps, obj = se_simple)
#   top_env <- new_bioc_top_env(unreplicate = unreplicate)
#   bot_assay_env <- new_bioc_bot_env(context = "assays", parent = top_env)
#   # expanded <- expand_groups2(groups$row_groups, groups$col_groups)
#   # out <- map2(
#   #   expanded[[".rows::.indices"]],
#   #   expanded[[".cols::.indices"]],
#   #   .f = function(row, col, n) {
#   #     mat_index(row, col, nrows = n)
#   #   }, n = nrow(obj)
#   # )
#   # attr(out, "plyxp:::row_chop_ind") <- expanded[[".rows::.indices"]]
#   # attr(out, "plyxp:::col_chop_ind") <- expanded[[".cols::.indices"]]
#   # attr(out, "type") <- attr(groups, "type")
#   mask_assay <- biocmask_assay$new(
#     assays(se_simple),
#     assay_grps,
#     .nrow = nrow(obj),
#     .ncol = ncol(obj),
#     .env_bot = rlang::env(
#       bot_assay_env,
#       vec_chop_assays = vec_chop_assays,
#       vec_chop_assays_col = vec_chop_assays_col,
#       vec_chop_assays_row = vec_chop_assays_row
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
#     .data = plyxp::se(se_simple),
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
#     .ncol_groups = length(.col_ind),
#     .row_ind = unreplicate(.indices$rows),
#     .nrow_groups = length(.row_ind),
#     mapper = ~ ((.x - 1L) %% .nrow_groups) + 1L,
#     reshape = ~ bioc_rep(.x, times = .ncol_groups)
#   )

#   mm$link_ctx(view_assays_from_rows)
#   mm$link_ctx(view_rows_from_assays)
#   mm$link_ctx(new_view_spec("assays", "assays"), "data")

#   mm$views$rows$asis_access$direction

#   mm$extended$rows$asis_access$counts
#   mm$extended$rows$reshape_access$counts

#   # v <- link_view(
#   #   mm,
#   #   view_assays_from_rows
#   # )
#   # v$top_env$.__cache_map__.
# }
