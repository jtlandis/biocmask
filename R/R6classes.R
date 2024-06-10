
# - do not manipulate .data
# - lazily represent .data
# - for this mask, lazily represent some type
#   of data mold transform
# - have last layer be an active binding
#   that will force the evaluaiton of the mold
#   if called
# - new columns added will need to be updated

# This is trivial to do for a single class.
# but i would like to formally write
#

box::use(
  vctrs[vec_rep, vec_rep_each]
)

TidyBiocMask <- R6::R6Class(
  "TidyBiocMask",
  class = FALSE,
  cloneable = FALSE,
  public = list(
    initialize = function(.data, .env, ...) {
      private$.names <- .names <- names(.data)
      private$.lazy_data <- new.env(parent = .env)
      env_bind_lazy(private$.lazy_data,
                    !!! lapply(setNames(nm = .names),
                               function(x) quo(.data[[!!x]])),
                    ...)
      private$.lazy_mold <- new.env(parent = private$.lazy_data)
      private$.actv_data <- new.env(parent = private$.lazy_mold)
      private$.data_mask <- new_data_mask(private$.actv_data, private$.lazy_data)
    },
    eval_expr = function(quo, name) {
      eval_tidy(quo, data = private$.data_mask)
    },
    bind_lazy = function(quo, name) {
      env_bind_lazy(private$.lazy_data, !!name := !!quo)
    },
    bind_mold = function(quo, name) {
      env_bind_lazy(private$.lazy_mold, !!name := !!quo)
    },
    bind_actv = function(name) {
      fun <- new_function(pairlist(), body = as.name(name), env = private$.lazy_mold)
      env_bind_active(private$.actv_data, !!name := fun)
    },
    push = function(name) {
      mods <- private$.new_names
      cur_names <- private$.names
      if (!name %in% mods) {
        private$.new_names <- c(mods, name)
      }
      if (!name %in% cur_names) {
        private$.names <- c(cur_names, name)
      }
      invisible(self)
    }
  ),
  active = list(
    lazy_data = function(value) {
      if (!missing(value)) 
        stop("`$lazy_data` is read only")
      private$.lazy_data
    },
    lazy_mold = function(value) {
      if (!missing(value)) 
        stop("`$lazy_mold` is read only")
      private$.lazy_mold
    },
    actv_data = function(value) {
      if (!missing(value)) 
        stop("`$actv_data` is read only")
      private$.actv_data
    },
    data_mask = function(value) {
      if (!missing(value)) 
        stop("`$data_mask` is read only")
      private$.data_mask
    },
    names = function(value) {
      if (!missing(value))
        stop("`$name` is read only")
      private$.names
    },
    modified = function(value) {
      if (!missing(value))
        stop("`$modified` is read only")
      private$.new_names
    }
  ),
  private = list(
    .lazy_data = NULL,
    .lazy_mold = NULL,
    .actv_data = NULL,
    .data_mask = NULL,
    .names = character(),
    .new_names = character()
  )
)

TidySEMask <- R6::R6Class(
  "TidySEMask",
  cloneable = F,
  public = list(
    poke_ctx = function(ctx) {
      old_ctx <- private$.ctx
      if (length(old_ctx) > 0 && length(ctx) > 0) {
        cli::cli_abort("attempting to evaluate within {.fn {ctx}} when already within {.fn {old_ctx}}")
      }
      private$.ctx <- ctx
      old_ctx
    },
    initialize = function(.data, .env = NULL, .fn = NULL, ...) {
      # browser()
      private$.caller_env <- .env <- .env %||% parent.frame()
      # private$.fn <- .fn
      private$.nrow <- .nrow <- nrow(.data)
      private$.ncol <- .ncol <- ncol(.data)
      private$.len <- .nrow * .ncol
      assay_mask <- assays(.data) |>
        TidyBiocMask$new(.env = .env)
      rows_mask <- rowData(.data) |>
        TidyBiocMask$new(.env = .env)
      cols_mask <- colData(.data) |>
        TidyBiocMask$new(.env = .env)
      env_bind(assay_mask$lazy_data,
               .rows = as_data_pronoun(rows_mask$lazy_data),
               .cols = as_data_pronoun(cols_mask$lazy_data),
               .nrow = .nrow, .ncol = .ncol)
      env_bind(rows_mask$lazy_data,
               .assay = as_data_pronoun(assay_mask$lazy_data),
               .cols = as_data_pronoun(cols_mask$lazy_data),
               .nrow = .nrow, .ncol = .ncol)
      env_bind(cols_mask$lazy_data,
               .assay = as_data_pronoun(assay_mask$lazy_data),
               .rows = as_data_pronoun(rows_mask$lazy_data),
               .nrow = .nrow, .ncol = .ncol)
      # main_mask <- env(assay_mask$actv_data,
      #                  .mask_assay = assay_mask$data_mask,
      #                  .mask_rows = rows_mask$data_mask,
      #                  .mask_cols = cols_mask$data_mask,
      #                  poke_ctx = self$poke_ctx)
      # env_bind(
      #   main_mask,
      #   .mask_main = as_data_pronoun(self)
      # )
      private$.mask_assay <- assay_mask
      private$.mask_rows <- rows_mask
      private$.mask_cols <- cols_mask
      # assign based on names.
      lapply(assay_mask$names,
             private$complete_lazy_bind_assay)
      lapply(rows_mask$names,
             private$complete_lazy_bind_rows)
      lapply(cols_mask$names,
             private$complete_lazy_bind_cols)
      # private$finalize_mask(main_mask, .fn)
      self
    },
    eval_mutate_assays = function(quo, name) {
      mask <- private$.mask_assay
      result <- eval_tidy(quo, data = mask$data_mask)
      self$env_bind_assay(result, name)
      mask$push(name)
    },
    eval_mutate_rows = function(quo, name) {
      mask <- private$.mask_rows
      result <- eval_tidy(quo, data = mask$data_mask)
      self$env_bind_rows(result, name)
      mask$push(name)
    },
    eval_mutate_cols = function(quo, name) {
      mask <- private$.mask_cols
      result <- eval_tidy(quo, data = mask$data_mask)
      self$env_bind_cols(result, name)
      mask$push(name)
    },
    finalize_mutate_data = function(.data) {
      private$finalize_mutate_assays(.data) |>
        private$finalize_mutate_rows() |>
        private$finalize_mutate_cols()
    },
    # expected to be called from mutate context
    # in which data should be available immediately
    # In other words, no need lazily bind it.
    env_bind_assay = function(value, name) {
      # browser()
      assay_lazy_env <- private$.mask_assay$lazy_data
      ## validate dim
      len <- length(value)
      .nrow <- private$.nrow
      .ncol <- private$.ncol
      is_valid_matrix <- is.matrix(value) && identical(dim(value), c(.nrow, .ncol))
      is_valid_vec <- is.vector(value) && identical(len, private$.len)
      # enforce that value is either a vector
      if (!(is_valid_matrix || is_valid_vec)) {
        cli::cli_abort("expecting assignment to assays which requires either a 
                         vector of {private$.len} or dimensions of {(.nrow)} 
                         rows and {(.ncol)} columns")
      }
      if (is_valid_vec) {
        value <- matrix(value, nrow = .nrow, ncol = .ncol)
      }
      env_bind(assay_lazy_env, !!name := value)
      private$complete_lazy_bind_assay(name)
    },
    env_bind_rows = function(value, name) {
      row_lazy_env <- private$.mask_rows$lazy_data
      # enforce value is a vector
      .nrow <- private$.nrow
      .ncol <- private$.ncol
      len <- length(value)
      is_valid_vector <- is.vector(value) && (len %in% c(1L, .nrow))
      is_len_one <- len == 1L
      
      if (!is_valid_vector) {
        cli::cli_abort("expected a vector in rows of size 1L or {(.nrow)}")
      }
      if (is_len_one) {
        value <- vec_rep(value, times = .nrow)
      }
      env_bind(row_lazy_env, !!name := value)
      
      private$complete_lazy_bind_rows(name)
    },
    env_bind_cols = function(value, name) {
      col_lazy_env <- private$.mask_cols$lazy_data
      # enforce value is a vector
      .ncol <- private$.ncol
      .nrow <- private$.nrow
      len <- length(value)
      is_valid_vector <- is.vector(value) && (len %in% c(1L, .ncol))
      is_len_one <- len == 1L
      
      if (!is_valid_vector) {
        cli::cli_abort("expected a vector in rows of size 1L or {(.ncol)}")
      }
      if (is_len_one) {
        value <- vec_rep(value, times = .ncol)
      }
      env_bind(col_lazy_env, !!name := value)
      
      private$complete_lazy_bind_cols(name)
    }
  ),
  private = list(
    .eval_mask = NULL,
    .caller_env = NULL,
    .mask_assay = NULL,
    .mask_rows = NULL,
    .mask_cols = NULL,
    .nrow = integer(),
    .ncol = integer(),
    .len = integer(),
    .ctx = character(),
    .fn = character(),
    
    complete_lazy_bind_assay = function(name) {
      # browser()
      # assumes that name is already been
      # lazily bound or fully bound to the top
      # env in the mask
      assay_lazy_env <- private$.mask_assay$lazy_data
      name_sym <- as.name(name)
      # in rows and columns masks, create lazy
      # bindings that mold data
      quo_row <- new_quosure(
        expr(lapply(1:.nrow, function(i) (!!name_sym)[i,, drop = FALSE])),
        assay_lazy_env
      )
      private$.mask_rows$bind_mold(quo_row, name)
      quo_col <- new_quosure(
        expr(lapply(1:.ncol, function(i) (!!name_sym)[, i, drop = FALSE])),
        assay_lazy_env
      )
      private$.mask_cols$bind_mold(quo_col, name)
      # in rows and columns masks, 
      # create active bindings that force mold eval
      private$.mask_rows$bind_actv(name)
      private$.mask_cols$bind_actv(name)
    },
    complete_lazy_bind_rows = function(name) {
      row_lazy_env <- private$.mask_rows$lazy_data
      # in columns, simply repeat it as a column
      name_sym <- as.name(name)
      quo_col <- new_quosure(
        expr(vec_rep(list(!!name_sym), times = .ncol)),
        row_lazy_env
      )
      private$.mask_cols$bind_mold(quo_col, name)
      # in assays, repeat value as vector
      quo_assay <- new_quosure(
        expr(vec_rep(!!name_sym, times = .ncol)),
        row_lazy_env
      )
      private$.mask_cols$bind_mold(quo_col, name)
      # in rows and columns masks, 
      # create active bindings that force mold eval
      private$.mask_assay$bind_actv(name)
      private$.mask_cols$bind_actv(name)
    },
    complete_lazy_bind_cols = function(name) {
      col_lazy_env <- private$.mask_cols$lazy_data
      # in rows, simply repeat it as a column
      name_sym <- as.name(name)
      quo_row <- new_quosure(
        expr(vec_rep(list(!!name_sym), times = .nrow)),
        col_lazy_env
      )
      private$.mask_rows$bind_mold(quo_row, name)
      # in assays, repeat value as vector
      quo_assay <- new_quosure(
        expr(vec_rep_each(!!name_sym, times = .nrow)),
        col_lazy_env
      )
      private$.mask_assay$bind_mold(quo_assay, name)
      # in rows and columns masks, 
      # create active bindings that force mold eval
      private$.mask_assay$bind_actv(name)
      private$.mask_cols$bind_actv(name)
    },
    finalize_mutate_assays = function(.data) {
      mask <- private$.mask_assay
      data_env <- mask$lazy_data
      for (new_assay in mask$modified) {
        assay(.data, new_assay, withDimnames = FALSE) <- data_env[[new_assay]]
      }
      .data
    },
    finalize_mutate_rows = function(.data) {
      mask <- private$.mask_rows
      data_env <- mask$lazy_data
      for (new_row_data in mask$modified) {
        rowData(.data)[[new_row_data]] <- data_env[[new_row_data]]
      }
      .data
    },
    finalize_mutate_cols = function(.data) {
      mask <- private$.mask_cols
      data_env <- mask$lazy_data
      for (new_col_data in mask$modified) {
        colData(.data)[[new_col_data]] <- data_env[[new_col_data]]
      }
      .data
    }
  )
)

`skip!` <- structure(list(), class = "skip")
skip <- function() {
  `skip!`
}
print.skip <- function(x, ...) cat("<skip>\n")
is_skip <- function(x) inherits(x, "skip")

this_env <- environment()
TidySEMask$new(se, this_env, "mutate") -> SEMask
