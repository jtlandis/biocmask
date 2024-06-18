
#' @importFrom rlang eval_tidy new_data_mask env_bind_lazy env_bind_active new_function

#' TidyBiocMaskAbstraction
#' 
#' @description
#' Creates a simple `rlang::new_data_mask`. This is used in conjunction
#' with a MaskManager.
#' 
#' creates a mask which enforces the following
#' search paths:
#' 
#' actv_data --> lazy_mold --> lazy_data --> .env --...--> .env_top
#' 
#'  - actv_data: active binding functions to data in `lazy_mold`
#'  - lazy_mold: lazily bound expression on `lazy_data` that reshapes into
#'               an expected tidy vector for this mask's context
#'  - lazy_data: lazy binding to BioConductor expression to access underlying
#'               data as is.
#'  - .env     : environment that `lazy_data` will be a child of
#'  - .env_top : top of search path. .env should be a child of this.
#' 
TidyBiocMaskAbstraction <- R6::R6Class(
  "TidyBiocMaskAbstraction",
  class = FALSE,
  cloneable = FALSE,
  public = list(
    #' @param .data a data object with names
    #' @param .env environment that mask should enherit from
    #' @param .env_top top level environment.
    initialize = function(.data, .env = NULL, .env_top = NULL, .ngroups = 1L, ...) {
      # delete this
      .env <- new.env(parent = .env)
      rlang::env_bind_active(
        .env,
        .current_group_id = new_function(pairlist(), quote(.current_group_id), private))
      private$.names <- .names <- names(.data)
      private$.ngroups <- .ngroups
      .names <- setNames(nm = .names)
      .size <- length(.names) + 20L
      private$.lazy_data <- new.env(parent = .env)
      env_bind_lazy(private$.lazy_data,
                    !!! lapply(.names,
                               function(x) quo(.data[[!!x]])))
      private$.chops <- new.env(parent = private$.lazy_data, size = .size)
      private$.actv_chop <- new.env(parent = private$.chops, size = .size)
      purrr::map(.names, function(name) self$bind_chop(as.name(name), name))
      
      private$.lazy_mold <- lapply(seq_len(.ngroups),
                                   function(i, p, s) new.env(parent = p, size = s),
                                   p = private$.actv_chop, s = .size)
      private$.actv_data <- lapply(private$.lazy_mold, function(p, s) new.env(parent = p, size = s), s = .size)
      private$.data_mask <- lapply(private$.actv_data, new_data_mask, top = .env_top)
    },
    eval_expr = function(quo, name) {
      eval_tidy(quo, data = private$.data_mask)
    },
    bind_lazy = function(quo, name) {
      env_bind_lazy(private$.lazy_data, !!name := !!quo)
    },
    bind_chop = function(quo, name) {
      nm <- as.name(name)
      env_bind_lazy(private$.chops, !!name := !!private$chop(!!quo), .eval_env = private$.lazy_data)
      fun <- new_function(
        pairlist(),
        body = expr({
          if (is.null(.current_group_id)) 
            !!nm
          else
            .subset2(!!nm, .current_group_id)
          }), env = private$.chops)
      env_bind_active(private$.actv_chop, !!name := fun)
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
    },
    set_group = function(i) {
      if (i > private$.ngroups)
        private$.current_group_id <- NULL
      else 
        private$.current_group_id <- i
      invisible(self)
    },
    inc_group = function() {
      curr_id <- private$.current_group_id
      if (is.null(curr_id))
        curr_id <- 0L
      self$set_group(curr_id + 1L)
    }
  ),
  active = list(
    cur_group_id = function(value) {
      private$.current_group_id
    },
    lazy_data = function(value) {
      if (!missing(value)) 
        stop("`$lazy_data` is read only")
      private$.lazy_data
    },
    actv_chop = function(value) {
      if (!missing(value))
        stop("`$actv_chop` is read only")
      private$.actv_chop
    },
    lazy_mold = function(value) {
      if (!missing(value)) 
        stop("`$lazy_mold` is read only")
      # private$.lazy_mold
      .subset2(private$.lazy_mold, self$cur_group_id)
    },
    actv_data = function(value) {
      if (!missing(value)) 
        stop("`$actv_data` is read only")
      # private$.actv_data
      .subset2(private$.actv_data, self$cur_group_id)
    },
    data_mask = function(value) {
      if (!missing(value)) 
        stop("`$data_mask` is read only")
      .subset2(private$.data_mask, self$cur_group_id)
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
    .names = character(),
    .new_names = character(),
    .current_group_id = NULL,
    .ngroups = NULL,
    # shared environments
    
    #' lazily bound data that
    #' accesses its associated column
    #' AsIs
    .lazy_data = NULL,
    #' lazily chopped data
    .chops = NULL,
    #' active binding to get
    #' current chop
    .actv_chop = NULL,
    ## the following are lists with length
    ## equal in length to number of groups
    ## These
    .lazy_mold = NULL,
    .actv_data = NULL,
    .data_mask = NULL,
    
    #functions
    chop = function(name, ...) {
      nm_expr <- enexpr(name)
      expr(list(!!nm_expr))
    }
  )
)


GroupedTidyBiocMaskAbstraction <- R6::R6Class(
  "GroupedTidyBiocMaskAbstraction",
  inherit = TidyBiocMaskAbstraction,
  class = FALSE,
  cloneable = FALSE,
  public = list(
    initialize = function(.data, ..., .indices) {
      super$initialize(.data, ...)
      private$.lazy_data$.indices <- .indices
    }
  ),
  private = list(
    #functions
    chop = function(name) {
      nm_expr <- enexpr(name)
      expr(vec_chop(!!nm_expr, indices = .indices))
    }
  )
)

GroupedAssayTidyBiocMaskAbstraction <- R6::R6Class(
  "GroupedAssayTidyBiocMaskAbstraction",
  inherit = TidyBiocMaskAbstraction,
  class = FALSE,
  cloneable = FALSE,
  public = list(
    initialize = function(.data, ..., .row_indices = NULL, .col_indices = NULL) {
      super$initialize(.data, ...)
      private$.lazy_data$.row_indices <- .row_indices
      private$.lazy_data$.col_indices <- .col_indices
    }
  ),
  private = list(
    #functions
    chop = function(name) {
      nm_expr <- enexpr(name)
      expr(vec_chop_assays(!!nm_expr, row_indices = .row_indices, col_indices = .col_indices))
    }
  )
)

GroupedRowsAssay <- R6::R6Class(
  "GroupedRowsAssay",
  inherit = GroupedAssayTidyBiocMaskAbstraction,
  class = FALSE,
  cloneable = FALSE,
  private = list(
    chop = function(name) {
      nm_expr <- enexpr(name)
      expr(vec_chop_assays_row(!!nm_expr, row_indices = .row_indices))
    }
  )
)

GroupedColsAssay <- R6::R6Class(
  "GroupedColsAssay",
  inherit = GroupedAssayTidyBiocMaskAbstraction,
  class = FALSE,
  cloneable = FALSE,
  private = list(
    chop = function(name) {
      nm_expr <- enexpr(name)
      expr(vec_chop_assays_col(!!nm_expr, col_indices = .col_indices))
    }
  )
)

bioc_mask <- function(.data) {
  TidyBiocMaskAbstraction$new(.data, .env = bot_env, .env_top = top_env, .ngroups = 1L)
}

bioc_mask_grouped <- function(.data, indices) {
  GroupedTidyBiocMaskAbstraction$new(.data, .env = bot_env, .env_top = top_env,
                                     .ngroups = length(indices),
                                     .indices = indices)
}

bioc_mask_grouped_assay <- function(.data, row_ind, col_ind) {
  row_grouped <- !is.null(row_ind)
  type <- ""
  if (row_grouped) {
    type <- "row"
  }
  col_grouped <- !is.null(col_ind)
  if (col_grouped) {
    type <- paste0(type, "col")
  }
  `_class` <- switch (type,
    rowcol = GroupedAssayTidyBiocMaskAbstraction,
    row = GroupedRowsAssay,
    col = GroupedColsAssay
  )
  `_class`$new(.data, .env = bot_env, .env_top = top_env,
               .ngroups = length(row_ind),
               .row_indices = row_ind,
               .col_indices = col_ind)
}






