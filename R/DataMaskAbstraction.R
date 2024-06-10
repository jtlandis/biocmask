
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
    initialize = function(.data, .env = NULL, .env_top = NULL, ...) {
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
