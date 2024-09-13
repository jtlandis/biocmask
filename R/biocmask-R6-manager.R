
#' @title `biocmask` Data Mask Manager
#' @name BiocmaskManager
#' @description
#' This object organizes serveral [biocmasks][biocmask::BiocDataMask], allowing
#' expressions to be evaluated in different contexts. This object is the return
#' value of [`new_biocmask()`][biocmask::new_biocmask]
#' 
#' The "connectedness" of each mask managed by this object is dependent on the
#' developer. The biocmasks passed to `.mask` argument may stem from the same
#' shared environment, or may have cyclical relationships.
#' @return An R6 object inheriting `biocmask_manager`.
#' @example 
#'  
#' manager <- new_biocmask(se_simple)
#' manager$ctx
#' q <- biocmask_quos(counts_1 = counts + 1, rows(d = direction),
#'                    cols(is_drug = condition=="drug"),
#'                    .ctx_default = "assays",
#'                    .ctx_opt = c("rows", "cols"))
#' manager$eval(q[[1]])
#' manager$results()
#' manager$eval(q[[2]])
#' manager$results()
#' 
#' 
biocmask_manager <- R6::R6Class(
  "biocmask_manager",
  public = list(
    #' @param .data Original data
    #' @param .masks list of named biocmask objects
    #' @param .ctx_env shared context environment
    #' @param .extended_env other extended environments
    initialize = function(.data, .masks, .ctx_env, .extended_env) {
      private$.data <- .data
      private$.masks <- .masks
      private$.ctx_env <- .ctx_env
      private$.extended_env <- .extended_env
    },
    #' @description
    #' provides a sequence for iterate over the groups
    along_ctx = function() {
      seq_len(private$.ctx_env[["biocmask:::n_groups"]])
    },
    #' @description
    #' eval an expression in the current context 
    #' @param quo a quosure or quoted expression
    #' @param env an environment
    #' @return returns evaluated `quo` in the form of a chop
    eval = function(quo, env = caller_env()) {
      mask <- private$.masks[[private$.ctx_env[["biocmask:::ctx"]]]]
      # expand across into more biocmask_quos
      quos <- expand_across(quo, mask = self, error_call = caller_call())
      for (k in seq_along(quos)) {
        quo <- quos[[k]]
        quo_data <- attr(quo, "biocmask:::data")
        chop_out <- biocmask_manager_eval(
          quo = quo,
          env = env,
          n_groups = self$n_groups,
          mask = mask,
          private = private
        )
        name <- if (quo_data$is_named) 
          quo_data$name
        else
          as_label(quo_get_expr(quo))
        mask$bind(name = name, value = chop_out)
      }
      invisible(self)
    },
    #' @description
    #' collects the evaluated results with biocmasks
    #' @return named list for each mask containing named list of evaluated 
    #' expressions.
    results = function() {
      lapply(private$.masks, function(m) m$results())
    }
  ),
  active = list(
    #' @field ctx get and set the current context
    ctx = function(ctx) {
      if (!missing(ctx)) {
        private$.ctx_env[["biocmask:::ctx"]] <- match.arg(ctx, c("assays", "rows", "cols"))
      }
      private$.ctx_env[["biocmask:::ctx"]]
    },
    #' @field ctx_mask get the current context biocmask
    ctx_mask = function(value) {
      if (!missing(value)) stop("`$ctx_mask` is read only")
      private$.masks[[self$ctx]]
    },
    #' @field n_groups get the current context biocmask group size
    n_groups = function(value) {
      if (!missing(value)) stop("`$n_groups` is read only")
      private$.ctx_env[["biocmask:::n_groups"]]
    },
    #' @field group_id get and set the current context biocmask group id
    group_id = function(id) {
      if (!missing(id)) {
        private$.ctx_env[["biocmask:::ctx:::group_id"]] <- id
      }
      private$.ctx_env[["biocmask:::ctx:::group_id"]]
    },
    #' @field masks get the private list of masks
    masks = function(value) {
      if (!missing(value)) stop("`$masks` is read only")
      private$.masks
    },
    #' @field extended other environments extended from a context mask.
    extended = function(value) {
      if (!missing(value)) stop("`$exteded` is read only")
      private$.extended_env[[private$.ctx_env[["biocmask:::ctx"]]]]
    }
  ),
  private = list(
    .data = NULL,
    .masks = list(),
    .ctx_env = NULL,
    .extended_env = list()
  )
)

# Created this scoped function so that on.exit could be called
# in case of an error. Need to write test
# mutate(se, counts = stop("check rlang::env_parents()"))
biocmask_manager_eval <- function(quo, env, n_groups, mask, private) {
  chop_out <- vector("list", n_groups)
  if (!is.null(quo_get_expr(quo))) {
    # weird bug with eval_tidy?
    # parent structure gets changed
    on.exit(env_poke_parent(top_env, baseenv()), add = TRUE)
    for (i in seq_len(n_groups)) {
      private$.ctx_env[["biocmask:::ctx:::group_id"]] <- i
      result <- mask$eval(quo, env = env)
      chop_out[[i]] <- result
    }
  }
  chop_out
}

## created for a hypothetical usage of passing a `biocmask_manager`
## as the data argument to `biocmask_eval_select()`.
## This use case is not used within the package, so we should not
## write documentation for something that isn't used until we actual
## use it...
# setOldClass("biocmask_manager")
# 
# setMethod("assays", signature = "biocmask_manager",
#           definition = function(x, withDimnames = TRUE, ...) {
#             x$masks[["assays"]]$ptype
#           })
# 
# setMethod("rowData", signature = "biocmask_manager",
#           definition = function(x, withDimnames = TRUE, ...) {
#             x$masks[["rows"]]$ptype
#           })
# 
# setMethod("colData", signature = "biocmask_manager",
#           definition = function(x, withDimnames = TRUE, ...) {
#             x$masks[["cols"]]$ptype
#           })
