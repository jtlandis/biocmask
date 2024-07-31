biocmask_manager <- R6::R6Class(
  "biocmask_manager",
  public = list(
    initialize = function(.data, .masks, .ctx_env, .extended_env) {
      private$.data <- .data
      private$.masks <- .masks
      private$.ctx_env <- .ctx_env
      private$.extended_env <- .extended_env
    },
    along_ctx = function() {
      seq_len(private$.ctx_env[["biocmask:::n_groups"]])
    },
    #' @description
    #' eval an expression in the current context 
    #' @param quo a quosure or quoted expression
    #' @param env an environment
    eval = function(quo, env = caller_env()) {
      mask <- private$.masks[[private$.ctx_env[["biocmask:::ctx"]]]]
      mask$eval(quo, env = env)
    },
    #' @description
    #' eval an expression in a contextual mask provided
    #' by the user.
    #' @param quo a quosure or quoted expression
    #' @param env an environment
    #' @param mask a biocmask, ideally something returned
    #' by self$ctx_mask.
    eval_ctx_mask = function(quo, env = caller_env(), mask = self$ctx_mask) {
      mask$eval(quo, env = env)
    }
  ),
  active = list(
    ctx = function(ctx) {
      if (!missing(ctx)) {
        private$.ctx_env[["biocmask:::ctx"]] <- match.arg(ctx, c("assays", "rows", "cols"))
      }
      private$.ctx_env[["biocmask:::ctx"]]
    },
    ctx_mask = function(value) {
      if (!missing(value)) stop("`$ctx_mask` is read only")
      private$.masks[[self$ctx]]
    },
    n_groups = function(value) {
      if (!missing(value)) stop("`$n_groups` is read only")
      private$.ctx_env[["biocmask:::n_groups"]]
    },
    group_id = function(id) {
      if (!missing(id)) {
        private$.ctx_env[["biocmask:::ctx:::group_id"]] <- id
      }
      private$.ctx_env[["biocmask:::ctx:::group_id"]]
    }
  ),
  private = list(
    .data = NULL,
    .masks = list(),
    .ctx_env = NULL,
    .extended_env = list()
  )
)
