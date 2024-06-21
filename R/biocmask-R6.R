

#' @param .expr an expression that will be evaluated and bound in `.env_expr`. 
#' It is recommened to use `base::quote()` so that `.expr` can contain diffusion
#' operators such as `!!`
#' @param .env_expr the environment that `.expr` is evaluated in as a quosure
#' @param .env_bind where the expression will be bound
#' @param type the type of binding function to implement
#' @return a function taking a single argument `name` and performs a binding
#' determined by `type`.
#' @examples
#' 
#' # example code
#' 
#' env <- new_environment(list(
#'   .iris = iris,
#'   .nrow = 10),
#'   baseenv())
#' binding_func <- add_bind(
#'   .expr = quote(lapply(1:.nrow, function(i, x) x[i], x = .iris[[!!name]])),
#'   .expr_env = env,
#'   type = "lazy")
#' binding_func("Sepal.Width")
#' env$Sepal.Width
#' 
add_bind <- function(.expr, .env_expr, .env_bind = .env_expr, type = c("standard", "lazy","active")) {
  type <- match.arg(type, c("standard", "lazy", "active"))
  fun <- switch(type,
                standard = expr(env_bind),
                lazy = expr(env_bind_lazy),
                active = expr(env_bind_active))
  name_unquo <- quote(!!name)
  quosure_unquo <- quote(!!quosure)
  new_function(
    args = alist(name=), 
    body = expr({
    # browser()
    name_sym <- as.name(name)
    quosure <- new_quosure(expr(!!.expr), env = !!.env_expr)
    (!!fun)(!!.env_bind, !!name_unquo := !!quosure_unquo)
    }))
}

env <- new_environment(list(
  .iris = iris,
  .nrow = 10),
  baseenv())
binding_func <- add_bind(
  .expr = quote(lapply(1:.nrow, function(i, x) x[i], x = .iris[[!!name]])),
  .env_expr = env,
  type = "lazy")
binding_func("Sepal.Width")
env$Sepal.Width


biocmask <- R6::R6Class(
  "biocmask",
  public = list(
    initialize = function(.data, .indices = NULL) {
      private$.data <- .data
      private$.indices <- .indices
      private$make_current_group_info()
      
      private$.names <- setNames(nm = names(.data))
      private$.env_size <- length(.names) + 20L
      
      private$make_foreign_data()
      
      private$make_data_lazy()
      
      private$make_chop_data()
      
      # get current chop
      private$env_mask_bind <- new.env(parent = private$env_data_chop, size = .size)
      env_bind_active(
        private$env_mask_bind,
        !!! lapply(.names,
                   function(name, env) {
                     name <- sym(name)
                     new_function(
                       args = pairlist(),
                       body = expr(.subset2(!!name, `biocmask:::current_group_id`)),
                       env = env
                     )
                   },
                   env = private$env_data_chop)
      )
      
      invisible(self)
      
    },
    on_bind = function(.fun) {
      private$.on_bind <- append(private$.on_bind,
                                 .fun)
      invisible(self)
    },
    bind = function(name, value) {
      private$.bind_self(name, value)
      for (func in private$.on_bind) {
        func(name)
      }
      invisible(self)
    },
    eval = function(quo) {
      mask <- new_data_mask(private$env_mask_bind, top = private)
      eval_tidy(quo, data = mask)
    }
  ),
  active = list(
    environments = function() {
      env_parents(private$env_mask_bind, private)
    }
  ),
  private = list(
    make_current_group_info = function() {
      private$env_current_group_info <- new_environment(
        list(
          `biocmask:::current_group_id` = 1L,
          `biocmask:::current_group_size` = 0L
        ),
        private
      )
    },
    make_foreign_data = function() {
      private$env_foreign_data <- new.env(parent = private$env_current_group_info)
    },
    make_data_lazy = function() {
      .data <- private$.data
      # normal data ... do we need it to be lazy??
      private$env_data_lazy <- new.env(
        parent = private$env_foreign_data,
        size = private$.env_size)
      env_bind_lazy(
        private$env_data_lazy,
        !!! lapply(private$.names, function(x) quo(.data[[!!x]])))
    },
    make_data_chop = function(.indices) {
      # chops
      private$env_data_chop <- new.env(
        parent = private$env_data_lazy,
        size = private$.env_size)
      private$handle_chops(.indices)
      env_bind_lazy(
        private$env_data_chop,
        !!! lapply(.names, as.name) |>
          lapply(private$chop_data) |>
          lapply(new_quosure,
                 env = private$env_data_lazy)
      )
    },
    handle_chops = function(indices) {
      private$.indices <- indices
      private$.ngroups <- if (is.null(indices)) 1L else length(indices)
      fun <- private$get_chop_fun()
      test_output <- fun(test)
      stopifnot(is_expression(test_output))
      # environment(fun) <- private$env_data_lazy
      private$chop_data <- fun
      invisible(self)
    },
    # extending function
    get_chop_fun = function() {
      if (is.null(private$.indices)) {
        function(name) {
          name <- enexpr(name)
          expr(list(!!name))
        }
      } else {
        function(name) {
          name <- enexpr(name)
          expr(vec_chop(!!name, indices = private$.indices))
        } 
      }
    },
    # function to chop data
    chop_data = NULL,
    .bind_self = function(name, value) {
      private$env_data_lazy[[name]] <- value
      name_sym <- sym(name)
      quo <- new_quosure(
        private$chop_data(!!name_sym),
        env = private$env_data_lazy
      )
      env_bind_lazy(
        private$env_data_chop,
        !!name := !!quo
      )
      fun <- new_function(
        args = pairlist(),
        body = expr(.subset2(!!name_sym, `biocmask:::current_group_id`)),
        env = private$env_data_chop
      )
      env_bind_active(
        private$env_mask_bind,
        !!name := fun
      )
      invisible(self)
    },
    .on_bind = list(),
    
    # data input
    .data = NULL,
    # list of indices
    .indices = NULL,
    # type of grouping, "none", "group"
    .grouped = NULL,
    .ngroups = NULL,
    
    #inital names of `.data`
    .names = NULL,
    # initial size of environments
    # number of elements of `.data` + 20L
    .env_size = NULL,
    
    
    #' holds grouping information
    #' for this object
    env_current_group_info = NULL,
    #' foreign bindings
    env_foreign_data = NULL,
    env_data_lazy = NULL,
    env_data_chop = NULL,
    env_mask_bind = NULL
  )
)

im <- biocmask$new(iris, list(1:50, 51:100, 101:150))
im$eval(quote(Sepal.Width))

biocmask_assay <- R6::R6Class(
  "biocmask_assay",
  inherit = biocmask,
  public = list(
    initialize = function(.data, .indices, .nrow, .ncol) {
      super$initialize(.data, .indices)
      env_bind(
        private$env_current_group_info,
        .nrow = .nrow,
        .ncol = .ncol
      )
      assay_group_type <- group_type(.indices)
      current_names <- private$.names
      #finalize bindings
      switch(
        assay_group_type,
        none = {
          # no groupings, row and col contexts should
          # return the same environment bindings
          private$.env_row_ctx <- private$.env_col_ctx <- private$env_mask_bind
        },
        rowcol = {
          # row and col groupings exist, but row & col
          # contexts cannot be evaluated at the same time
          # setup empty environments
          row_ctx_chop <- new_environment()
          col_ctx_chop <- new_environment()
          
          
          
          # self$on_bind(
          #   add_bind(
          #     quote(vec_chop_assays_row(!!name), private$.row_indices),
          #     .env_expr = private$lazy_data,
          #     .env_bind = row_ctx_chop,
          #     type = "lazy"
          #   )
          # )
          # self$on_bind(
          #   add_bind(
          #     quote(vec_chop_assays_col(!!name), private$.col_indices),
          #     .env_expr = private$lazy_data,
          #     .env_bind = col_ctx_chop,
          #     type = "lazy"
          #   )
          # )
          # private$.env_row_ctx <- new.
          
        },
        row = {
          
        },
        col = {
          
        })
      if (!is.null(.indices$.rows)) {
        env_bind_active(
          private$env_current_group_info,
          .nrow = function() length(.subset2(.indices$.rows, `biocmask:::current_group_id`))
        )
      }
      if (!is.null(.indices$.cols)) {
        env_bind_active(
          private$env_current_group_info,
          .ncol = function() length(.subset2(.indices$.cols, `biocmask:::current_group_id`))
        )
      }
      
    }
  ),
  private = list(
    get_chop_fun = function() {
      .indices <- private$.indices
      if (is.null(.indices)) {
        private$.env_row_chop <- private$.env_col_chop <- private$env_data_chop
        return(function(name) {
          name <- enexpr(name)
          expr(list(!!name))
        })
      } else {
        type <- attr(.indices, "type")
        private$.ngroups <- nrow(.indices)
        fun <- switch(
          type,
          rowcol = {
            # row and col contexts are available
            private$.env_row_chop <- new_environment()
            private$.env_col_chop <- new_environment()
            self$on_bind(
              add_bind(
                quote(vec_chop_assays_row(!!name), private$.row_indices),
                .env_expr = private$lazy_data,
                .env_bind = private$.env_row_chop,
                type = "lazy"
              )
            )
            self$on_bind(
              add_bind(
                quote(vec_chop_assays_col(!!name), private$.col_indices),
                .env_expr = private$lazy_data,
                .env_bind = private$.env_col_chop,
                type = "lazy"
              )
            )
            function(name) {
              name <- enexpr(name)
              expr(vec_chop_assays(!!name, private$.indices))
            }
          },
          row = {
            private$.env_row_ctx <- new_environment()
            private$.env_col_ctx <- new_environment()
            function(name) {
              name <- enexpr(name)
              expr(vec_chop_assays_row(!!name, private$.indices))
            }
          },
          col = function(name) {
            name <- enexpr(name)
            expr(vec_chop_assays_col(!!name, private$.indices))
          }
        )
        return(fun) 
      }
    },
    .row_indices = NULL,
    .env_row_ctx = NULL,
    .col_indices = NULL,
    .env_col_ctx = NULL
  )
)
