

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
      
      private$env_current_group_info <- new_environment(
        list(
          `biocmask:::current_group_id` = 1L,
          `biocmask:::current_group_size` = 0L
        ),
        private
      )
      
      .names <- setNames(nm = names(.data))
      .size <- length(.names) + 20L
      
      private$env_foreign_data <- new.env(parent = private$env_current_group_info)
      
      # normal data ... do we need it to be lazy??
      private$env_data_lazy <- new.env(parent = private$env_foreign_data, size = .size)
      env_bind_lazy(
        private$env_data_lazy,
        !!! lapply(.names, function(x) quo(.data[[!!x]])))
      
      # chops
      private$env_data_chop <- new.env(parent = private$env_data_lazy, size = .size)
      private$handle_chops(.indices)
      env_bind_lazy(
        private$env_data_chop,
        !!! lapply(.names, as.name) |>
          lapply(private$chop_data) |>
          lapply(new_quosure,
                 env = private$env_data_lazy)
      )
      
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
      env_parents(private$env_mask_bind)
    }
  ),
  private = list(
    # extending function
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
    
    
    # list of indices
    .indices = NULL,
    # type of grouping, "none", "group"
    .grouped = NULL,
    .ngroups = NULL,
    
    #' holds grouping information
    #' for this object
    env_current_group_info = NULL,
    #' foreign bindings
    env_foreign_data = NULL,
    env_data_lazy = NULL,
    env_data_chop = NULL,
    env_mask_bind = NULL
    
    #minimal envir variables
    # `{` = base::`{`,
    # `(` = base::`(`,
    # list = base::list,
    # .subset2 = base::.subset2,
    # `<-` = base::`<-`,
    # vec_chop = vctrs::vec_chop,
    # enexpr = rlang::enexpr
  )
)

im <- biocmask$new(iris, list(1:50, 51:100, 101:150))
im$eval(quote(Sepal.Width))
