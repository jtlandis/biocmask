
#' @export
vec_rep <- new_generic("vec_rep", dispatch_args = "x",
                       function(x, times, ..., 
                                error_call = caller_env(), x_arg = "x",
                                times_arg = "times") {
                             S7_dispatch()
                           })

method(vec_rep, class_vctrs) <- function(x, times, ..., 
                                             error_call = caller_env(), x_arg = "x",
                                             times_arg = "times") {
  vctrs::vec_rep(x = x, times = times, ..., x_arg = x_arg, times_arg = times_arg, error_call = error_call)
}

method(vec_rep, class_s4_vctrs) <- function(x, times, ..., 
                                            error_call = caller_env(),
                                            x_arg = "x",
                                            times_arg = "times") {

  times <- vctrs::vec_cast(times, integer(1), x_arg = times_arg)
  if (length(length) != 1L) abort("`times` must be a single number")
  rep(x, times)
}

#' @export
vec_rep_each <- new_generic("vec_rep_each", dispatch_args = "x",
                       function(x, times, ..., 
                                error_call = caller_env(), x_arg = "x",
                                times_arg = "times") {
                         S7_dispatch()
                       })

method(vec_rep_each, class_vctrs) <- function(x, times, ..., 
                                         error_call = caller_env(),
                                         x_arg = "x",
                                         times_arg = "times") {
  vctrs::vec_rep_each(x = x, times = times, ..., x_arg = x_arg,
                      times_arg = times_arg, error_call = error_call)
}

method(vec_rep_each, class_s4_vctrs) <- function(x, times, ..., 
                                                error_call = caller_env(),
                                                x_arg = "x",
                                                times_arg = "times") {
  
  times <- vctrs::vec_cast(times, integer(1), x_arg = times_arg)
  if (length(length) != 1L) abort("`times` must be a single number")
  rep(x, each = times)
}