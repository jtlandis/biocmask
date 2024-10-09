conflict_info <- c(
    "Note that these packages are exclusive and present different APIs ",
    "for working with `SummarizedExperiment` objects. As such conflicts ",
    "and unexpected behaviors may arise!"
)

rule <- function (pad = "-", gap = 2L) {
  paste0(rep(pad, getOption("width") - gap), collapse = "")
}

tidySE_opt <- list(quote(tidySummarizedExperiment), "tidySummarizedExperiment")

attaching_tidySE <- function(expr) {
  if (is_call(expr, name = "library")) {
    call <- match.call(library, call = expr)
    out <- tidySE_opt |>
      vapply(identical, y = call$package, logical(1)) |>
      any()
    return(out)
  } 
  FALSE
  
}

you_have_loaded <- function(pkg1, pkg2) {
  cli::cli_rule()
  cli::cli_alert_warning(
    paste0("you have loaded `",pkg1,"` after `",pkg2,"`\n")
    )
  cli::cli_text("")
  cli::cli_text(conflict_info)
  cli::cli_text("")
}

# a package can either be loaded, or completely attached. R only informs
# of conflicts when the package is loaded, and when attached these can be
# overwritten silently

.onAttach <- function(libname, pkgname) {
  # check if `tidySummarizedExperiment` is already loaded
  conflict <- grep("tidySummarizedExperiment", search(), value = TRUE)
  if (length(conflict)) {
    packageStartupMessage(you_have_loaded("biocmask","tidySummarizedExperiment"))
  }
  # set hook for if `tidySummarizedExperiment` gets attached latter
  setHook(
    packageEvent(pkgname = "tidySummarizedExperiment",
                 event = "attach"),
    function(...) {
      packageStartupMessage(you_have_loaded("tidySummarizedExperiment","biocmask"))
    }
    
  )
  # hook for when tidySummarizedExperiment:: is used after biocmask is attached
  setHook(
    packageEvent(pkgname = "tidySummarizedExperiment",
                 event = "onLoad"),
    function(...) {
      calls <- sys.calls()
      called_via_library <- vapply(calls, attaching_tidySE, logical(1))
      if (!any(called_via_library)) {
        # if this callback was not executed via library(tidySummarizedExperiment)
        # then warn that biocmask dplyr methods have likely been overwritten!
        # otherwise, let the .onAttach method warn the user
        rlang::inform(
          .frequency = "regularly", 
          .frequency_id = "tidySummarizedExperiment_loaded_after_biocmask_attach",
          class = "biocmask_loaded_conflict",
          message = c(
            paste0(
              "You have likely loaded `tidySummarizedExperiment` after ",
              "`biocmask` via `tidySummarizedExperiment::...`"
            ),
            "This has likely caused `dplyr` methods from `biocmask` to be over-written!",
            conflict_info))
      }
      
    }
  )
}

.onDetach <- function(libpath) {
  # remove verbose message when `biocmask` is removed from attach list
  setHook(packageEvent("tidySummarizedExperiment", "attach"), NULL, "replace")
  setHook(packageEvent("tidySummarizedExperiment", "onLoad"), NULL, "replace")
}

# check_dplyr_s3_method <- function(method) {
#   tbl <- getNamespace("dplyr")[[".__S3MethodsTable__."]]
#   res <- tbl[[method]]
#   if (!is.null(res) && is.function(res)) {
#     env_name <- environmentName(environment(res))
#     if (env_name != "biocmask") {
#       rlang::warn(
#         paste0(c("`biocmask` shows that `", env_name, "` is already loaded")))
#     }
#   }
#   res
# }
