#' @returns
#' API for using S4 classes with rlang data masks
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom purrr imap
#' @importFrom purrr iwalk
#' @importFrom purrr map
#' @importFrom purrr map_int
#' @importFrom purrr map2
#' @importFrom purrr pmap
#' @importFrom purrr reduce
#' @importFrom purrr walk
#' @importFrom rlang .env
#' @importFrom S4Vectors metadata
#' @importFrom S4Vectors metadata<-
#' @importFrom S7 method
#' @importFrom S7 method<-
#' @importFrom S7 new_generic
#' @importFrom S7 S7_dispatch
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom tidyr nest unnest chop
#' @importFrom tidyselect eval_select
#' @importFrom tidyselect starts_with
#' @importFrom utils .AtNames
#' @importFrom vctrs field
#' @importFrom vctrs list_unchop
#' @importFrom vctrs new_list_of
#' @importFrom vctrs new_rcrd
#' @importFrom vctrs obj_is_list
#' @importFrom vctrs vec_c
#' @importFrom vctrs vec_check_size
#' @importFrom vctrs vec_chop
#' @importFrom vctrs vec_group_loc
#' @importFrom vctrs vec_ptype
#' @importFrom vctrs vec_ptype_abbr
#' @importFrom vctrs vec_recycle_common
#' @importFrom vctrsvec_rep
#' @importFrom vctrsvec_rep_each
#' @importFrom vctrs vec_restore
#' @importFrom vctrs vec_size_common
## usethis namespace: end
NULL

# rlang functions -- figure out all dependencies later
# abort %||% is_empty eval_tidy quo_set_env new_data_mask
# call2 new_quosure expr caller_env new_environment as_label splice enquos
# dots_list env_bind env_bind_lazy env_bind_active new_function is_expression
# env_parents inject as_data_pronoun quo_get_env syms
