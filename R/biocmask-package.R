#' @returns
#' API for using S4 classes with rlang data masks
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom rlang .env
#' @importFrom S7 S7_dispatch
#' @importFrom tidyselect eval_select
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
#' @importFrom vctrs vec_rep
#' @importFrom vctrs vec_rep_each
#' @importFrom vctrs vec_restore
#' @importFrom vctrs vec_size_common
## usethis namespace: end
NULL

# rlang functions -- figure out all dependencies later
# abort %||% is_empty eval_tidy quo_set_env new_data_mask
# call2 new_quosure expr caller_env new_environment as_label splice enquos
# dots_list env_bind env_bind_lazy env_bind_active new_function is_expression
# env_parents inject as_data_pronoun quo_get_env syms
