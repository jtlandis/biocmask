
#' @keywords internal
"_PACKAGE"

#' @importFrom dplyr mutate group_by ungroup arrange reframe across everything
#'  select rename_with bind_cols
#' @importFrom tidyselect eval_select
#' @import rlang 
#' @importFrom purrr walk map walk2 map2 map_int 
#' @importFrom tidyr nest unnest chop
#' @importFrom vctrs vec_c vec_recycle_common vec_size_common vec_chop list_unchop
#' vec_rep vec_rep_each vec_group_loc
NULL

# rlang functions -- figure out all dependencies later
# abort %||% is_empty eval_tidy quo_set_env new_data_mask
# call2 new_quosure expr caller_env new_environment as_label splice enquos
# dots_list env_bind env_bind_lazy env_bind_active new_function is_expression
# env_parents inject as_data_pronoun quo_get_env syms