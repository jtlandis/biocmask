
#' @keywords internal
"_PACKAGE"

#' @importFrom dplyr across arrange bind_cols everything filter group_by
#' group_data group_vars mutate n reframe rename rename_with rows_update
#' summarise ungroup
#' @import rlang 
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr iwalk walk map map2 map_int reduce walk 
#' @importFrom tidyr nest unnest chop
#' @importFrom vctrs vec_c vec_recycle_common vec_size_common vec_chop list_unchop
#' vec_rep vec_rep_each vec_group_loc
NULL

# rlang functions -- figure out all dependencies later
# abort %||% is_empty eval_tidy quo_set_env new_data_mask
# call2 new_quosure expr caller_env new_environment as_label splice enquos
# dots_list env_bind env_bind_lazy env_bind_active new_function is_expression
# env_parents inject as_data_pronoun quo_get_env syms