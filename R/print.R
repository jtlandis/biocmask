

sep_ <- function(n) {
  x <- vctrs::vec_rep("|", times = n)
  class(x) <- "sep!"
  x
}

#' @export
`vec_ptype_abbr.sep!` <- function(x) {
  NULL
}

#' @export
print.SummarizedExperiment <- function(x, n = 10, ...) {
  
  n_slice <- ceiling(n / 4)
  top_n <- ceiling(n/2)
  bot_n <- floor(n/2)
  nr <- nrow(x)
  row_slice <- if (nr < 2 * n_slice) {
    seq_len(nr)
  } else {
    c(1:n_slice, (nr - n_slice + 1):nr)
  }
  
  nc <- ncol(x)
  col_slice <- if (nc < 2 * n_slice) {
    seq_len(nc)
  } else {
    c(1:n_slice, (nc - n_slice + 1):nc)
  }
  # get first 5 and last 5 rows and cols
  x_ <- x[row_slice, col_slice]
  nr <- nrow(x_)
  nc <- ncol(x_)
  .features <- rownames(x_) %||% seq_len(nr)
  .samples <- colnames(x_) %||% seq_len(nc)
  assays_ <- map(assays(x_), as.vector)
  row_ <- map(as_tibble(rowData(x_)), vctrs::vec_rep, times = nc)
  col_ <- map(as_tibble(colData(x_)), vctrs::vec_rep_each, times = nr)
  nn <- nc * nr
  out <- c(
    list(
      .features = vctrs::vec_rep(.features, times = nc),
      .samples = vctrs::vec_rep(.samples, times = nr)
    ),
    list(sep_(nn)),
    assays_,
    list(sep_(nn)),
    row_,
    list(sep_(nn)),
    col_
  )
  # browser()
  
  attr(out, "row.names") <- c(NA_integer_, - nr * nc)
  class(out) <- c("SE_abstraction","tbl_df", "tbl", "data.frame")
  # browser()
  top_half <- 1:top_n
  bot_half <- (nn - bot_n + 1):nn
  out_sub <- out[c(top_half, bot_half),]
  if (nrow(out_sub)==nrow(out)) {
    attr(out_sub, "biocmask:::has_break_at") <- 0L
  } else {
    attr(out_sub, "biocmask:::has_break_at") <- max(top_half)
  }
  attr(out_sub, "biocmask:::data") <- x
  print(out_sub, n = n, ...)
  invisible(x)
}

collapse <- function(x) {
  paste(x, collapse = ", ")
}

#' @export
format.biocmask_pillar_rid_type <- function(x, width= NULL, ...) {
  ""
}

#' @export
format.biocmask_pillar_rid_shaft <- function(x, width, ...) {
  new_ornament(
    style_subtle(
      align(x$row_ids, width = width, align = "right")
    ),
    width = width, align = "right"
  )
}

#' @export
ctl_new_rowid_pillar.SE_abstraction <- function(controller,
                                                x, width, ...,
                                                title = NULL, type = NULL) {
  
  if (val <- attr(controller, "biocmask:::has_break_at")) {
    # browser()
    template <- names(ctl_new_pillar(controller, vector(), width, 
                                     title = title))
    if (!length(template)) {
      return(NULL)
    }
    out <- map(set_names(template), function(.x) "")
    top_half <- 1:val
    bot_half <- rev(seq_len(nrow(controller) - (val + 1L)))
    data <- if (length(bot_half)) {
      spec <- sprintf("n-%i", bot_half)
      c(top_half,
        align(
          c(spec,
            "n"),
          align = "left")
        )
    } else {
      c(top_half, "n")
    }
    if ("type" %in% template) {
      out$type <- pillar_component(
        structure(
          list(), class = "biocmask_pillar_rid_type",
          width = 1L
        )
      )
    }
    data <- new_pillar_shaft(
      list(row_ids = data),
      width = max(nchar(data)),
      class = "biocmask_pillar_rid_shaft"
    )
    if ("data" %in% template) {
      out$data <- pillar_component(data)
    }
    new_pillar(out, width = attr(data, "width", exact = TRUE))
  } else {
    NextMethod()
  }
}

#' @export
`pillar_shaft.sep!` <- function(x) {
  new_pillar_shaft_simple(
    style_subtle(x),
    align = "left",
    min_width = 1L
  )
}

#' @export
tbl_sum.SE_abstraction <- function(x) {
  se <- attr(x, "biocmask:::data")
  out <- dim_desc(se)
  out <- sprintf("A %s-tibble Abstraction: %s", class(se), out)
  # names(out) <- sprintf("A %s-tibble Abstraction", class(se))
  if (!is.null(groups <- metadata(se)[["group_data"]])) {
    gv <- group_vars(se)
    vars <- c(
      if (!is_empty(gv$row_groups)) sprintf("rows(%s)", collapse(gv$row_groups)),
      if (!is_empty(gv$col_groups)) sprintf("cols(%s)", collapse(gv$col_groups))
    )
    out <- c(out, sprintf("Groups: %s", collapse(vars)))
  }
  out
}

#' @export
tbl_format_setup.SE_abstraction <- function(x, width, ...,
                                            n, max_extra_cols,
                             max_footer_lines, focus) {
  
  setup <- NextMethod()
  if (val <- attr(x, "biocmask:::has_break_at")) {
    body_idx <- val + 2L
    prev_line <- gregexec("[^ |]+", cli::ansi_strip(setup$body[body_idx]))
    position <- prev_line[[1]]
    len <- attr(position, "match.length")
    new_line <- vctrs::vec_rep(" ", times = setup$width)
    new_line[position + floor(len / 2)] <- style_subtle(cli::symbol$continue)
    setup$body <- append(setup$body,
                         paste(new_line, collapse = ""),
                         body_idx) |>
      glue::as_glue()
  }
  setup
}

#' @export
ctl_new_pillar.SE_abstraction <- function(controller, x, width, ..., title = NULL) {
  
  if (inherits(x, "sep!")) {
    p <-pillar(x, title = "|", ...)
    class(p$title[[1]]) <- "blank_pillar_title"
    class(p$type[[1]]) <- "blank_pillar_type"
    attr(p$type, "width") <- 1L
    attr(p$type, "min_width") <- 1L
    attr(p$type[[1]], "width") <- 1L
    attr(p$type[[1]], "min_width") <- 1L
    p
  } else {
    NextMethod()
  }
}

# tbl_format_body.SE_abstraction <-function(x, setup) {
#   # browser()
#   out <- NextMethod()
#   out
# }

format.blank_pillar_type <- format.blank_pillar_title <- function(x, width, ...) {
  style_subtle("|")
}

# tbl_format_header.SE_abstraction

#' weave <- function(x) {
#'   n_ <- length(x)
#'   ind <- seq_along(x)
#'   out <- integer(n_)
#'   mid_point <- ceiling(n_/2)
#'   is_even <- ind %%2 == 0
#'   out[!is_even] <- 1:mid_point
#'   out[is_even] <- n_:(mid_point + 1L)
#'   out
#' }
#' 
#' col_spacing <- function(x, width, threshold) {
#'   sum(cumsum(x)/width < threshold)
#' }
#' 
#' get_width <- function(x) {
#'   attr(x, "width", exact = TRUE)
#' }
#' 
#' #' @export
#' print.SummarizedExperiment <- function(x, width = NULL) {
#'   width <- width %||% cli::console_width() %||% 80L
#'   block_height <- min(nrow(x), 10L)
#'   # if
#'   if (nrow(x) <= 15L ) {
#'     block_height <- nrow(x)
#'   }
#' 
#'   # get the names
#'   row_nms <- rownames(x) %||% as.character(seq_len(nrow(x)))
#'   col_nms <- colnames(x) %||% as.character(seq_len(ncol(x)))
#' 
#'   # start with assays
#'   # see how many columns of assays we can display
#'   # reserve 50% AT MOST for this data
#' 
#'   # check how much space row_nms will require for the block_height
#'   # this can only take AT MOST 10% space
#'   row_weave_i <- sort(weave(row_nms)[1:block_height])
#'   row_nms_n <- nchar(row_nms[row_weave_i]) + 1
#'   row_width_p <- row_nms_n/width
#'   rows_width <- min(max(row_width_p), .1)
#' 
#'   # still reserve 50% of horizontal space for col_names
#'   # we have 50% - rows_width left (at least 40%)
#'   weave_cols_i <- weave(col_nms)
#'   cols_widths <- nchar(col_nms) + 1
#'   # number of columns that fit spacing requirements based on names (not data yet)
#'   ncols_ <- col_spacing(cols_widths[weave_cols_i], width, threshold = .5 - rows_width)
#'   col_ <- colData(x)
#'   col_col_disp <- sort(weave(col_)[1:min(block_height, ncol(col_))])
#'   col_select <- sort(weave_cols_i[1:ncols_])
#'   col_ <- col_[col_select, col_col_disp]
#' 
#'   assay_ <- assay(x)[row_weave_i, col_select]
#' 
#'   # now find how many columns of the rows we can fit with 45% resereved space
#'   row_ <- rowData(x)
#'   row_col_weave_i <- weave(row_)
#'   row_col_widths <- nchar(colnames(row_)) + 1
#'   # maybe compute if we have extra room? probably rare
#'   ncols_row_ <- col_spacing(row_col_widths[row_col_weave_i], width, threshold = .45)
#'   row_col_select <- sort(row_col_weave_i[1:ncols_row_])
#'   row_ <- row_[row_weave_i, row_col_select]
#' 
#' 
#' }
#' 
#' format_title <- function(titles, width) {
#'   coldata_nms_width <- min(coldata_nms_width, max(coldata_cols_titles_w))
#'   widths <- map_int(titles, get_width)
#'   is_large <- widths > width
#'   idx <- integer(length(titles))
#'   if (any(is_large)) {
#'     idx[is_large] <- cumsum(is_large[is_large]) + curr_footnote_idx
#'     curr_footnote_idx <<- max(idx)
#'   }
#'   out <- map2_chr(
#'     titles,
#'     idx,
#'     ~ format(.x, width = width, footnote_idx = .y)
#'   )
#'   attr(out, "has_footnote") <- idx
#'   out
#' }
#' 
#' pillar_shaft_width <- function(x, width) {
#'   shaft <- pillar_shaft(x)
#'   attr(shaft, "width") <- width + 1L
#'   shaft
#' }
#' 
#' format.SummarizedExperiment <- function(x, n = 6, width = NULL) {
#'   curr_footnote_idx <- 0L
#'   width <- width %||% cli::console_width() %||% 80L
#'   block_height <- min(nrow(x), n)
#'   # if
#'   if (nrow(x) <= n * 1.5 ) {
#'     block_height <- nrow(x)
#'   }
#'   nr <- nrow(x)
#'   nc <- ncol(x)
#'   weave_seq_i <- weave(seq_len(nr))
#'   weave_i <- sort(weave_seq_i[1:n])
#'   weave_seq_j <- weave(seq_len(nc))
#'   weave_j <- sort(weave_seq_j[1:n])
#'   x_ <- x[weave_i, weave_j]
#' 
#'   #format the columns of colData
#'   col_ <- as_tibble(colData(x_))
#'   coldata_cols_n <- min(ncol(col_), block_height)
#'   if (ncol(col_) <= n * 1.5) {
#'     coldata_cols_n <- ncol(col_)
#'   }
#'   coldata_weave <- sort(weave(col_)[1:coldata_cols_n])
#'   col_ <- col_[coldata_weave]
#'   coldata_cols_titles <- map(colnames(col_), new_pillar_title)
#'   coldata_cols_titles_w <- map_int(coldata_cols_titles, get_width)
#' 
#' 
#'   # minimal space for abbreviation
#'   coldata_cols_types <- map(col_, new_pillar_type)
#'   coldata_cols_types_w <- map_int(coldata_cols_types, get_width)
#' 
#'   #maximal width allotted
#'   nms_width_max <- floor(0.15 * width)
#'   # available space for names from col data
#'   coldata_nms_width <- nms_width_max - max(coldata_cols_types_w)
#'   # take minimal space
#'   coldata_nms_width <- min(coldata_nms_width, max(coldata_cols_types_w))
#'   # allot up to 15% of width for rownames/colData colnames
#'   row_nms_ <- map(rownames(x_), new_pillar_title)
#'   row_nms_w <- map_int(row_nms_, get_width)
#'   # minimal space for rownames titles
#'   row_nms_w_need <- min(nms_width_max, max(row_nms_w))
#'   row_nms_width_fin <- max(row_nms_w_need, coldata_nms_width + max(coldata_cols_types_w))
#' 
#'   #
#'   coldata_nms_width <- row_nms_width_fin - max(coldata_cols_types_w)
#'   coldata_title_formatted <- format_title(coldata_cols_titles, width = coldata_nms_width)
#'   coldata_ftnt <- attr(coldata_title_formatted, "has_footnote")
#'   coldata_title_formatted <- align(coldata_title_formatted,
#'                                    width = coldata_nms_width,
#'                                    align = "left")
#' 
#' 
#' 
#'   names_formatted <- c(
#'     paste0(coldata_title_formatted,
#'            align(map_chr(coldata_cols_types, format, width =  max(coldata_cols_types_w)),
#'                  align =  "right",
#'                  width =  max(coldata_cols_types_w))),
#'     align(c(".rows/.cols", "",
#'     format_title(row_nms_, width = row_nms_width_fin)),
#'     width = row_nms_width_fin,
#'     align = "left")
#'   )
#' 
#'   # start with assays and colnames
#'   assay_ <- assay(x_)
#'   col_nms <- colnames(x_)
#'   nc_sub <- ncol(x_)
#'   col_pillars <- vector('list', nc_sub)
#'   weave_seq_j_sub <- weave(seq_len(nc_sub))
#'   curr_width <- 0L
#'   curr_width_min <- 0L
#'   k <- 0L
#'   assay_width <- floor(0.5 * width) - row_nms_width_fin
#'   for (i in weave_seq_j_sub) {
#'     k <- k + 1L
#'     pill <- pillar(x = assay_[,i, drop = T], title = col_nms[i])
#'     curr_width <- curr_width + get_width(pill$data) + 1
#'     if (k==(n + 1L) || curr_width > assay_width) {
#'       break
#'     }
#'     col_pillars[[i]] <- pill
#'   }
#'   # we made it to the ith iteration
#'   seq_j <- seq_len(nc_sub)
#'   seq_j <- seq_j[!vapply(col_pillars, is.null, F)]
#'   col_ <- col_[seq_j,]
#'   col_pillars <- col_pillars[seq_j]
#'   col_pillars_w <- map_int(col_pillars, ~get_width(.x$data))
#' 
#'   #coldata shafts
#'   coldata_shafts <- map(col_,
#'                         ~map2(.x, col_pillars_w, ~pillar_shaft_width(.x, .y)))
#'   coldata_shafts <- map(coldata_shafts,
#'                         function(.y){
#'                           map_chr(.y,
#'                                   ~{
#'                                     width <- get_width(.x) - 1L
#'                                     align(
#'                                     format(.x, width = width),
#'                                     align = "left",
#'                                     width = width)})
#'                         })
#'   assay_titles <- map2(col_pillars, col_pillars_w, ~format_title(.x$title, .y))
#'   assay_title_ftnt <- map_int(assay_titles, attr, "has_footnote")
#'   assay_titles <- map_chr(assay_titles, ~.x)
#'   assay_shafts <- map2(col_pillars, col_pillars_w, ~format(.x$data[[1]], width = .y))
#'   assay_shafts <- map(
#'     seq_along(row_nms_),
#'     function(.x, shafts) {
#'       map_chr(shafts, `[[`, .x)
#'     }, shafts = map(assay_shafts, format)
#'   )
#'   rhs <- c(
#'     coldata_shafts,
#'     list(assay_titles),
#'     list(strrep(" ", times = col_pillars_w)),
#'     assay_shafts
#'   )
#'   rhs <- map2(names_formatted, rhs,
#'               ~c(.x, .y))
#' 
#'   row_ <- as_tibble(rowData(x_))
#'   row_col_nms <- colnames(row_)
#'   nc_sub <- ncol(row_)
#'   row_col_pillars <- vector('list', nc_sub)
#'   row_col_weave <- weave(seq_len(nc_sub))
#'   curr_width <- 0L
#'   curr_width_min <- 0L
#'   k <- 0L
#'   row_col_width <- floor(0.5 * width) 
#'   for (i in row_col_weave) {
#'     k <- k + 1L
#'     pill <- pillar(x = row_[[i]], title = row_col_nms[i])
#'     curr_width <- curr_width + get_width(pill$data) + 1
#'     if (k==(n + 1L) || curr_width > row_col_width) {
#'       break
#'     }
#'     row_col_pillars[[i]] <- pill
#'   }
#'   row_col_pillars <- Filter(Negate(is.null), row_col_pillars)
#'   row_col_w <- map_int(row_col_pillars, ~get_width(.x$data))
#'   row_col_titles <- map(row_col_pillars, `[[`, "title")
#'   row_col_titles <- map2(row_col_titles, row_col_w, ~format_title(.x, .y))
#'   row_col_ftnt <- map_int(row_col_titles, attr, "has_footnote")
#'   row_col_titles <- map_chr(row_col_titles, ~.x)
#'   row_col <- pmap(
#'     list(row_col_titles,
#'          map(row_col_pillars, `[[`, "type"),
#'          map(row_col_pillars, `[[`, "data"),
#'          row_col_w),
#'     function(.x, .y, .z, width) {
#'       c(align(.x, width = width), align(format(.y[[1]], width = width),
#'                   "left", width = width), 
#'         format(format(.z[[1]], width = width),width= width))
#'     }
#'   )
#'   rhs_n <- length(rhs)
#'   lhs <- vector('list', rhs_n)
#'   row_col2 <- map(
#'     seq_along(row_col[[1]]),
#'     function(.x, shafts) {
#'       map_chr(shafts, `[[`, .x)
#'     }, shafts = row_col
#'   )
#'   annotations <- c(
#'     sprintf("# A %s", class(x)),
#'     sprintf("# %s %s %s",
#'             format(dim(x)[1], big.mark=",", digits = 1, scientific = F),
#'             cli::symbol$times, 
#'             format(dim(x)[2], big.mark=",", digits = 1, scientific = F)),
#'     sprintf("# Assays=%s", paste(names(assays(x)), collapse = ", ")),
#'     if (!is.null(groups <- metadata(x)[["group_data"]])) {
#'       gv <- group_vars(x)
#'       c(
#'         if (!is_empty(gv$row_groups)) sprintf("# rows(%s)", paste(gv$row_groups, collapse = ", ")),
#'         if (!is_empty(gv$col_groups)) sprintf("# cols(%s)", paste(gv$col_groups, collapse = ", "))
#'       )
#'     }
#'   )
#'   lhs[] <- strrep(" ", sum(row_col_w) + length(row_col[[1]]))
#'   lhs[1:length(annotations)] <- map2(
#'     lhs[1:length(annotations)],
#'     annotations,
#'     function(into, anno) {
#'       substr(into, 1, nchar(anno)) <- anno
#'       cli::style_italic(cli::col_grey(into))
#'     }
#'   )
#'  
#'   
#'   lhs[(rhs_n-length(row_col[[1]]) + 1L):rhs_n] <- row_col2
#'   out <- map2(lhs, rhs, ~c(.x, .y))
#'   walk(out, ~{cat(.x);cat("\n")})
#'   invisible(x)
#' 
#' }
#' 
#' biocmask_annotation <- function(x, ...) {
#'   UseMethod("biocmask_annotation")
#' }
#' 
#' biocmask_annotation.SummarizedExperiment <- function(x, width) {
#'   out <- c(
#'     sprintf("# A %s", class(x)),
#'     sprintf("# %s %s %s",
#'             format(dim(x)[1], big.mark=",", digits = 1, scientific = F),
#'             cli::symbol$times, 
#'             format(dim(x)[2], big.mark=",", digits = 1, scientific = F)),
#'     sprintf("# Assays=%s", paste(names(assays(x)), collapse = ", ")),
#'     if (!is.null(groups <- metadata(x)[["group_data"]])) {
#'       gv <- group_vars(x)
#'       c(
#'         if (!is_empty(gv$row_groups)) sprintf("# rows(%s)", paste(gv$row_groups, collapse = ", ")),
#'         if (!is_empty(gv$col_groups)) sprintf("# cols(%s)", paste(gv$col_groups, collapse = ", "))
#'       )
#'     }
#'   )
#'   attr(out, "width") <- max(nchar(out))
#'   attr(out, "max_width") <- floor(0.5 * width)
#'   out
#' }
#' 
#' biocmask_prepare_coldata <- function(x, n_cols) {
#'   col_ <- as_tibble(colData(x))
#'   if (is_empty(col_)) return(NULL)
#'   n <- min(ncol(col_), n)
#'   col_ <- col_[1:n]
#'   
#'   imap(col_, ~pillar(.x, title = .y))
#'   
#' }
#' 
#' biocmask_prepare_assay <- function(x) {
#'   assay_ <- assay(x)
#'   if (is_empty(assay_)) return(NULL)
#'   map2(seq_len(ncol(assay_)),
#'        colnames(assay_),
#'       ~pillar(assay_[,.x, drop = TRUE], title = .y))
#' }
#' 
#' biocmask_prepare_rowdata <- function(x) {
#'   row_ <- as_tibble(rowData(x))
#'   if (is_empty(row_)) return(NULL)
#'   pillars <- imap(row_, ~pillar(.x, title = .y))
#'   tibble(
#'     pillars,
#'     id = seq_along(pillars),
#'     title_w = map_int(pillars, ~get_width(.x$title[[1]])),
#'     type_w = map_int(pillars, ~get_width(.x$type[[1]])),
#'     data_w = map_int(pillars, ~get_width(.x$data[[1]]))
#'   ) |>
#'     dplyr::mutate(w = median(dplyr::c_across(c(title_w, type_w, data_w))),.by = id) |>
#'     dplyr::select(-id)
#' }
#' 
#' biocmask_titles <- function(col_pillars, row_names) {
#'   
#'   col_titles <- map(col_pillars, ~.x$title[[1]])
#'   row_titles <- map(row_names, new_pillar_title)
#'   tibble(
#'     titles = c(col_titles, row_titles),
#'     title_w = map_int(titles, get_width),
#'     extra_w = {
#'       w <- integer(length(titles))
#'       w[seq_along(col_titles)] <- map_int(map(col_pillars, `[[`, "type"), get_width) + 1
#'       w
#'     },
#'     width = title_w + extra_w
#'   )
#' }
#' 
#' prep_col_assay <- function(col_pillars, assay_pillars) {
#'   col_widths <- map_int(col_pillars, ~get_width(.x$data))
#'   col_max_w <- max(col_widths)
#'   assay_titles <- map(assay_pillars, ~.x$title[[1]])
#'   assay_data_w <- map_int(assay_pillars, ~get_width(.x$data))
#'   assay_title_w <- map_int(assay_titles, get_width)
#'   tibble(
#'     titles = assay_titles,
#'     id = seq_along(assay_titles),
#'     title_w = assay_title_w,
#'     assay_w = assay_data_w,
#'     col_w = col_max_w,
#'     col_w_all = list(col_widths),
#'     col_w_m = median(col_widths)
#'   )  |>
#'     dplyr::mutate(w = median(dplyr::c_across(c(title_w, assay_w, col_w_m))),.by = id) |>
#'     dplyr::select(-id)
#'   
#' }
#' 
#' format_ <- function(x, n = 6, width = NULL) {
#'   width <- width %||% cli::console_width() %||% 80L
#'   if (width < 40L) {
#'     rlang::inform("setting width to 40L")
#'     width <- 40L
#'   }
#'   annotations <- biocmask_annotation(x, width = width)
#'   anno_n <- length(annotations)
#'   nr <- min(nrow(x), n)
#'   nc <- min(ncol(x), n)
#'   seq_i <- seq_len(nr)
#'   seq_j <- seq_len(nc)
#'   
#'   
#'   x_ <- x[seq_i, seq_j]
#'   col_pillars <- biocmask_prepare_coldata(x_, n)
#'   assay_pillars <- biocmask_prepare_assay(x_)
#'   row_ <- biocmask_prepare_rowdata(x_)
#'   
#'   lhs_titles <- biocmask_titles(col_pillars, rownames(x_))
#'   lhs_data <- prep_col_assay(col_pillars, assay_pillars)
#'   
#'   
#'   
#' }
