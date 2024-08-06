

weave <- function(x) {
  n_ <- length(x)
  ind <- seq_along(x)
  out <- integer(n_)
  mid_point <- ceiling(n_/2)
  is_even <- ind %%2 == 0
  out[!is_even] <- 1:mid_point
  out[is_even] <- n_:(mid_point + 1L)
  out
}

col_spacing <- function(x, width, threshold) {
  sum(cumsum(x)/width < threshold)
}

#' @export
print.SummarizedExperiment <- function(x, width = NULL) {
  width <- width %||% cli::console_width() %||% 80L
  block_height <- min(nrow(x), 10L)
  # if
  if (nrow(x) <= 15L ) {
    block_height <- nrow(x)
  }
  
  # get the names
  row_nms <- rownames(x) %||% as.character(seq_len(nrow(x)))
  col_nms <- colnames(x) %||% as.character(seq_len(ncol(x)))
  
  # start with assays
  # see how many columns of assays we can display
  # reserve 50% AT MOST for this data
  
  # check how much space row_nms will require for the block_height
  # this can only take AT MOST 10% space
  row_weave_i <- sort(weave(row_nms)[1:block_height])
  row_nms_n <- nchar(row_nms[row_weave_i]) + 1
  row_width_p <- row_nms_n/width
  rows_width <- min(max(row_width_p), .1)
  
  # still reserve 50% of horizontal space for col_names
  # we have 50% - rows_width left (at least 40%)
  weave_cols_i <- weave(col_nms)
  cols_widths <- nchar(col_nms) + 1
  # number of columns that fit spacing requirements based on names (not data yet)
  ncols_ <- col_spacing(cols_widths[weave_cols_i], width, threshold = .5 - rows_width)
  col_ <- colData(x)
  col_col_disp <- sort(weave(col_)[1:min(block_height, ncol(col_))])
  col_select <- sort(weave_cols_i[1:ncols_])
  col_ <- col_[col_select, col_col_disp]
  
  assay_ <- assay(x)[row_weave_i, col_select]
  
  # now find how many columns of the rows we can fit with 45% resereved space
  row_ <- rowData(x)
  row_col_weave_i <- weave(row_)
  row_col_widths <- nchar(colnames(row_)) + 1
  # maybe compute if we have extra room? probably rare
  ncols_row_ <- col_spacing(row_col_widths[row_col_weave_i], width, threshold = .45)
  row_col_select <- sort(row_col_weave_i[1:ncols_row_])
  row_ <- row_[row_weave_i, row_col_select]
  
  
}

format.SummarizedExperiment <- function(x, n = 10, width = NULL) {
  width <- width %||% cli::console_width() %||% 80L
  block_height <- min(nrow(x), n)
  # if
  if (nrow(x) <= n * 1.5 ) {
    block_height <- nrow(x)
  }
  nr <- nrow(x)
  nc <- ncol(x)
  weave_seq_i <- weave(seq_len(nr))
  weave_i <- sort(weave_seq_i[1:n])
  weave_seq_j <- weave(seq_len(nc))
  weave_j <- sort(weave_seq_j[1:n])
  x_ <- x[weave_i, weave_j]
  
  #format the columns of colData
  col_ <- as_tibble(colData(x_))[0,]
  coldata_cols_n <- min(ncol(col_), block_height)
  if (ncol(col_) <= n * 1.5) {
    coldata_cols_n <- nrow(col_)
  }
  coldata_weave <- sort(weave(col_)[1:coldata_cols_n])
  coldata_pillars <- purrr::imap(col_[coldata_weave],
                                 ~pillar(x = .x, title = .y))
  
  # minimal space for abbreviation
  abbr_w <- max(purrr::map_int(coldata_pillars, ~attr(.x$type, "min_width")))
  abbr <- format(purrr::map_chr(coldata_pillars, 
                                ~cli::col_grey(cli::style_italic(sprintf("<%s>",
                                                                         .x$type[[1]][[1]])))),
                 justify = "right")
  #maximal width allotted
  row_nms_width_max <- floor(0.15 * width)
  # available space for names from col data
  row_nms_width <- row_nms_width_max - abbr_w
  # 
  coldata_nms_pillar <- pillar(colnames(col_[coldata_weave]))
  # minimal spacing for coldata colnames and abbrv
  coldata_nms_pillar_w <- min(attr(coldata_nms_pillar$data, "width"), row_nms_width)
  
  # allot up to 15% of width for rownames/colData colnames
  row_nms_ <- pillar(rownames(x_))
  row_nms_width_ <- min(attr(row_nms_$data, "width"), row_nms_width_max)
  
  row_nms_width_fin <- max(row_nms_width_, coldata_nms_pillar_w)
  
  names_formatted <- c(
    paste0(format(pillar(colnames(col_[coldata_weave])),
                  width = row_nms_width_fin - abbr_w)[-1],
           abbr),
    format(c(".rows/.cols",
    "",
    format(row_nms_, width = row_nms_width_fin)[-1]), 
    justify = "left")
  )
  
  # start with assays and colnames
  assay_ <- assay(x_)
  col_nms <- colnames(x_)
  nc_sub <- ncol(x_)
  col_pillars <- vector('list', nc_sub)
  weave_seq_j_sub <- weave(seq_len(nc_sub))
  curr_width <- 0L
  curr_width_min <- 0L
  k <- 0L
  assay_width <- floor(0.5 * width) - row_nms_width
  for (i in weave_seq_j_sub) {
    k <- k + 1L
    pill <- pillar(x = assay_[,i, drop = T], title = col_nms[i])
    curr_width <- curr_width + attr(pill$data, "width")
    curr_width_min <- curr_width_min + attr(pill$data, "min_width")
    if (k==(n + 1L) || curr_width_min > assay_width) {
      break
    }
    col_pillars[[i]] <- pill
  }
  # we made it to the ith iteration
  seq_j <- seq_len(nc)
  seq_j <- seq_j[!vapply(col_pillars, is.null, F)]
  col_pillars <- col_pillars[seq_j]
  
  
  pillar(c(vapply(coldata_pillars, function(x) x$title[[1]][[1]], FUN.VALUE = ""), c(".rows/.cols"),
    row_nms_$data[[1]][[1]]))
  
  
  
  
}