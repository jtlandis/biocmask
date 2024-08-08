

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
  # browser()
  top_n <- ceiling(n/2)
  bot_n <- floor(n/2)
  nr <- nrow(x)
  row_slice <- if (nr < 2 * n) {
    seq_len(nr)
  } else {
    c(1:n, (nr - n + 1):nr)
  }
  
  nc <- ncol(x)
  col_slice <- if (nc < 2 * n) {
    seq_len(nc)
  } else {
    c(1:n, (nc - n + 1):nc)
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
  # if (diff(min(top_half):max(bot_half)))
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
  setup$rows_total <- prod(dim(attr(x, "biocmask:::data")))
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

#' @export
tbl_format_footer.SE_abstraction <-function(x, setup, ...) {
  
  out <- NextMethod()
  c(
    style_subtle(sprintf("# %s n = %s",
                         cli::symbol$info,
                         format(setup$rows_total, big.mark = ",",
                                scientific = FALSE, digits = 1))),
    out
  )
}

#' @export
format.blank_pillar_type <- function(x, width, ...) {
  style_subtle("|")
}

#' @export
format.blank_pillar_title <- format.blank_pillar_type

