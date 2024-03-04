
geom_text <- function(...) {
  ggtext::geom_richtext(
    ..., 
    fill=NA, 
    label.color=NA, 
    size=2)
}

ggally_text <- function (
    label, 
    mapping = ggplot2::aes(color = I("black")), 
    xP = 0.5, 
    yP = 0.5, 
    xrange = c(0, 1), 
    yrange = c(0, 1), 
    ...) {
  theme <- ggplot2::theme_get()
  p <- ggplot2::ggplot() + 
    ggplot2::xlim(xrange) + 
    ggplot2::ylim(yrange) + 
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(), 
      panel.grid.major = ggplot2::element_line(
        colour = GGally:::ifnull(
          theme$panel.background$fill, NA)
      ), 
      panel.background = ggplot2::element_rect(fill = GGally:::ifnull(theme$panel.grid.major$colour, NA))) + 
    labs(x = NULL, y = NULL)
  
  new_mapping <- ggplot2::aes(
    x = !!xP * diff(xrange) + min(xrange, na.rm = TRUE), 
    y = !!yP * diff(yrange) + min(yrange, na.rm = TRUE))
  if (is.null(mapping)) {
    mapping <- new_mapping
  }
  else {
    mapping <- GGally:::add_and_overwrite_aes(mapping, new_mapping)
  }
  if (!is.null(mapping$colour)) {
    p <- p + 
      geom_text(label = label, mapping = mapping, ...) + 
      ggplot2::guides(colour = "none")
  }
  else if ("colour" %in% names(aes(...))) {
    p <- p + geom_text(label = label, mapping = mapping, ...)
  }
  else {
    bg <- GGally:::ifnull(theme$panel.background$fill, "grey92")
    fg <- GGally:::ifnull(theme$axis.text$colour, "gray30")
    colour <- (scales::colour_ramp(c(bg, fg)))(0.75)
    p <- p + geom_text(label = label, mapping = mapping, colour = colour, ...)
  }
  p <- p + ggplot2::theme(legend.position = "none")
  p
}

ggally_statistic <- function(data, mapping, text_fn, title, na.rm = NA, display_grid = FALSE, 
                             justify_labels = "right", justify_text = "left", sep = ": ", 
                             family = "mono", title_args = list(), group_args = list(), 
                             align_percent = 0.5, title_hjust = 0.5, group_hjust = 0.5) {
  set_if_not_there <- function(obj, key, value) {
    obj <- as.list(obj)
    obj
  }
  title_args <- set_if_not_there(title_args, "hjust", title_hjust)
  group_args <- set_if_not_there(group_args, "hjust", group_hjust)
  xData <- GGally::eval_data_col(data, mapping$x)
  yData <- GGally::eval_data_col(data, mapping$y)
  colorData <- GGally::eval_data_col(data, mapping$colour)
  if (is.numeric(colorData)) {
    stop("`mapping` color column must be categorical, not numeric")
  }
  display_na_rm <- is.na(na.rm)
  if (display_na_rm) {
    na.rm <- TRUE
  }
  if (isTRUE(na.rm)) {
    if (!is.null(colorData) && (length(colorData) == length(xData))) {
      rows <- complete.cases(xData, yData, colorData)
    }
    else {
      rows <- complete.cases(xData, yData)
    }
    if (any(!rows)) {
      if (!is.null(colorData) && (length(colorData) == 
                                  length(xData))) {
        colorData <- colorData[rows]
      }
      xData <- xData[rows]
      yData <- yData[rows]
      if (isTRUE(display_na_rm)) {
        total <- sum(!rows)
        if (total > 1) {
          warning("Removed ", total, " rows containing missing values")
        }
        else if (total == 1) {
          warning("Removing 1 row that contained a missing value")
        }
      }
    }
  }
  xVal <- xData
  yVal <- yData
  for (mappingName in names(mapping)) {
    itemData <- GGally::eval_data_col(data, mapping[[mappingName]])
    if (!inherits(itemData, "AsIs")) {
      mapping[[mappingName]] <- NULL
    }
  }
  xValNum <- as.numeric(xVal)
  yValNum <- as.numeric(yVal)
  xmin <- min(xValNum, na.rm = TRUE)
  xmax <- max(xValNum, na.rm = TRUE)
  xrange <- c(xmin - 0.01 * (xmax - xmin), xmax + 0.01 * (xmax - 
                                                            xmin))
  ymin <- min(yValNum, na.rm = TRUE)
  ymax <- max(yValNum, na.rm = TRUE)
  yrange <- c(ymin - 0.01 * (ymax - ymin), ymax + 0.01 * (ymax - 
                                                            ymin))
  if (!is.null(colorData) && !inherits(colorData, "AsIs")) {
    cord <- plyr::ddply(data.frame(x = xData, y = yData, color = colorData), 
                        "color", function(dt) {
                          text_fn(dt$x, dt$y)
                        })
    colnames(cord)[2] <- "text"
    lev <- levels(as.factor(colorData))
    ord <- rep(-1, nrow(cord))
    for (i in 1:nrow(cord)) {
      for (j in seq_along(lev)) {
        if (identical(as.character(cord$color[i]), as.character(lev[j]))) {
          ord[i] <- j
        }
      }
    }
    cord <- cord[order(ord[ord >= 0]), ]
    cord$label <- stringr::str_c(format(cord$color, justify = justify_labels), 
                                 sep, format(cord$text, justify = justify_text))
    ggally_text_args <- append(list(label = stringr::str_c(title, 
                                                           sep, text_fn(xVal, yVal)), mapping = mapping, xP = 0.5, 
                                    yP = 0.9, xrange = xrange, yrange = yrange), title_args)
    p <- do.call(ggally_text, ggally_text_args)
    xPos <- rep(align_percent, nrow(cord)) * diff(xrange) + 
      min(xrange, na.rm = TRUE)
    yPos <- seq(from = 0.9, to = 0.2, length.out = nrow(cord) + 
                  1)
    yPos <- yPos * diff(yrange) + min(yrange, na.rm = TRUE)
    yPos <- yPos[-1]
    cordf <- data.frame(xPos = xPos, yPos = yPos, labelp = cord$label)
    cordf$labelp <- factor(cordf$labelp, levels = cordf$labelp)
    geom_text_args <- append(list(data = cordf, aes(x = !!as.name("xPos"), 
                                                    y = !!as.name("yPos"), label = !!as.name("labelp"), 
                                                    color = !!as.name("labelp"))), group_args)
    p <- p + do.call(geom_text, geom_text_args)
  }
  else {
    ggally_text_args <- append(list(label = paste0(title, 
                                                   sep, text_fn(xVal, yVal), collapse = ""), mapping, 
                                    xP = 0.5, yP = 0.5, xrange = xrange, yrange = yrange), 
                               title_args)
    p <- do.call(ggally_text, ggally_text_args)
  }
  if (!isTRUE(display_grid)) {
    p <- p + 
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank(), 
        panel.border = ggplot2::element_rect(
          linetype = "solid", 
          color = theme_get()$panel.background$fill,
          fill = "transparent"))
  }
  p + ggplot2::theme(legend.position = "none")
}

orth_smooth <- function(data, mapping, ...) {
  fit <- pracma::odregress(
    dplyr::pull(data, !!mapping$x), dplyr::pull(data, !!mapping$y)
  )
  
  grob <- grid::grobTree(
    grid::textGrob(
      glue::glue("y={round(fit$coef[2],1)}+{round(fit$coef[1],2)}x"),
      x = 0.05, y = 0.9, hjust = 0,
      gp = grid::gpar(fontsize = 6)
    )
  )
  # GGally::ggally_autopoint(data = data, mapping = mapping, ...) +
  ggplot(data, mapping) +
    scattermore::geom_scattermore(alpha=0.1, pointsize = 5) + 
    ggplot2::geom_abline(slope = fit$coef[1], intercept = fit$coef[2], linetype = "dashed") +
    ggplot2::geom_abline() +
    ggplot2::annotation_custom(grob)
}

ggally_icc <- function(data, mapping, ...) {
  # ggplot2::update_geom_defaults("text", list(size = 2))
  text_fn <- function(x, y) {
    co <- cor.test(x, y)
    iccC <- irr::icc(cbind(x, y), model = "twoway", type = "c")
    iccA <- irr::icc(cbind(x, y), model = "twoway", type = "a")
    tmp <- glue::glue(
      "cor: <sub>{co.lbound}</sub>{co.value}<sub>{co.ubound}</sub><br><br>{iccC$icc.name}: <sub>{iccC.lbound}</sub>{iccC.value}<sub>{iccC.ubound}</sub><br><br>{iccA$icc.name}: <sub>{iccA.lbound}</sub>{iccA.value}<sub>{iccA.ubound}</sub>",
      co.value = round(co$estimate, digits=2),
      co.lbound = round(co$conf.int[1], digits=2),
      co.ubound = round(co$conf.int[2], digits=2),
      iccC.value = round(iccC$value, digits=2),
      iccC.lbound = round(iccC$lbound, digits=2),
      iccC.ubound = round(iccC$ubound, digits=2),
      iccA.value = round(iccA$value, digits=2),
      iccA.lbound = round(iccA$lbound, digits=2),
      iccA.ubound = round(iccA$ubound, digits=2)) 
  }
  ggally_statistic(
    data,
    mapping,
    title = "",
    sep = "",
    text_fn = text_fn,
    ...
  )
}

plot_struct <- function(volumes, StructName, base_size = 6) {
  volumes |> 
    dplyr::select(tidyselect::contains(.env$StructName)) |>
    dplyr::rename_with(
      .fn = ~ stringr::str_remove(.x, glue::glue("_{StructName}"))
    ) |>
    dplyr::rename_with(
      .fn = ~ stringr::str_replace(.x, "_", "\n")
    ) |>
    dplyr::rename_with(
      .fn = ~ stringr::str_replace(.x, "FIRST", "FSL")
    ) |>
    GGally::ggpairs(
      upper = list(
        continuous = GGally::wrap(ggally_icc)
      ),
      lower = list(
        continuous = GGally::wrap(orth_smooth, alpha = 0.25)
        # continuous = "smooth",
      ),
      diag = list(continuous = "barDiag"),
      progress = FALSE
    ) +
    ggplot2::ggtitle(StructName) +
    ggplot2::theme_gray(base_size = base_size)
}

subscript_formatting <- function(x){
  stringr::str_replace_all(
    x,
    pattern = "@@",
    replacement = "$_{") |>
    stringr::str_replace_all(
      "@",
      "}$") 
}


make_spaghetti <- function(cog_dif_sig, cog_dif_dir, hemisphere) {
  dplyr::left_join(
    cog_dif_sig |>
      dplyr::select(predictor, N, ds_med, Hemisphere, ds_lower, ds_upper),
    cog_dif_dir |>
      dplyr::mutate(estimate = abs(estimate)),
    by = dplyr::join_by(predictor, N, Hemisphere)) |>
    dplyr::filter(stringr::str_detect(Hemisphere, hemisphere)) |>
    na.omit() |>
    tidyr::pivot_longer(
      c(tidyselect::starts_with("dd"), tidyselect::starts_with("ds")), 
      names_to = c("Effect", "level"), 
      names_sep = "_",
      values_to = "value") |>
    dplyr::mutate(
      Effect = dplyr::case_match(
        Effect, 
        "dd" ~ "Different\nDirection",
        "ds" ~ "Different\nSignificance"),
      Effect = factor(
        Effect, 
        levels = c("Different\nSignificance", "Different\nDirection")),
      predictor = stringr::str_sub(predictor, 3),
      rho = glue::glue("({round(estimate, digits = 2)})"),
      Variable = interaction(predictor, rho, sep = " ", lex.order=FALSE),
      Variable = forcats::fct_rev(Variable)) |>
    tidyr::pivot_wider(names_from = level) |>
    ggplot2::ggplot(ggplot2::aes(x=N, y=med, linetype=Effect)) +
    ggplot2::facet_wrap(~Variable, nrow = 6) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin=lower, ymax=upper), alpha = 0.5, color="black") +
    ggplot2::scale_color_viridis_c(option="inferno", limits = c(0, 0.2)) +
    ggplot2::scale_x_continuous(
      "N Participants",
      breaks = c(0, 50, 100),
      labels = c(0, 50, 100)) +
    ggplot2::ggtitle(glue::glue("UKB {hemisphere}")) +
    ggplot2::theme_gray(base_size = 8)
}

make_vol_comparison <- function(ukb_vols){
  a <- ukb_vols |>
    plot_struct("Hippocampus", base_size = 8) |>
    GGally::ggmatrix_gtable() |>
    patchwork::wrap_elements()
  b <- ukb_vols |>
    plot_struct("Amygdala", base_size = 8) |>
    GGally::ggmatrix_gtable() |>
    patchwork::wrap_elements()
  
  a + b + 
    patchwork::plot_layout(ncol = 1) + 
    patchwork::plot_annotation(tag_levels = "a", tag_suffix = ")") & 
    ggplot2::theme(legend.position = "bottom")
}

make_biases_perc <- function(ukb_vols_long) {
  
  a <- ukb_vols_long |>
    tidyr::pivot_wider(names_from = Method, values_from = Volume) |>
    dplyr::mutate(
      .average = (FIRST + FreeSurfer) / 2,
      .difference = (FIRST - FreeSurfer) 
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .average, y = .difference)) +
    ggplot2::facet_wrap(~Structure, scales = "free") +
    scattermore::geom_scattermore(alpha=0.1, pointsize = 3) +
    ggplot2::stat_density2d(
      ggplot2::aes(
        alpha = ggplot2::after_stat(ndensity),
        fill = ggplot2::after_stat(ndensity)),
      geom = "raster",
      contour = FALSE) +
    agree::geom_ba() +
    ggplot2::scale_fill_viridis_c(
      option = "inferno", 
      name="Normalized Density") +
    ggplot2::scale_alpha(range = c(0,1), guide = "none") +
    ggplot2::xlab(
      expression("Average (cm$^3$) (FSL + FreeSurfer)/2")) +
    ggplot2::ylab(
      "Difference (cm$^3$)\n(FSL - FreeSurfer)") +
    ggplot2::theme(
      legend.direction = "horizontal", 
      legend.position = "inside", 
      legend.justification = "right", 
      legend.justification.inside = c(1,0), 
      legend.title.position = "top", 
      legend.key.width = unit(.35, "inch"))
  
  b <- ukb_vols_long |>
    tidyr::pivot_wider(names_from = Method, values_from = Volume) |>
    dplyr::mutate(
      .average = (FIRST + FreeSurfer) / 2,
      .difference = (FIRST - FreeSurfer) /.average * 100
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .average, y = .difference)) +
    scattermore::geom_scattermore(alpha=0.1, pointsize = 3) +
    ggplot2::stat_density2d(
      ggplot2::aes(
        alpha = ggplot2::after_stat(ndensity),
        fill = ggplot2::after_stat(ndensity)),
      geom = "raster",
      contour = FALSE) +
    agree::geom_ba() +
    ggplot2::scale_fill_viridis_c(option = "inferno", name="Normalized\nDensity") +
    ggplot2::scale_alpha(range = c(0,1)) +
    ggplot2::xlab(
      "Average (cm$^3$) (FSL + FreeSurfer)/2") +
    ggplot2::ylab(
      "Difference (\\%)\n(FSL - FreeSurfer)/Average") +
    ggplot2::theme(legend.position = "none")
  
  a + b + 
    patchwork::plot_layout(nrow = 2, heights = c(3,2)) + 
    patchwork::plot_annotation(tag_levels = "a", tag_suffix = ")")
}

make_why_flat <- function(icc_data){
  icc_data |> 
    dplyr::filter(
      icc %in% c(0.1, 0.8), 
      rho == 0.1, 
      N < 300, 
      !(N/10)%%2) |> 
    dplyr::mutate(
      p_x1 = p_x1 < 0.05, 
      p_x2 = p_x2 < 0.05, 
      Significant = dplyr::case_when(
        p_x1 & p_x2 ~ "Both",
        p_x1 & !p_x2 ~ "One",
        !p_x1 & p_x2 ~ "One",
        !p_x1 & !p_x2 ~ "Neither",
      ),
      Significant = factor(
        Significant, 
        levels = c("Neither", "One", "Both"))) |>
    dplyr::rename(ICC=icc) |>
    ggplot2::ggplot(
      ggplot2::aes(x=r_x1, y=r_x2, color=Significant)) + 
    ggplot2::facet_grid(N~ICC, labeller= "label_both") + 
    scattermore::geom_scattermore() +
    ggplot2::xlab("First Correlation") +
    ggplot2::ylab("Second Correlation") +
    ggplot2::scale_color_viridis_d(option = "inferno", end = 0.8)
}
