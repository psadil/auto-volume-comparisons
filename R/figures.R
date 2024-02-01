


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
    ggplot2::geom_abline(slope = fit$coef[1], intercept = fit$coef[2]) +
    ggplot2::geom_abline(linetype = "dashed") +
    ggplot2::annotation_custom(grob)
}

ggally_icc <- function(data, mapping, ...) {
  ggplot2::update_geom_defaults("text", list(size = 2))
  text_fn <- function(x, y) {
    co <- cor.test(x, y)
    iccC <- irr::icc(cbind(x, y), model = "twoway", type = "c")
    iccA <- irr::icc(cbind(x, y), model = "twoway", type = "a")
    glue::glue(
      "cor: {co.value} ({co.lbound}, {co.ubound}) \n{iccC$icc.name}: {iccC.value}({iccC.lbound}, {iccC.ubound})\n{iccA$icc.name}: {iccA.value}({iccA.lbound}, {iccA.ubound})",
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
  GGally::ggally_statistic(
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
