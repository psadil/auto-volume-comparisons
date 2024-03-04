

.get_one_cog_cor <- function(ukb_vols, N){
  ukb_vols |>
    dplyr::slice_sample(n=N) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::starts_with("f."), 
        \(x) list(cor.test(FIRST, x, method="spear")),
        .names = "FIRST_{.col}"),
      dplyr::across(
        tidyselect::starts_with("f."), 
        \(x) list(cor.test(FreeSurfer, x, method="spear")),
        .names = "FreeSurfer_{.col}"),
    )
}

get_amyg_vols <- function(
    ukb_vols_long, 
    cognitive, 
    cog_cor_full_ukb, 
    predictor,
    max_predictors=100){
  
  cog_cor_max_ukb <- cog_cor_full_ukb |>
    dplyr::filter(stringr::str_detect(predictor, "f\\.[[:digit:]]+\\.[2].*")) |>
    dplyr::slice_max(abs(estimate), n=max_predictors) |>
    dplyr::distinct(predictor)
  
  cog_keep <- cognitive |>
    dplyr::semi_join(ukb_vols_long, by = dplyr::join_by(f.eid)) |>
    dplyr::select(f.eid, tidyselect::any_of(unique(cog_cor_max_ukb$predictor))) |>
    na.omit() |>
    purrr::discard(~sum(is.na(.x))/length(.x) >= .5) 
  
  hemisphere <- unique(cog_cor_full_ukb$Hemisphere)
  
  ukb_vols_long |>
    dplyr::filter(
      stringr::str_detect(Structure, "Amygdala"),
      stringr::str_detect(Hemisphere, hemisphere)) |>
    dplyr::select(f.eid, Structure, Hemisphere, Method, Volume) |>
    tidyr::pivot_wider(names_from = Method, values_from = Volume) |>
    dplyr::right_join(cog_keep[,c(1, predictor)], by = dplyr::join_by(f.eid)) |>
    dplyr::select(-Structure)
  
}

.cor_with <- function(data, col){
  x <- data[[col]]
  y <- subset(data, select = -c(FreeSurfer, FIRST))
  # y <- dplyr::select(data, -FreeSurfer, -FIRST)
  
  suppressWarnings({
    lapply(y, cor.test, x=x)
  })
}

sample_cog_cor <- function(amyg_vols, N, i){
  # n_row <- nrow(amyg_vols) 
  amyg <- polars::as_polars_lf(amyg_vols)
  
  amyg$select(
    row=polars::pl$struct(polars::pl$all())$sample(n=N*i, with_replacement=TRUE)
  )$with_columns(i=polars::pl$lit(1:i)$rep(N))$unnest(
  )$melt(
    id_vars = c("i", "f.eid", "FIRST", "FreeSurfer", "Hemisphere"),
    variable_name="predictor"
  )$group_by(
    "i", "predictor", "Hemisphere"
  )$agg(
    polars::pl$corr("FreeSurfer", "value", method = "spearman")$alias("FreeSurfer"),
    polars::pl$corr("FIRST", "value", method = "spearman")$alias("FIRST")
  )$with_columns(
    N = polars::pl$lit(as.integer(N))
  )$melt(
    id_vars = c("predictor", "N", "i", "Hemisphere"),
    variable_name = "Method",
    value_name = "estimate"
  )$collect()$to_data_frame() |>
    tibble::as_tibble() |>
    dplyr::mutate(p.value = .corr.p(r=estimate, n=.data$N))
  
  # polars::pl$concat(
  #   purrr::map(seq_len(i), ~amyg$sample(n=N)$with_columns(i=polars::pl$lit(.x)))
  # )$melt(
  #   id_vars = c("i", "f.eid", "FIRST", "FreeSurfer"),
  #   variable_name="predictor"
  # )$group_by(
  #   "i", "predictor"
  # )$agg(
  #   polars::pl$corr("FreeSurfer", "value", method = "spearman")$alias("FreeSurfer"),
  #   polars::pl$corr("FIRST", "value", method = "spearman")$alias("FIRST")
  #   )$with_columns(N = polars::pl$lit(as.integer(N)))$to_data_frame() |>
  #   tibble::as_tibble()
  
  
  # f <- dplyr::select(amyg_vols, tidyselect::starts_with("f."))
  # first <- amyg_vols$FIRST
  # freesurfer <- amyg_vols$FreeSurfer
  
  
  # d <- tidyr::crossing(N=N, i=seq_len(i)) |>
  #   dplyr::mutate(data = purrr::map(N, ~amyg_vols[sample.int(n_row, size=.x),])) |>
  #   tidyr::unnest(data) |>
  #   tidyr::pivot_longer(tidyselect::starts_with("f."), names_to = "predictor") |>
  #   polars::as_polars_df()
  
  
  # tmp <- bench::mark(
  #   tidyr::crossing(N=N, i=seq_len(i)) |>
  #     dplyr::mutate(data = purrr::map(N, ~amyg[sample.int(n_row, size=.x),])) |>
  #     tidyr::unnest(data),
  #   tidyr::crossing(N=N, i=seq_len(i)) |>
  #     dplyr::mutate(data = purrr::map(N, ~amyg_vols[sample.int(n_row, size=.x),])) |>
  #     tidyr::unnest(data),
  #   check = FALSE,
  #   iterations = 2,
  #   filter_gc = FALSE
  # )
  
  
  # bench::mark(
  #   pl$concat(
  #     purrr::map(seq_len(i), ~amyg$sample(n=N)$with_columns(i=pl$lit(.x)))
  #   )$melt(
  #     id_vars = c("i", "FIRST", "FreeSurfer"),
  #     variable_name="predictor"
  #   )$group_by(
  #     "i", "predictor"
  #   )$agg(
  #     pl$corr("value", "FreeSurfer")$name$prefix("FreeSurfer"),
  #     pl$corr("value", "FIRST")$name$prefix("FIRST")),
  #   purrr::map(seq_len(i), ~amyg_vols[sample.int(n_row, size=N),]) |>
  #   dplyr::bind_rows(.id = "i") |>
  #     tidyr::pivot_longer(tidyselect::starts_with("f."), names_to = "predictor") |>
  #     dplyr::summarise(
  #       FIRST=cor(value, FIRST),
  #       FreeSurfer=cor(value, FreeSurfer),
  #       .by = c(i, predictor)
  #     ),
  #   tidyr::crossing(N=N, i=seq_len(i)) |>
  #     dplyr::mutate(data = purrr::map(N, ~amyg_vols[sample.int(n_row, size=.x),])) |>
  #     tidyr::unnest(data) |>
  #     dplyr::summarise(
  #       dplyr::across(
  #         tidyselect::starts_with("f."),
  #         list(
  #           FIRST=\(x) list(cor(x, FIRST)),
  #           FreeSurfer=\(x) list(cor(x, FreeSurfer)))),
  #       .by = c(N, i)
  #     ),
  #   check = FALSE,
  #   iterations = 2,
  #   filter_gc = FALSE
  # )
  # 
  # tidyr::crossing(N=N, i=seq_len(i)) |>
  #   dplyr::mutate(data = purrr::map(N, ~amyg_vols[sample.int(n_row, size=.x),])) |>
  #   tidyr::unnest(data) |>
  #   dplyr::summarise(
  #     dplyr::across(
  #       tidyselect::starts_with("f."),
  #       list(
  #         FIRST=\(x) list(cor.test(x, FIRST)),
  #         FreeSurfer=\(x) list(cor.test(x, FreeSurfer)))),
  #     .by = c(N, i)
  #   ) |>
  #   tidyr::pivot_longer(
  #     tidyselect::starts_with("f."),
  #     values_to = "fit",
  #     names_to = c("predictor", "Method"),
  #     names_sep = "_"
  #   ) |>
  #   dplyr::mutate(
  #     p.value = purrr::map_dbl(fit, ~.x$p.value),
  #     estimate = purrr::map_dbl(fit, ~.x$estimate)) |>
  #   dplyr::select(-fit) 
  
  # tidyr::crossing(Method = c("FIRST", "FreeSurfer")) |>
  # dplyr::mutate(
  #   fit = purrr::map2(data, Method, ~.cor_with(.x, .y))) |>
  # dplyr::select(-data) |>
  # dplyr::mutate(fit = purrr::map(
  #   fit,
  #   ~tibble::enframe(.x, name = "predictor", value = "fit"))) |>
  # tidyr::unnest(fit) |>
  # dplyr::mutate(
  #   p.value = purrr::map_dbl(fit, ~.x$p.value),
  #   estimate = purrr::map_dbl(fit, ~.x$estimate)) |>
  # dplyr::select(-fit)
}


get_cog_cor_full_ukb <- function(cognitive, ukb_vols_long, hemisphere){
  checkmate::assert_choice(hemisphere, c("Left", "Right"))
  ukb_vols_long |>
    dplyr::filter(
      stringr::str_detect(Structure, "Amygdala"),
      stringr::str_detect(Hemisphere, hemisphere),
    ) |>
    dplyr::summarize(
      Volume = mean(Volume),
      .by = c(f.eid, Hemisphere, Structure)) |>
    dplyr::left_join(cognitive, by = dplyr::join_by(f.eid)) |>
    tidyr::pivot_wider(
      names_from = c(Structure), 
      values_from = "Volume") |>
    purrr::discard(~sum(is.na(.x))/length(.x) >= .5) |>
    dplyr::rename(eid=f.eid) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::starts_with("f."), 
        \(x) list(cor.test(Amygdala, x, method = "spear"))),
      .by = Hemisphere
    ) |>
    tidyr::pivot_longer(
      -Hemisphere, names_to = "predictor", values_to = "fit") |>
    tidyr::separate(predictor, c("predictor"), sep = "_") |>
    dplyr::mutate(fit = purrr::map(fit, broom::tidy)) |>
    tidyr::unnest(fit) 
}

get_cog_dif_sig <- function(cog_cor, cog_cor_full_ukb){
  
  d0 <- cog_cor_full_ukb |>
    dplyr::select(predictor, estimate, Hemisphere)
  
  cog_cor |>
    na.omit() |> # occasionally sample datasets with zero sd
    dplyr::select(Method, predictor, value=p.value, N, i, Hemisphere) |>
    tidyr::pivot_wider(names_from = Method) |>
    dplyr::summarise(
      different_significance_s = 
        sum(
          (FIRST < 0.05 & FreeSurfer >= 0.05) | 
            (FreeSurfer < 0.05 & FIRST >= 0.05), 
          na.rm = TRUE),
      different_significance_n =  sum(FIRST < 0.05 | FreeSurfer < 0.05, na.rm = TRUE),
      .by = c(predictor, N, Hemisphere)) |>
    dplyr::mutate(
      a = 1/2 + different_significance_s,
      b = 1/2 + different_significance_n - different_significance_s,
      ds_med = qbeta(0.5, a, b),
      ds_lower = qbeta(0.025, a, b),
      ds_upper = qbeta(0.975, a, b),
      ds_avg = different_significance_s / different_significance_n) |>
    dplyr::left_join(d0, by=dplyr::join_by(predictor, Hemisphere))
  
}

get_cog_dif_dir <- function(cog_cor, cog_cor_full_ukb){
  
  d0 <- cog_cor_full_ukb |>
    dplyr::select(predictor, estimate, Hemisphere)
  
  cog_cor |>
    na.omit() |> # occasionally sample datasets with zero sd
    dplyr::select(Method, predictor, estimate, p.value, N, i, Hemisphere) |>
    tidyr::pivot_wider(names_from = Method, values_from = c(estimate, p.value)) |>
    dplyr::mutate(same_sign = sign(estimate_FIRST) == sign(estimate_FreeSurfer)) |>
    dplyr::select(-tidyselect::starts_with("estimate")) |>
    dplyr::summarise(
      different_direction_s = sum(!same_sign[p.value_FreeSurfer < 0.05 & p.value_FIRST < 0.05], na.rm = TRUE),
      different_direction_n = sum(p.value_FreeSurfer < 0.05 & p.value_FIRST < 0.05, na.rm = TRUE),
      .by = c(predictor, N, Hemisphere)
    ) |>
    dplyr::mutate(
      a = 1/2 + different_direction_s,
      b = 1/2 + different_direction_n - different_direction_s,
      dd_med = qbeta(0.5, a, b),
      dd_lower = qbeta(0.025, a, b),
      dd_upper = qbeta(0.975, a, b),
      dd_avg = different_direction_s / different_direction_n) |>
    dplyr::select(predictor, N, tidyselect::starts_with("dd"), different_direction_n, Hemisphere) |>
    dplyr::left_join(d0, by=dplyr::join_by(predictor, Hemisphere))
}


.corr.p <- function(r, n){
  t <- (r * sqrt(n - 2))/sqrt(1 - r^2)
  p <- -2 * expm1(pt(abs(t), (n - 2), log.p = TRUE))
  p[p > 1] <- 1
  p
}

.fit_one_amyg_vols_cors <- function(split){
  d <- rsample::assessment(split) |> 
    polars::as_polars_lf()
  
  d$melt(
    id_vars = c("f.eid", "Method", "Volume"),
    variable_name="predictor"
  )$group_by(
    c("Method", "predictor")
  )$agg(
    polars::pl$corr("Volume", "value", method = "spearman")$alias("estimate")
  )$collect()$to_data_frame() |>
    tibble::as_tibble()
}

bootstrap_amyg_vols_cors <- function(amyg_vols, cog_dif_dir, hemisphere, batch, times = 5) {
  
  hemi_cog_dif_dir <- cog_dif_dir |>
    dplyr::filter(Hemisphere == hemisphere)
  
  predictors <- hemi_cog_dif_dir |>
    dplyr::distinct(predictor) |>
    dplyr::filter(predictor %in% names(amyg_vols)) |>
    purrr::pluck("predictor") 
  
  lf <- amyg_vols |> 
    dplyr::filter(Hemisphere == hemisphere) |>
    dplyr::select(f.eid, FIRST, FreeSurfer, tidyselect::all_of(predictors)) |>
    polars::as_polars_lf()
  
  N <- nrow(amyg_vols)
  
  lf$select(
    row=polars::pl$struct(polars::pl$all())$sample(n=times*N, with_replacement=TRUE)
  )$with_columns(
    i=polars::pl$lit(1:times)$rep(N)
  )$unnest(
  )$melt(
    id_vars = c("f.eid", "i", predictors),
    variable_name = "Method",
    value_name = "Volume"
  )$melt(
    id_vars = c("f.eid", "Method", "Volume", "i"),
    variable_name="predictor"
  )$group_by(
    c("Method", "predictor", "i")
  )$agg(
    polars::pl$corr("Volume", "value", method = "spearman")$alias("estimate")
  )$collect()$to_data_frame() |>
    tibble::as_tibble() |>
    dplyr::mutate(
      batch = .env$batch,
      Hemisphere = unique(amyg_vols$Hemisphere))
  
  
  # rsample::bootstraps(times = times, apparent = TRUE) |>
  # mutate(fit = purrr::map(splits, .fit_one_amyg_vols_cors)) |>
  # dplyr::select(-splits) |>
  # tidyr::unnest(fit) |>
  # dplyr::left_join(
  #   dplyr::distinct(hemi_cog_dif_dir, predictor, rho=estimate),
  #   by=dplyr::join_by(predictor))
}
