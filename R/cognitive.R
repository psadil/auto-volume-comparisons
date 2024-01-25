


sample_cog_cor <- function(ukb_vols_long, cognitive, N, i){
  
  cog_0 <- dplyr::slice_sample(cognitive, n=N) |>
    purrr::discard(~sum(is.na(.x))/length(.x) >= .5) 
  
  ukb_vols_long |>
    dplyr::filter(
      stringr::str_detect(Structure, "Amygdala"),
    ) |>
    dplyr::summarise(
      Volume = sum(Volume), 
      .by = c(f.eid, Structure, Method)) |>
    dplyr::right_join(cog_0, by = dplyr::join_by(f.eid)) |>
    dplyr::filter(!is.na(Structure)) |>
    dplyr::select(
      tidyselect::where(
        \(x) is_character(x) || isTRUE(sd(x, na.rm=TRUE) > 0))) |>
    tidyr::pivot_wider(
      names_from = c(Structure, Method), 
      values_from = "Volume") |>
    dplyr::rename(eid=f.eid) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::starts_with("f."), 
        \(x) list(cor.test(Amygdala_FIRST, x)),
        .names = "FIRST_{.col}"),
      dplyr::across(
        tidyselect::starts_with("f."), 
        \(x) list(cor.test(Amygdala_FreeSurfer, x)),
        .names = "FreeSurfer_{.col}"),
    ) |>
    tidyr::pivot_longer(
      tidyselect::everything(), 
      names_to = "predictor", 
      values_to = "fit") |>
    tidyr::separate(predictor, c("Method", "predictor"), sep = "_") |>
    dplyr::mutate(fit = purrr::map(fit, broom::tidy)) |>
    tidyr::unnest(fit) |>
    dplyr::select(Method, predictor, estimate, p.value, conf.low, conf.high) |>
    dplyr::mutate(N=N, i=i)
}


get_cog_cor_full_ukb <- function(cognitive, ukb_vols_long){
  ukb_vols_long |>
    dplyr::filter(
      stringr::str_detect(Structure, "Amygdala"),
    ) |>
    dplyr::summarise(
      Volume = sum(Volume), 
      .by = c(f.eid, Structure, Method)) |>
    dplyr::left_join(cognitive, by = dplyr::join_by(f.eid)) |>
    tidyr::pivot_wider(
      names_from = c(Structure, Method), 
      values_from = "Volume") |>
    purrr::discard(~sum(is.na(.x))/length(.x) >= .5) |>
    dplyr::rename(eid=f.eid) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::starts_with("f."), 
        \(x) list(cor.test(Amygdala_FIRST, x)),
        .names = "FIRST_{.col}"),
      dplyr::across(
        tidyselect::starts_with("f."), 
        \(x) list(cor.test(Amygdala_FreeSurfer, x)),
        .names = "FreeSurfer_{.col}"),
    ) |>
    tidyr::pivot_longer(
      tidyselect::everything(), names_to = "predictor", values_to = "fit") |>
    tidyr::separate(predictor, c("Method", "predictor"), sep = "_") |>
    dplyr::mutate(fit = purrr::map(fit, broom::tidy)) |>
    tidyr::unnest(fit) 
}

get_cog_dif_sig <- function(cog_cor, cog_cor_full_ukb){
  
  d0 <- cog_cor_full_ukb |>
    dplyr::summarize(
      estimate = mean(estimate), 
      .by=c(predictor))
  
  cog_cor |>
    dplyr::select(Method, predictor, value=p.value, N, i) |>
    tidyr::pivot_wider(names_from = Method) |>
    dplyr::mutate(
      different_significance_s = 
        sum((dplyr::between(FIRST, 0.025, 0.05) & FreeSurfer >= 0.05) | (dplyr::between(FreeSurfer, 0.025, 0.05) & FIRST >= 0.05)),
      different_significance_n =  sum(FIRST < 0.05 | FreeSurfer < 0.05),
      .by = c(predictor, N)) |>
    dplyr::mutate(
      ds_lower = qbeta(0.025, 1/2 + different_significance_s, 1/2 + different_significance_n),
      ds_upper = qbeta(0.975, 1/2 + different_significance_s, 1/2 + different_significance_n),
      ds_med = qbeta(0.5, 1/2 + different_significance_s, 1/2 + different_significance_n)) |>
    dplyr::left_join(d0, by=dplyr::join_by(predictor))
  
}

get_cog_dif_dir <- function(cog_cor, cog_cor_full_ukb){
  
  d0 <- cog_cor_full_ukb |>
    dplyr::summarize(estimate = mean(estimate), .by=c(predictor))
  
  cog_cor |>
    dplyr::mutate(
      both_sig = all(p.value < 0.05), .by=c(predictor, N, i)) |>
    dplyr::filter(both_sig) |>
    dplyr::select(Method, predictor, value=estimate, N, i) |>
    tidyr::pivot_wider(names_from = Method) |>
    dplyr::summarise(
      s = mean(sign(FIRST) != sign(FreeSurfer)),
      .by = c(predictor, N)
    ) |>
    dplyr::left_join(d0, by=dplyr::join_by(predictor)) |>
    dplyr::filter(abs(estimate) > 0.05) 
}
