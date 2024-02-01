
get_partials <- function(ukb_vols_long){
  
  # ukb_vols_long |>
  #   dplyr::filter(
  #     stringr::str_detect(Method, "FSSUBSEG", TRUE)) |>
  #   tidyr::pivot_wider(names_from = Method, values_from = Volume) |>
  #   dplyr::mutate(
  #     .difference = FIRST - FreeSurfer,
  #     .average = (FIRST + FreeSurfer) / 2,
  #     acq_day=(acq_day - min(acq_day)) |> as.numeric()) |>
  #   na.omit() |>
  #   dplyr::mutate(
  #     age2 = age^2,
  #     acq_day2 = acq_day^2,
  #     dplyr::across(
  #       c(age, age2, t1_motion, head_size, acq_day, acq_day2), 
  #       \(x) (x - median(x)) / mad(x)),
  #     .by = c(site, Hemisphere)) |>
  #   dplyr::group_nest(Hemisphere, Structure) |>
  #   dplyr::mutate(
  #     diff = purrr::map(
  #       data,
  #       ~LMMstar::lmm(
  #         .difference ~ site + (age*sex):site + age2:site + t1_motion:site + head_size:site + acq_day:site + acq_day2:site,
  #         data = .x) |> 
  #         LMMstar::partialCor(df=FALSE) |>
  #         tibble::as_tibble(rownames = "term")
  #     ),
  #     avg = purrr::map(
  #       data,
  #       ~LMMstar::lmm(
  #         .average ~ site + (age*sex):site + age2:site + t1_motion:site + head_size:site + acq_day:site + acq_day2:site,
  #         data = .x) |> 
  #         LMMstar::partialCor(df=FALSE) |>
  #         tibble::as_tibble(rownames = "term")
  #     )
  #   ) |>
  #   dplyr::select(-data) |>
  #   tidyr::pivot_longer(
  #     c(tidyselect::ends_with("diff"), tidyselect::ends_with("avg")), 
  #     names_to = "response") |>
  #   tidyr::unnest(value) 
  
  
  tmp <- ukb_vols_long |>
    dplyr::filter(Structure=="Amygdala") |>
    tidyr::pivot_wider(names_from = Method, values_from = Volume) |>
    dplyr::mutate(
      .difference = FIRST - FreeSurfer,
      .average = (FIRST + FreeSurfer) / 2,
      acq_day=(acq_day - min(acq_day)) |> as.numeric()) |>
    na.omit() |>
    dplyr::mutate(
      age2 = age^2,
      acq_day2 = acq_day^2,
      dplyr::across(
        c(age, age2, t1_motion, head_size, acq_day, acq_day2), 
        \(x) (x - median(x)) / mad(x)),
      .by = c(site, Hemisphere)) |>
    dplyr::mutate(site = forcats::fct_relabel(site, stringr::str_remove, " \\(imaging\\)"))
  
  tmp |> 
    dplyr::group_nest(Hemisphere) |>
    dplyr::mutate(
      m = purrr::map(
        data,
        ~modelr::model_matrix(
          .x,
          ~site + (age*sex):site + age2:site + t1_motion:site + head_size:site + acq_day:site + acq_day2:site
        ) |>
          dplyr::select(-`(Intercept)`) |>
          dplyr::rename_with(\(x) stringr::str_replace_all(x, ":", "_"))
      ),
      cor = purrr::map2(
        m, data, 
        ~correlation::correlation(.x, dplyr::select(.y, .difference, .average)) |>
          tibble::as_tibble())) |>
    dplyr::select(-data, -m) |>
    tidyr::pivot_longer(c(cor), names_to = "type") |>
    tidyr::unnest(value)  
  
}
