
get_partials <- function(ukb_vols_long){
  
  tmp0 <- arrow::open_dataset("data/ukb26883_bulk.parquet") |>
    dplyr::select(f.eid, site=f.54.2.0) |>
    dplyr::collect() |>
    dplyr::mutate(
      dplyr::across(tidyselect::everything(), as.numeric),
      f.eid = as.character(f.eid)) |>
    na.omit() |>
    dplyr::mutate(site = factor(site))
  
  ukb_vols_long |>
    dplyr::filter(
      stringr::str_detect(Method, "FSSUBSEG", TRUE)) |>
    tidyr::pivot_wider(names_from = Method, values_from = Volume) |>
    dplyr::mutate(
      .difference = FIRST - FreeSurfer,
      .average = (FIRST + FreeSurfer) / 2,
      acq_day=(acq_day - min(acq_day)) |> as.numeric()) |>
    dplyr::right_join(tmp0, by = dplyr::join_by(f.eid)) |>
    na.omit() |>
    dplyr::mutate(
      age2 = age^2,
      acq_day2 = acq_day^2,
      dplyr::across(
        c(age, age2, t1_motion, head_size, acq_day, acq_day2), 
        \(x) (x - median(x)) / mad(x)),
      .by = site) |>
    dplyr::group_nest(Hemisphere, Structure) |>
    dplyr::mutate(
      diff = purrr::map(
        data,
        ~LMMstar::lmm(
          .difference ~ site + (age*sex):site + age2:site + t1_motion:site + head_size:site + acq_day:site + acq_day2:site,
          data = .x) |> 
          LMMstar::partialCor(df=FALSE) |>
          tibble::as_tibble(rownames = "term")
      ),
      avg = purrr::map(
        data,
        ~LMMstar::lmm(
          .average ~ site + (age*sex):site + age2:site + t1_motion:site + head_size:site + acq_day:site + acq_day2:site,
          data = .x) |> 
          LMMstar::partialCor(df=FALSE) |>
          tibble::as_tibble(rownames = "term")
      )
    ) |>
    dplyr::select(-data) |>
    tidyr::pivot_longer(
      c(tidyselect::ends_with("diff"), tidyselect::ends_with("avg")), 
      names_to = "response") |>
    tidyr::unnest(value) 
}
