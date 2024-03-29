
.repeat_row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

.get_single_fit <- function(
    N,
    rho,
    nu = 0,
    beta0 = 0,
    sigma_epsilon = 1,
    sigma_c = 1,
    sigma_lambda = 1,
    sigma_delta = 1){
  
  epsilon <- rnorm(N, sd=sigma_epsilon)
  lambda <- rnorm(N, 0, sigma_lambda)
  c0 <- rnorm(1, sd=sigma_c)
  x <- nu + lambda + c0 + epsilon
  delta <- rnorm(N, sd=sigma_delta)
  beta1 <- rho * sigma_lambda/sigma_delta
  y <- beta0 + beta1 * lambda + delta
  cor.test(y, x)
}

.icc_to_sigma_epsilon <- function(icc, sigma_lambda = 1, sigma_c = 0){
  # (1 / icc - 1) * sigma_lambda^2
  # icc(A,1) = sigma_lambda^2 / (sigma_lambda^2 + sigma_c^2 + sigma_epsilon^2)
  sqrt(sigma_lambda^2 - icc * (sigma_c^2 + sigma_lambda^2)) / sqrt(icc)
}

get_single_data <- function(
    N, i,
    rho = 0.1,
    nu = 0,
    beta0 = 0,
    icc = 1,
    sigma_c = 1,
    sigma_delta = 1){
  
  # model of using one method only (e.g., researcher prefers either FSL or FS)
  sigma_lambda <- 1
  sigma_epsilon <- .icc_to_sigma_epsilon(icc, sigma_lambda=sigma_lambda, sigma_c=sigma_c)
  
  tidyr::crossing(
    i = seq_len(.env$i),
    N = .env$N, 
    rho = .env$rho) |>
    dplyr::mutate(
      fit = purrr::map2(
        N, rho, 
        .get_single_fit, 
        nu=nu, 
        beta0=beta0, 
        sigma_epsilon=sigma_epsilon, 
        sigma_c=sigma_c, 
        sigma_lambda=sigma_lambda, 
        sigma_delta=sigma_delta),
      p = purrr::map_dbl(fit, ~.x$p.value),
      r = purrr::map_dbl(fit, ~.x$estimate),
      S = sign(rho) == sign(r),
      M = abs(r) / abs(rho)) |>
    dplyr::select(-fit) |>
    dplyr::summarise(
      rho = unique(rho), 
      S = 1-mean(S[p < 0.05]), 
      M = mean(M[p < 0.05]),
      .by = c(rho, N)) |>
    dplyr::mutate(
      II = 1 - purrr::map2_dbl(N, rho, ~pwr::pwr.r.test(.x, .y)$power),
      rho = factor(rho)) |>
    tidyr::pivot_longer(c(S,M, II), names_to = "type", values_to = "error") 
}



sim_icc <- function(icc, rho, n = 50){
  Sigma <- matrix(
    c(
      1, icc, rho, 
      icc, 1, rho, 
      rho, rho, 1),
    nrow=3,
    byrow=TRUE)
  
  MASS::mvrnorm(n, c(0,0,0), Sigma) # consider also empirical == TRUE
}

.ck_to_cks <- function(c_k){
  c(-c_k, c_k) / 2
}

.ck_to_sigmac <- function(c_k){
  cks <- .ck_to_cks(c_k)
  cks^2 |> sum()
}

.get_both_fit <- function(
    N,
    rho,
    nu = 0,
    beta0 = 0,
    sigma_epsilon = 1,
    c_k = 0,
    sigma_lambda = 1,
    sigma_delta = 1){
  
  epsilon <- MASS::mvrnorm(
    N, c(0,0), 
    matrix(c(sigma_epsilon^2, 0, 0, sigma_epsilon^2), nrow = 2))
  
  lambda <- rnorm(N, sd=sigma_lambda)
  
  c0 <- .ck_to_cks(c_k)
  
  x <- nu + lambda + .repeat_row(c0, N) + epsilon
  
  delta <- rnorm(N, sd=sigma_delta)
  beta1 <- rho * sigma_delta / sigma_lambda
  
  y <- beta0 + beta1 * lambda + delta
  
  fit_x1y <- cor.test(y, x[,1])
  fit_x2y <- cor.test(y, x[,2])
  
  list(fit_x1y, fit_x2y)
}

get_icc_data <- function(
    i, N, icc, rho,
    sigma_lambda=1,
    nu = 0,
    beta0 = 0,
    c_k = 0,
    sigma_delta = 1){
  
  # if estimating with both methods, how often do they agree?
  sigma_epsilon <- .icc_to_sigma_epsilon(
    icc, 
    sigma_lambda=sigma_lambda, 
    sigma_c=.ck_to_sigmac(c_k))
  
  tidyr::crossing(i = seq_len(i), icc=icc, rho=rho, N=N) |>
    dplyr::mutate(
      fits = purrr::pmap(
        list(rho=rho, N=N), 
        .get_both_fit,
        nu=nu, 
        beta0=beta0, 
        sigma_epsilon=sigma_epsilon, 
        c_k=c_k, 
        sigma_lambda=sigma_lambda, 
        sigma_delta=sigma_delta),
      p_x1 = purrr::map_dbl(fits, ~.x[[1]]$p.value),
      p_x2 = purrr::map_dbl(fits, ~.x[[2]]$p.value),
      r_x1 = purrr::map_dbl(fits, ~.x[[1]]$estimate),
      r_x2 = purrr::map_dbl(fits, ~.x[[2]]$estimate),
      same_sign = sign(r_x1) == sign(r_x2)
    ) |>
    dplyr::select(-fits)
}

get_icc_data2 <- function(icc_data){
  icc_data |> 
    dplyr::summarise(
      different_direction_s = sum(!same_sign[p_x1 < 0.05 & p_x2 < 0.05]),
      different_direction_n = sum(p_x1 < 0.05 & p_x2 < 0.05),
      different_significance_s = sum(
        (p_x1 < 0.05 & p_x2 >= 0.05) | (p_x1 >= 0.05 & p_x2 < 0.05)),
      different_significance_n =  sum(p_x1 < 0.05 | p_x2 < 0.05),
      .by = c(icc, rho, N)) |>
    dplyr::mutate(
      ds_a = 1/2 + different_significance_s,
      ds_b = 1/2 + different_significance_n - different_significance_s,
      dd_a = 1/2 + different_direction_s,
      dd_b = 1/2 + different_direction_n - different_direction_s,
      dd_lower = qbeta(0.025, dd_a, dd_b),
      dd_med = qbeta(0.5, dd_a, dd_b),
      dd_upper = qbeta(0.975, dd_a, dd_b),
      dd_avg = different_direction_s / different_direction_n,
      ds_lower = qbeta(0.025, ds_a, ds_b),
      ds_upper = qbeta(0.975, ds_a, ds_b),
      ds_med = qbeta(0.5, ds_a, ds_b),
      ds_avg = different_significance_s / different_significance_n) |>
    dplyr::select(icc, rho, N, tidyselect::starts_with("dd"), tidyselect::starts_with("ds"))
}

get_sigma_lambda <- function(ukb_vols_long){
  amg <- ukb_vols_long |> 
    dplyr::filter(Structure=="Amygdala")
  
  fit <- lme4::lmer(Volume ~ Method + (1 | f.eid), data=amg, REML = TRUE)
  fit_summary <- summary(fit)
  sigma_lambda <- fit_summary$varcor$f.eid |> as.numeric()
}
