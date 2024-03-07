library(targets)

source("R/load.R")
source("R/figures.R")
source("R/smerrors.R")
source("R/partials.R")
source("R/cognitive.R")


targets::tar_option_set(
  packages = c("ggplot2", "rlang"),
  memory = "transient"
)

list(
  tar_target(ukb_parquet, "data/ukb677207_bulk.parquet", format = "file"),
  tar_target(ukb_vols, load_ukb_vols(ukb_parquet), format = "parquet"),
  tar_target(ukb_dd, load_ukb_data_dictionary(), format = "parquet"),
  tar_target(ukb_vols_long, load_ukb_vols2(ukb_vols), format = "parquet"),
  tar_target(rhos, c(0.01,  0.1, 0.2)),
  tar_target(sm_N, c(seq(10, 100, by=10))),
  tar_target(icc, c(0.1, 0.2, 0.3, 0.4, 0.6, 0.8)),
  # tar_target(rhos, c(0.01)),
  # tar_target(sm_N, c(seq(100, 100, by=10))),
  # tar_target(icc, c(0.5)),
  tar_target(sigma_lambda, get_sigma_lambda(ukb_vols_long)),
  tar_target(
    icc_data, 
    get_icc_data(
      N=sm_N, 
      rho=rhos, 
      i=1000000, 
      icc=icc, 
      c_k=0,
      sigma_lambda=sigma_lambda), 
    pattern = cross(rhos, sm_N, icc),
    format = "parquet"),
  tar_target(
    icc_data2, 
    get_icc_data2(icc_data), 
    pattern = map(icc_data),
    format = "parquet"),
  tar_target(
    partials,
    get_partials(ukb_vols_long),
    format = "parquet"
  ),
  tar_target(cognitive_tsv, "data/cognitive.tsv", format = "file"),
  tar_target(cognitive, prep_cognitive(cognitive_tsv), format = "parquet"),
  tar_target(hemisphere, c("Left", "Right")),
  tar_target(
    cog_cor_full_ukb,
    get_cog_cor_full_ukb(
      cognitive=cognitive, 
      ukb_vols_long=ukb_vols_long,
      hemisphere=hemisphere),
    format = "parquet",
    pattern = map(hemisphere)
  ),
  tar_target(predictor, 1+seq_len(50)),
  tar_target(
    amyg_vols, 
    get_amyg_vols(
      ukb_vols_long=ukb_vols_long,
      cognitive=cognitive,
      cog_cor_full_ukb=cog_cor_full_ukb,
      predictor=predictor,
      max_predictors=50), 
    format = "parquet",
    pattern = cross(predictor, cog_cor_full_ukb)),
  tar_target(
    cog_cor, 
    sample_cog_cor(
      amyg_vols=amyg_vols,
      N=sm_N,
      i=1000000), 
    format = "parquet",
    pattern = cross(sm_N, amyg_vols)),
  tar_target(
    cog_dif_sig, 
    get_cog_dif_sig(cog_cor=cog_cor, cog_cor_full_ukb=cog_cor_full_ukb),
    format = "parquet",
    pattern = map(cog_cor)),
  tar_target(
    cog_dif_dir, 
    get_cog_dif_dir(cog_cor=cog_cor, cog_cor_full_ukb=cog_cor_full_ukb),
    format = "parquet",
    pattern = map(cog_cor)),
  tar_target(batch, seq_len(1)),
  tar_target(
    amyg_vols_cors, 
    bootstrap_amyg_vols_cors(
      amyg_vols=amyg_vols, 
      cog_dif_dir=cog_dif_dir, 
      hemisphere=hemisphere, 
      batch=batch,
      times = 1000),
    format = "parquet",
    pattern = cross(amyg_vols, batch)),
  tar_target(fig_vol_comparison, make_vol_comparison(ukb_vols), packages = c("patchwork")),
  tar_target(
    fig_biases_perc, 
    make_biases_perc(ukb_vols_long), 
    packages = c("patchwork")),
  tar_target(why_flat, make_why_flat(icc_data)),
  tarchetypes::tar_quarto(manuscript)
)
