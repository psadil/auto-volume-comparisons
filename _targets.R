library(targets)

source("R/load.R")
source("R/figures.R")
source("R/smerrors.R")
source("R/partials.R")
source("R/cognitive.R")

targets::tar_option_set(
  packages = c("ggplot2", "rlang"),
  controller = crew::crew_controller_local(workers = 4)
)

list(
  tar_target(ukb_parquet, "data/ukb677207_bulk.parquet", format = "file"),
  tar_target(ukb_vols, load_ukb_vols(ukb_parquet), format = "parquet"),
  tar_target(ukb_dd, load_ukb_data_dictionary(), format = "parquet"),
  tar_target(ukb_vols_long, load_ukb_vols2(ukb_vols), format = "parquet"),
  tar_target(rhos, c(0.01,  0.1)),
  tar_target(sm_N, c(seq(10, 100, by=10))),
  tar_target(
    sm_data, 
    get_single_data(N=sm_N, rho=rhos, i=10000), 
    pattern = cross(rhos, sm_N),
    format = "parquet",
    cue = tar_cue("never")),
  tar_target(icc, c(0.1, 0.2, 0.5, 0.8, 0.9)),
  tar_target(
    icc_data, 
    get_icc_data(N=sm_N, rho=rhos, i=1000000, icc=icc), 
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
    format = "parquet",
    packages = "LMMstar",
    cue = tar_cue("never")
  ),
  tar_target(cognitive_tsv, "data/cognitive.tsv", format = "file"),
  tar_target(cognitive, prep_cognitive(cognitive_tsv), format = "parquet"),
  tar_target(
    cog_cor_full_ukb,
    get_cog_cor_full_ukb(cognitive=cognitive, ukb_vols_long=ukb_vols_long),
    format = "parquet"
  ),
  tar_target(
    cog_cor, 
    sample_cog_cor(
      ukb_vols_long=ukb_vols_long,
      cognitive=cognitive,
      cog_cor_full_ukb=cog_cor_full_ukb,
      N=sm_N,
      i=100000), 
    format = "parquet",
    pattern = map(sm_N)),
  tar_target(
    cog_dif_sig, 
    get_cog_dif_sig(cog_cor=cog_cor, cog_cor_full_ukb=cog_cor_full_ukb),
    format = "parquet"),
  tar_target(
    cog_dif_dir, 
    get_cog_dif_dir(cog_cor=cog_cor, cog_cor_full_ukb=cog_cor_full_ukb),
    format = "parquet"),
  tarchetypes::tar_quarto(manuscript)
)
