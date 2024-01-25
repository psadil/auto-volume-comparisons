

load_ukb_data_dictionary <- function() {
  raw <- readr::read_tsv("https://biobank.ndph.ox.ac.uk/~bbdatan/Data_Dictionary_Showcase.tsv")
  
  raw |>
    dplyr::filter(
      FieldID %in% c(
        25011, 
        25012, 
        25013, 
        25014, 
        25015, 
        25016, 
        25017, 
        25018, 
        25019, 
        25020, 
        25021, 
        25022, 
        25023, 
        25024, 
        26558, 
        26589, 
        26559, 
        26590, 
        26560, 
        26591, 
        26561,
        26592, 
        26562, 
        26593, 
        26563, 
        26594, 
        26564, 
        26595, 
        26714, 
        26715, 
        26641,
        26663, 
        26609, 
        26619)) |>
    dplyr::select(Path, Field, Participants, Link) |>
    dplyr::mutate(Path = stringr::str_extract(Path, "(?<= structural brain MRI > ).*"))
  # nodes <- rvest::read_html(html) |>
  #   rvest::html_element("body") |> 
  #   rvest::html_nodes("table")
  
  # metadata <- nodes[[1]] |> rvest::html_table() 
  # coding_nodes <- tail(nodes, length(nodes)-2) 
  # coding_names <- coding_nodes |> 
  #   rvest::html_attr("summary") |>
  #   stringr::str_extract("[[:digit:]]+") 
  # coding <- coding_nodes |> 
  #   rvest::html_table(convert = FALSE) |>
  #   rlang::set_names(coding_names) |>
  #   bind_rows(.id = "coding")
  # nodes[[2]] |> 
  #   rvest::html_table() 
}


load_ukb_vols <- function(dataset) {
  arrow::open_dataset(dataset) |>
    dplyr::rename_with(~stringr::str_c("f.", .x) |> stringr::str_replace("-", ".")) |>
    dplyr::filter(!is.na("25733-2.0")) |> # get "usable" scans; https://www.jiscmail.ac.uk/cgi-bin/wa-jisc.exe?A2=ind1807&L=UKB-NEUROIMAGING&D=0&P=1613
    dplyr::select(
      f.eid, 
      sex = f.31.0.0,
      age = f.21003.2.0,
      t1_motion=f.24419.2.0,
      acq_day = f.53.2.0,
      head_size = f.25000.2.0,
      icv = f.26521.2.0,
      site=f.54.2.0,
      tidyselect::matches("(25011|25012|25013|25014|25015|25016|25017|25018|25019|25020|25021|25022|25023|25024)\\.2"),
      tidyselect::matches("(26558|26589)\\.2"), # ASEG thalamus-proper
      tidyselect::matches("(26559|26590)\\.2"), # ASEG caudate
      tidyselect::matches("(26560|26591)\\.2"), # ASEG putamen
      tidyselect::matches("(26561|26592)\\.2"), # ASEG pallidum
      tidyselect::matches("(26562|26593)\\.2"), # ASEG hippocampus
      tidyselect::matches("(26563|26594)\\.2"), # ASEG amygdala
      tidyselect::matches("(26564|26595)\\.2"), # ASEG accumbens
      tidyselect::matches("(26714|26715)\\.2"), # FS SUBSEG whole-thalamus
      tidyselect::matches("(26641|26663)\\.2"), # FS SUBSEG whole-hippocampus
      tidyselect::matches("(26609|26619)\\.2") # FS SUBSEG whole-amygdala
    ) |>
    na.omit() |>
    dplyr::collect() |>
    tidyr::pivot_longer(
      c(-f.eid, -sex, -age, -t1_motion, -acq_day, -head_size, -icv, -site)) |>
    dplyr::mutate(
      hemisphere = dplyr::case_when(
        stringr::str_detect(
          name, 
          "25011|25013|25015|25017|25019|25021|25023|26714|26641|26609|26558|26559|26560|26561|26562|26563|26564"
        ) ~ "Left",
        TRUE ~ "Right"
      ),
      source = dplyr::case_when(
        stringr::str_detect(name, "26714|26715|26641|26663|6609|26619") ~ "FSSUBSEG",
        stringr::str_detect(
          name, 
          "25011|25012|25013|25014|25015|25016|25017|25018|25019|25020|25021|25022|25023|25024"
        ) ~ "FIRST",
        TRUE ~ "FreeSurfer"
      ),
      StructName = dplyr::case_when(
        stringr::str_detect(name, "25011|25012|26558|26589|26714|26715") ~ "Thalamus",
        stringr::str_detect(name, "25013|25014|26559|26590") ~ "Caudate",
        stringr::str_detect(name, "25015|25016|26560|26591") ~ "Putamen",
        stringr::str_detect(name, "25017|25018|26561|26592") ~ "Pallidum",
        stringr::str_detect(name, "25019|25020|26562|26593|26641|26663") ~"Hippocampus",
        stringr::str_detect(name, "25021|25022|26563|26594|26609|26619") ~ "Amygdala",
        stringr::str_detect(name, "25023|25024|26564|26595") ~ "Accumbens"
      ),
      instance = stringr::str_extract(name, ".([23]).", group=1),
      value = value / 1000
    ) |>
    dplyr::select(-name, -instance) |>
    tidyr::unite(Structure, c(hemisphere, StructName, source)) |>
    tidyr::pivot_wider(names_from = Structure) |>
    na.omit() |>
    dplyr::select(-tidyselect::ends_with("FSSUBSEG"))
}

load_ukb_vols2 <- function(ukb_vols) {
  ukb_vols |>
    tidyr::pivot_longer(
      c(-f.eid, -sex, -age, -t1_motion, -acq_day, -head_size, -icv, -site),
      names_sep = "_",
      names_to = c("Hemisphere", "Structure", "Method"),
      values_to = "Volume")
}


prep_cognitive <- function(file){
  readr::read_tsv(file, show_col_types = FALSE) 
}


# confound_vars_to_fields <- function(variables){
#   commas <- stringr::str_split(variables, ",")[[1]]
#   steps <- commas[stringr::str_which(commas, "[[:digit:]]+:[[:digit:]]:[[:digit:]]+")] |>
#     stringr::str_split(":") |>
#     purrr::map(as.integer)
#   ranges <- commas[stringr::str_which(commas, "^[[:digit:]]+:[[:digit:]]+$")] |>
#     stringr::str_split(":") |>
#     purrr::map(as.integer)
#   out <- commas[stringr::str_which(commas, ":", negate=TRUE)] |>
#     as.integer()
#   
#   for (s in steps){
#     out <- vctrs::vec_c(out, seq(s[1], to=s[3], by=s[2]))
#   }
#   for (r in ranges){
#     out <- vctrs::vec_c(out, r[1]:r[2])
#   }
#   out
# }
# 
# raw <- readr::read_tsv("https://git.fmrib.ox.ac.uk/falmagro/ukb_unconfound_v2/-/raw/master/generate_initial_data/gen_nonIDPs/myCategories.tsv?inline=false") |>
#   filter(stringr::str_detect(Category, "cognitive"))






