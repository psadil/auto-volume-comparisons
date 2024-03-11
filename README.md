
## Comparison of Volumes Estimated by FreeSurfer and FSL

This repo holds the manuscript and analyses for *Comparing Automated Subcortical Volume Estimation Methods; Amygdala Volumes Estimated by FSL and FreeSurfer Have Poor Consistency*. A preprint is available on biorxiv: https://doi.org/10.1101/2024.03.07.583900.


## Reproducing Analyses

### Data

The report uses data from the UKB (Application 33278, Dataset 677207, Basket 4090939). 

### Environment

The computing environment is tracked with [renv](https://rstudio.github.io/renv/index.html). The environment can be installed with 

```{r}
renv::restore()
```

### Analyses and Manuscript

Analyses are structured with the [targets](https://github.com/ropensci/targets) package. To reproduce the analyses and regenerate the manuscript, run

```{r}
targets::tar_make()
```

The final target will render the quarto project, producing _manuscript/index.pdf.

