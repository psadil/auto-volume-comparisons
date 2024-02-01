
## Comparison of Volumes Estimated by FreeSurfer and FSL

This repo holds the manuscript and analyses for *Comparing Automated Subcortical Volume Estimation Methods; Amygdala Volumes Estimated by FSL and FreeSurfer have Low Consistency*.


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

