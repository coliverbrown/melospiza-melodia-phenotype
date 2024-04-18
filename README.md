# Analyzing Phenotype in R
## Caitlyn Oliver Brown
### Last update: 18 Aug 2023
[![DOI](https://zenodo.org/badge/575142379.svg)](https://zenodo.org/doi/10.5281/zenodo.10994577)
<br>

Repo for analyzing phenotypic differences in *Melospiza melodia* subspecies. Includes R script, raw data file, cleaned data file, and figures produced in the script.

---

### Required Packages and Tools
- [devtools](https://devtools.r-lib.org/)
- [tidyverse](https://www.tidyverse.org/packages/)
- [ggplot2](https://ggplot2.tidyverse.org/)
- [ggord](https://fawda123.github.io/ggord/reference/ggord.html)
- [skin.meas.split](https://github.com/coliverbrown/CleanORNISFull) 
- Color-blind friendly pallete from https://personal.sron.nl/~pault/data/colourschemes.pdf
<br>

### Important Files in Repository
- `melospiza-melodia-fullflat-raw.csv` raw data
- `melospiza-melodia-filtered.csv` filtered data used for analysis

### Steps in script analysis

1. Clean raw data <br>
    - import raw data
    - split "SKINMEAS" column into seven columns with individual measurements
    - filter to include adult males
    - remove outliers
    - export cleaned data table into csv file
2. Summary statistics for each subspecies
3. Plotting
    - Plot boxplot for each measurement
    - Plot PCA
4. MANOVA and ANOVA
    - MANOVA test to identify statistical significance (all variables of interest)
    - ANOVA test for each individual variable of interest
    - Tukey's HSD test to determine which subspecies pair has statistically significance difference
5. Correlations
    - Correlations test of variables of interest