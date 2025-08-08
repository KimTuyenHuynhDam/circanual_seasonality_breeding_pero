# Circannual Breeding and Methylation in *Peromyscus*

This project explores how **birth timing** (season and month) influences reproductive patterns and DNA methylation in *Peromyscus* species.

## 📂 Repositories Overview

- **`breeding_patterns - win, sum/`**  
  Analysis of breeding data based on **birth season** (Winter vs. Summer)  
  → Reproductive output patterns across decades of historical colony records.

- **`database - circannual/`**  
  Analysis of breeding data based on **birth month**  
  → Circannual reproductive trends modeled over 5-year intervals.

- **`methylation/`**  
  DNA methylation modeling based on **birth season and birth month**  
  → CpG-level analysis using linear models, sine/cosine transformation, and FDR correction.

## 🧪 Summary of Methods

- Breeding analyses use descriptive summaries and binning by year or season.
- Methylation models compare nested linear regressions (null vs. seasonal/circannual predictors).
- Age, sex, and species are included as covariates.

## 💻 Software & Key Packages

### R (v4.3.1)
Used for breeding data analysis and methylation modeling:

- `tidyverse`, `broom`, `lubridate`, `circular`, `ggplot2`,  
  `readxl`, `openxlsx`, `janitor`, `reshape2`, `RColorBrewer`

### Python (3.10)
Used for visualizations and GAM/SINE-based circannual trend modeling:

- `pandas`, `matplotlib`, `pygam`, `scikit-learn`,  
  `scipy`, `math`, `openpyxl`

---
## 📎 Citation

Huynh-Dam, KT., Jaeger, C., Naderi, A. et al. Circannual breeding and methylation are impacted by the equinox in Peromyscus. BMC Biol 23, 149 (2025). https://doi.org/10.1186/s12915-025-02251-6

## 🔗 License

This project is open-source under the MIT License.
