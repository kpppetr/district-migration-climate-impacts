# district-migration-climate-impacts
Replication code and data for "Differential effects of extreme climate impacts on global district-level net migration"

Software & versions
- Computer: MacBook Pro
- Chip / Architecture: Apple M2, aarch64 (ARM64)
- Memory: 16 GB RAM
- Operating system: macOS Sequoia 15.6.1 (build 24G90)
- R: 4.5.1 (2025-06-13)
- BLAS/LAPACK: Apple Accelerate / vecLib; LAPACK 3.12.1

These are the versions the software has been tested on. The code should run on other recent R versions (≥ 4.2) and OSes, but those have not been tested here.

Hardware:
- No non-standard hardware required.
- Recommended: ≥ 16 GB RAM for full model runs (Bayesian models can be memory-intensive).
- Disk space: ~5–20 GB.

R package dependencies:
- Unique package list: BVAR, arm, brms, caret, car, data.table, dplyr, foreign, ggplot2, lme4, LRTesteR, mvtnorm, optimx, readr, reshape2, rgdal, sandwich, sf, stargazer, tidyr, tidyverse
- Installation time - typically 2–8 minutes

Instructions for use the data
To reproduce all quantitative results reported in the manuscript and SI:

Load data "data.csv" and run the R scripts in the following order:

A. code/Descriptives.R
Generates:
- Table S.1 — Descriptive statistics 
- Figure S.1 — Total number of extreme climate events per district-year 
- Figure S.2 — Histogram of years with positive/negative net migration 
- Figure S.3 — Density plots (key variables) 
- Figure S.4 — Annual trends (averaged across districts)
- Figure S.5 — Residual plots of log(net migration) 
- Figure S.6 — Correlation matrix (RHS vars, 1% level)
Note: Descriptives are intended to characterize the full sample

Typical runtime: ~2–6 minutes.

B. code/Main_Analysis.R
Generates:
- Table 2 & Table 3 (SI) — Global analysis of single climate events & net migration (fixed & random effects)
- Table 4 — Adds crop failure to analysis
- Figure S.7 — Marginal effects (incl. crop failure)
- Figure S.8 — Fixed effects (SI) 
- Figure 2 — Marginal effects (random effects)
- Figure 3 — Model performance comparison (AIC & RMSE)

Typical runtime: demo settings often ~5–10 minutes .

C. code/bvar_single.R and code/bvar_single_income.R
Generates (single events BVAR):
- Figure 4 (main text) — effects from BVAR single-event models
- Figure S.12 — Heterogeneous effects: negative net migration
- Figure S.13 — Heterogeneous effects: positive net migration 

Typical runtime: demo settings often ~5–20 minutes each.

D. code/bvar_compound.R and code/bvar_compound_income.R

Generates (compound events BVAR):
- Figure 5 (main text) — Compound climate events and net migration
- Figure S.9 (SI) — Descriptives for compound events 
- Figure S.14 — Heterogeneous effects (compound, negative net)
- Figure S.15 — Heterogeneous effects (compound, positive net)

Typical runtime: similar to single-event BVAR.

## License

**Software (code):** Apache License 2.0 (OSI-approved).  
**Data & documentation:** Creative Commons Attribution 4.0 (CC BY 4.0).  
**SPDX identifiers:** Code → `Apache-2.0`; Data → `CC-BY-4.0`.
