# Structurally Conditioned Diffusion Reproduces Skills-Based Stratification

[![R](https://img.shields.io/badge/R-%3E%3D4.0-blue.svg)](https://www.r-project.org/)

[![O*NET](data:image/png;base64,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)](https://www.onetcenter.org/)

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

This repository contains the replication code for the paper **"Structurally Conditioned Diffusion Reproduces Skills-Based Stratification"** (Cantillan, 2025).  The study demonstrates that the diffusion of occupational skill requirements is governed by an **asymmetric gravity rule** that actively filters physical requirements out of high-status occupations, contributing to the reproduction of labor market stratification. 

### Key Contribution

We identify a systematic pattern of **Asymmetric Trajectory Channeling (ATC)**:

- **Socio-cognitive skills** diffuse readily along upward status trajectories and over longer structural distances
- **Physical-sensory skills** are penalized by upward status gaps and channeled into lateral or downward trajectories
- These asymmetries are **amplified for skills high in the nested dependency hierarchy**

## Theoretical Framework

### The Gravity Model

We model the adoption pressure $P$ exerted by a requirement $s$ moving from source $i$ to target $j$ as:

$$P_{i \to j, s} \sim \frac{\mathcal{M}_i \times \mathcal{A}_j}{\mathcal{D}_{ij}^{\gamma} \times \exp(\Theta_{ij,s})}$$

Where:
- $\mathcal{M}_i$ = **Emission mass** (source capacity)
- $\mathcal{A}_j$ = **Absorptive capacity** (target receptivity)
- $\mathcal{D}_{ij}$ = **Structural distance** (baseline friction)
- $\Theta_{ij,s}$ = **Directional friction** decomposed into upward ($\Delta^{\uparrow}$) and downward ($\Delta^{\downarrow}$) status gaps

### Interval-Censored Identification Strategy

O*NET's rolling-panel nature (≈20% of occupations updated annually) creates "false zeros" that bias standard event-history estimates. We implement an **interval-censored design** treating adoption as a latent event bounded by observation windows, following [Abrevaya & Muris (2020)](https://doi. org/10.1016/j.jeconom.2019.12.007).

## Key Findings

| Domain | Upward Diffusion | Downward Diffusion | Interpretation |
|--------|------------------|-------------------|----------------|
| **Socio-cognitive** | Facilitated | Moderate friction | Travels upward and long-range |
| **Physical-sensory** | Strong penalty | Facilitated | Trapped in lateral/downward orbits |

### Nestedness as Gravity Multiplier

Skills high in the nested dependency hierarchy show amplified asymmetries:
- **Cognitive + High nestedness**: Reduced upward friction → enhanced upward mobility
- **Physical + High nestedness**: Steeper upward penalties → reinforced channeling

## Data Sources

| Source | Description | Years |
|--------|-------------|-------|
| [O*NET Database](https://www. onetcenter. org/) | Skills, Abilities, Knowledge, Work Activities | 2015–2024 (v15.1 → v29.2) |
| [BLS OES](https://www.bls.gov/oes/) | Occupational wages and employment | 2015, 2024 |
| [BLS Education](https://www. bls.gov/emp/tables/education-and-training-by-occupation. htm) | Educational requirements by occupation | 2015–2024 |
| SOC Crosswalks | Occupational code concordances | 2010–2019 |

## Repository Structure

```
skill_diffusion/
│
├── data/
│   ├── db_15_1/                    # O*NET 15.1 (2015) raw data
│   ├── db_29_2_text/               # O*NET 29.2 (2024) raw data
│   ├── crosswalk/                  # SOC code crosswalks
│   ├── national_M2015_dl. xlsx      # BLS wages 2015
│   ├── national_M2024_dl.xlsx      # BLS wages 2024
│   ├── Abilities. txt               # O*NET abilities data
│   ├── Skills.txt                  # O*NET skills data
│   ├── Knowledge.txt               # O*NET knowledge data
│   ├── Work Activities.txt         # O*NET work activities
│   └── Work Styles.txt             # O*NET work styles
│
├── scripts/
│   ├── 00_MASTER. R                 # Main execution script
│   ├── 01_SETUP_FUNCTIONS.R        # Configuration and helper functions
│   ├── 02_ESTIMATE_MODELS.R        # Gravity model estimation (CMLE)
│   ├── 03_CEM_MATCHING.R           # Coarsened Exact Matching
│   ├── 04_FIGURES_MAIN.R           # Main text figures
│   ├── 05_SUPPLEMENTARY. R          # Supplementary analyses
│   ├── 06_flow_networks.R          # Flow network visualizations
│   ├── 00_gravity_models.R         # Full gravity + bootstrap pipeline
│   ├── 00_dyads_occup.R            # Dyad construction
│   └── 00_names_occup.R            # Occupation name processing
│
├── output/
│   ├── figures/                    # Generated figures
│   └── tables/                     # Regression tables
│
├── images/                         # Paper figures
│
├── skill_diffusion. Rproj
└── README.md
```

## Methodology

### Statistical Specification

We estimate the gravity model using **Composite Maximum Likelihood (CMLE)** with two-way fixed effects:

```r
# Core specification (simplified)
feglm(
  diffusion ~ delta_up_wage * domain * nestedness_tercile + 
              delta_down_wage * domain * nestedness_tercile + 
              structural_distance * domain,
  family = binomial(link = "cloglog"),
  fixef = c("source", "target"),
  cluster = c("source", "target")
)
```

### Key Variables

| Variable | Definition |
|----------|------------|
| `delta_up_wage` | $\Delta^{\uparrow} = \max(0, \text{Status}_j - \text{Status}_i)$ |
| `delta_down_wage` | $\Delta^{\downarrow} = \max(0, \text{Status}_i - \text{Status}_j)$ |
| `domain` | Socio-cognitive vs.  Sensory/physical |
| `nestedness_tercile` | Skill position in dependency hierarchy (Low/Mid/High) |
| `structural_distance` | Composite: Jaccard overlap, skill network distance, worker flows |
| `cs` | Contribution to nestedness (enabling capacity) |
| `reach` | Forward-reachable set in dependency network |

### Identification Diagnostics

1. **Placebo Leads**: Future gaps should not predict past adoptions
2. **Within-Cluster Permutation**: Shuffling sources within exposure clusters
3. **Worker Flow Distances**: Replication with distances from realized mobility
4. **Out-of-Sample Prediction**: Train 2015–2019, test 2020–2024

### Robust Inference

**Node-Level Bootstrap** ($B=1000$): Resampling occupations preserves network dependence structure for valid confidence intervals. 

## Installation

### R Dependencies

```r
# Core packages
install.packages(c(
  "data.table",
  "fixest",
  "ggplot2",
  "scales",
  "patchwork"
))

# Matching and diagnostics
install.packages(c("MatchIt", "cobalt"))

# Network analysis
install.packages(c("igraph", "ggraph"))

# Data processing
install.packages(c("Matrix", "readxl", "stringr", "dplyr", "tidyr"))
```

### System Requirements

- **R** ≥ 4.0
- **RAM**: ≥ 16GB (large dyadic dataset)
- **OS**: Windows, macOS, or Linux

## Usage

### Full Pipeline

```r
# Run complete analysis
source("scripts/00_MASTER.R")
```

### Step-by-Step

```r
# 1. Setup functions and configuration
source("scripts/01_SETUP_FUNCTIONS.R")

# 2.  Estimate gravity models with interval-censoring
source("scripts/02_ESTIMATE_MODELS.R")

# 3.  Robustness: Coarsened Exact Matching
source("scripts/03_CEM_MATCHING.R")

# 4. Generate main figures
source("scripts/04_FIGURES_MAIN.R")

# 5. Supplementary materials
source("scripts/05_SUPPLEMENTARY.R")

# 6. Flow network visualizations
source("scripts/06_flow_networks.R")
```

## Outputs

### Main Figures

| Figure | Description | Script |
|--------|-------------|--------|
| Figure 1 | Adoption rate vs. gap deciles by domain | `04_FIGURES_MAIN.R` |
| Figure 2 | Socio-cognitive skills: destination shifts | `04_FIGURES_MAIN. R` |
| Figure 3 | Physical skills: destination shifts | `04_FIGURES_MAIN.R` |
| Figure 4 | ATC × Nestedness interaction | `04_FIGURES_MAIN. R` |

### Supplementary Figures

| Figure | Description |
|--------|-------------|
| S1 | Skill-level profiles (socio-cognitive) |
| S2 | Skill-level profiles (sensory/physical) |

## Citation

```bibtex
@article{cantillan2025skill,
  author  = {Cantillan, Roberto},
  title   = {Structurally Conditioned Diffusion Reproduces Skills-Based Stratification},
  journal = {Working Paper},
  year    = {2025},
  institution = {Pontificia Universidad Católica de Chile}
}
```

## References

Key theoretical and methodological references:

- Alabdulkareem, A., et al. (2018). Unpacking the polarization of workplace skills. *Science Advances*. 
- Hosseinioun, P., et al. (2025). The skill ladder.  *Working Paper*.
- Abrevaya, J., & Muris, C. (2020).  Interval-censored regression with fixed effects. *Journal of Econometrics*. 
- Cheng, S., & Park, B. (2020). Flows and boundaries.  *Social Forces*.
- Xie, Y.  (2015). Assortative mating without assortative preference. *PNAS*. 

## License

This project is licensed under the MIT License. 

## Contact

**Roberto Cantillan**  
Department of Sociology  
Pontificia Universidad Católica de Chile

- GitHub: [@rcantillan](https://github.com/rcantillan)
- Email: [rcantillan@uc.cl]

---

*Doctoral dissertation research — Department of Sociology, Pontificia Universidad Católica de Chile*