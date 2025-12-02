# Structurally Conditioned Diffusion Reproduces Skills-Based Stratification

[![R](https://img.shields.io/badge/R-%3E%3D4.0-blue.svg)](https://www.r-project.org/)

[![O*NET](https://www.google.com/imgres?imgurl=x-raw-image%3A%2F%2F%2Fb133650b996aa4077d1ef947c094d697c4565ab627b4a1884937423cd688a5a2&tbnid=B6RqmQtlM7FU6M&vet=10CAQQxiAoAWoXChMIiKGX5LafkQMVAAAAAB0AAAAAEAY..i&imgrefurl=https%3A%2F%2Fwww.onetcenter.org%2Fdl_files%2Fpaw%2FProducts_at_Work.pdf&docid=hR6SYh2UgLSLXM&w=400&h=175&q=o*net%20logo%20.png&ved=0CAQQxiAoAWoXChMIiKGX5LafkQMVAAAAAB0AAAAAEAY)](https://www.onetcenter.org/)

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