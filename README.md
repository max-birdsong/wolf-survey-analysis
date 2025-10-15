# Wolf Survey Analysis — Longitudinal Tolerance Trends (2012–2023)

Multi-language data pipeline (Python + SQL + R) for analyzing public attitudes toward wolves across three Montana survey waves.

## 📊 Project Overview

Montana Fish, Wildlife & Parks (FWP) conducted three statewide surveys (2012, 2017, 2023) to track public attitudes toward wolves and wolf management. This repository contains the **complete analytical pipeline** that:

- Harmonizes three survey waves with different schemas and variable naming conventions
- Implements deduplication logic for respondents appearing across multiple waves and strata
- Applies post-stratification weights to ensure results represent Montana's adult population
- Produces weighted, publication-ready longitudinal trend visualizations
- Supports predictive modeling of wolf tolerance (see [Wolf Tolerance Predictor App](https://wolf-tolerance-predictor-app.streamlit.app/))

**Key Results:**
- Tolerance for wolves has steadily increased across all stakeholder groups (2012→2023)
- Distant wolf sightings reliably build tolerance; property damage reports erode it
- Hunter identity moderates how experiences translate into attitudes
- Full findings: [Research Summary PDF](reports/wolf_final_survey_report.pdf)

---

## 🗂️ Repository Structure
```
wolf-survey-analysis/
├── data/
│   ├── instruments/          # Survey questionnaires and codebooks (2012, 2017, 2023)
│   ├── raw/                  # Original SPSS files (not included in repo - confidential)
│   └── processed/            # Cleaned, harmonized datasets
├── scripts/
│   ├── Python/               # Data harmonization and integration
│   ├── SQL/                  # Deduplication, validation, and EDA
│   └── R/                    # Weighted analysis and visualization
├── reports/
│   ├── Figures/              # PNG exports and PowerPoint deck
│   └── wolf_final_survey_report.pdf
└── README.md
```

---

## 🔧 Pipeline Steps

### **1. Survey Design & Implementation**
📂 `data/instruments/`

The 2023 questionnaire was co-designed to match 2012/2017 on key constructs (tolerance, trust, experiences) while expanding measurement of encounters and policy preferences. Survey instruments include:

- Questionnaires (English, mail survey)
- Coding manuals with variable definitions
- Sampling frames and weighting procedures

**Why this matters:** Ensuring comparability across waves allows defensible longitudinal trend analysis.

[→ View survey instruments](data/instruments/)

---

### **2. Build Unified Dataset (Python)**
📓 `scripts/Python/Clean_Harmonize.ipynb`

**Challenge:** Four SPSS files (general public, landowners, deer hunters, wolf hunters) with different schemas and variable naming conventions.

**Solution:** Python notebook that:
- Reads raw SPSS files using `pyreadstat`
- Standardizes variable names to a common convention
- Maps response codes to consistent scales (e.g., 1-5 Likert)
- Creates shared household keys for tracking respondents across years
- Adds license flags (hunting participation) from administrative data
- Exports harmonized CSVs for each stratum

**Output:** Four clean, standardized CSV files ready for cross-wave integration.

[→ View notebook](scripts/Python/Clean_Harmonize.ipynb)

---

### **3. Cross-Wave Integration (Python)**
📓 `scripts/Python/Merge_All_Years.ipynb`

**Challenge:** Prior waves (2012, 2017) used different question wording, response scales, and survey flow.

**Solution:** Python notebook that:
- Maps 2012/2017 variables to the canonical 2023 layout
- Tags each record with wave identifier
- Logs mismatches and records requiring manual review
- Creates documentation of variable mappings and transformations
- Exports a unified `MasterData` table spanning all three waves

**Output:** Single longitudinal dataset with 1,758 unique respondents and consistent variable definitions.

[→ View notebook](scripts/Python/Merge_All_Years.ipynb)

---

### **4. Deduplication & Standardization (SQL)**
📄 `scripts/SQL/Wolf_data_clean.sql`

**Challenge:** Some respondents appear more than once (across years, or in multiple strata if they hold multiple licenses).

**Solution:** PostgreSQL script that:
- Identifies duplicates using household ID + demographics
- Applies rule-based prioritization:
  - Keep most specific stratum (wolf hunter > deer hunter > landowner > general)
  - Keep most recent wave when same person appears multiple times
- Flags edge cases for manual review
- Enforces referential integrity (e.g., valid stratum codes, non-null keys)
- Creates final clean table with one row per respondent

**Output:** De-duplicated dataset with 1,758 unique records.

[→ View SQL script](scripts/SQL/Wolf_data_clean.sql)

---

### **5. Exploratory Data Analysis (SQL)**
📄 `scripts/SQL/Wolf_exploratory_data_analysis.sql`

**Purpose:** Quick validation and health checks before full modeling.

**Queries include:**
- Counts by wave and stratum (sample composition)
- Distribution of tolerance and trust variables
- Cross-tabulations of experiences by group (hunters vs. non-hunters)
- Missingness patterns and data quality flags

**Output:** Summary statistics confirming data integrity and informing modeling decisions.

[→ View SQL script](scripts/SQL/Wolf_exploratory_data_analysis.sql)

---

### **6. Weighted Reporting & Visualization (R)**
📄 `scripts/R/03_plots_longitudinal.R`

**Purpose:** Apply post-stratification weights and generate publication-ready visualizations.

**R script:**
- Loads clean dataset from SQL
- Applies survey weights using `survey` package
  - Adjusts for selection probability (oversampled groups)
  - Post-stratifies to Montana census benchmarks (age, gender, region)
- Generates weighted means and 95% confidence intervals
- Creates faceted charts showing tolerance trends by group (2012→2023)
- Exports:
  - PNG files for each figure
  - PowerPoint deck with embedded charts
  - CSV tables with weighted estimates

**Key Visualizations:**
- Tolerance over time by stakeholder group
- Distribution shifts (5-level scale)
- Encounter effects on tolerance (regression coefficients)
- Encounter prevalence by group

[→ View R script](scripts/R/03_plots_longitudinal.R)

---

### **7. Stakeholder Deliverables**

#### **PowerPoint Deck** 📊
📄 `reports/Figures/wolves_graphs_longitudinal.pptx`

Stakeholder-ready slides with:
- Weighted longitudinal trends (error bars, 95% CI)
- Key takeaways in plain language
- Source notes and methodology footnotes
- Built directly from R code for reproducibility

[→ Download deck](reports/Figures/wolves_graphs_longitudinal.pptx)

#### **Research Summary (PDF)** 📄
📄 `reports/wolf_final_survey_report.pdf`

Formal write-up for FWP leadership and public audiences:
- Executive summary of trends (2012→2023)
- Methods (survey design, weighting, analysis)
- Key findings (tolerance, encounters, group differences)
- Management implications

[→ Read report](reports/wolf_final_survey_report.pdf)

---

## 🛠️ Technologies Used

| Language/Tool | Purpose |
|---------------|---------|
| **Python 3.9+** | Data harmonization, cross-wave integration |
| `pandas`, `numpy` | Data manipulation |
| `pyreadstat` | Read SPSS files |
| **SQL (PostgreSQL 13)** | Deduplication, validation, EDA |
| **R 4.1+** | Weighted analysis, visualization |
| `survey` | Post-stratification weights, design-based inference |
| `ggplot2` | Publication-quality charts |
| `officer` | Export PowerPoint decks from R |

---

## 📈 Key Findings

### Tolerance Trends (2012→2023)
- **General public:** +0.9 points (1-5 scale)
- **Deer hunters:** +1.3 points
- **Landowners:** Modest increase, but remain less tolerant overall
- **Wolf hunters:** Smallest change, lowest tolerance

### Encounter Effects (Logistic Regression)
- ✅ **Distant sightings:** Positive effect on tolerance
- ❌ **Near-home sightings:** Strong negative effect
- ❌ **Property damage (vicarious):** Largest negative effect
- ⚠️ **Tracks:** Small negative effect

### Group Differences
- Hunters report 34-75% more wolf encounters than non-hunters
- Hunter identity moderates how experiences translate into attitudes
- Vicarious property damage strongly erodes hunters' attitudes but has no effect on non-hunters

**Full results:** [Research Summary PDF](reports/wolf_final_survey_report.pdf)

---

## 🚀 Reproducibility

### Prerequisites
```bash
# Python
pip install pandas numpy pyreadstat jupyter

# SQL
# Requires PostgreSQL 13+ (or adapt to your SQL dialect)

# R
install.packages(c("survey", "ggplot2", "dplyr", "officer"))
```

### Run the Pipeline
```bash
# 1. Harmonize raw SPSS files
jupyter notebook scripts/Python/Clean_Harmonize.ipynb

# 2. Integrate across waves
jupyter notebook scripts/Python/Merge_All_Years.ipynb

# 3. Load data into PostgreSQL
psql -d wolf_survey -f scripts/SQL/Wolf_data_clean.sql

# 4. Run EDA
psql -d wolf_survey -f scripts/SQL/Wolf_exploratory_data_analysis.sql

# 5. Generate weighted visualizations
Rscript scripts/R/03_plots_longitudinal.R
```

### Output
- `data/processed/MasterData.csv` — Clean, harmonized longitudinal dataset
- `reports/Figures/*.png` — Weighted trend charts
- `reports/Figures/wolves_graphs_longitudinal.pptx` — Stakeholder deck

---

## 📊 Related Projects

- **[Wolf Tolerance Predictor App](https://wolf-tolerance-predictor-app.streamlit.app/)** — Interactive Streamlit app using logistic regression to predict individual tolerance based on 5 key variables (reduced from 50+ candidates)
- **[Portfolio Project Page](https://max-birdsong.github.io/wolf.html)** — Full project write-up with Tableau dashboards showing longitudinal trends

---

## 📧 Contact

**Max Birdsong, PhD**  
📧 maxbirdsong1221@gmail.com  
🔗 [Portfolio](https://max-birdsong.github.io) | [LinkedIn](#) | [GitHub](https://github.com/max-birdsong)

---

## 📄 License

This code is provided for transparency and reproducibility. Survey data are confidential and not included in this repository. For data access inquiries, contact Montana Fish, Wildlife & Parks.

---

## 🙏 Acknowledgments

- Montana Fish, Wildlife & Parks (project sponsor)
- Survey respondents (1,758 Montana residents)
- [Any collaborators/advisors]

---

**Citation:**  
If you use this code or findings in your work, please cite:  
> Birdsong, M. (2024). *Wolf Survey Analysis: Longitudinal Tolerance Trends in Montana (2012–2023)*. GitHub repository. https://github.com/max-birdsong/wolf-survey-analysis
