# Gender Discrimination Lawsuit Analysis
## Salary and Rank Differences at Houston College of Medicine

**Debunking False Claims Through Data Analytics**

[![R](https://img.shields.io/badge/R-276DC3?style=flat&logo=r&logoColor=white)](https://www.r-project.org/)
[![Statistics](https://img.shields.io/badge/Statistics-Regression_Analysis-blue)](https://github.com/yourusername/houston-lawsuit)
[![Legal Analytics](https://img.shields.io/badge/Legal-Analytics-red)](https://github.com/yourusername/houston-lawsuit)

**Team 6:** Liu Chang, Xu Anlan, Chen Zixuan, Li Ang, Arif Farhan Bukhori, Simon Eppig  
**Course:** AN6003 – Analytics Strategy | AY 2025/2026  
**Institution:** Nanyang Business School, Nanyang Technological University

---

## 📊 Executive Summary

This project analyzes alleged gender discrimination in salary and promotion practices at Houston College of Medicine using rigorous statistical methods. Through comprehensive data analysis of **261 faculty members**, we debunk claims of systemic gender bias by demonstrating that **experience and department—not gender—are the decisive factors** determining compensation and rank.

**Key Findings:**
- **No evidence of gender discrimination** in salary or promotions when controlling for relevant factors
- **Experience is the sole decisive factor** for high earners (top 25%)
- Women work **~80.5% of men's hours** (51 vs 62 hours/week), explaining promotion timing differences
- Women are **overrepresented in lower-paying departments**, not systematically underpaid
- **Salary growth rates are identical** for both genders (9.8% vs 10.0%)

**Legal Implication:** The data does not support claims of systematic gender-based discrimination at Houston College of Medicine.

---

## 🎯 Business Problem

**Context:** Houston College of Medicine faces a gender discrimination lawsuit alleging:
1. Women receive lower salaries than men for equivalent work
2. Women experience slower promotion timelines
3. Systematic bias exists in compensation decisions

**Objective:** Use data analytics to determine whether observed salary and rank differences are due to:
- **Discriminatory practices** (gender bias), OR
- **Legitimate factors** (experience, department, role type)

---

## 📁 Project Structure

```
houston-lawsuit-analysis/
│
├── Team 6 Lawsuit.R                   # Complete R analysis script
├── Presentation.pdf                   # Executive presentation slides
├── Lawsuit.csv                        # Faculty salary & rank data
│
├── analysis/
│   ├── part1_exploratory.R           # Lines 10-931: Full analysis
│   └── part2_presentation.R          # Lines 935-1499: Visualization code
│
├── outputs/
│   ├── descriptive_stats/            # Summary tables
│   ├── regression_results/           # Model outputs
│   └── visualizations/               # Charts and graphs
│
└── README.md                         # This file
```

---

## 🚀 Getting Started

### Prerequisites

```r
# Required R packages
install.packages(c(
  "data.table",
  "tidyverse",
  "GGally",
  "corrplot",
  "scales",
  "patchwork"
))
```

### Data Source

**Dataset:** `Lawsuit.csv` (Houston College of Medicine faculty data)
- **Records:** 261 faculty members
- **Variables:** 9 attributes (ID, Dept, Gender, Clin, Cert, Prate, Exper, Rank, Sal94, Sal95)
- **Period:** 1994-1995 academic year

### Running the Analysis

```r
# 1. Set your working directory
setwd("path/to/your/data")

# 2. Source the complete analysis
source("Team 6 Lawsuit.R")

# The script is divided into two parts:
# Part 1 (Lines 10-931): Complete analytical process
# Part 2 (Lines 935-1499): Presentation visualizations
```

---

## 🔍 Methodology

### 1. Data Preparation

**Variables:**
- **Department:** Biochemistry, Physiology, Genetics, Pediatrics, Medicine, Surgery
- **Gender:** Female (106), Male (155)
- **Clinical Emphasis:** Research (101) vs Clinical (160)
- **Certification:** Board Certified (188) vs Not Certified (73)
- **Rank:** Assistant (112), Associate (64), Full Professor (85)
- **Experience:** Years since PhD (0-35 years)
- **Salary:** 1994 & 1995 compensation

**Data Quality:**
- No missing values
- No duplicate records
- Consistent data formats

### 2. Descriptive Analytics

#### Staff Demographics

| Category | Distribution | Key Insight |
|----------|--------------|-------------|
| **Gender** | 41% Female, 59% Male | Proportions vary significantly by department |
| **Ranks** | 43% Assistant, 25% Associate, 32% Full Prof | Experience strongly correlates with rank |
| **Experience** | Junior (0-7): 38%, Mid (7-12): 29%, Senior (12+): 33% | Women average 7.5 years, men 12.1 years |
| **Departments** | Medicine (31%), Biochemistry (19%), Surgery (15%), etc. | Salary ranges: $91k (Physiology) to $319k (Surgery) |

#### Salary Distribution Analysis

```
Average Salary by Department (1995):
- Surgery:        $319,218 (88% male)
- Medicine:       $200,000 (62% male)
- Pediatrics:     $150,000 (67% female)
- Biochemistry:   $120,000 (60% male)
- Genetics:       $110,000 (52% female)
- Physiology:     $91,218 (50/50 split)
```

**Key Finding:** Salary differences correlate with **department choice**, not gender within departments.

### 3. Adjusted Experience Analysis

**Problem:** Raw experience years don't account for actual working hours.

**Solution:** Adjust experience based on Stanford (1996) research showing:
- **Female doctors:** 51 hours/week × 46 weeks/year = 2,346 hours/year
- **Male doctors:** 62 hours/week × 47 weeks/year = 2,914 hours/year
- **Female workload:** ~80.5% of male workload

**Formula:**
```
Exper_Actual = Exper_Basic × (51/62) × (46/47)
```

**Result:** When adjusted for actual working hours, **women's promotion patterns align with men's**.

---

## 📈 Statistical Analysis

### 1. Chi-Square Tests: Rank vs Gender

**Hypothesis:** Does gender correlate with rank within experience groups?

| Experience Group | Chi-Square P-value | Interpretation |
|------------------|-------------------|----------------|
| **0-7 years** | 0.057 | No significant correlation |
| **7-12 years** | 0.012** | Significant at α=0.05 |
| **12+ years** | 0.465 | No significant correlation |

**Follow-up for 7-12 years group:**
- Chi-square test: **Salary Level vs Gender** → p = 0.595
- **Conclusion:** Within this group, salary (not gender) is the more critical variable explaining rank differences

### 2. Promotion Timeline Analysis

**Average Years Required for Promotion (Adjusted Experience):**

| Promotion Stage | Female | Male | Difference |
|-----------------|--------|------|------------|
| Assistant → Associate | 2.9 years | 4.3 years | **Women promoted faster** |
| Associate → Full Professor | 2.9 years | 3.3 years | **Women promoted faster** |

**Statistical Test:** No significant correlation between gender and promotion (p > 0.05 for most experience groups)

### 3. Simple Linear Regression

#### Model A: Salary ~ Gender
```r
lm(Sal95 ~ Gender, data = data_clean)
```

| Metric | Value | Interpretation |
|--------|-------|----------------|
| **Coefficient (Male)** | $64,037 | Men earn more on average |
| **P-value** | 3.64e-9*** | Statistically significant |
| **Adj R²** | 0.1226 | Explains only **12.3%** of variance |

**Problem:** "Significance ≠ Large effect"—Gender appears significant but explains very little salary variation.

#### Model B: Salary ~ Experience
```r
lm(Sal95 ~ Exper, data = data_clean)
```

| Metric | Value | Interpretation |
|--------|-------|----------------|
| **Coefficient (Exper)** | $4,547/year | Each year adds $4,547 |
| **P-value** | 1.39e-7*** | Highly significant |
| **Adj R²** | 0.0983 | Explains 9.8% of variance |

### 4. Multivariable Linear Regression (Full Model)

```r
lm(Sal95 ~ Gender + Dept + Rank + Exper + Prate + Cert + Clin, data = data_clean)
```

**Results:**

| Variable | P-value | Significance | Interpretation |
|----------|---------|--------------|----------------|
| **Gender** | 0.423 | ❌ Not significant | Gender does not predict salary |
| **Department** | | | |
| - Physiology | 0.033* | ✅ Significant | Lower salary vs baseline |
| - Genetics | 0.003** | ✅ Significant | Lower salary vs baseline |
| - Pediatrics | 0.037* | ✅ Significant | Lower salary vs baseline |
| - Medicine | 2.91e-15*** | ✅ Highly significant | Higher salary |
| - Surgery | 2e-16*** | ✅ Highly significant | Highest salary |
| **Rank (Linear)** | 1.28e-10*** | ✅ Highly significant | Strong predictor |
| **Experience** | 4.81e-16*** | ✅ Highly significant | **Strongest predictor** |
| **Certification** | 4.09e-6*** | ✅ Significant | Board certified earn more |
| **Clinical Emphasis** | 0.029* | ✅ Significant | Clinical emphasis earns more |

**Model Fit:** Adj R² = **0.90** (explains 90% of salary variation)

**Conclusion:** Once career structure is controlled, **gender has no independent effect on salary**.

### 5. Logistic Regression: High Salary Group (Top 25%)

```r
glm(high_salary ~ Gender + Dept + Rank + Exper + Prate + Cert + Clin,
    family = binomial, data = mod_df)
```

**Results:**

| Variable | P-value | Significance | Odds Ratio Interpretation |
|----------|---------|--------------|---------------------------|
| **Gender** | 0.602 | ❌ Not significant | Gender does not predict high earnings |
| **Experience** | 0.00564** | ✅ Significant | **Only significant predictor** (OR ≈ 1.38) |
| Department | 0.99+ | ❌ Not significant | No effect within high earners |
| Certification | 0.103 | ❌ Not significant | No effect within high earners |
| Clinical Emphasis | 0.395 | ❌ Not significant | No effect within high earners |

**Conclusion:** For top earners, **experience is the sole decisive factor**. Gender plays no role.

---

## 📊 Key Visualizations

### 1. Salary Distribution (1995)
```
[Histogram with density curve showing right-skewed distribution]
Range: $50k - $450k
Mean: ~$150k
Mode: ~$120k
```

### 2. Ranks by Gender
```
Female:                          Male:
┌─────────────────────────┐     ┌─────────────────────────┐
│ Assistant: 65%          │     │ Assistant: 28%          │
│ Associate: 20%          │     │ Associate: 28%          │
│ Full Professor: 15%     │     │ Full Professor: 45%     │
└─────────────────────────┘     └─────────────────────────┘
```
**Note:** Differences disappear when adjusting for actual working hours.

### 3. Salary by Department (with Gender Breakdown)
```
Surgery:      $319k   [█████████] 88% Male
Medicine:     $200k   [████████] 62% Male  
Pediatrics:   $150k   [████] 67% Female
Biochemistry: $120k   [█████] 60% Male
Genetics:     $110k   [████] 52% Female
Physiology:   $91k    [████] 50/50 Split
```

### 4. Average Salary by Dept, Gender & Adjusted Experience

**Finding:** Within each department and experience level, male and female salaries are nearly identical.

Example (Medicine Dept):
- 0-7 years: Female $130k, Male $135k (3.8% difference)
- 7-12 years: Female $185k, Male $180k (-2.7% difference)
- 12+ years: Female $245k, Male $250k (2.0% difference)

### 5. Salary Growth Rate (1994-1995)
```
Female: 10.0% average increase
Male:   9.8% average increase
Difference: +0.2% (not statistically significant)
```

---

## 🔬 Technical Highlights

### Advanced R Techniques

**1. Custom Experience Adjustment Function**
```r
calc_actual_experience <- function(data) {
  data %>%
    mutate(
      weekly_hours = ifelse(Gender == "Male", 62, 51),
      annual_weeks = ifelse(Gender == "Male", 47, 46),
      annual_actual_hours = weekly_hours * annual_weeks,
      actual_hours_total = annual_actual_hours * Exper,
      male_annual_hours = 62 * 47,
      actual_years_male_base = round(actual_hours_total / male_annual_hours, 2)
    )
}
```

**2. Multivariable Regression with Factor Variables**
```r
# Handling ordered factors (Rank) properly
data_clean <- data_clean %>%
  mutate(Rank = factor(Rank, 
                       levels = c("Assistant", "Associate", "Full Professor"),
                       ordered = TRUE))

# Full regression model
m2 <- lm(Sal95 ~ Gender + Dept + Rank + Exper + Prate + Cert + Clin,
         data = data_clean)
```

**3. Logistic Regression for High Salary Group**
```r
# Define top 25% as "high salary"
q75 <- quantile(mod_df$Sal95, 0.75, na.rm = TRUE)
mod_df <- mod_df %>%
  mutate(high_salary = ifelse(Sal95 > q75, 1, 0))

# Logistic model
m_sal_a <- glm(high_salary ~ Gender + Rank + Exper + Dept + Clin + Cert + Prate,
               family = binomial, data = mod_df)
```

### Statistical Methods Applied

1. **Chi-Square Tests** - Independence testing between categorical variables
2. **Simple Linear Regression** - Univariate relationships
3. **Multiple Linear Regression** - Multivariate salary predictors
4. **Logistic Regression** - Binary outcome (high salary yes/no)
5. **Descriptive Statistics** - Mean, median, quartiles by group
6. **Data Transformation** - Experience adjustment calculations
7. **Factor Analysis** - Categorical variable encoding

---

## 📋 Detailed Findings

### Finding 1: No Gender Pay Gap Within Departments

**Evidence:**
- Average salary comparison **by department, gender, and adjusted experience** shows **no systematic differences**
- Example: In Surgery (highest-paying dept), 7-12 year experience group:
  - Female average: $290,000
  - Male average: $295,000
  - Difference: 1.7% (not statistically significant)

**Visual Proof:** See Slide 9 presentation chart showing overlapping salary bars across all departments.

### Finding 2: Women Are Promoted Faster (Adjusted Hours)

**Evidence:**
- After adjusting for actual working hours:
  - Assistant → Associate: Women 2.9 years vs Men 4.3 years
  - Associate → Full Professor: Women 2.9 years vs Men 3.3 years

**Chi-Square Results:**
- No significant gender-rank correlation in most experience groups
- Where correlation exists (7-12 years), **salary** is the confounding variable, not gender

### Finding 3: Experience Explains Everything

**Regression Evidence:**

| Model | R² | Gender Significance | Experience Significance |
|-------|-----|--------------------|-----------------------|
| Gender only | 0.123 | p < 0.001*** | N/A |
| Experience only | 0.098 | N/A | p < 0.001*** |
| **Full model** | **0.90** | **p = 0.423** ❌ | **p < 0.001*** ✅** |

**Interpretation:**
- Gender appears significant in isolation due to **confounding with experience**
- Once experience, department, rank, etc. are controlled, gender effect disappears
- Experience remains the strongest predictor across all models

### Finding 4: Department Choice Matters

**Women's Department Distribution:**
- 67% in Pediatrics (avg salary: $150k)
- 52% in Genetics (avg salary: $110k)
- 50% in Physiology (avg salary: $91k)

**Men's Department Distribution:**
- 88% in Surgery (avg salary: $319k)
- 62% in Medicine (avg salary: $200k)
- 60% in Biochemistry (avg salary: $120k)

**Note:** These choices may reflect **personal preferences, specialization interests, or work-life balance priorities**—not discriminatory hiring practices.

### Finding 5: Identical Salary Growth

**1994-1995 Salary Increase:**
- Female doctors: 10.0% average
- Male doctors: 9.8% average
- Statistical test: p = 0.89 (no significant difference)

**Implication:** Current salary practices show **no evidence of ongoing discrimination**.

---

## 🎓 Legal & Statistical Interpretation

### Statistical Significance vs Practical Significance

**Important Distinction:**
- **Statistical significance (p-value)** tells us if an effect exists
- **Effect size (R²)** tells us if the effect is meaningful

**In this case:**
- Gender has a **statistically significant** effect in simple regression (p < 0.001)
- BUT it explains only **12.3% of variance** (very small effect)
- When proper controls are added, the effect **disappears entirely** (p = 0.423)

### Simpson's Paradox in Action

This case demonstrates **Simpson's Paradox**:
- **Aggregate level:** Women earn less on average ($130k vs $160k)
- **Disaggregated level:** Women earn the same within department-experience groups

**Explanation:** The aggregate difference is due to **compositional effects** (women in lower-paying depts with less experience), not discrimination.

### Legal Standard for Discrimination

**Requirements to prove discrimination:**
1. ✅ Disparity exists (e.g., salary gap)
2. ❌ Disparity cannot be explained by legitimate factors
3. ❌ Evidence of discriminatory intent or practice

**Verdict:** This analysis demonstrates that observed disparities **CAN be explained by legitimate factors** (experience, department, hours worked), failing requirement #2.

---

## 📊 Results Summary

| Claim | Finding | Statistical Evidence |
|-------|---------|---------------------|
| **Women earn less than men** | TRUE in aggregate, FALSE within groups | Simple regression: p < 0.001; Full model: p = 0.423 |
| **Women are promoted slower** | FALSE (when adjusted for hours) | Chi-square: p > 0.05; Promotion time: Women faster |
| **Gender predicts salary** | FALSE (when controlling for structure) | Multivariate R² = 0.90, gender p = 0.423 |
| **Experience predicts salary** | TRUE (sole decisive factor) | All models: p < 0.001, Logistic: OR = 1.38 |
| **Department predicts salary** | TRUE (explains most variance) | Surgery $319k vs Physiology $91k |
| **Systematic discrimination exists** | **NO EVIDENCE** | All tests fail to show gender effect after controls |

---

## 💡 Key Learnings

### Statistical Concepts Demonstrated

1. **Confounding Variables:** Gender appeared significant due to correlation with experience and department
2. **Simpson's Paradox:** Aggregate differences reversed at disaggregated level
3. **Model Specification:** Importance of including relevant control variables
4. **Multiple Testing:** Need for various analytical approaches (chi-square, regression, logistic)
5. **Practical vs Statistical Significance:** Small R² despite significant p-value

### Analytical Skills Applied

- **Data Cleaning:** Factor conversion, missing value handling
- **Feature Engineering:** Adjusted experience calculation
- **Exploratory Data Analysis:** Distribution analysis, correlation matrices
- **Hypothesis Testing:** Chi-square tests for independence
- **Regression Modeling:** Linear and logistic regression
- **Model Comparison:** Simple vs multivariate models
- **Visualization:** Professional presentation-quality charts

### Real-World Application

- **Legal Analytics:** Using statistics to support/refute discrimination claims
- **HR Analytics:** Understanding drivers of compensation decisions
- **Causal Inference:** Distinguishing correlation from causation
- **Communication:** Presenting complex statistical findings to non-technical audiences

---

## 🛠️ Technologies Used

- **R 4.x** - Statistical computing
- **Libraries:**
  - `data.table` - Fast data manipulation
  - `tidyverse` (dplyr, ggplot2, tidyr) - Data wrangling and visualization
  - `GGally` - Extended ggplot2 functionality
  - `corrplot` - Correlation visualization
  - `scales` - Axis formatting
  - `patchwork` - Multiple plot layouts
- **Statistical Methods:**
  - Simple & Multiple Linear Regression
  - Logistic Regression
  - Chi-Square Tests
  - ANOVA
  - Descriptive Statistics

---

## 📈 Presentation Structure

**Slide-by-Slide Breakdown:**

1. **Title Slide** - Case introduction
2. **Staff Facts** - 261 employees, salary distribution, ranks
3. **Five Categories** - Departments, experience, emphasis, certification, gender
4. **Rank Factors** - Department, experience, gender correlations
5. **Salary Factors** - Five main predictors of compensation
6. **Adjusted Experience** - Working hours correction methodology
7. **Promotion Analysis** - Women promoted faster when adjusted
8. **Experience & Department** - Key differences between genders
9. **Pay Gap Explanation** - Within-group comparisons show no gap
10. **Simple Regression** - Gender & experience as univariate predictors
11. **Multiple Regression** - Full model showing experience as sole factor
12. **Regression Summary** - Three models compared
13. **Final Summary** - No evidence of discrimination

---

## 📝 Recommendations

### For Houston College of Medicine

**Defensive Strategy:**
1. **Present this analysis** in court as expert testimony
2. **Emphasize statistical rigor** - multiple methods confirming same conclusion
3. **Highlight adjusted experience** - women working fewer hours explains timing
4. **Show identical growth rates** - current practices are fair
5. **Demonstrate departmental differences** - choice, not discrimination

**Proactive Measures:**
1. **Monitor hiring practices** - ensure no gender bias in recruitment
2. **Track promotion timelines** - continue verifying fairness
3. **Document decision criteria** - transparent standards for raises/promotions
4. **Encourage department diversity** - but respect personal choices
5. **Regular audits** - periodic statistical reviews

### For Analysts

**Best Practices Demonstrated:**
1. Start with **exploratory analysis** before jumping to modeling
2. Use **multiple analytical methods** to triangulate findings
3. **Adjust for known confounders** (e.g., working hours)
4. Present **both aggregate and disaggregated** views
5. Distinguish **statistical from practical** significance
6. Create **professional visualizations** for non-technical audiences

---

## 🔄 Reproducibility

### Full Analysis Workflow

```r
# 1. Load packages
library(data.table)
library(tidyverse)
library(GGally)
library(corrplot)
library(scales)
library(patchwork)

# 2. Set working directory
setwd("path/to/data")

# 3. Run Part 1: Complete Analysis (Lines 10-931)
# - Data cleaning and factor conversion
# - Descriptive statistics
# - Correlation analysis
# - Experience adjustment
# - Promotion analysis
# - Salary comparison by groups
# - Regression models

# 4. Run Part 2: Presentation Visuals (Lines 935-1499)
# - Custom color scheme
# - Slide-specific charts
# - Professional formatting

# 5. Outputs generated automatically
```

### Data Requirements

**Minimum viable dataset:**
- Employee ID
- Department
- Gender
- Years of experience
- Rank (ordinal)
- Salary (at least 2 time points)
- Clinical vs Research emphasis
- Certification status

---

## 📄 License

This project is submitted as academic coursework for AN6003 at Nanyang Technological University. All analysis is based on publicly available case study data.

**Data License:** Educational use only  
**Code License:** MIT License (analysis code)

---

## 🙏 Acknowledgments

- **Course Instructors** for providing the case study and analytical framework
- **Stanford University Research (1996)** for physician working hours data
- **Houston College of Medicine** (fictional case) for the dataset
- **Legal Analytics Community** for discrimination analysis best practices

---

## 📚 References

1. Russel, S. (1996). *Stanford Studies Gender Gap in Doctors' Pay / Longer hours, specialties make the difference.* SFGate. [Link](https://www.sfgate.com/news/article/Stanford-Studies-Gender-Gap-in-Doctors-Pay-2986283.php)

2. James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). *An Introduction to Statistical Learning.* Springer.

3. Wooldridge, J. M. (2015). *Introductory Econometrics: A Modern Approach.* Cengage Learning.

---

## 🔍 Keywords

`gender discrimination`, `salary analysis`, `regression analysis`, `legal analytics`, `HR analytics`, `statistical modeling`, `Simpson's Paradox`, `confounding variables`, `logistic regression`, `equal pay`, `R statistics`, `data-driven decision making`

---

*Last Updated: January 2025*

**Status:** ✅ Analysis Complete | 📊 Presentation Delivered | ⚖️ Legal Defense Ready
