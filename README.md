# Multivariate Analysis 2025 – Final Project

This repository hosts my **Multivariate Analysis Final Project (2025)**. The goal of this project is to **replicate and extend** the findings from:

**Federico Vegetti and Levente Littvay (2022)**  
*Belief in conspiracy theories and attitudes toward political violence*  
**Italian Political Science Review**  
📄 [Read the article](https://www.cambridge.org/core/journals/italian-political-science-review-rivista-italiana-di-scienza-politica/article/belief-in-conspiracy-theories-and-attitudes-toward-political-violence/89835D7B5CD9036BC8717E484D5F4C0B)  
🔗 DOI: [10.1017/ipo.2021.17](https://doi.org/10.1017/ipo.2021.17)

---

## 📦 Data & Materials

- The original dataset and replication materials by the authors are publicly available on Harvard Dataverse:  
  [https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/I7FSYP](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/I7FSYP)

- This repository includes:
  - 📄 **DOCS/Corbellini Filippo_Multivariate Analysis Final Assignment.pdf** – My full replication paper
  - 🧪 **SCRIPTS/00_script.R** – Full R script for data management, CFA, regression models, and extensions

---

## 🔧 Required R Packages

Make sure to install the following R packages before running the script:

```r
install.packages(c("tidyverse", "rio", "corrplot", "psych", "lavaan", "semTools", "texreg"))
