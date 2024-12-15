# **One Way ANOVA Shiny Application**

<p align="center">
  <img src="https://img.shields.io/badge/Language-R-276DC3?style=for-the-badge&logo=r&logoColor=white" alt="R">
  <img src="https://img.shields.io/badge/Framework-Shiny-55C6F8?style=for-the-badge&logo=rstudio&logoColor=white" alt="Shiny">
</p>

## 🎯 Project Overview
This project is a Shiny web application designed to perform a **One-Way ANOVA test**. It offers an intuitive interface for uploading datasets, testing statistical assumptions, performing ANOVA, and conducting post-hoc analysis like Tukey's HSD test.  

---

## 🚀 Features

- **Upload Data**: Load a CSV file and view the dataset directly within the application.
- **Homogeneity Testing**: Conduct Bartlett's test to check for equal variances across groups.
- **One-Way ANOVA**: Perform the statistical test to determine group differences.
- **Post-hoc Analysis**: Run Tukey's HSD test to identify specific group differences.
- **Educational Resource**: Learn about ANOVA assumptions and hypotheses directly within the app.
- **User-Friendly Tabs**: Navigate easily through features like data preview, tests, and results.

---

## 🛠️ Tools & Technologies

- **R Programming Language**
- **Shiny Framework**
- **ggplot2** for visualizations
- **shinythemes** for a polished UI

---

## 🏗️ Architecture & Workflow

1. **Data Upload**: Import a CSV file with numeric values and grouping variables.
2. **Preview Data**: View the uploaded dataset in a structured table format.
3. **Test Assumptions**: Check the homogeneity of variances using Bartlett's test.
4. **Run ANOVA**: Perform the One-Way ANOVA test and view detailed results.
5. **Tukey's Test**: Conduct post-hoc analysis to identify significant group differences.

---

## 📋 How to Run the Application

### 1️⃣ Prerequisites

- Install R (version 4.0 or above)
- Install the required R packages:
  ```R
  install.packages(c("shiny", "ggplot2", "shinythemes"))




# One-way-anova-using-R

One Way Anova
The one-way analysis of variance (ANOVA), also known as one-factor ANOVA,is an extension of independent two-sample t-test for comparing means in a situation where there are more than two groups. In one-way ANOVA, the data is organized into several groups base on one single grouping variable (also called factor variable).


https://varunpatel.shinyapps.io/One-way-anova-using-R/
