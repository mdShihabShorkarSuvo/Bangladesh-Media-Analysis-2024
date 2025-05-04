# 📊 Bangladesh Media Coverage Analysis 2024 (Al Jazeera)
**Author:** Md. Shihab Shorkar  
**Project Type:** NLP, Topic Modeling, and Sentiment Analysis  
**Tools Used:** R, tidyverse, tm, tidytext, topicmodels, sentimentr, ggplot2, Rtsne  

---

## 📜 Project Overview

This project performs an in-depth analysis of **Al Jazeera’s 2024 news coverage of Bangladesh**, aiming to uncover dominant themes and emotional tones throughout the year. By applying **Latent Dirichlet Allocation (LDA)** topic modeling and **sentiment analysis**, the project extracts insights into the major socio-political and cultural narratives surrounding Bangladesh.

---

## 🔍 Objectives

- Identify major themes in Al Jazeera’s reporting on Bangladesh using unsupervised topic modeling (LDA).
- Measure sentiment trends to understand the emotional framing of these topics.
- Visualize trends and patterns across 12 months to observe media focus shifts.

---

## 📌 Key Features

- 🧼 **Data Preprocessing:**  
  Custom stopword removal, lowercasing, noise cleaning, whitespace stripping, and optional stemming.

- 📊 **TF-IDF Analysis:**  
  Important term identification based on frequency-weighted metrics.

- 🧠 **Topic Modeling (LDA):**  
  Discovery of 6 dominant themes using the `topicmodels` package in R.

- 😐 **Sentiment Analysis:**  
  Sentence-level sentiment scoring using `sentimentr` and `syuzhet` libraries.

- 🎨 **Advanced Visualizations:**  
  - Word clouds per topic  
  - Line plots of topic proportions over months  
  - Sentiment polarity plots per topic  
  - t-SNE plots of topic distributions across documents  

---

## 🗂️ Topics Identified

1. **Protests and Students**  
2. **Governance and Leadership**  
3. **Religion and Minorities**  
4. **Sports and Cricket**  
5. **Refugees and Migration**  
6. **Election and Politics**

---

## 📈 Visual Output

All generated visualizations, including:

- ✅ Word Clouds (Topic-wise)  
- ✅ Topic Trends Over Time  
- ✅ Sentiment Distributions  
- ✅ t-SNE Topic Clusters  

📦 **Can be found inside the attached ZIP file: `AlJazeera_BD_Analysis_Plots.zip`**

---

## ▶️ How to Run

1. Clone this repository to your machine.
2. Make sure the following R packages are installed:

```r
install.packages(c("dplyr", "tm", "SnowballC", "topicmodels", "tidytext", 
                   "ggplot2", "tidyr", "wordcloud", "RColorBrewer", 
                   "sentimentr", "Rtsne"))

source("bangladesh_media_analysis_2024.R")
