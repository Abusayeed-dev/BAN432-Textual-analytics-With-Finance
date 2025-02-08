# BAN432-Textual-analytics-With-Finance
## Textual Analysis of NVIDIA in Bloomberg News

## Candidate Numbers: 11, 23, 40, 54

**Autumn 2024**  
**Course:** Applied Textual Data Analysis for Business and Finance (BAN432)  
**Institution:** Norges Handelshøyskole (Norwegian School of Economics)  
**Grade Achieved:** A  

## Overview
This project investigates the representation of NVIDIA in Bloomberg News between 2016 and 2024 using advanced textual analysis techniques. The study includes sentiment analysis, company similarity evaluations, topic modeling, and stock price correlation analyses, leveraging natural language processing (NLP) and statistical modeling. 

## Tasks and Methods

### Task 1: Corpus Construction
- Created three corpora derived from Bloomberg News transcripts.
- Processed text data by filtering for relevant mentions of NVIDIA, removing stopwords, and transforming it into a document-term matrix.

### Task 2: Word Embedding and Cosine Similarity
- Applied word embedding models to determine the most cosine similar companies to NVIDIA from 2016 to 2024.
- Tracked the evolution of related companies over time using SP500 firm data.

### Task 3: Sentiment, Frequency, and Stock Return Analysis
- Conducted sentiment analysis using three lexicons (Jockers-Rinker, Loughran-McDonald, Socal-Google).
- Analyzed the correlation between sentiment, media frequency, and stock price changes.
- Performed a linear regression with interaction terms to estimate the influence of sentiment and media attention on NVIDIA’s stock returns.

### Task 4: Stock Price and Key Terms Analysis
- Examined unigrams and bigrams associated with stock price movements.
- Applied log-ratio metrics to identify terms strongly linked to high or low stock price periods.

### Task 5: Topic Modeling with LDA
- Implemented Latent Dirichlet Allocation (LDA) to extract key topics discussed about NVIDIA.
- Analyzed how topic prevalence shifted over time, reflecting changes in industry trends and investor sentiment.

## Data Sources
- Bloomberg News articles (2016-2024).
- NVIDIA stock price data from financial market sources.

## Tools & Technologies
- **Programming Language:** R
- **Libraries Used:** tidyverse, tm, quanteda, topicmodels, ggplot2, dplyr, tidyquant
- **NLP Techniques:** Sentiment Analysis, Cosine Similarity, LDA Topic Modeling
- **Statistical Methods:** Correlation Analysis, Linear Regression with Interaction Terms

## Installation & Usage
1. Clone this repository:
   ```sh
   git clone https://github.com/yourusername/NVIDIA_Text_Analysis.git
   ```
2. Install dependencies in R:
   ```r
   install.packages(c("tidyverse", "tm", "quanteda", "topicmodels", "ggplot2", "dplyr", "tidyquant"))
   ```
3. Run the analysis scripts:
   ```r
   source("analysis.R")
   ```

## Results
- Identified significant changes in NVIDIA’s related companies over time.
- Found correlations between sentiment, frequency, and stock price trends.
- Highlighted key unigrams and bigrams associated with stock price fluctuations.
- Uncovered key discussion themes about NVIDIA using topic modeling.

## Contributors
- Candidate Numbers: 11, 23, 40, 54

## License
This project is licensed under the MIT License.

---

For further inquiries, feel free to open an issue or contact the contributors.

