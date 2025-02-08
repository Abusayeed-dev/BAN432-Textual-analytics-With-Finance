
# Load  packages
library(tidyverse)       # Includes ggplot2, dplyr, readxl, etc.
library(tidytext)        # Text mining
library(rvest)           # Web scraping
library(tibble)          # Data frames
library(stringr)         # String manipulation
library(quanteda)        # Text analysis
library(wordcloud)       # Word cloud visualization
library(stopwords)       # Stopwords management
library(ggplot2)         # Visualization (part of tidyverse)
library(patchwork)       # Combine ggplots
library(gt)              # Create tables
library(tm)              # Text mining
library(textir)          # Text regression
library(SentimentAnalysis) # Sentiment analysis
library(tokenizers)      # Tokenization
library(slam)            # Sparse matrix operations
library(Matrix)          # Matrix operations
library(udpipe)          # NLP pipeline
library(textdata)        # Text data
library(igraph)          # Network analysis
library(knitr)           # Dynamic reporting
library(vader)           # Sentiment analysis
library(tictoc)          # Timing operations
library(text2vec)        # Vector-based text mining
library(purrr)           # Functional programming (part of tidyverse)
library(writexl)         # Export to Excel
library(readxl)          # Import from Excel (part of tidyverse)
library(lubridate)       # Date manipulation (part of tidyverse)
library(sentimentr)      # Sentiment analysis
library(lexicon)         # Sentiment lexicons
library(parallel)        # Parallel computation
library(SnowballC)       # Text stemming
library(topicmodels)     # Topic modeling
library(furrr)           # Parallelized purrr functions
library(tidyquant)       # Financial analysis and data for SP500
library(stargazer)       # Tables
library(RColorBrewer)    # Color palettes
library(pryr)            # inspection of memory usage if needed

# AI disclosure -----

#> We have used the LLM from OpenAI as a supportive function during coding, 
#> while we specified what we wanted. 
#> We would initiate the coding using various packages and methods, that we were 
#> familiar with. ChatGPT (2024 nov version) was then used as a supportive function in
#> finding errors in the code, or to quickly do  recursive tasks by quickly changing 
#> codes with specified wishes. For instance ChatGPT has sorted the libraries by 
#> categories as you see above. For the most part we knew about the structure of 
#> text mining from class and assignments, and have used class notes where we could. 
#> ChatGPT were then also sometimes used as a dictionary giving quick access 
#> to introductions to topics and packages that we then investigated further. 
#> The code was inspected by us personally and when needed 
#> adjusted to our specific needs. ChatGPT was also used to quickly set up 
#> ggplots such as plots, barplots, etc. When we were unsure of how to use a package
#> we would ask ChatGPT to " Teach me about how to use ...." and then we would 
#> support that with reading package descriptions and other supportive package material.

# The LLM model used: 
# OpenAI. (2024). ChatGPT (November 2024 version). Retrieved from https://chat.openai.com 



# First we will clean one year of data to become familiar with the dataset. 

# ----Working with one year of data: 2016 ---- ----------######

# list of csv file in the directory
wd <- getwd()

files <- list.files(path = wd, 
                     pattern = "\\.csv$", 
                     recursive = T,
                     full.names = T)

# upload dataset as a csv using tidyverse read_csv:
folder <- "/raw_TV_data_2016_to_2024/"
  
df16<- read_csv(paste0(wd, folder, "Bloomberg.Text.2016.1.csv"))
View(df16)
# we notice three columns with link details, date_time for the full episode, and the text column
# create colnames for the dataset df16:
c("link_details", "Date_Time", "Conversation" ) -> colnames(df16)
 

# Access row 2, column 2
cell_value <- df16[2, 2] 
 
# Print the value
print(cell_value) # Bloomberg Businessweek Debrief: A Conversation With Jamie Dimon : BLOOMBERG : December 31; 2016 12:00pm-1:01pm EST
 

 # Extract date-time information using a regular expression
 df16 <- df16 %>%
   mutate(
     date_time = str_extract(Date_Time, "(January|February|March|April|May|June|July|August|September|October|November|December) \\d{1,2}; \\d{4} \\d{1,2}:\\d{2}(am|pm)-\\d{1,2}:\\d{2}(am|pm)"), 
     # one of the months, space, one or two digits, four digits, kolon, one or two digits, am or pm, -, two digits, kolon, space, am or pm. 
   )
 
 # str_extract() helpfiles
 # regex {base}
 
 
 df16 <- df16 %>%
   mutate(
     # Separate date, start time, and end time
     date_part = str_extract(date_time, "(January|February|March|April|May|June|July|August|September|October|November|December) \\d{1,2}; \\d{4}"),
     date_1 = str_extract(date_time, "(January|February|March|April|May|June|July|August|September|October|November|December) \\d{1,2}; \\d{4}"), # the month and two digits month, and four digits year
     start_time = str_extract(date_time, "\\d{1,2}:\\d{2}(am|pm)"),
     end_time = str_extract(date_time, "(?<=-)\\d{1,2}:\\d{2}(am|pm)"),
     
     # Combine date with start and end times and parse to datetime
     start_datetime = mdy_hm(paste(date_part, start_time), tz = "EST"),
     end_datetime = mdy_hm(paste(date_part, end_time), tz = "EST")
   ) %>% select(-c(Date_Time,date_part,start_time,end_time, date_time))
 
# mutate date
df16$date <- format(mdy(df16$date_1),"%Y-%m-%d" ) #lubridate
View(df16)

# for further analysis we filter out the TV shows / entries that mention NVIDIA. later we will evaluate what to use. 
nvidia_conversations <- df16[grepl("\\bNVIDIA\\b", df16$Conversation, ignore.case = TRUE), ] # ignoring large or small cases

# merge rows with same date into one string per day
nvidia_daily_2016  <- nvidia_conversations %>% 
  group_by(date) %>% 
  summarize(Conversation_daily = str_c(Conversation, collapse = " "), .groups = "drop")





# ---- The whole dataset ---- ---- # Corpus 1
# list of csv files in the directory
wd <- getwd()
folder <- "/raw_TV_data_2016_to_2024/"
files <- list.files(path = paste0(wd, folder), 
                    pattern = "\\.csv$", 
                    recursive = T,
                    full.names = T)

results_df <- data.frame()

tic("Looping over all the years of data from Bloomberg Business news took this long: ")
# loop over files, split the texts, and filter rows for NVIDIA:

for (x in files){
  temp <- read_csv(x)
  
  # create a colnames for data dataset
  c("link_details", "Date_Time", "Conversation" ) -> colnames(temp)

  # Extract date-time information using a regular expression
  temp <- temp %>%
    mutate(date_time = str_extract(Date_Time, "(January|February|March|April|May|June|July|August|September|October|November|December) \\d{1,2}; \\d{4} \\d{1,2}:\\d{2}(am|pm)-\\d{1,2}:\\d{2}(am|pm)"))
  
  temp <- temp %>%
    mutate(
      # Separate date, start time, and end time
      date_part = str_extract(date_time, "(January|February|March|April|May|June|July|August|September|October|November|December) \\d{1,2}; \\d{4}"),
      date_1 = str_extract(date_time, "(January|February|March|April|May|June|July|August|September|October|November|December) \\d{1,2}; \\d{4}"), # any of the 12 months and two digits month, and four digits year
      date <- format(mdy(date_1),"%Y-%m-%d" ),
      
      start_time = str_extract(date_time, "\\d{1,2}:\\d{2}(am|pm)"),
      end_time = str_extract(date_time, "(?<=-)\\d{1,2}:\\d{2}(am|pm)"),
      
      # Combine date with start and end times and parse to datetime
      start_datetime = mdy_hm(paste(date_part, start_time), tz = "EST"),
      end_datetime = mdy_hm(paste(date_part, end_time), tz = "EST")
    ) %>% select(-c(Date_Time,date_part,start_time,end_time, date_time))
  
  # mutate date
  temp$date <- format(mdy(temp$date_1),"%Y-%m-%d" ) #lubridate
  
  # for further analysis we filter out the TV shows / entries that mention NVIDIA. later we will evaluate what to use. 
  temp <- temp[grepl("\\bNVIDIA\\b", temp$Conversation, ignore.case = TRUE), ]
  
  # merge rows with same date into one string per day
  temp  <- temp %>% 
    group_by(date) %>% 
    summarize(Conversation_daily = str_c(Conversation, collapse = " "), .groups = "drop")
  
  results_df <- rbind(results_df, temp)
  remove(temp)
}

toc() # Looping over all the years of data from Bloomberg Business news took this long: : 453.178 sec elapsed

save(results_df, file="results_df_nvidia.Rdata")






# ---- DTM, CORPUS 1 -------

# count the amount of times NVIDIA is mentioned:
# tokenize
# create dtm
# convert to tibble
# link "NVIDIA" token with dates
# visualize counts


# if needed load the filtered raw data:
load("results_df_nvidia.Rdata") # Corpus 1

# creating a corous, by first tokenization
toks <- tokenize_words((results_df$Conversation_daily), lowercase = T, stopwords = stopwords("en"), strip_punct = T, strip_numeric = T, simplify = F)
corpus_raw_filtered <- Corpus(VectorSource(toks))

#> tokenize_sentences(): For sentence-level tokenization.
#> tokenize_ngrams(): For n-grams (e.g., bigrams, trigrams).
#> tokenize_regex(): For tokenizing with custom patterns.
#> print(toks[1])

dtm <- DocumentTermMatrix(corpus_raw_filtered,
                          control = list( 
                            removePunctuation = T,
                            stopwords = stopwords("en"),
                            stemming = F,
                            removeNumbers = T,
                            wordLengths = c(2, 20)) ) # we dont want bounds. 4-20 letter words?

# Filter out terms that appear only four times or less
dtm_filtered <- dtm[, findFreqTerms(dtm, lowfreq = 5)]

save(dtm_filtered, file="dtm_nvidia_filtered.Rdata")


# transform the dtm to a tibble for further data manipulation:
tibble <- as_tibble(as.matrix(dtm_filtered))
View(tibble)


# colnames
colnames <- colnames(tibble)
filtered_words <- colnames[nchar(colnames) ==2]
filtered_words

# merge with data
tibble$date <- results_df$date
# move column to the front
tibble <- tibble %>% select(date, everything())
tibble$date <- as.Date(tibble$date)


# testing the different spellings of nvidia

sum(tibble$nvidia) # 9273
sum(tibble$nvidias) # 484
# Before filtering for frequenzy:
sum(tibble$nvidiaat) # 1
sum(tibble$nvidiand) # 1
sum(tibble$nvidian) # 2
sum(tibble$nvidiare) # 2
sum(tibble$nvidiat) # 1
sum(tibble$nvidiahics) # 1
sum(tibble$nvidiaocks) # 1
sum(tibble$nvidianumbers) # 1
sum(tibble$nvidiasin) # 1
sum(tibble$nvidiasualcomm) # 1
sum(tibble$nvidiae) # 1
sum(tibble$nvidiak) # 1
sum(tibble$nvidianvidia) # 1
sum(tibble$nvidiaish) # 1
sum(tibble$nvidiaed) # 1
sum(tibble$nvidiar) # 1


# sum up the different nvidia columns: nvidia and nvidias (or other similar ones not filtered out):
tibble <- tibble %>% mutate(
  nvidia_sum = rowSums(select(., starts_with("nvidia")), na.rm=TRUE)
)

tibble$nvidia_sum
# combine with stock price
  # Uploading datset
  # cleaning dataset: 
    # numeric to numeric, removing dollarsigns
    # as date, and change the format from month-day-year ot year-month-day
  
nvidia_price_df <- read_csv(paste0("NVIDIA_stock_prices.csv"))
nvidia_price_df$Date <- as.Date(nvidia_price_df$Date, format = "%m/%d/%Y")

# substituting the dollarsign in the numerics rows with "" using gsub, and renaming some columns:
nvidia_price_df$`Close/Last` <- as.numeric(gsub("\\$", "", nvidia_price_df$`Close/Last`))
nvidia_price_df$Open <- as.numeric(gsub("\\$", "", nvidia_price_df$Open))
nvidia_price_df$High <- as.numeric(gsub("\\$", "", nvidia_price_df$High))
nvidia_price_df$Low <- as.numeric(gsub("\\$", "", nvidia_price_df$Low))
nvidia_price_df <- nvidia_price_df %>% rename(date = Date)
nvidia_price_df <- nvidia_price_df %>% rename(close = 'Close/Last')

nvidia <- tibble %>% select(date, nvidia_sum)
nvidia_price_count <- full_join(nvidia, nvidia_price_df, by="date")

# saving the dataset, containing columns from the dtm, prices and date:
save(nvidia_price_count, file="nvidia_price_count.Rdata")

# ---- Earnings release dates
# Earnings release dates for NVDIA:
earnings <- read_excel("nvdia_earnings_reports_date.xlsx")
earnings <- earnings %>% rename(Earnings_date = 'Earnings Date', 
                                surprise_eps = 'Surprise(%)',
                                eps_estimate = 'EPS Estimate',
                                reported_eps = 'Reported EPS')


# Getting the clean date
earnings <- earnings %>% 
  mutate(
    Date_clean = str_extract(Earnings_date, "\\b\\w+ \\d{1,2}, \\d{4}\\b"),
    surprise_eps_numeric = ifelse(surprise_eps == "-", NA, parse_number(surprise_eps)),
    Earnings_date = mdy(Date_clean),
    Earnings_marker = -10,
    Direction_for_surprise = as.factor(ifelse(surprise_eps_numeric > 0, "Up",
                             ifelse(surprise_eps_numeric < 0, "Down", "Neutral"))),
    date = Earnings_date
  ) %>% 
  select(-Date_clean)  # This line removes the Date_clean column if desired

print(earnings)

# filter out Na in Direction_for_surprise
earnings <- earnings %>% filter(!is.na(Direction_for_surprise))

# merging with the dataset nvidia_price_count:
earnings_join <- left_join(nvidia_price_count, earnings, by="date")

save(earnings_join, file="earnings_join_daily_values.Rdata")



# ---- VISUALIZATION Corpus 1 -----


# visualize
# https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html 
nvidia_plot <- ggplot(data = earnings_join, aes(x = date)) +
  geom_col(aes(y = nvidia_sum), fill = "blue", width = 5, alpha=1) +       # Bars for count
  geom_line(aes(y = close), color = "darkred", linewidth = 1) +        # Line for price
  geom_col(aes(y = Earnings_marker, fill= Direction_for_surprise), width = 8, alpha =1)+ # vertical lines for earnings release dates.
  scale_y_continuous(
    name = "Times mentioned in Bloomberg News",                                               # Left y-axis for count
    sec.axis = sec_axis(~., name = "Price ($)")                   # Right y-axis for price
  ) +
  scale_fill_manual(
    name = "Earnings surprise",
    values = c("Down" = "red2", "Up" = "green3"),
    labels= c("Down" = "Negative surprise", "Up"="Positive surprise", "NA" ="NA")
  )+
  labs(title = "NVIDIA in Bloomberg News and stock price", x = "Date")+
  labs(subtitle = "with markings of earnings release dates and if reported earnings surprised positively or negatively compared to estimated earnings.")+
  theme_gray()

# Save the plot as a PNG file
ggsave(filename = "nvidia_plot.png", plot = nvidia_plot) # width = 10, height = 6, dpi = 300

# Save the plot datafile:
save(nvidia_plot, file="nvidia_plot.Rdata")
# save as picture file:

load("nvidia_plot.Rdata")





# ---- Corpus 1 description -----

#1. Describe the corpus: Please build a corpus relevant for your analysis, based on the data we provide.
#Describe the final corpus in one table so that the reader understands the size, structure, etc. of your corpus.

"> object_size(corpus_raw_filtered) 
187.63 MB
> corpus_raw_filtered
<<SimpleCorpus>>
Metadata:  corpus specific: 1, document level (indexed): 0
Content:  documents: 929"


"> object_size(dtm) 
46.35 MB
> dtm
<<DocumentTermMatrix (documents: 929, terms: 93379)>>
Non-/sparse entries: 2500556/84248535
Sparsity           : 97%
Maximal term length: 20
Weighting          : term frequency (tf)"



object_size(corpus_raw_filtered) 
#https://rdpeng.github.io/RProgDA/the-role-of-physical-memory.html 



# ---- Corpus 1 - Descriptive tables -----

# Here we create descriptive tables for Corpus 1


# Calculate term frequencies and keep only the top 5,000 terms
top_5000_terms <- names(sort(col_sums(dtm_filtered), decreasing = TRUE))[1:5000]
dtm_top_5000 <- dtm[, top_5000_terms]

# Check the dimensions of the filtered DTM (should be number of documents x 5000)
#dim(dtm_top_5000)


dim(dtm_filtered)
#> dim(dtm_filtered)
#[1]   929 30918

#tibble is the dtm_filtered as tibble

# Get term frequencies and select the top 10 terms
tibble_present <- tibble %>% select(-date)
term_frequencies <- colSums(tibble_present)


top_terms <- sort(term_frequencies, decreasing = TRUE)[1:100]
#   em         timeend    timestart    will         pm         us      think    
#   1689386     195246     195246     132207     107397      85532      76660 

# remove some meaning less terms:
term_frequencies <- term_frequencies[!names(term_frequencies) %in% c("timeend", "timestart", "pm", "am", "em", "bloomberg")]
top_terms <- sort(term_frequencies, decreasing = TRUE)[1:100]

# Calculate summary metrics
total_words <- sum(term_frequencies)
num_docs <- nrow(tibble_present)
vocab_size <- ncol(tibble_present)
avg_words_per_doc <- round(total_words / num_docs, 2)

# Create the combined summary table
corpus_summary <- data.frame(
  Metric = c(
    "Number of Documents",
    "Total Words",
    "Vocabulary Size (Unique Terms)",
    "Average Words per Document"
  ),
  Value = c(
    num_docs,
    total_words,
    vocab_size,
    avg_words_per_doc
  )
)

# Display the combined summary table
print(corpus_summary)

top_ten_terms <- data.frame(
  "Top ten terms:" = c(
    paste(names(top_terms[1])),
    paste(names(top_terms[2])),
    paste(names(top_terms[3])),
    paste(names(top_terms[4])),
    paste(names(top_terms[5])),
    paste(names(top_terms[6])),
    paste(names(top_terms[7])),
    paste(names(top_terms[8])),
    paste(names(top_terms[9])),
    paste(names(top_terms[10]))
  ),
  "Total count" = c(
    as.integer(top_terms[1]),
    as.integer(top_terms[2]),
    as.integer(top_terms[3]),
    as.integer(top_terms[4]),
    as.integer(top_terms[5]),
    as.integer(top_terms[6]),
    as.integer(top_terms[7]),
    as.integer(top_terms[8]),
    as.integer(top_terms[9]),
    as.integer(top_terms[10])
  )
)

print(top_ten_terms)


write_xlsx(corpus_summary, "table_1_corpus.xlsx")
write_xlsx(top_ten_terms, "table_1_terms.xlsx")





# Creating Corpus 2 -----
# Load the original data
load("results_df_nvidia.Rdata")

#View(results_df)

# Function to clean text
clean_text <- function(text) {
  text %>%
    str_replace_all("\\[\\[.*?\\]\\]", "") %>%  # Remove metadata like [[TIME.START]]
    str_replace_all("<.*?>", "") %>%           # Remove HTML tags
    str_replace_all("[^a-zA-Z\\s]", " ") %>%   # Remove special characters and numbers (keep letters and spaces)
    str_squish() %>%                           # Remove extra whitespace
    str_to_lower()                             # Convert to lowercase
}


# Create a new dataset with cleaned text
cleaned_data <- results_df %>%
  mutate(Cleaned_Text = clean_text(Conversation_daily)) %>%
  select(date, Cleaned_Text) # Keep only date and cleaned text

# View the new dataset
#View(cleaned_data)


# Function to extract NVIDIA and context (case-insensitive)
extract_context <- function(text, keyword, pre_words = 50, post_words = 50) {
  # Add `(?i)` to make the regex case-insensitive) 
  pattern <- paste0("(?i)(\\b\\w+\\b\\s*){0,", pre_words, "}", "\\b", keyword, "\\b", "(\\s*\\b\\w+\\b){0,", post_words, "}")
  # Simplify result to a character vector
  matches <- str_extract_all(text, pattern)
  unlist(matches) # Return as a simple character vector
}

# Create a new dataset for NVIDIA-related context with surrounding words # including both nvidia and nvidias
nvidia_context <- cleaned_data %>%
  # filter(str_detect(Cleaned_Text, "(?i)\\bNVIDIA(s)?\\b")) %>% # Case-insensitive filter open for nvidias and nvidia
  mutate(nvidia_context = map(Cleaned_Text, ~ extract_context(.x, "(?i)\\bNVIDIA(s)?\\b", 50, 50))) %>% # https://www.rdocumentation.org/packages/poldis/versions/0.1.2/topics/extract_context 
  unnest(nvidia_context) %>%
  select(date, nvidia_context) # Select only relevant columns

nvidia_context

length(unique(nvidia_context$date)) #897 dates.
length(unique(cleaned_data$date)) #929 dates.

nrow(nvidia_context) #7569



###
# why? 
# Remove duplicate rows from the nvidia_context dataset
nvidia_context <- nvidia_context %>%
  distinct() # removes from 7584 rows to 7533 rows (51 rows). is this a good idea?
nrow(nvidia_context) #7519

# View or inspect the new dataset
print(head(nvidia_context, n=15)) # i still see some identical rows
# View(nvidia_context) 


# Combine rows with the same date into a single string
nvidia_context <- nvidia_context %>%
  group_by(date) %>%
  summarise(nvidia_context_combined = paste(nvidia_context, collapse = " ")) %>%
  ungroup()

# View the combined dataset
head(nvidia_context)
#View(nvidia_context)



# Clean the text data
nvidia_context_clean <- nvidia_context %>%
  mutate(
    nvidia_context_cleaned = nvidia_context_combined %>% 
      # Remove time-related elements
      str_replace_all("\\d{1,2}[:.]\\d{2}(\\s?(am|pm))?", "") %>% # am, pm
      # Remove scanned metadata and unnecessary details
      str_replace_all("scanned .*", "") %>%
      # Remove URLs if present
      str_replace_all("http[s]?://\\S+\\s?", "") %>%
      # Remove punctuation
      str_replace_all("[[:punct:]]", " ") %>%
      # Remove extra whitespaces
      str_replace_all("\\s+", " ") %>%
      # Convert to lowercase for consistency
      tolower()
  ) %>%
  select(date, nvidia_context_cleaned)

# View the cleaned data
head(nvidia_context_clean)
# View(nvidia_context_clean)

# final version of cleaning?
nvidia_context_clean <- nvidia_context %>%
  mutate(
    # Remove time-related elements
    nvidia_context_cleaned = nvidia_context_combined %>% 
      str_replace_all("\\b\\d{1,2}[:.]\\d{2}(\\s?(am|pm))?\\b", "") %>%
      # Remove topic frequency lists
      str_replace_all("topic frequency .*", "") %>%
      # Remove metadata and URLs
      str_replace_all("scanned .*|http[s]?://\\S+\\s?", "") %>%
      # Remove punctuation and repeated spaces
      str_replace_all("[[:punct:]]", " ") %>%
      str_replace_all("\\s+", " ") %>%
      # Remove numbers
      str_replace_all("\\b\\d+\\b", "") %>%
      # Convert to lowercase
      tolower() %>%
      # Remove stop words (requires tidytext stop_words)
      str_remove_all(paste0("\\b(", paste(stop_words$word, collapse = "|"), ")\\b")) %>%
      # Trim spaces
      str_trim()
  ) %>%
  select(date, nvidia_context_cleaned)

head(nvidia_context_clean)
# View first few rows of cleaned data
View(nvidia_context_clean)

save(nvidia_context_clean, file="nvidia_context_clean.Rdata")


# -----------



# ---- Corpus 2 TABLES FOR THE FILTERED DATASET ----
# Here we create descriptive tables for Corpus 2

load("nvidia_context_clean.Rdata")

toks <- tokenize_words((nvidia_context_clean$nvidia_context_cleaned), lowercase = T, stopwords = stopwords("en"), strip_punct = T, strip_numeric = T, simplify = F)
corpus_raw_filtered <- Corpus(VectorSource(toks))



#> tokenize_sentences(): For sentence-level tokenization.
#> tokenize_ngrams(): For n-grams (e.g., bigrams, trigrams).
#> tokenize_regex(): For tokenizing with custom patterns.
#> print(toks[1])

dtm <- DocumentTermMatrix(corpus_raw_filtered,
                          control = list( 
                            removePunctuation = T,
                            stopwords = stopwords("en"),
                            stemming = F,
                            removeNumbers = T))

tibble <- as_tibble(as.matrix(dtm))

# Get term frequencies and select the top 10 terms
#tibble_present <- tibble %>% select(-date)

term_frequencies <- colSums(tibble)


top_terms <- sort(term_frequencies, decreasing = TRUE)[1:100]
#   em         timeend    timestart    will         pm         us      think    
#   1689386     195246     195246     132207     107397      85532      76660 

# remove some meaning less terms:
term_frequencies <- term_frequencies[!names(term_frequencies) %in% c("timeend", "timestart", "pm", "am", "em", "bloomberg")]
top_terms <- sort(term_frequencies, decreasing = TRUE)[1:100]

# Calculate summary metrics
total_words <- sum(term_frequencies)
num_docs <- nrow(tibble)
vocab_size <- ncol(tibble)
avg_words_per_doc <- round(total_words / num_docs, 2)

# Create the combined summary table
corpus_summary <- data.frame(
  Metric = c(
    "Number of Documents",
    "Total Words",
    "Vocabulary Size (Unique Terms)",
    "Average Words per Document"
  ),
  Value = c(
    num_docs,
    total_words,
    vocab_size,
    avg_words_per_doc
  )
)

# Display the combined summary table
print(corpus_summary)

top_ten_terms <- data.frame(
  "Top ten terms:" = c(
    paste(names(top_terms[1])),
    paste(names(top_terms[2])),
    paste(names(top_terms[3])),
    paste(names(top_terms[4])),
    paste(names(top_terms[5])),
    paste(names(top_terms[6])),
    paste(names(top_terms[7])),
    paste(names(top_terms[8])),
    paste(names(top_terms[9])),
    paste(names(top_terms[10]))
  ),
  "Total count" = c(
    as.integer(top_terms[1]),
    as.integer(top_terms[2]),
    as.integer(top_terms[3]),
    as.integer(top_terms[4]),
    as.integer(top_terms[5]),
    as.integer(top_terms[6]),
    as.integer(top_terms[7]),
    as.integer(top_terms[8]),
    as.integer(top_terms[9]),
    as.integer(top_terms[10])
  )
)

print(top_ten_terms)

write_xlsx(corpus_summary, "table_2_corpus.xlsx")
write_xlsx(top_ten_terms, "table_2_terms.xlsx")






# 2. Corpus 1 - Competition questions - all years combined ----

# Use a word embedding model to explore which other firms are connected to, or
# are similar to, NVIDIA. Did this group of firms change over the years?

# the group of firms similar to nvidia over the years: investigate by year.
# create a word embedding model for each year
# list most associated companies (filter columns based upon a list ? )




# if needed load the filtered raw data:
load("results_df_nvidia.Rdata") 
    # uploads results_df 
    # with the filtered raw data (filtered for NVIDIA containing data on a daily basis)


# Tokenize the cleaned text data
# Tokenize text 
tokens <- word_tokenizer(results_df$Conversation_daily)

# Create an iterator over tokens
it <- itoken(tokens, progressbar = FALSE)

# Create a vocabulary and prune to retain only frequent terms
vocab <- create_vocabulary(it, stopwords = stopwords("en")) %>%
  prune_vocabulary(term_count_min = 5)

# Create a vectorizer
vectorizer <- vocab_vectorizer(vocab)

# Create Term-Co-occurrence Matrix with a context window of 5
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)

# Initialize and train the GloVe model
glove_model <- GlobalVectors$new(rank = 50, x_max = 10) 
word_vectors <- glove_model$fit_transform(tcm, n_iter = 20)

# Get the word vector for "NVIDIA"
nvidia_vector <- word_vectors["nvidia", , drop = FALSE] # here we only use nvidia, and not nvidias

# Calculate cosine similarity between "NVIDIA" and all other terms
similar_terms <- sim2(x = word_vectors, y = nvidia_vector, method = "cosine")

# Sort and select the top 100 terms most similar to "NVIDIA"
similar_terms_sorted <- sort(similar_terms[, 1], decreasing = TRUE)[2:501]  # Exclude "NVIDIA" itself
similar_terms_df_all <- data.frame(
  Term = names(similar_terms_sorted),
  Similarity = similar_terms_sorted
)



# Display the terms similar to "NVIDIA"
#print(similar_terms_df_all)
write_xlsx(similar_terms_df_all, "similar_cosine_terms_all.xlsx")






# ---- Corpus 1 - Competition analysis - loop for every year ----



# if needed load the filtered raw data:
load("results_df_nvidia.Rdata") 
# uploads results_df 
# with the filtered raw data (filtered for NVIDIA containing data on a daily basis)


similar_terms_list <- list()

# set up parallel session
#plan(multisession)


for (x in c(2016:2024)){

  split_data <- filter(results_df, year(date) == x)
  
  # Tokenize text 
  tokens <- word_tokenizer(split_data$Conversation_daily)
  
  # Create an iterator over tokens
  it <- itoken(tokens, progressbar = FALSE)
  
  # Create a vocabulary and prune to retain only frequent terms, less than 5 are pruned
  vocab <- create_vocabulary(it, stopwords = stopwords("en")) %>%
    prune_vocabulary(term_count_min = 5)
  
  # Create a vectorizer
  vectorizer <- vocab_vectorizer(vocab)
  
  # Create Term-Co-occurrence Matrix with a context window of 5
  tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)
  
  # Initialize and train the GloVe model
  glove_model <- GlobalVectors$new(rank = 50, x_max = 10) 
  word_vectors <- glove_model$fit_transform(tcm, n_iter = 20)
  
  # Get the word vector for "NVIDIA"
  nvidia_vector <- word_vectors["nvidia", , drop = FALSE] # here we only use nvidia, and not nvidias
  
  # Calculate cosine similarity between "NVIDIA" and all other terms
  similar_terms <- sim2(x = word_vectors, y = nvidia_vector, method = "cosine")
  
  # Sort and select the top 100 terms most similar to "NVIDIA"
  similar_terms_sorted <- sort(similar_terms[, 1], decreasing = TRUE)[2:401]  # Exclude "NVIDIA" itself
  
  similar_terms_annual <- data.frame(
    Term = names(similar_terms_sorted),
    Similarity = similar_terms_sorted, 
    year = x)
  
  similar_terms_list[[as.character(x)]] <- similar_terms_annual
  
}

# notes it uses all 20 iterations. perhaps it could improve to increase that. 

# Combine annual
similar_terms_df <- bind_rows(similar_terms_list)
similar_terms_df <- as_tibble(similar_terms_df)
save(similar_terms_df, file="similar_terms_annual.Rdata")
load("similar_terms_annual.Rdata")

# make tables
similar_20_annual <- similar_terms_df %>% group_by(year) %>% slice_head(n=20) %>% ungroup()
save(similar_20_annual, file="similar_terms_20_annual.Rdata")


write_xlsx(similar_terms_df, "similar_cosine_annual.xlsx")

# Show in a table the cosine simlar terms
similar_terms_df %>% group_by(year) %>% slice_head(n=10) %>% ungroup()


  
# ---- Corpus 1 - Filtering companies based on constituents in present sp500 ----

#> we have tried to find a list of historical index constituents on WRDS, but they had removed the list. 
#> Instead we will use todays constituent list from tidyquant, knowingly that 
#> there will be a survival bias. tidyquant uses this website: https://www.ssga.com/us/en/intermediary/fund-finder


sp500 <- tq_index("SP500") # tidyquant uses: https://www.ssga.com/us/en/intermediary/fund-finder 


# inspecting company names
sp500$company %>% sort()

#> the companies are listed in capital letters with name nad either corp, inc, grp, plc. 
#> Our cosine model uses individual terms so far, and in the daily spoken language, 
#> companies are most likely mentioned as apple, and not apple corp. Therefor we 
#> will filter using noly the first word of company names. this will rule out some
#> company name components for instance "united parcel service" would be "united" if only the first 
#> term is used. Using bigrams might not solve this, since we are using raw transcripts. 

#> we will use the list similar_terms_sorted, with 100 terms for each year, and the full
#> list for the whole dataset as a whole
#> 
similar_terms_df_all <- read_excel("similar_cosine_terms_all.xlsx") 
    # 500 terms most associated with nvidia during whole period as a whole, ie. 2016-2024

load("similar_terms_annual.Rdata") 
    # similar_terms_df
    # 100 terms per year for each year isolated, in total 900

# using only the first word from the list of companies by extracting the first word:
sp500 <- sp500 %>% mutate(
  company_name = word(company, 1), # the first word in the company name
  company_name = str_replace(company_name, "\\..*$", ""), # remove .com from amazon.com
  company_name = tolower(company_name)) # make lower letters. 

# remove one of the google stocks (goog or googl) but keep alphabet
sp500 <- sp500 %>% filter(sp500$symbol != "GOOG")

# remove PSA Public Storage company from the list to avoid confusion with PEG
# public service enterprise:
sp500 <- sp500 %>% filter(sp500$symbol != "PSA")


list_sp500 <- sp500$company_name %>% sort() # is now a list of company names in sp500 at present day
length(unique(list_sp500)) #480
length(list_sp500) #504

# add company names that we think are relevant to NVIDIA but not in sp500:
# openai
list_sp500 <- append(list_sp500, c("openai", "google", "facebook"))




# apparently there are a few companies that have the same name after the text rendering. 



# Similar companoes for the whole period ----
# Filtering the list based on the full period:
company_list_in_data <- similar_terms_df_all %>% filter(Term %in% list_sp500)
str(company_list_in_data) # data.frame

# Recombining with their full name (where applicable, with some risk of malpacement )
similar_companies_full_data <- left_join(company_list_in_data, sp500, by= c("Term" = "company_name")) %>% select(Term, Similarity, symbol, company)

# get the 10 most simlar companies
similar_companies_full_data %>% slice_head(n=20)
write_xlsx(similar_companies_full_data, "similar_companies_full_data.xlsx")



# similar companies for each year isolated wihtin the 100 most simliar terms:
similar_terms_df # 900 rows, 100 per year
company_list_annual <- similar_terms_df %>% filter(Term %in% list_sp500)
similar_companies_annual <- left_join(company_list_annual, sp500, by= c("Term" = "company_name")) %>% select(Term, Similarity, symbol, company, year)
# remove one of the google stocks classes:
similar_companies_annual <- similar_companies_annual %>% filter(symbol != "GOOG")

similar_companies_annual <- similar_companies_annual %>% group_by(year) %>% mutate(
  Rank = row_number()
) %>% ungroup()


# get the 5 most similar companies for each year within 100 most similar terms:
similar_companies_annual_5 <- similar_companies_annual %>% group_by(year) %>% slice_head(n = 5) %>% ungroup() %>% print(n=60)

write_xlsx(similar_companies_annual, "similar_companies_annual.xlsx")
write_xlsx(similar_companies_annual_5, "similar_companies_annual_5.xlsx")


# Overview of counts of individual companies
frequenzy <- similar_companies_annual_5 %>% group_by(symbol) %>% summarise(Frequency = n()) %>% arrange(desc(Frequency))
frequenzy <- left_join(frequenzy,sp500 , by="symbol") %>% select(symbol, company, Frequency)
write_xlsx(frequenzy, "frequenzy_annual_5.xlsx")


# create a visual representation of the most similar companies:

# barplot?
library(ggplot2)
library(dplyr)

# Filter data to ensure you have data for all 9 years
# Assuming there is a column named 'Year' in the dataset
years <- unique(similar_companies_annual_5$year)

print(similar_companies_sorted, n=45)
#reorder(category, value, 
#        FUN = function(x) sum(x))

# Create barplots with independent x-axes and sorted Tickers
#https://www.geeksforgeeks.org/order-bars-in-ggplot2-bar-graph/

ggplot(similar_companies_sorted, aes(x = symbol, y = Similarity, fill = Similarity)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year, ncol = 3, scales = "free_x") +  # Independent x-axis per plot
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Companies with highest cosine similarity to Nvidia by year",
       x = "Ticker",
       y = "Similarity")


# Set up a 3x3 plotting grid
par(mfrow = c(3, 3), mar = c(5, 4, 4, 2))  # Adjust margins for better spacing

# Loop through each year and create a barplot
unique_years <- unique(similar_companies_annual_5$year)

for (Year in unique_years) {
  # Filter and sort data for the current year
  year_data <- subset(similar_companies_annual_5, year == Year)
  year_data <- year_data[order(-year_data$Similarity), ]  # Sort in descending order
  
  barplot(
    height = year_data$Similarity,
    names.arg = year_data$symbol,
    main = paste("Year:", Year),
    las = 2,  # Rotate x-axis labels for readability
    col = "lightblue",
    cex.names = 1,  # Adjust size of x-axis labels
    ylim = c(0, max(similar_companies_annual_5$Similarity, na.rm = TRUE))  # Consistent y-axis range
  )
}

# Set up a 3x3 plotting grid
par(mfrow = c(3, 3), mar = c(5, 4, 4, 2))  # Adjust margins for better spacing

# Loop through each year and create a barplot with unique colors
unique_years <- unique(similar_companies_annual_5$year)

for (Year in unique_years) {
  # Filter and sort data for the current year
  year_data <- subset(similar_companies_annual_5, year == Year)
  year_data <- year_data[order(-year_data$Similarity), ]  # Sort in descending order
  
  # Generate a unique color for each Ticker (symbol)
  colors <- rainbow(length(year_data$symbol))  # Use a palette like rainbow
  
  # Create the barplot
  barplot(
    height = year_data$Similarity,
    names.arg = year_data$symbol,
    main = paste("Year:", Year),
    las = 2,  # Rotate x-axis labels for readability
    col = colors,  # Assign unique colors
    cex.names = 1,  # Adjust size of x-axis labels
    ylim = c(0, max(similar_companies_annual_5$Similarity, na.rm = TRUE))  # Consistent y-axis range
  )
}


# Set up a 3x3 plotting grid
par(mfrow = c(3, 3), mar = c(5, 4, 4, 2))  # Adjust margins for better spacing

# Get unique symbols across all years and assign colors
unique_symbols <- unique(similar_companies_annual_5$symbol)
symbol_colors <- setNames(rainbow(length(unique_symbols)), unique_symbols)  # Map symbols to colors

# Loop through each year and create a barplot with consistent colors
unique_years <- unique(similar_companies_annual_5$year)

for (Year in unique_years) {
  # Filter and sort data for the current year
  year_data <- subset(similar_companies_annual_5, year == Year)
  year_data <- year_data[order(-year_data$Similarity), ]  # Sort in descending order
  
  # Assign colors based on the consistent mapping
  colors <- symbol_colors[year_data$symbol]
  
  # Create the barplot
  barplot(
    height = year_data$Similarity,
    names.arg = year_data$symbol,
    main = paste("Year:", Year),
    las = 2,  # Rotate x-axis labels for readability
    col = colors,  # Use consistent colors
    cex.names = 1,  # Adjust size of x-axis labels
    ylim = c(0, max(similar_companies_annual_5$Similarity, na.rm = TRUE))  # Consistent y-axis range
  )
}

# Add a global legend (optional)
par(mfrow = c(1, 1))  # Reset plotting grid
plot.new()  # Open a new blank plot
legend("center", legend = names(symbol_colors), fill = symbol_colors, cex = 0.8, ncol = 2)



# Creating corpus 2 ------------

# Load the original data
load("results_df_nvidia.Rdata")

#View(results_df)

# Function to clean text
clean_text <- function(text) {
  text %>%
    str_replace_all("\\[\\[.*?\\]\\]", "") %>%  # Remove metadata like [[TIME.START]]
    str_replace_all("<.*?>", "") %>%           # Remove HTML tags
    str_replace_all("[^a-zA-Z\\s]", " ") %>%   # Remove special characters and numbers (keep letters and spaces)
    str_squish() %>%                           # Remove extra whitespace
    str_to_lower()                             # Convert to lowercase
}


# Create a new dataset with cleaned text
cleaned_data <- results_df %>%
  mutate(Cleaned_Text = clean_text(Conversation_daily)) %>%
  select(date, Cleaned_Text) # Keep only date and cleaned text

# View the new dataset
#View(cleaned_data)

# Function to extract NVIDIA and context (case-insensitive)
extract_context <- function(text, keyword, pre_words = 50, post_words = 50) {
  # Add `(?i)` to make the regex case-insensitive) 
  pattern <- paste0("(?i)(\\b\\w+\\b\\s*){0,", pre_words, "}", "\\b", keyword, "\\b", "(\\s*\\b\\w+\\b){0,", post_words, "}")
  # Simplify result to a character vector
  matches <- str_extract_all(text, pattern)
  unlist(matches) # Return as a simple character vector
}

# Create a new dataset for NVIDIA-related context with surrounding words # including both nvidia and nvidias
nvidia_context <- cleaned_data %>%
  # filter(str_detect(Cleaned_Text, "(?i)\\bNVIDIA(s)?\\b")) %>% # Case-insensitive filter open for nvidias and nvidia
  mutate(nvidia_context = map(Cleaned_Text, ~ extract_context(.x, "(?i)\\bNVIDIA(s)?\\b", 50, 50))) %>% # https://www.rdocumentation.org/packages/poldis/versions/0.1.2/topics/extract_context 
  unnest(nvidia_context) %>%
  select(date, nvidia_context) # Select only relevant columns

nvidia_context

length(unique(nvidia_context$date)) #897 dates.
length(unique(cleaned_data$date)) #929 dates.

nrow(nvidia_context) #7569



###
# why? 
# Remove duplicate rows from the nvidia_context dataset
nvidia_context <- nvidia_context %>%
  distinct() # removes from 7584 rows to 7533 rows (51 rows). is this a good idea?
nrow(nvidia_context) #7519

# View or inspect the new dataset
print(head(nvidia_context, n=15)) # i still see some identical rows
# View(nvidia_context) 


# Combine rows with the same date into a single string
nvidia_context <- nvidia_context %>%
  group_by(date) %>%
  summarise(nvidia_context_combined = paste(nvidia_context, collapse = " ")) %>%
  ungroup()

# View the combined dataset
head(nvidia_context)
#View(nvidia_context)



# Clean the text data
nvidia_context_clean <- nvidia_context %>%
  mutate(
    nvidia_context_cleaned = nvidia_context_combined %>% 
      # Remove time-related elements
      str_replace_all("\\d{1,2}[:.]\\d{2}(\\s?(am|pm))?", "") %>% # am, pm
      # Remove scanned metadata and unnecessary details
      str_replace_all("scanned .*", "") %>%
      # Remove URLs if present
      str_replace_all("http[s]?://\\S+\\s?", "") %>%
      # Remove punctuation
      str_replace_all("[[:punct:]]", " ") %>%
      # Remove extra whitespaces
      str_replace_all("\\s+", " ") %>%
      # Convert to lowercase for consistency
      tolower()
  ) %>%
  select(date, nvidia_context_cleaned)

# View the cleaned data
head(nvidia_context_clean)
# View(nvidia_context_clean)

# unnecessary? 
nvidia_context_clean <- nvidia_context %>%
  mutate(
    # Remove time-related elements
    nvidia_context_cleaned = nvidia_context_combined %>% 
      str_replace_all("\\b\\d{1,2}[:.]\\d{2}(\\s?(am|pm))?\\b", "") %>%
      # Remove topic frequency lists
      str_replace_all("topic frequency .*", "") %>%
      # Remove metadata and URLs
      str_replace_all("scanned .*|http[s]?://\\S+\\s?", "") %>%
      # Remove punctuation and repeated spaces
      str_replace_all("[[:punct:]]", " ") %>%
      str_replace_all("\\s+", " ") %>%
      # Remove numbers
      str_replace_all("\\b\\d+\\b", "") %>%
      # Convert to lowercase
      tolower() %>%
      # Remove stop words (requires tidytext stop_words)
      str_remove_all(paste0("\\b(", paste(stop_words$word, collapse = "|"), ")\\b")) %>%
      # Trim spaces
      str_trim()
  ) %>%
  select(date, nvidia_context_cleaned)

head(nvidia_context_clean)
# View first few rows of cleaned data
View(nvidia_context_clean)

save(nvidia_context_clean, file="nvidia_context_clean.Rdata")



#3. Corpus 2 - Tone and quantity of coverage: ---- ############################ 
load("nvidia_context_clean.Rdata") # this is corpus 2


# Function to clean text
clean_text <- function(text) {
  text %>%
    str_replace_all("\\[\\[.*?\\]\\]", "") %>%  # Remove metadata like [[TIME.START]]
    str_replace_all("<.*?>", "") %>%           # Remove HTML tags
    str_replace_all("[^a-zA-Z\\s]", " ") %>%   # Remove special characters and numbers (keep letters and spaces)
    str_squish() %>%                           # Remove extra whitespace
    str_to_lower()                             # Convert to lowercase
}


# Create a new dataset with cleaned text
cleaned_data <- results_df %>%
  mutate(Cleaned_Text = clean_text(Conversation_daily)) %>%
  select(date, Cleaned_Text) # Keep only date and cleaned text


# remove excess witespcae?
df <- nvidia_context_clean %>% mutate(nvidia_text = str_squish(nvidia_context_clean$nvidia_context_cleaned))
word <- df$word_count <- str_count(df$nvidia_text, "\\S+")
sum(word) #113273

words <- nvidia_context_clean$word_count <- str_count(nvidia_context_clean$nvidia_context_cleaned, "\\S+")
sum(words) #113273

# it looks better but doesnt affect the word count. 



# Tone and quantity of coverage: Explore if tone and quantity of NVIDIA’s TV coverage correlates
# with its stock price.

# sentiment score per day
# count per day

#> thoughts for the approach
#> the stock could be building momentum depending on the amount of atention it gets in the emdia. 
#> based upon the sentiment this shold be either a negative or positive thing. we want to make a lienar model 
#> investigating the importance of mediaattention, sentiment and their interaction term. 
#> 
#> Then we can also investigate if there is some correlation between stock moentum, media attention momentum and sentiment (momentum)
#> 
#> the dependent variable is the return, and the independent variables:
#> 
#> same day count
#> laged days count: 1, 2, 3, 4, 5,
#> a moving average for the count: 2, 3, 4, 5, 10 days of moving averages
#> 
#> sentiment same day score
#> lagged sentiment score: days 1, 2, 3, 4, 5
#> a moving aveage for the count: 2, 3, 4, 5, 10 days of moving averages

#



# ---- Sentiment Analysis - Corpus 1 and 2 ##############################
load("nvidia_context_clean.Rdata")
load("results_df_nvidia.Rdata")
range(nvidia_context_clean$date) # [1] "2016-01-15" "2024-06-27"
length(unique(nvidia_context_clean$date)) # 897
# The data for this part of the assignment


# CORPUS 1
# Function to clean text
clean_text <- function(text) {
  text %>%
    str_replace_all("\\[\\[.*?\\]\\]", "") %>%  # Remove metadata like [[TIME.START]]
    str_replace_all("<.*?>", "") %>%           # Remove HTML tags
    str_replace_all("[^a-zA-Z\\s]", " ") %>%   # Remove special characters and numbers (keep letters and spaces)
    str_squish() %>%                           # Remove extra whitespace
    str_to_lower()                             # Convert to lowercase
}


# Create a new dataset with cleaned text
cleaned_data <- results_df %>%
  mutate(Cleaned_Text = clean_text(Conversation_daily)) %>%
  select(date, Cleaned_Text) # Keep only date and cleaned text



# use the file
#> using cleaned_data we have the full tv sjhows, and that would include 
#> data about a lot of other companies. Fro this analysis we will first analyze this data. And efter that we will 
#> compare with a narrower dataset. 

#> we will perform an analysis on the cleaned_data and later on a filtered dataset for contextual word embedding. 


# Perform text-level sentiment analysis and calculate average sentiment per document
# Here we choose a lexicon with sentiment loughan mcdonald, which is a finance 
# https://sraf.nd.edu/loughranmcdonald-master-dictionary/ 

# the sentiment is the sentiment related to the word nvidia, by weighting others words that are used in the context of nvidia. 
# we then use this for each days extracted texts. 

# https://www.tidytextmining.com/sentiment 

#> we have considered what lexicon to use for the sentiment analysis. since the text is from a news
#> room with people talking, and not from a finance report, we think that we should find a 
#> sentiment lexocin that takes into account that it is spoken words, as well as the subject is a company. 
#> 
#> in tidytextminin.com/sentiment they decribe three lexicons focusing on single words. 
#> the bing and nrc lexicon is a binary classification
#> the AFINN lexicon assignes word siwth a score form negative -5 to positive 5. 
#> 

 # chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://cran.r-project.org/web/packages/lexicon/lexicon.pdf
#get_sentiments(lexicon = "afinn")
range(hash_sentiment_loughran_mcdonald$y )
  # positive or negative () is-1 to 1
  # https://sraf.nd.edu/loughranmcdonald-master-dictionary/ # Accounting and finance

range(hash_sentiment_jockers_rinker$y) # (-2 to 1)
  # https://rdrr.io/cran/lexicon/man/hash_sentiment_jockers_rinker.html (costumer reviews (?)) 
  # a more general sentiment lexicon

hash_sentiment_socal_google 
  # caputres the emotional eintensity of words (from -30 to +30)

# quick calculation of the large datset I try to make it run with parllel compuing and splitting the text
# into years:


# Split the data by year
cleaned_data$Year <- format(as.Date(cleaned_data$date), "%Y") # Extract year from date
data_split <- split(cleaned_data, cleaned_data$Year)

# function for sentiment analysis
compute_sentiments <- function(data) {
  data$loughran_mcdonald <- sentiment_by(
    data$Cleaned_Text,
    polarity_dt = hash_sentiment_loughran_mcdonald
  )$ave_sentiment
  
  data$jockers_rinker <- sentiment_by(
    data$Cleaned_Text,
    polarity_dt = hash_sentiment_jockers_rinker
  )$ave_sentiment
  
  data$socal_google <- sentiment_by(
    data$Cleaned_Text,
    polarity_dt = hash_sentiment_socal_google
  )$ave_sentiment
  
  data$daily_count <- str_count(data$Cleaned_Text, "(?i)\\bNVIDIA(s)?\\b")
  
  return(data)
}

results <- parLapply(data_split, compute_sentiments)



# Corpus 2 - Sentiment analysis of narrow data ----
# Split the data by year
nvidia_context_cleaned$Year <- format(as.Date(nvidia_context_clean$date), "%Y") # Extract year from date
data_split <- split(nvidia_context_clean, nvidia_context_clean$Year)

# function for sentiment analysis
compute_sentiments <- function(data) {
  data$loughran_mcdonald <- sentiment_by(
    data$nvidia_context_cleaned,
    polarity_dt = hash_sentiment_loughran_mcdonald
  )$ave_sentiment
  
  data$jockers_rinker <- sentiment_by(
    data$nvidia_context_cleaned,
    polarity_dt = hash_sentiment_jockers_rinker
  )$ave_sentiment
  
  data$socal_google <- sentiment_by(
    data$nvidia_context_cleaned,
    polarity_dt = hash_sentiment_socal_google
  )$ave_sentiment
  
  data$daily_count <- str_count(data$nvidia_context_cleaned, "(?i)\\bNVIDIA(s)?\\b")
  
  return(data)
}

results <- lapply(data_split, compute_sentiments)

sentiment_narrow <- do.call(rbind, results)
library(lubridate)

sentiment_narrow <- sentiment_narrow %>% 
  mutate(
    date <- as.Date(date)
  )

save(sentiment_narrow, file="sentiment_narrow.Rdata")



# merge with pricedata
load("earnings_join_daily_values.Rdata") # including data from yahoo finance for earnigns calls of eps and the surprise. 

earnings_join %>% 
  arrange(date) %>% 
  select(date, nvidia_sum) 


# merging
# arranging date
sentiment_narrow <- sentiment_narrow %>% 
  mutate(
    date = as.Date(date)
  )
earnings_join <- earnings_join %>% 
  mutate(
    date = as.Date(date)
  )

cleaned_data_w_prices_sentiment <- left_join(earnings_join, sentiment_narrow, by = "date")

# saving
save(cleaned_data_w_prices_sentiment, file ="cleaned_data_w_prices_sentiment.Rdata")






# missing values:
# nvidia is only being mentioned sometimes in blommberg news. this would be an
# indicator of being mentioned in others news as well, however... a missing value 
# indicates little to no attention from Bloomberg News.the discussio is largely 
# around earnigns release dates. when not mentioned we could say that the sentiment 
# would be neutral? or as the last observed value. 

load("cleaned_data_w_prices_sentiment.Rdata")

# so far we use a linear regression to estimate a relationship between sentiment, 
# quantity and daily stock return. 


colnames(cleaned_data_w_prices_sentiment)

# Data for the linear regression:
lin_reg <- cleaned_data_w_prices_sentiment %>% 
  select(date, close, jockers_rinker, loughran_mcdonald, socal_google, daily_count)

#[1] "date"                   "nvidia_sum"             "close"                  "Volume"                 "Open"                   "High"                   "Low"                    "Symbol"                 "Earnings_date"         
#[10] "eps_estimate"           "reported_eps"           "surprise_eps"           "surprise_eps_numeric"   "Earnings_marker"        "Direction_for_surprise" "nvidia_context_cleaned" "Year"                   "loughran_mcdonald"     
#[19] "jockers_rinker"         "socal_google"           "daily_count"


# Data for plotting (using melt to tranform to a long format suitable for ggplot2)
library(reshape2)
library(ggplot2)
library(ggplot2)
library(dplyr)


# Prepare data for plotting by melting, using Close.Last instead of Close
plot_data <- lin_reg %>%
  select(date, close, jockers_rinker, loughran_mcdonald, socal_google, daily_count) %>% 
  melt(id.var = "date")
                                                       


ggplot() +
  geom_line(
    data = plot_data %>% filter(variable == "close"), 
    aes(x = date, y = value, colour = variable)
  ) +

  geom_point(
    data = plot_data %>% filter(variable %in% c("jockers_rinker", "loughran_mcdonald", "socal_google")),
    aes(x = date, y = value, colour = variable)
  ) +
  
  geom_smooth(
    data= plot_data %>% filter (variable %in% c("jockers_rinker", "loughran_mcdonald", "socal_google")),
    aes(x = date, y = value, colour = variable),
    method = "loess",
    se = TRUE
  ) +
  
  geom_bar(
    data = plot_data %>% filter( variable == "daily_count"),
    aes(x = date, y =value, fill = 'red',width = 5, alpha = 1),
    stat = "identity"
  ) +
  
  # Facet by variable with free y-scales
  facet_grid(variable ~ ., scales = "free_y") +
  
  # Add labels and title
  labs(color = 'Price and sentiment:',
       fill = 'Daily frequenzy') +
  ggtitle("NVIDIA: Daily Sentiment, Frequenzy and Stock Price")




# With bars:
sentiment_plot <- ggplot() +
  geom_line(
    data = plot_data %>% filter(variable == "close"), 
    aes(x = date, y = value, colour = variable)
  ) +
  geom_bar(
    data = plot_data %>% filter(variable %in% c("daily_count", "jockers_rinker", "loughran_mcdonald", "socal_google")),
    aes(x = date, y = value, fill = variable, alpha =1),
    stat = "identity",
    width = 8
  ) +

  # Facet by variable with free y-scales
  facet_grid(variable ~ ., scales = "free_y") +
  
  ggtitle("NVIDIA: Daily Sentiment, Frequenzy and Stock Price")+
  theme(
    legend.position = "bottom"
  )


ggsave("")



# Corpus 2 - LINEAR REGRESSIONS FOR QUANTITY AND SENTIMENT


# ---- Linear regression for count and return ----
# Calculating features, including daily return based upon the provided close:
# working with continuos price data
load("cleaned_data_w_prices_sentiment.Rdata")

lin_reg <- cleaned_data_w_prices_sentiment %>% 
  select(date, close, jockers_rinker, loughran_mcdonald, socal_google, daily_count) 
#%>% 
 # mutate(
#    daily_count_jockers_rinker = daily_count*jockers_rinker,
#    daily_count_loughran_mcdonald = daily_count*loughran_mcdonald, 
#    daily_count_socal_google = daily_count*socal_google
#  )

lin_reg <- lin_reg %>% 
  arrange(date) %>% 
  mutate( #lagged returns
    lag_ret0 = log(close / lag(close, 1)),
    lag_ret1 = log(lag(close, 1) / lag(close, 2)), 
    lag_ret2 = log(lag(close, 2) / lag(close, 3)),
    lag_ret3 = log(lag(close, 3) / lag(close, 4))
  ) %>% 
  mutate(
    lag_count1 = lag(daily_count, 1), #,
    lag_count2 = lag(daily_count, 2),
    lag_count3 = lag(daily_count, 3)
  ) %>% mutate(
    lag_jocker1 = lag(jockers_rinker, 1),
    lag_loughran1 = lag(loughran_mcdonald, 1),
    lag_socal1 = lag(socal_google, 1)
  )

library(tidyquant)
library(tidymodels)
# with interaction terms -----
# model fit
model_return_simple <- lm(lag_ret0 ~ daily_count*jockers_rinker + daily_count*loughran_mcdonald + daily_count*socal_google, data = lin_reg)
senntiemet_model <- tidy(model_return_simple)
writexl::write_xlsx(senntiemet_model, "sentiment_lin_reg.xlsx")

summary(model_return_simple)

# correlation matirx
library(corrplot)
corr_data <- lin_reg %>% select(-date, -close) %>%  # lin_reg is the dataset prepared for linear regression analysis and correlation in previous step
  select(lag_ret0, daily_count, lag_ret1, lag_count1, 
         jockers_rinker, loughran_mcdonald, socal_google)

sentiments_scores <- lin_reg %>% select(jockers_rinker, loughran_mcdonald, socal_google)
corr_sentiment <- cor(sentiments_scores, use="complete.obs")
corrplot(corr_sentiment, 
         method = "color", 
         type = "upper", 
         tl.col = "black", 
         tl.srt=45,
         addCoef.col = "black")


corr_data <- corr_data %>% 
  rename(
    "Daily return" = "lag_ret0",
    "Daily frequency" = "daily_count",
    "Lag1 Return" = "lag_ret1",
    "Lag1 Frequency" = "lag_count1",
    "Jockers-Rinker" = "jockers_rinker",
    "Loughran-McDonald" = "loughran_mcdonald", 
    "Socal-Google" = "socal_google"
  )
cor_matrix <- cor(corr_data, use = "complete.obs") #not data
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         tl.col = "black", 
         tl.srt=45,
         addCoef.col = "black")

corr_list <- colnames(corr_data)
save()


corr_matrix_df <- as.data.frame(cor_matrix)
writexl::write_xlsx(corr_matrix_df, "corr_matrix_df.xlsx")

# dependent variable = return ---- 
# a simple linear model using daily values (no lags):
# model fit
#model_return_simple <- lm(lag_ret0 ~ daily_count + close + jockers_rinker + loughran_mcdonald + socal_google, data = lin_reg)
#senntiemet_model <- tidy(model_return_simple)

#library(writexl)
#write_xlsx(senntiemet_model, "sentiment_lin_reg.xlsx")
#
#summary(model_return_simple)
#Residuals:
#      Min        1Q    Median        3Q       Max 
#-0.202588 -0.021383 -0.000144  0.019723  0.235517 
#
#Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)   
#(Intercept)        2.622e-03  2.544e-03   1.031  0.30310   
#daily_count        2.076e-04  3.681e-04   0.564  0.57287   
#close             -2.513e-05  5.437e-05  -0.462  0.64404   
#jockers_rinker     1.502e-02  4.580e-03   3.280  0.00109 **
#loughran_mcdonald  1.254e-02  5.113e-03   2.453  0.01438 * 
#socal_google      -1.149e-03  1.889e-03  -0.608  0.54328   
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# linear regression only estimates on days with observations. limited to days with observations. 

# using a few lagged counts
#model_return_moderate <- lm(lag_ret0 ~ daily_count + lag_count1 + lag_count2 + lag_count3, data = lin_reg)
#tidy(model_return_moderate)
#summary(model_return_moderate)


# dependent variable = count ----
# a simple linear model
# model fit
#model_count_simple <- lm(daily_count ~ lag_ret0, data = lin_reg)
#tidy(model_count_simple)
#summary(model_count_moderate)
# using a few lagged counts
model_count_moderate <- lm(daily_count ~ lag_ret0 + lag_ret1 +
                             lag_ret0*jockers_rinker + lag_ret1*jockers_rinker +  
                             lag_ret0*loughran_mcdonald + lag_ret1*loughran_mcdonald + 
                             lag_ret0*socal_google + lag_ret1*socal_google
                             , data = lin_reg)
tidy(model_count_moderate)

regression_results_count <- tidy(model_count_moderate)
write_xlsx(regression_results_count, "regression_results_count.xlsx" )

# stargazer tables
table_html <- stargazer(list(model_return_simple, model_count_moderate), 
          keep.stat = c("n","adj.rsq"), type="text", 
          report = ('vc*t'),
          out = "table_count_return_combined.html")



# Question 4 and 5 ------------

# -----------------------------
# 1. Setup and Load Libraries
# -----------------------------

# Set your working directory
setwd("C://Users//Mikke//OneDrive//Desktop//NHH//Applied Textual Data Analysis//Final Exam")


# Load stop words from tidytext
data("stop_words")

# -----------------------------
# Load and Inspect Data
# -----------------------------

# Load the original corpus
load("results_df_nvidia.Rdata") # Corpus 1

# View the original corpus
# View(results_df)

# -----------------------------
# Text Cleaning
# -----------------------------

# Function to clean text
clean_text <- function(text) {
  text %>%
    str_replace_all("\\[\\[.*?\\]\\]", "") %>%  # Remove metadata like [[TIME.START]]
    str_replace_all("<.*?>", "") %>%           # Remove HTML tags
    str_replace_all("\\b\\d{1,2}[:.]\\d{2}\\s?(am|pm)?\\b", "") %>% # Remove time formats
    str_replace_all("[^a-zA-Z0-9\\s]", " ") %>% # Remove special characters (keep alphanumeric and spaces)
    str_squish() %>%                           # Remove extra whitespace
    str_to_lower()                             # Convert to lowercase
}


# Create a new dataset with cleaned text
cleaned_data_corpus3 <- results_df %>%
  mutate(Cleaned_Text = clean_text(Conversation_daily)) %>%
  select(date, Cleaned_Text) # Keep only date and cleaned text

# View the cleaned data (Uncomment if using RStudio)
# View(cleaned_data)

# -----------------------------
# Extract NVIDIA Context
# -----------------------------

# Function to extract NVIDIA and its context (case-insensitive)
extract_context <- function(text, keyword, pre_words = 25, post_words = 25) {
  # Regex case-insensitive
  pattern <- paste0("(?i)(?:\\b\\w+\\b\\s*){0,", pre_words, "}", keyword, "(?:\\s*\\b\\w+\\b){0,", post_words, "}")
  
  # Extract matches
  matches <- str_extract_all(text, pattern)
  unlist(matches) # Return as a simple character vector
}

# Corpus 3
# Extract context around "NVIDIA" and "NVIDIAS" with adjusted window
nvidia_context_corpus3 <- cleaned_data_corpus3 %>%
  filter(str_detect(Cleaned_Text, "(?i)nvidi(?:a|as)")) %>% # Case-insensitive filter for "nvidia" or "nvidias"
  mutate(nvidia_context = map(Cleaned_Text, ~ extract_context(.x, "nvidi(?:a|as)", pre_words = 25, post_words = 25))) %>%
  unnest(nvidia_context) %>%
  select(date, nvidia_context) %>% # Select only relevant columns
  distinct() # Remove duplicate rows


# View the NVIDIA context data
# View(nvidia_context)

# -----------------------------
# Combine Context by Date
# -----------------------------

# Combine rows with the same date into a single string
nvidia_context_combined_corpus3 <- nvidia_context_corpus3 %>%
  group_by(date) %>%
  summarise(nvidia_context_combined = paste(nvidia_context, collapse = " ")) %>%
  ungroup()

# View the combined dataset 
# head(nvidia_context_combined)
# View(nvidia_context_combined)

# -----------------------------
# Further Text Cleaning
# ----------------------------
# -----------------------------
# -----------------------------

# Remove some obvious host names
named_entities <- c("jonathan", "cory", "emily", "caroline", "ferro", "quinn", "vonnie", "haslinda", "anand", "theresa", "julie", "chang", "sam", "xi", "jack", "crumpton", "romaine", "taylor", "chun")

# Create a new cleaned dataset with enhanced cleaning
nvidia_context_clean_corpus3 <- nvidia_context_combined_corpus3 %>%
  mutate(
    nvidia_context_cleaned = nvidia_context_combined %>%
      # Remove specific named entities
      str_remove_all(paste0("\\b(", paste(named_entities, collapse = "|"), ")\\b")) %>%
      
      # Remove topic frequency lists and other irrelevant patterns
      str_replace_all("topic frequency .*", "") %>%
      
      # Remove URLs if present
      str_replace_all("http[s]?://\\S+\\s?", "") %>%
      
      # Remove punctuation
      str_replace_all("[[:punct:]]", " ") %>%
      
      # Remove numbers
      str_replace_all("\\b\\d+\\b", "") %>%
      
      # Convert to lowercase
      tolower() %>%
      
      # Remove stop words
      str_remove_all(paste0("\\b(", paste(stop_words$word, collapse = "|"), ")\\b")) %>%
      
      # Normalize whitespace (replaces multiple spaces with a single space and trims)
      str_squish()
  ) %>%
  select(date, nvidia_context_cleaned)

# View the cleaned data
# head(nvidia_context_clean)
# View(nvidia_context_clean)



save(nvidia_context_clean_corpus3, file="nvidia_context_clean_corpus3.Rdata")






############################# Task 4 ####################################
# -----------------------------
# Load and Prepare Stock Price Data
# -----------------------------

# Load stock price data
nvidia_stock_corpus3 <- read.csv("NVIDIA_stock_prices.csv")

# Clean the closing prices and convert to numeric
nvidia_stock_corpus3 <- nvidia_stock_corpus3 %>%
  mutate(
    Close.Last = as.numeric(gsub("[$,]", "", Close.Last)), # Remove $ and commas, convert to numeric
    Date = as.Date(Date, format = "%m/%d/%Y") # Convert Date to Date format
  )

# Categorize stock prices into high and low based on year
nvidia_stock_corpus3 <- nvidia_stock_corpus3 %>%
  mutate(price_category = ifelse(format(Date, "%Y") >= "2023", "high", "low"))

# -----------------------------
# Merge Text Data with Stock Prices
# -----------------------------

# Ensure `nvidia_context_cleaned`'s date is in Date format for merging
nvidia_context_clean_corpus3 <- nvidia_context_clean_corpus3 %>%
  mutate(Date = as.Date(date, format = "%Y-%m-%d")) # Convert to Date format

# Merge `nvidia_context_cleaned` with stock price data
nvidia_context_with_prices_corpus3 <- nvidia_context_clean_corpus3 %>%
  left_join(nvidia_stock_corpus3, by = "Date") %>% # Merge datasets on Date
  filter(!is.na(Close.Last)) %>% # Remove rows without price data
  select(Date, nvidia_context_cleaned, price_category) # Keep relevant columns

# View the merged data
# View(nvidia_context_with_prices)

# -----------------------------
# Tokenize into Bigrams
# -----------------------------

# Define additional obvious things that are not related to clean
custom_stop_words <- c("pm", "emily", "bloomberg", "business", "flash", "update", "stories", "time", "00am", "00pm")

# Tokenize into bigrams with enhanced filtering
bigrams_clean_corpus3 <- nvidia_context_with_prices_corpus3 %>%
  unnest_tokens(bigram, nvidia_context_cleaned, token = "ngrams", n = 2) %>% # Create bigrams
  separate(bigram, into = c("word1", "word2"), sep = " ") %>% # Split into individual words
  filter(
    !word1 %in% stop_words$word, 
    !word2 %in% stop_words$word, # Remove stop words
    !word1 %in% custom_stop_words, 
    !word2 %in% custom_stop_words, # Remove custom stop words
    !str_detect(word1, "nvidia"),
    !str_detect(word2, "nvidia"),
    !str_detect(word1, "\\d"), # Remove bigrams with numbers
    !str_detect(word2, "\\d"), 
    nchar(word1) > 1, # Optionally remove very short words
    nchar(word2) > 1
  ) %>%
  unite(bigram, word1, word2, sep = " ") %>% # Recombine into bigram
  count(price_category, bigram, sort = TRUE) # Count frequency by price category

# View the cleaned bigram data
# head(bigrams_clean)

# -----------------------------
# Filter and Refine Bigrams
# -----------------------------

# Remove bigrams that are purely numerical or contain "nvidia" (additional safety)
bigram_freq_cleaned_corpus3 <- bigrams_clean_corpus3 %>%
  filter(
    !is.na(bigram),                      # Remove NA bigrams
    !str_detect(bigram, "^\\d+$")       # Remove bigrams that are purely numbers
  )

# Define the number of top bigrams to display
top_n <- 20

# Top bigrams for high stock prices
top_high_bigrams_corpus3 <- bigrams_clean_corpus3 %>%
  filter(price_category == "high") %>%
  arrange(desc(n)) %>%
  slice_head(n = top_n)

# Top bigrams for low stock prices
top_low_bigrams_corpus3 <- bigrams_clean_corpus3 %>%
  filter(price_category == "low") %>%
  arrange(desc(n)) %>%
  slice_head(n = top_n)

# Combine the top bigrams for visualization
combined_bigrams_corpus3 <- bind_rows(
  top_high_bigrams_corpus3 %>% mutate(category = "High"),
  top_low_bigrams_corpus3 %>% mutate(category = "Low")
)

# Plot the top bigrams
ggplot(combined_bigrams_corpus3, aes(x = reorder(bigram, n), y = n, fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ category, scales = "free_y") +
  labs(
    title = "Top 20 Bigrams for High and Low NVIDIA Stock Prices",
    x = "Bigram",
    y = "Frequency",
    fill = "Price Category"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )

## View(combined_bigrams)

# -----------------------------
# Analyze Bigrams by Price Category
# -----------------------------

# Calculate log-ratio for distinctive bigrams
bigram_log_ratios_refined_corpus3 <- bigram_freq_cleaned_corpus3 %>%
  pivot_wider(names_from = price_category, values_from = n, values_fill = 0) %>%
  mutate(log_ratio = log2((high + 1) / (low + 1))) %>% # Calculate log-ratio
  arrange(desc(log_ratio))

# Top bigrams for high stock prices
top_high_bigrams_refined_corpus3 <- bigram_log_ratios_refined_corpus3 %>%
  filter(log_ratio > 0) %>%
  slice_max(order_by = log_ratio, n = 20)

# Top bigrams for low stock prices
top_low_bigrams_refined_corpus3 <- bigram_log_ratios_refined_corpus3 %>%
  filter(log_ratio < 0) %>%
  slice_min(order_by = log_ratio, n = 20)

# Combine the top bigrams for visualization
top_bigrams_corpus3 <- bind_rows(
  top_high_bigrams_refined_corpus3 %>% mutate(category = "High"),
  top_low_bigrams_refined_corpus3 %>% mutate(category = "Low")
)

# View the top bigrams 
# print(top_bigrams)

# -----------------------------
# Visualization of Top Bigrams
# -----------------------------

# Plot the top bigrams with log-ratio
ggplot(top_bigrams_corpus3, aes(x = reorder(bigram, log_ratio), y = log_ratio, fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Distinctive Bigrams for High vs Low NVIDIA Stock Prices",
    x = "Bigram",
    y = "Log-Ratio (High vs Low)",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )




################ Unigrams ######################

# Tokenize into unigrams with enhanced filtering
unigrams_clean_corpus3 <- nvidia_context_with_prices_corpus3 %>%
  unnest_tokens(word, nvidia_context_cleaned) %>% # Create unigrams
  filter(
    !word %in% stop_words$word,          # Remove standard stop words
    !word %in% custom_stop_words,        # Remove custom stop words
    !str_detect(word, "nvidia"),         # Exclude words containing "nvidia"
    !str_detect(word, "\\d"),            # Exclude words with numbers
    nchar(word) > 1                       # Exclude very short words
  ) %>%
  count(price_category, word, sort = TRUE) # Count frequency by price category

# View the cleaned unigram data
# head(unigrams_clean)


# Define the number of top unigrams to display
top_n <- 20

# Top unigrams for high stock prices
top_high_unigrams_corpus3 <- unigrams_clean_corpus3 %>%
  filter(price_category == "high") %>%
  arrange(desc(n)) %>%
  slice_head(n = top_n)

# Top unigrams for low stock prices
top_low_unigrams_corpus3 <- unigrams_clean_corpus3 %>%
  filter(price_category == "low") %>%
  arrange(desc(n)) %>%
  slice_head(n = top_n)

# Combine the top unigrams for visualization
combined_unigrams_corpus3 <- bind_rows(
  top_high_unigrams_corpus3 %>% mutate(category = "High"),
  top_low_unigrams_corpus3 %>% mutate(category = "Low")
)

# Plot the top unigrams
ggplot(combined_unigrams_corpus3, aes(x = reorder(word, n), y = n, fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip() + # Flip coordinates for better readability
  facet_wrap(~ category, scales = "free_y") + # Separate facets for high and low
  labs(
    title = "Top 20 Unigrams for High and Low NVIDIA Stock Prices",
    x = "Unigram",
    y = "Frequency",
    fill = "Price Category"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )





# -----------------------------
# Log-Ratio Analysis of Unigrams
# -----------------------------

unigram_log_ratios_corpus3 <- unigrams_clean_corpus3 %>%
  pivot_wider(names_from = price_category, values_from = n, values_fill = 0) %>%
  mutate(log_ratio = log2((high + 1) / (low + 1))) %>% # Adding 1 to avoid division by zero
  arrange(desc(log_ratio))

# Top unigrams for high stock prices
top_high_unigrams_log_corpus3 <- unigram_log_ratios_corpus3 %>%
  filter(log_ratio > 0) %>%
  slice_max(order_by = log_ratio, n = top_n)

# Top unigrams for low stock prices
top_low_unigrams_log_corpus3 <- unigram_log_ratios_corpus3 %>%
  filter(log_ratio < 0) %>%
  slice_min(order_by = log_ratio, n = 16)

# Combine for visualization
top_unigrams_log_corpus3 <- bind_rows(
  top_high_unigrams_log_corpus3 %>% mutate(category = "High"),
  top_low_unigrams_log_corpus3 %>% mutate(category = "Low")
)

# Plot log-ratio top unigrams
ggplot(top_unigrams_log_corpus3, aes(x = reorder(word, log_ratio), y = log_ratio, fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Distinctive Unigrams for High vs Low NVIDIA Stock Prices",
    x = "Unigram",
    y = "Log-Ratio (High vs Low)",
    fill = "Category"
  ) +
  theme_minimal()






######### TOPIC MODEL ##########
#########   Task 5    ##########


# -----------------------------
# LDA Modeling Across All Years
# -----------------------------

# Load necessary libraries
library(tidytext)
library(topicmodels)
library(ggplot2)
library(dplyr)
library(tidyr)

# Define custom stop words
custom_stop_words <- c("pm", "emily", "bloomberg", "business", 
                       "flash", "update", "stories", "time", 
                       "00am", "00pm")

# Prepare data for LDA
all_year_data_corpus3 <- nvidia_context_with_prices_corpus3 %>%
  mutate(year = format(as.Date(Date), "%Y")) %>%  # Extract year
  group_by(year) %>%  # Group by year
  summarise(text = paste(nvidia_context_cleaned, collapse = " "), .groups = "drop")  # Combine text by year

# Tokenize and clean the text
tidy_data_corpus3 <- all_year_data_corpus3 %>%
  unnest_tokens(word, text) %>%  # Tokenize
  anti_join(stop_words, by = "word") %>%  # Remove stop words
  filter(!word %in% custom_stop_words) %>%  # Remove custom stop words
  mutate(word = wordStem(word, language = "en"))  # Apply stemming

# Create a Document-Term Matrix (DTM)
dtm_corpus3 <- tidy_data_corpus3 %>%
  count(year, word) %>%  # Count word occurrences
  cast_dtm(document = year, term = word, value = n)  # Create DTM with year as the document

# Fit the LDA model using Gibbs Sampling with custom alpha and eta
k <- 8  # Number of topics
alpha <- 0.8  # Sparsity of topic distribution per document
eta <- 0.1    # Sparsity of word distribution per topic

lda_model <- LDA(dtm_corpus3, k = k, method = "Gibbs", control = list(
  seed = 1234,
  burnin = 1000,  # Number of burn-in iterations
  iter = 2000,    # Total number of Gibbs sampling iterations
  thin = 100,     # Thinning interval
  alpha = alpha,
  delta = eta     # Use 'delta' instead of 'eta' in Gibbs
))

# Extract top terms for each topic
lda_topics <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# View the resulting topics
print(lda_topics)


# View the resulting topics
print(lda_topics)


# Visualize top terms per topic
ggplot(lda_topics, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(title = "Top 10 Terms per LDA Topic",
       x = "Terms",
       y = "Beta") +
  theme_minimal()

# Extract document-topic proportions
lda_gamma <- tidy(lda_model, matrix = "gamma")  # Extract topic proportions for each year

# Visualize topic distribution over years
ggplot(lda_gamma, aes(x = document, y = gamma, fill = factor(topic))) +
  geom_col(position = "stack") +
  labs(title = "LDA Topic Distribution Across Years",
       x = "Year",
       y = "Proportion",
       fill = "Topic") +
  theme_minimal()


# Extract the top terms for each topic
top_terms_per_topic <- lda_topics %>%
  group_by(topic) %>%
  summarise(top_terms = paste(term, collapse = ", "))  # Combine terms for each topic

# View the top terms for each topic
print(top_terms_per_topic)
view(top_terms_per_topic)




## Wordclouds
# Here we will create 
# Extract terms and their probabilities (beta values) for all topics
lda_terms <- tidy(lda_model, matrix = "beta")

# Prepare data for Topic 1
topic1_terms <- lda_terms %>%
  filter(topic == 1) %>%
  arrange(desc(beta)) %>%
  head(50)  # Adjust 'head' value as needed

# Create word cloud for Topic 1
set.seed(1234)
wordcloud(
  words = topic1_terms$term,
  freq = topic1_terms$beta,
  scale = c(4, 0.5),
  max.words = 50,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)
title("Topic 1")

# Prepare data for Topic 6
topic6_terms <- lda_terms %>%
  filter(topic == 6) %>%
  arrange(desc(beta)) %>%
  head(50)

# Create word cloud for Topic 6
set.seed(1234)
wordcloud(
  words = topic6_terms$term,
  freq = topic6_terms$beta,
  scale = c(4, 0.5),
  max.words = 50,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)
title("Topic 6")



# Prepare data for Topic 4
topic4_terms <- lda_terms %>%
  filter(topic == 4) %>%
  arrange(desc(beta)) %>%
  head(50)

# Create word cloud for Topic 4
set.seed(1234)
wordcloud(
  words = topic4_terms$term,
  freq = topic4_terms$beta,
  scale = c(4, 0.5),
  max.words = 50,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)
title("Topic 4")



# Prepare data for Topic 5
topic5_terms <- lda_terms %>%
  filter(topic == 5) %>%
  arrange(desc(beta)) %>%
  head(50)

# Create word cloud for Topic 5
set.seed(1234)
wordcloud(
  words = topic5_terms$term,
  freq = topic5_terms$beta,
  scale = c(4, 0.5),
  max.words = 50,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)
title("Topic 5")



# Prepare data for Topic 8
topic8_terms <- lda_terms %>%
  filter(topic == 8) %>%
  arrange(desc(beta)) %>%
  head(50)

# Create word cloud for Topic 8
set.seed(1234)
wordcloud(
  words = topic8_terms$term,
  freq = topic8_terms$beta,
  scale = c(4, 0.5),
  max.words = 50,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)
title("Topic 8")





# Creating Corpus 3 AND Tables ----  

load("nvidia_context_clean_corpus3.Rdata")
print(nvidia_context_clean_corpus3, n=5)

toks <- tokenize_words((nvidia_context_clean_corpus3$nvidia_context_cleaned), lowercase = T, stopwords = stopwords("en"), strip_punct = T, strip_numeric = T, simplify = F)
corpus_raw_filtered <- Corpus(VectorSource(toks))



dtm <- DocumentTermMatrix(corpus_raw_filtered,
                          control = list( 
                            removePunctuation = T,
                            stopwords = stopwords("en"),
                            stemming = F,
                            removeNumbers = T))

tibble <- as_tibble(as.matrix(dtm))

# Get term frequencies and select the top 10 terms
#tibble_present <- tibble %>% select(-date)

term_frequencies <- colSums(tibble)


top_terms <- sort(term_frequencies, decreasing = TRUE)[1:100]
#   em         timeend    timestart    will         pm         us      think    
#   1689386     195246     195246     132207     107397      85532      76660 

# remove some meaning less terms:
term_frequencies <- term_frequencies[!names(term_frequencies) %in% c("timeend", "timestart", "pm", "am", "em", "bloomberg")]
top_terms <- sort(term_frequencies, decreasing = TRUE)[1:100]

# Calculate summary metrics
total_words <- sum(term_frequencies)
num_docs <- nrow(tibble)
vocab_size <- ncol(tibble)
avg_words_per_doc <- round(total_words / num_docs, 2)

# Create the combined summary table
corpus_summary <- data.frame(
  Metric = c(
    "Number of Documents",
    "Total Words",
    "Vocabulary Size (Unique Terms)",
    "Average Words per Document"
  ),
  Value = c(
    num_docs,
    total_words,
    vocab_size,
    avg_words_per_doc
  )
)

# Display the combined summary table
print(corpus_summary)

top_ten_terms <- data.frame(
  "Top ten terms:" = c(
    paste(names(top_terms[1])),
    paste(names(top_terms[2])),
    paste(names(top_terms[3])),
    paste(names(top_terms[4])),
    paste(names(top_terms[5])),
    paste(names(top_terms[6])),
    paste(names(top_terms[7])),
    paste(names(top_terms[8])),
    paste(names(top_terms[9])),
    paste(names(top_terms[10]))
  ),
  "Total count" = c(
    as.integer(top_terms[1]),
    as.integer(top_terms[2]),
    as.integer(top_terms[3]),
    as.integer(top_terms[4]),
    as.integer(top_terms[5]),
    as.integer(top_terms[6]),
    as.integer(top_terms[7]),
    as.integer(top_terms[8]),
    as.integer(top_terms[9]),
    as.integer(top_terms[10])
  )
)

print(top_ten_terms)

write_xlsx(corpus_summary, "table_3_corpus.xlsx")
write_xlsx(top_ten_terms, "table_3_terms.xlsx")



 
 
 
 
 
 




 
 
 
 
 
 