# Sum-up: Utilizing Automated Text Analysis to Support Interpretation of Narrative Feedback
Combine topic modeling and sentiment analysis to identify individual students' gaps, and highlight their strengths and weaknesses across predefined competency domains and professional activities.

This function runs a series of text processing and analysis steps including text cleaning, tokenization, lemmatization, topic modeling, and sentiment analysis. It then classifies sentences into topics and generates an output summarizing the results.

This function performs the following steps:
- Cleans the input text data using `text_clean`.
- Tokenizes the text into sentences and removes stopwords.
- Lemmatizes and annotates the sentences using a UDPipe model.
- Counts word frequencies and excludes stopwords.
- Performs topic modeling on the word counts.
- Runs sentiment analysis based on the specified method (Grasp or SentimentR).
- Classifies sentences into topics using the topic classification model.
- Generates output summarizing the topics and sentiment.
  
```R
library("sumup")

data(example_data)
ex_data <- example_data
ex_settings  <- set_default_settings()
ex_settings  <- update_setting(ex_settings , "language", "en")
ex_settings  <- update_setting(ex_settings , "use_sentiment_analysis", "sentimentr")
result <- run_sumup(ex_data, ex_settings )
```

