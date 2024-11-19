# Load necessary libraries
library(dplyr)
library(tidyr)
library(plotly)
library(stringr)
library(stm)
library(textstem)
library(wordcloud) # For word cloud visualization (optional)
library(RColorBrewer) # For color palettes

# Define custom dictionary for word replacement
custom_dict <- c(
  "qualiti" = "quality", "problem" = "problem",
  "caus" = "cause", "neighborhood" = "neighborhood",
  "smoke" = "smoke", "pollut" = "pollute", "respiratori" = "respiratory",
  "peopl" = "people", "resourc" = "resource", "vulner" = "vulnerability",
  "experi" = "experience", "diseas" = "disease", "bodi" = "body",
  "communiti" = "community", "reduc" = "reduce", "improv" = "improve",
  "particip" = "participate", "injustic" = "injustice", "promot" = "promote",
  "togeth" = "together", "advoc" = "advocate", "bodi" = "body",
  "initi" = "initialize", "citi" = "city", "compani" = "company",
  "activ" = "active", "anoth" = "another", "resourc" = "resource",
  "agenc" = "agency", "voic" = "voice", "lifestyl" = "lifestyle",
  "natur" = "nature", "provid" = "provide", "environ" = "environment",
  "sens" = "sense", "activ" = "active", "valu" = "value",
  "properti" = "properties", "sunstrok" = "sunstroke",
  "experienc" = "experience", "basketbal" = "basketball",
  "temperatur" = "temperature", "condit" = "condition",
  "alreadi" = "already", "emerg" = "emergency",
  "immedi" = "immediately", "bodi" = "body", "extrem" = "extrem",
  "guidanc" = "guidance", "awar" = "aware", "energi" = "energy",
  "increas" = "increase", "encourag" = "encourage", "busi" = "busy",
  "consumpt" = "consumption", "abil" = "ability", "alreadi" = "already",
  "vehicl" = "vehicle", "mani" = "many", "pollution-rel" = "pollution-related",
  "negat" = "negate", "contribut" = "contribute", "resid" = "reside",
  "issu" = "issue", "emiss" = "emission"
)

# Function to replace words using the custom dictionary
replace_word_lemma <- function(word) {
  if (word %in% names(custom_dict)) {
    return(custom_dict[[word]])
  } else {
    return(word)
  }
}

# Enhanced Topic Modeling Function
perform_topic_modeling <- function(
    survey_data, demographic_variable,
    survey, num_topics = 2) {
  # Import stoplist (optional, based on your needs)
  # malletwords <-
  # scan("./data/report_data/mallet.txt", character(), quote = "")


  print(head(survey_data))

  # Clean and preprocess responses
  example_open <- survey_data
  names(example_open)[2] <- "response"

  # Text Cleaning Steps
  example_open$response_cleaned <- tolower(example_open$response)
  example_open$response_cleaned <-
    gsub("[[:punct:]0-9]", " ", example_open$response_cleaned)
  example_open$response_cleaned <-
    removeWords(example_open$response_cleaned, stopwords("english"))
  example_open$response_cleaned <-
    stripWhitespace(example_open$response_cleaned)
  example_open$response_cleaned <-
    wordStem(example_open$response_cleaned, language = "english")
  example_open$response_cleaned <-
    lemmatize_words(example_open$response_cleaned)

  survey_data <- example_open

  # Preprocess the text data for STM
  processed_texts <- textProcessor(
    documents = survey_data$response_cleaned,
    metadata = survey_data
  )
  out <- prepDocuments(
    processed_texts$documents, processed_texts$vocab,
    processed_texts$meta
  )
  docs <- out$documents
  vocab <- out$vocab
  meta <- out$meta

  # Fit the STM model
  topic_model <- stm(
    documents = docs, vocab = vocab,
    K = num_topics, data = meta, max.em.its = 150, init.type = "Spectral"
  )

  # Extract top words for each topic
  top_words <- labelTopics(topic_model, n = 10)
  topic_words <- top_words$prob

  # Convert top words to human-readable format
  human_readable_topics <- apply(topic_words, 1, function(topic) {
    words <- unlist(strsplit(topic, ", "))
    readable_words <- sapply(words, replace_word_lemma)
    paste(readable_words, collapse = ", ")
  })

  # Display human-readable topics
  for (i in 1:num_topics) {
    topic <- paste("Topic", i, "-", human_readable_topics[i])
    print(topic)
  }

  # Get document-topic matrix
  doc_topic_matrix <- topic_model$theta

  # Convert matrix to data frame
  doc_topic_df <- as.data.frame(doc_topic_matrix)
  doc_topic_df$Document <- rownames(doc_topic_df)

  # Summarize topic proportions
  topic_sums <- colSums(doc_topic_matrix)
  topic_descriptions <- paste0(
    "Topic ", 1:num_topics, " - ",
    human_readable_topics
  )

  # Apply text wrapping to topic descriptions
  topic_wrapped <- str_wrap(topic_descriptions, width = 40)

  # Create a data frame for plotting
  plot_data <- data.frame(
    Topic = topic_wrapped,
    Proportion = topic_sums / sum(topic_sums)
  )

  # Define a visually appealing color palette
  palette <- brewer.pal(n = max(4, num_topics), name = "Pastel2")

  # Create the Plotly bar chart
  p <- plot_ly(
    data = plot_data,
    x = ~Proportion,
    y = ~Topic,
    type = "bar",
    orientation = "h",
    marker = list(color = palette),
    text = ~ paste(Topic),
    hoverinfo = "text",
    textposition = "auto"
  ) %>%
    layout(
      title = "Responses",
      xaxis = list(
        title = "Proportion of Topics",
        range = c(0, max(plot_data$Proportion) * 1.1),
        titlefont = list(size = 14, family = "Inter"),
        tickfont = list(size = 12, family = "Inter")
      ),
      yaxis = list(title = "", showticklabels = FALSE),
      margin = list(l = 200, r = 50, t = 100, b = 50),
      plot_bgcolor = "rgb(255, 255, 255)",
      paper_bgcolor = "rgb(255, 255, 255)",
      font = list(family = "Inter, sans-serif", size = 12, fontweight = 600),
      showlegend = FALSE
    )

  # Return the Plotly object
  return(p)
}
