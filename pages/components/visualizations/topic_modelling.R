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
  "negat" = "negate", "contribut" = "contribute", "resid" = "reside"
)

replace_word_lemma <- function(word) {
  if (word %in% names(custom_dict)) {
    return(custom_dict[[word]])
  } else {
    return(word)
  }
}

perform_topic_modeling <- function(
    survey_data, demographic_variable,
    num_topics = 4) {
  # Import stoplist
  malletwords <-
    scan("./data/report_data/mallet.txt",
      character(),
      quote = ""
    )

  # Extract example question and demographic data
  example_open <- survey_data
  names(example_open)[2] <- "response"

  # example_open$response_cleaned <- tolower(gsub(
  #   "[[:punct:]]", " ",
  #   example_open$response
  # ))
  # example_open$response_cleaned <- removeWords(
  #   example_open$response_cleaned,
  #   c(stopwords("english"), malletwords)
  # )
  # example_open$response_cleaned <-
  #   lemmatize_words(example_open$response_cleaned)

  example_open$response_cleaned <- tolower(example_open$response)
  example_open$response_cleaned <-
    gsub("[[:punct:]0-9]", " ", example_open$response_cleaned)
  example_open$response_cleaned <-
    removeWords(example_open$response_cleaned, c(
      stopwords("english")
      # malletwords
    ))
  example_open$response_cleaned <-
    stripWhitespace(example_open$response_cleaned)
  example_open$response_cleaned <-
    wordStem(example_open$response_cleaned, language = "english")
  example_open$response_cleaned <-
    lemmatize_words(example_open$response_cleaned)

  survey_data <- example_open

  # Preprocess the text data
  processed_texts <- textProcessor(
    documents = survey_data$response,
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
  num_topics <- 2 # Choose an appropriate number of topics
  topic_model <- stm(
    documents = docs, vocab = vocab,
    K = num_topics, data = meta, max.em.its = 150, init.type = "Spectral"
  )

  print("***********")
  top_words <- labelTopics(topic_model)
  print(top_words)
  topic_words <- top_words$prob

  human_readable_topics <- apply(topic_words, 1, function(topic) {
    words <- unlist(strsplit(topic, ", "))
    readable_words <- sapply(words, function(word) {
      # Replace with custom dictionary if needed
      replace_word_lemma(word)
    })
    paste(readable_words, collapse = ", ")
  })

  for (i in 1:num_topics) {
    topic <- paste("Topic", i, "-", human_readable_topics[i])
    print(topic)
  }

  topics1 <- topic_words[1, ]
  topics2 <- topic_words[2, ]

  # topics1 <- paste(topics1, collapse = ", ")
  # topics2 <- paste(topics2, collapse = ", ")

  # topics1 <- paste("Topic 1 - ", topics1)
  # topics2 <- paste("Topic 2 - ", topics2)

  # print(topics1)
  # print(topics2)

  print("***********")


  # Get document-topic matrix
  doc_topic_matrix <- topic_model$theta

  # Convert matrix to data frame
  doc_topic_df <- as.data.frame(doc_topic_matrix)
  doc_topic_df$Document <- rownames(doc_topic_df)

  # Melt the data frame for plotting
  doc_topic_melted <- reshape2::melt(doc_topic_df, id.vars = "Document")

  # print(doc_topic_melted)

  # Get top words associated with each topic
  # top_words <- terms(topic_model, 10)

  # print(top_words)
  print("---------------------------------------------")
  # print(doc_topic_matrix)

  column_sum1 <- sum(doc_topic_matrix[, 1])
  print(paste("column_sum1", column_sum1))

  column_sum2 <- sum(doc_topic_matrix[, 2])
  print(paste("column_sum2", column_sum2))

  topic_sums <- colSums(doc_topic_matrix)
  topic_descriptions <- paste0(
    "Topic ", 1:num_topics, " - ",
    human_readable_topics
  )

  trace1 <- list(
    type = "bar",
    # x = c(column_sum1 / 400, column_sum2 / 400),
    # y = c(topics1, topics2),
    x = topic_sums / 400,
    y = topic_descriptions,
    marker = list(
      line = list(
        color = "rgb(8,48,107)",
        width = 1.5
      ),
      color = "rgb(158,202,225)"
    ),
    opacity = 0.6,
    orientation = "h"
  )
  data <- list(trace1)
  layout <- list(
    title = "Topic Modelling",
    xaxis = list(domain = c(0.15, 1)),
    margin = list(
      b = 80,
      l = 120,
      r = 10,
      t = 140
    ),
    barmode = "stack",
    plot_bgcolor = "rgb(255, 255, 255)",
    paper_bgcolor = "rgb(255, 255, 255)"
  )
  p <- plot_ly()
  p <- add_trace(p,
    type = trace1$type, x = trace1$x,
    y = trace1$y, marker = trace1$marker,
    opacity = trace1$opacity, orientation = trace1$orientation
  )
  p <- layout(p,
    title = layout$title,
    xaxis = layout$xaxis, margin = layout$margin,
    barmode = layout$barmode, plot_bgcolor = layout$plot_bgcolor,
    paper_bgcolor = layout$paper_bgcolor
  )
}
