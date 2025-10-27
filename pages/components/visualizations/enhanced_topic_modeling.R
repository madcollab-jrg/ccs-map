# Enhanced Topic Modeling with Heatmap Visualization
# Based on sections 10.1, 10.2.1 from explore_topic_modeling.Rmd

library(dplyr)
library(tidyr)
library(plotly)
library(stringr)
library(stm)
library(textstem)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(tidytext)
library(reshape2)
library(readr)
library(SnowballC)

# Enhanced custom dictionary for word replacement
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
  "issu" = "issue", "emiss" = "emission", "climat" = "climate",
  "chang" = "change", "recycl" = "recycle", "littl" = "little",
  "popul" = "popular", "exhaust" = "exhaust", "hospit" = "hospital",
  "suffer" = "suffer", "stroke" = "stroke", "symptom" = "symptom",
  "weather" = "weather", "hot" = "hot", "felt" = "felt",
  "know" = "know", "work" = "work", "high" = "high",
  "health" = "health", "due" = "due", "person" = "person",
  "dizzi" = "dizzy", "neighbor" = "neighbor", "help" = "help",
  "heat-rel" = "heat-related", "emergency" = "emergency", "city" = "city"
)

# Function to replace words using the custom dictionary
replace_word_lemma <- function(word) {
  if (word %in% names(custom_dict)) {
    return(custom_dict[[word]])
  } else {
    return(word)
  }
}

# Enhanced topic modeling function (Section 10.1)
perform_enhanced_topic_modeling <- function(survey_data, num_topics = 4) {
  # Load stopwords
  stopwords_path <- "data/report_data/mallet.txt"
  if (file.exists(stopwords_path)) {
    malletwords <- scan(stopwords_path, character(), quote = "")
  } else {
    malletwords <- c()
    warning("Stopwords file not found, using default stopwords only")
  }
  
  # Preprocess text data
  survey_data$response_cleaned <- tolower(survey_data$response)
  survey_data$response_cleaned <- gsub("[[:punct:]0-9]", " ", survey_data$response_cleaned)
  survey_data$response_cleaned <- removeWords(survey_data$response_cleaned, 
                                            c(stopwords("english"), malletwords))
  survey_data$response_cleaned <- stripWhitespace(survey_data$response_cleaned)
  
  # Apply stemming with error handling
  tryCatch({
    survey_data$response_cleaned <- wordStem(survey_data$response_cleaned, language = "english")
  }, error = function(e) {
    warning("wordStem failed, skipping stemming step")
  })
  
  # Apply lemmatization with error handling
  tryCatch({
    survey_data$response_cleaned <- lemmatize_words(survey_data$response_cleaned)
  }, error = function(e) {
    warning("lemmatize_words failed, skipping lemmatization step")
  })
  
  # Preprocess for STM
  processed_texts <- textProcessor(
    documents = survey_data$response,
    metadata = survey_data
  )
  
  out <- prepDocuments(
    processed_texts$documents, 
    processed_texts$vocab,
    processed_texts$meta
  )
  
  docs <- out$documents
  vocab <- out$vocab
  meta <- out$meta
  
  # Fit STM model
  topic_model <- stm(
    documents = docs, 
    vocab = vocab,
    K = num_topics, 
    data = meta, 
    max.em.its = 150, 
    init.type = "Spectral"
  )
  
  # Get topic labels
  top_words <- labelTopics(topic_model)
  topic_words <- top_words$prob
  
  # Apply custom dictionary to make topics more readable
  human_readable_topics <- apply(topic_words, 1, function(topic) {
    words <- unlist(strsplit(topic, ", "))
    readable_words <- sapply(words, function(word) {
      replace_word_lemma(word)
    })
    paste(readable_words, collapse = ", ")
  })
  
  # Get document-topic matrix
  doc_topic_matrix <- topic_model$theta
  
  # Return comprehensive results
  return(list(
    model = topic_model,
    docs = docs,
    vocab = vocab,
    meta = meta,
    doc_topic_matrix = doc_topic_matrix,
    top_words = top_words,
    human_readable_topics = human_readable_topics,
    processed_data = survey_data
  ))
}

# Function to analyze demographic representation per topic (Section 10.2.1)
analyze_demographic_representation <- function(topic_results, demographic_vars) {
  doc_topic_matrix <- topic_results$doc_topic_matrix
  meta <- topic_results$meta
  num_topics <- ncol(doc_topic_matrix)
  
  # Assign each document to its most likely topic
  topic_assignments <- apply(doc_topic_matrix, 1, which.max)
  
  # Add topic assignments to metadata
  meta$topic_assignment <- topic_assignments
  
  demographic_summaries <- list()
  
  for (demo_var in demographic_vars) {
    if (demo_var %in% names(meta)) {
      # Create summary table
      demo_summary <- meta %>%
        group_by(!!sym(demo_var), topic_assignment) %>%
        summarise(count = n(), .groups = 'drop') %>%
        pivot_wider(names_from = topic_assignment, 
                   values_from = count, 
                   names_prefix = "Topic_",
                   values_fill = 0) %>%
        mutate(total = rowSums(select(., starts_with("Topic_"))))
      
      # Calculate percentages
      topic_cols <- paste0("Topic_", 1:num_topics)
      demo_summary[paste0(topic_cols, "_pct")] <- demo_summary[topic_cols] / 
        demo_summary$total * 100
      
      demographic_summaries[[demo_var]] <- demo_summary
    }
  }
  
  return(demographic_summaries)
}

# Function to create demographic heatmap visualization (Section 10.2.1)
create_demographic_heatmap <- function(demographic_summaries, demo_var, topic_labels, representative_comments = NULL) {
  if (!demo_var %in% names(demographic_summaries)) {
    return(NULL)
  }
  
  demo_data <- demographic_summaries[[demo_var]]
  
  # Prepare data for heatmap
  heatmap_data <- demo_data %>%
    select(!!sym(demo_var), starts_with("Topic_") & !ends_with("_pct")) %>%
    pivot_longer(cols = starts_with("Topic_"), 
                names_to = "Topic", 
                values_to = "Count") %>%
    mutate(Topic = factor(Topic, levels = paste0("Topic_", 1:length(topic_labels))))
  
  # Create hover text with representative comments
  heatmap_data$hover_text <- apply(heatmap_data, 1, function(row) {
    demographic_group <- row[demo_var]
    topic <- row["Topic"]
    count <- row["Count"]
    
    # Find representative comments for this demographic group and topic
    if (!is.null(representative_comments)) {
      relevant_comments <- representative_comments[
        representative_comments$Topic == topic & 
        (representative_comments$Demographic_Group == "Overall" | 
         grepl(demographic_group, representative_comments$Demographic_Group, fixed = TRUE)), 
      ]
      
      if (nrow(relevant_comments) > 0) {
        # Get the most relevant comment (highest probability or first overall)
        best_comment <- relevant_comments[1, "Comment"]
        comment_text <- ifelse(nchar(best_comment) > 100, 
                              paste0(substr(best_comment, 1, 100), "..."), 
                              best_comment)
        
        return(paste0("Demographic: ", demographic_group, 
                     "<br>Topic: ", topic, 
                     "<br>Count: ", count,
                     "<br><br><b>Representative Comment:</b><br>", comment_text))
      }
    }
    
    # Fallback hover text without comments
    return(paste0("Demographic: ", demographic_group, 
                 "<br>Topic: ", topic, 
                 "<br>Count: ", count))
  })
  
  # Create heatmap using plotly
  p <- plot_ly(
    data = heatmap_data,
    x = ~Topic,
    y = ~get(demo_var),
    z = ~Count,
    type = "heatmap",
    colorscale = list(
      c(0, "white"),
      c(0.2, "lightblue"),
      c(0.4, "steelblue"),
      c(0.7, "darkblue"),
      c(1, "navy")
    ),
    showscale = TRUE,
    text = ~hover_text,
    hoverinfo = "text"
  ) %>%
    layout(
      title = "",  # Remove title
      xaxis = list(
        title = "Topic",
        ticktext = gsub("_", " ", unique(heatmap_data$Topic)),  # Remove underscores from x-axis labels
        tickvals = unique(heatmap_data$Topic)
      ),
      yaxis = list(
        title = "",  # Remove y-axis title
        showticklabels = TRUE  # Keep y-axis labels
      ),
      margin = list(l = 50, r = 50, t = 50, b = 50)  # Reduced margins since no title/labels
    )
  
  return(p)
}

# Function to extract representative comments (from section 10.3)
extract_representative_comments <- function(topic_results, num_comments = 3) {
  model <- topic_results$model
  meta <- topic_results$meta
  human_readable_topics <- topic_results$human_readable_topics
  num_topics <- length(human_readable_topics)
  
  all_comments <- data.frame()
  
  for (topic_num in 1:num_topics) {
    # Get overall representative comments
    thoughts <- findThoughts(model, 
                           texts = meta$response, 
                           topics = topic_num, 
                           n = num_comments)
    
    if (length(thoughts$docs[[1]]) > 0) {
      topic_comments <- data.frame(
        Topic = paste0("Topic_", topic_num),
        Comment = thoughts$docs[[1]],
        Demographic_Group = "Overall",
        Topic_Probability = NA,
        stringsAsFactors = FALSE
      )
      all_comments <- rbind(all_comments, topic_comments)
    }
    
    # Get representative comments by demographic group
    demographic_vars <- c("race_recode", "Gender", "income_recode", "edu_recode", "Year.of.Birth")
    
    for (demo_var in demographic_vars) {
      if (demo_var %in% names(meta)) {
        unique_groups <- unique(meta[[demo_var]])
        unique_groups <- unique_groups[!is.na(unique_groups)]
        
        for (group in unique_groups) {
          group_indices <- which(meta[[demo_var]] == group)
          if (length(group_indices) >= 2) {  # Need at least 2 documents for topic modeling
            group_texts <- meta$response[group_indices]
            
            # Get topic probabilities for this group
            group_topic_probs <- topic_results$doc_topic_matrix[group_indices, topic_num]
            top_indices <- order(group_topic_probs, decreasing = TRUE)[1:min(1, length(group_indices))]
            
            if (length(top_indices) > 0) {
              group_comments <- data.frame(
                Topic = paste0("Topic_", topic_num),
                Comment = group_texts[top_indices],
                Demographic_Group = paste(demo_var, ":", group),
                Topic_Probability = group_topic_probs[top_indices],
                stringsAsFactors = FALSE
              )
              all_comments <- rbind(all_comments, group_comments)
            }
          }
        }
      }
    }
  }
  
  return(all_comments)
}

# Function to create demographic summary table
create_demographic_summary_table <- function(demographic_summaries, demo_var) {
  if (!demo_var %in% names(demographic_summaries)) {
    return(NULL)
  }
  
  demo_data <- demographic_summaries[[demo_var]]
  
  # Select relevant columns for display (exclude percentage columns)
  display_cols <- c(demo_var, "total")
  topic_cols <- names(demo_data)[grepl("^Topic_[0-9]+$", names(demo_data))]
  
  # Create summary table with counts only (no percentages)
  summary_table <- demo_data %>%
    select(all_of(c(display_cols, topic_cols))) %>%
    arrange(desc(total)) %>%
    # Remove any rows with empty demographic values
    filter(!is.na(!!sym(demo_var)) & !!sym(demo_var) != "")
  
  return(summary_table)
}

# Main enhanced topic modeling function for open-ended questions
enhanced_topic_modeling_visualization <- function(
    survey_data, demographic_variable, 
    filter_input, num_topics = 4) {
  
  # Prepare data
  names(survey_data)[2] <- "response"
  
  # Filter data if needed
  if (!is.na(filter_input)) {
    survey_data <- survey_data %>%
      filter(!!sym(demographic_variable) == !!filter_input)
  }
  
  # Filter out empty responses
  survey_data <- survey_data %>%
    filter(!is.na(response) & nchar(trimws(response)) > 10)
  
  if (nrow(survey_data) < 5) {
    return(NULL)  # Not enough data for topic modeling
  }
  
  # Run enhanced topic modeling analysis
  topic_results <- perform_enhanced_topic_modeling(survey_data, num_topics)
  
  # Analyze demographic representation
  available_demo_vars <- c("race_recode", "Gender", "income_recode", "edu_recode", "Year.of.Birth")
  available_demo_vars <- available_demo_vars[available_demo_vars %in% names(survey_data)]
  
  if (length(available_demo_vars) > 0) {
    demographic_summaries <- analyze_demographic_representation(topic_results, available_demo_vars)
  } else {
    demographic_summaries <- list()
  }
  
  # Extract representative comments
  representative_comments <- extract_representative_comments(topic_results, num_comments = 3)
  
  # Create demographic heatmap for the selected demographic variable
  demographic_heatmap <- NULL
  if (demographic_variable %in% names(demographic_summaries)) {
    demographic_heatmap <- create_demographic_heatmap(
      demographic_summaries, 
      demographic_variable,
      topic_results$human_readable_topics,
      representative_comments
    )
  }
  
  # Create demographic summary table
  demographic_table <- NULL
  if (demographic_variable %in% names(demographic_summaries)) {
    demographic_table <- create_demographic_summary_table(
      demographic_summaries, 
      demographic_variable
    )
  }
  
  return(list(
    demographic_heatmap = demographic_heatmap,
    demographic_table = demographic_table,
    topic_results = topic_results,
    demographic_summaries = demographic_summaries,
    representative_comments = representative_comments
  ))
}
