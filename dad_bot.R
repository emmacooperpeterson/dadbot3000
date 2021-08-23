library(tidyverse)
library(markovifyR) # reticulate::conda_install(packages="markovify")

text_data <- read_csv("texts.csv", col_names=c("text")) %>% pull(text)

# markov_state_size: the probability of the next word depends on the two
# previous words. lower = weirder sentences (two is the default)

# max_overlap_ratio: generated sentences will not overlap original text
# by more than this % of sentence's word count
create_model <- function(input_text, markov_state_size, max_overlap_ratio) {
  generate_markovify_model(
    input_text=input_text,
    markov_state_size=as.integer(markov_state_size),
    max_overlap_ratio=max_overlap_ratio
  )
}

generate_texts <- function(markov_model, maximum_sentence_length, count=100) {
  texts <- markovify_text(
    markov_model=markov_model,
    maximum_sentence_length=maximum_sentence_length,
    output_column_name='text',
    count=count,
    tries=100,
    only_distinct=TRUE,
    return_message=FALSE
  ) %>% pull(text)

  return(texts[!is.na(texts)])
}





