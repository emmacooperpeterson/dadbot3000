library(tidyverse)
library(markovifyR) # reticulate::conda_install(packages="markovify")
library(future)
library(furrr)
library(tictoc)



#' Build a Markov model based on the input_text
#'
#' @param input_text character vector, each element is a text/sentence
#' @param markov_state_size numeric, the probability of the next word depends on
#' the two previous words. lower = weirder sentences (two is the default)
#' @param max_overlap_ratio numeric, generated sentences will not overlap
#' original text by more than this % of sentence's word count
#' @return Markov model object
create_model <- function(input_text, markov_state_size, max_overlap_ratio) {
  generate_markovify_model(
    input_text=input_text,
    markov_state_size=as.integer(markov_state_size),
    max_overlap_ratio=max_overlap_ratio
  )
}


#' Generate a set of texts from a pre-built model
#'
#' @param markov_model Model object from create_model
#' @param maximum_sentence_length int, length limit for generated texts
#' @param count numeric, number of sentence to generate
#' @return character vector, each element is a generated sentence
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



# Test various parameters -------------------------------------------------
# We need to determine some limits on each parameter in order to know what
# inputs we should allow within the app. Some values for these parameters
# result in little to no sentences being generated.


#' Build model and generate texts
#'
#' @param input_text character vector, each element is a text/sentence
#' @param markov_state_size numeric, the probability of the next word depends on
#' the two previous words. lower = weirder sentences (two is the default)
#' @param max_overlap_ratio numeric, generated sentences will not overlap
#' original text by more than this % of sentence's word count
#' @param maximum_sentence_length numeric, length limit for generated texts
#' @param return_texts bool, if TRUE return just the generated texts. If FALSE
#' return diagnostics
build_and_generate <- function(input_text, markov_state_size,
                        max_overlap_ratio, maximum_sentence_length,
                        return_texts=FALSE) {

  model <- create_model(
    input_text=input_text,
    markov_state_size=markov_state_size,
    max_overlap_ratio=max_overlap_ratio
  )

  texts <- generate_texts(
    markov_model=model,
    maximum_sentence_length=maximum_sentence_length
  )

  if (return_texts) return(texts)

  return(list(
    "markov_state_size" = markov_state_size,
    "max_overlap_ratio" = max_overlap_ratio,
    "maximum_sentence_length" = maximum_sentence_length,
    "num" = length(texts)
  )
  )
}


# read in training data
text_data <- read_csv("data/texts.csv", col_names=c("text")) %>% pull(text)


# specify params to test (expand.grid creates all possible combos)
params <- expand.grid(
  list(
    "markov_state_size" = 1:5,
    "max_overlap_ratio" = seq(0, 1, by=0.15),
    "maximum_sentence_length" = seq(1, 100, by=10)
  )
)


# process in parallel since there are so many combinations to test
# be warned: this takes a long time :-)
plan(future::multisession, workers = floor(availableCores() / 2))

tic()

result <- furrr::future_pmap_dfr(
  params,
  build_and_generate,
  input_text=text_data,
  .options=furrr_options(seed = TRUE),
  .progress=TRUE
)

toc()

# check the distribution of number of sentences generated by each combo
# markov_state_size: big dropoff after 2. limit to 1-2
mss <-
  result %>%
  group_by(markov_state_size) %>%
  summarize(avg_n = mean(num), med_n = median(num), total_n = sum(num))

# max_overlap_ratio: limit to 0.25-1
mor <-
  result %>%
  group_by(max_overlap_ratio) %>%
  summarize(avg_n = mean(num), med_n = median(num), total_n = sum(num))

# maximum_sentence_length: anything > 20 is probably okay
msl <-
  result %>%
  group_by(msl) %>%
  summarize(avg_n = mean(num), med_n = median(num), total_n = sum(num))



# Generate texts for each set of allowed inputs  --------------------------
# Now that we've determined reasonable limits for each parameter (above),
# we'll generate texts for each combination of inputs. This way, the app
# only needs to read in the correct set of pre-generated texts.

final_params <- expand.grid(
  list(
    "markov_state_size" = 1:2,
    "max_overlap_ratio" = seq(0.2, 0.8, by=0.2),
    "maximum_sentence_length" = seq(20, 100, by=20)
  )
)

plan(future::multisession, workers = floor(availableCores() / 2))

result <- furrr::future_pmap(
  final_params,
  build_and_generate,
  input_text=text_data,
  return_texts=TRUE,
  .options=furrr_options(seed = TRUE),
  .progress=TRUE
)

final_params$index <- 1:nrow(final_params)

saveRDS(final_params, file="dadbot3000/data/inputs.RDS")
saveRDS(result, file="dadbot3000/data/generated_texts.RDS")
