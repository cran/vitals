## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  eval = rlang::is_installed("ggplot2")
)

cat <- function(x, width = 0.9 * getOption("width")) {
  lines <- unlist(strsplit(x, "\n"))
  wrapped <- unlist(lapply(lines, strwrap, width = width))
  base::cat(wrapped, sep = "\n")
}

withr::local_envvar(list(VITALS_LOG_DIR = here::here("vignettes/data/logs/")))

# don't set this as the default `eval`, but use it as a
# flag for the computationally intensive steps
should_eval <- identical(Sys.getenv("VITALS_SHOULD_EVAL"), "true")

if (!should_eval) {
  load(here::here("vignettes/data/are_task.rda"))
  load(here::here("vignettes/data/are_task_openai.rda"))
}

## -----------------------------------------------------------------------------
library(vitals)
library(ellmer)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
glimpse(are)

## -----------------------------------------------------------------------------
cat(are$input[1])

## -----------------------------------------------------------------------------
cat(are$target[1])

## -----------------------------------------------------------------------------
# are_task <- Task$new(
#   dataset = are,
#   solver = generate(chat_anthropic(model = "claude-3-7-sonnet-latest")),
#   scorer = model_graded_qa(partial_credit = TRUE),
#   name = "An R Eval"
# )
# 
# are_task

## -----------------------------------------------------------------------------
# are_task$eval()

## -----------------------------------------------------------------------------
if (should_eval) {
  save(are_task, file = here::here("vignettes/data/are_task.rda"))
}

## -----------------------------------------------------------------------------
cat(are_task$get_samples()$result[1])

## -----------------------------------------------------------------------------
knitr::include_graphics("https://cdn-useast1.kapwing.com/static/templates/3-spiderman-pointing-meme-template-full-ca8f27e0.webp")

## -----------------------------------------------------------------------------
cat(are_task$get_samples()$scorer_chat[[1]]$last_turn()@text)

## -----------------------------------------------------------------------------
if (identical(Sys.getenv("IN_PKGDOWN"), "true")) {
  htmltools::tags$iframe(
    src = "../example-logs/vitals/index.html",
    width = "100%", 
    height = "600px",
    style = "border-radius: 10px; box-shadow: 0 5px 10px rgba(0, 0, 0, 0.3);"
  ) 
} else {
  knitr::include_graphics("data/are_viewer.png")
}

## -----------------------------------------------------------------------------
are_task_data <- vitals_bind(are_task)

are_task_data

are_task_data |>
  ggplot() +
  aes(x = score) +
  geom_bar()

## -----------------------------------------------------------------------------
# are_task_openai <- are_task$clone()
# are_task_openai$eval(solver_chat = chat_openai(model = "gpt-4o"))

## -----------------------------------------------------------------------------
if (should_eval) {
  save(are_task_openai, file = here::here("vignettes/data/are_task_openai.rda"))
}

## -----------------------------------------------------------------------------
are_task_eval <-
  vitals_bind(are_task, are_task_openai) |>
  mutate(
    task = if_else(task == "are_task", "Claude", "GPT-4o")
  ) |>
  rename(model = task)

are_task_eval |>
  mutate(
    score = factor(
      case_when(
        score == "I" ~ "Incorrect",
        score == "P" ~ "Partially correct",
        score == "C" ~ "Correct"
      ),
      levels = c("Incorrect", "Partially correct", "Correct"),
      ordered = TRUE
    )
  ) |>
  ggplot(aes(y = model, fill = score)) +
  geom_bar() +
  scale_fill_brewer(breaks = rev, palette = "RdYlGn")

## -----------------------------------------------------------------------------
library(ordinal)

are_mod <- clm(score ~ model, data = are_task_eval)

are_mod

## -----------------------------------------------------------------------------
grade_descriptor <- if (are_mod[["coefficients"]][3] > 0) "higher" else "lower"

## -----------------------------------------------------------------------------
confint(are_mod)

## -----------------------------------------------------------------------------
# # deploy the resulting logs inside of the page by bundling them into
# # `pkgdown/assets/`
# dest_dir <- here::here("pkgdown/assets/example-logs/vitals")
# vitals_bundle(
#   log_dir = here::here("vignettes/data/logs"),
#   output_dir = dest_dir,
#   overwrite = TRUE
# )

