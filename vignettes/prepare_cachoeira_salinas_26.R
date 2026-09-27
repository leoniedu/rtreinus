## Prepara o snapshot de dados da prova Cachoeira -> Salinas (2026-08-29).
## Rodar manualmente a partir da raiz do pacote (precisa de credenciais
## Treinus). O relatorio cachoeira_salinas_26.qmd le apenas o .rds gerado.
devtools::load_all()
library(dplyr)

session <- treinus_auth()

exercises <- treinus_get_exercises_db()

exercises_race <- exercises |>
  filter(
    as.Date(start) == "2026-08-29",
    start_time_as_string >= "04:00",
    start_time_as_string <= "10:00",
    !is.na(speed),
    ## skip aborted recordings (e.g. accidental 2-min runs); server can't analyze them
    total_time > 300
  )

exercise_analysis <- purrr::pmap(
  exercises_race,
  function(id_exercise, id_athlete, ...) {
    treinus_get_exercise_analysis(
      exercise_id = id_exercise,
      athlete_id = id_athlete,
      session = session
    )
  }
)

exercise_records <- purrr::map(
  exercise_analysis,
  ~ tibble(
    id_athlete = .x$data$Analysis$IdAthlete,
    id_exercise = .x$data$Analysis$IdExercise,
    fullname_athlete = .x$data$Analysis$User$FullName,
    treinus_extract_records(.x)
  )
) |>
  purrr::list_rbind()

dir.create("vignettes/data", showWarnings = FALSE)
saveRDS(exercise_records, "vignettes/data/cachoeira_salinas_26.rds")
