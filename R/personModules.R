personUI <- function(id) {
  ns <- NS(id)
  card(
    # full_screen = TRUE,
    card_body(
      fluidRow(column(2, actionButton(ns("create"), "Create")),
               column(2, actionButton(ns("duplicate"), "Duplicate")),
               column(2, actionButton(ns("delete"), "Delete"))),
       br(),
       fluidRow(
         tagList(
           createInputs(ns,
                        type = "person",
                        columns = columnNames(name = id, limit = 5) |> names())
          )
        ),
      # div(
      #   class = "reveal-on-full-screen",
      #   h5("Other columns"),
      #   fluidRow(
      #     tagList(createInputs(ns,
      #                          type = "person",
      #                          columns = c("person_id", "gender_concept_id", "year_of_birth"),
      #                          inverse = TRUE))
      #     )
      #   ),
      fillable = FALSE
      )
  )
  }

personServer <- function(id, cdm, trigger = NULL) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (!is.null(trigger)) {
      observeEvent(trigger(), {
        req(trigger() > 0)

        personTable <- cdm$person$data()

        new_choices <- personTable$person_id
        new_selected <- if (length(new_choices) > 0) new_choices[1] else NULL

        updateSelectInput(session, "person_id",
                          choices = new_choices,
                          selected = new_selected)

      })
    }
    # ------------------------------------------

    # CREATE
    observeEvent(input$create, {
      # Randomly select persons features
      gender_concept_id <- sample(c(8532, 8507), 1) # 8532 is Female, 8507 is male
      year_of_birth <- sample(1920:2004, 1)
      month_of_birth <- sample(seq(1, 12), 1)
      day_of_birth <- sample(seq(1, 31), 1)
      cdm$person$add(gender_concept_id = gender_concept_id,
                     year_of_birth = year_of_birth,
                     month_of_birth = month_of_birth,
                     day_of_birth = day_of_birth)
      personTable <- cdm$person$data()
      updateSelectInput(session, "person_id", choices = personTable$person_id, selected = personTable$person_id[length(personTable$person_id)])
    })

    # DUPLICATE
    observeEvent(input$duplicate, {
      cdm$person$add(gender_concept_id = input$gender_concept_id,
                        year_of_birth = input$year_of_birth,
                        month_of_birth = input$month_of_birth)
      personTable <- cdm$person$data()
      updateSelectInput(session, "person_id", choices = personTable$person_id, selected = personTable$person_id[length(personTable$person_id)])
    })

    # DELETE
    observeEvent(input$delete, {
      req(input$person_id)
      cdm$person$delete(person_id = as.integer(input$person_id))
      personTable <- cdm$person$data()
      # Update to last person before the deletion
      if (personTable$person_id %>% length() == 0) {
        # Update to null - If not, the value of the previous person remains after the person id is deleted
        updateSelectInput(session, "person_id", choices = personTable$person_id, selected = personTable$person_id[1])
        # ids <- c("gender_concept_id", "year_of_birth", "month_of_birth", "day_of_birth")
        # Update gender and year of birth for only that person
        updatePersonInputs(cdmPersonData = cdm$person$data(),
                           input_person_id = input$person_id,
                           ids = columnNames(name = id, limit = 5) |> names() |> tail(-1),
                           session = session)
      } else {
        updateSelectInput(session, "person_id", choices = personTable$person_id, selected = personTable$person_id[as.numeric(input$person_id) - 1])
      }

    })

    # UPDATE INTERFACE
    observeEvent(input$person_id, {
      req(input$person_id)
      ids <- setdiff(columnNames(name = "person") |> names(), "person_id")
      updatePersonInputs(cdmPersonData = cdm$person$data(),
                         input_person_id = input$person_id,
                         ids = ids[1:4],
                         session = session)

    }, ignoreInit = TRUE)

    # UPDATE DATA
    observeEvent({
      input$gender_concept_id
      input$year_of_birth
      input$month_of_birth
      input$day_of_birth
    }, {
      req(input$gender_concept_id,
          input$year_of_birth,
          input$month_of_birth,
          input$day_of_birth)
      cdm$person$update(person_id = input$person_id |> as.integer(),
                        gender_concept_id = input$gender_concept_id |> as.integer(),
                        year_of_birth = input$year_of_birth |> as.integer(),
                        month_of_birth = input$month_of_birth |> as.integer(),
                        day_of_birth = input$day_of_birth |> as.integer())

    }, ignoreInit = TRUE)

    return(reactive(input$person_id))

  })
}
