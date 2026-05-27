#' PatientDesigner chat module
#'
#' Internal chat module used by `patientDesigner()`.
#'
#' @param id Module id.
#' @noRd
patientDesignerChatUI <- function(id) {
  ns <- shiny::NS(id)

    shiny::tagList(
    shiny::tags$style(shiny::HTML(paste0(
      "#", ns("chat_shell"), " { display: flex; flex-direction: column; height: 75vh; gap: 0.75rem; }",
      "#", ns("messages"), " { flex: 1 1 auto; overflow-y: auto; padding: 0.75rem; background: #f8fafc;",
      " border: 1px solid #d9e2ec; border-radius: 0.5rem; }",
      "#", ns("composer"), " { display: flex; flex-direction: column; gap: 0.5rem; }",
      "#", ns("status"), " { min-height: 1.25rem; }",
      ".", ns("message"), " { margin-bottom: 0.75rem; display: flex; }",
      ".", ns("message_user"), " { justify-content: flex-end; }",
      ".", ns("message_assistant"), " { justify-content: flex-start; }",
      ".", ns("bubble"), " { max-width: 92%; padding: 0.65rem 0.8rem; border-radius: 0.75rem; line-height: 1.4; }",
      ".", ns("bubble_user"), " { background: #2f6fed; color: #fff; }",
      ".", ns("bubble_assistant"), " { background: #fff; color: #1f2933; border: 1px solid #d9e2ec; }",
      ".", ns("bubble_pending"), " { border-style: dashed; color: #52606d; }",
      ".", ns("meta"), " { font-size: 0.75rem; color: #6b7280; margin-bottom: 0.35rem; }"
    ))),
    shiny::div(
      id = ns("chat_shell"),
      shiny::div(
        class = "small text-muted",
        "Send a prompt to generate a test set and load it into PatientDesigner."
      ),
      shiny::uiOutput(ns("messages")),
      shiny::div(
        id = ns("composer"),
        shiny::uiOutput(ns("status")),
        shiny::textAreaInput(
          ns("draft"),
          label = NULL,
          placeholder = "Describe the patient or test case you want to create...",
          rows = 4,
          resize = "vertical"
        ),
        shiny::uiOutput(ns("send_ui"))
      )
    )
  )
}

#' @param reset_trigger Optional reactive trigger to clear the chat.
#' @param on_submit Optional callback invoked with the submitted prompt.
#' @param on_reset Optional callback invoked when the chat is reset.
#' @noRd
  patientDesignerChatServer <- function(id,
                                      reset_trigger = NULL,
                                      on_submit = NULL,
                                      on_reset = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    message_counter <- 0L
    busy <- shiny::reactiveVal(FALSE)

    next_message_id <- function() {
      message_counter <<- message_counter + 1L
      id <- message_counter
      id
    }

    messages <- shiny::reactiveVal(list(
      list(
        id = next_message_id(),
        role = "assistant",
        text = paste(
          "Test patients AI assistant.",
          "Describe the synthetic patient set you want to generate."
        ),
        pending = FALSE
      )
    ))

    append_message <- function(role, text, pending = FALSE) {
      current_messages <- messages()
      current_messages[[length(current_messages) + 1L]] <- list(
        id = next_message_id(),
        role = role,
        text = text,
        pending = pending
      )
      messages(current_messages)
      current_messages[[length(current_messages)]]$id
    }

    update_message <- function(message_id, text, pending = FALSE) {
      current_messages <- messages()
      idx <- vapply(current_messages, function(message) identical(message$id, message_id), logical(1))
      if (!any(idx)) {
        return(invisible(NULL))
      }
      current_messages[[which(idx)[1]]]$text <- text
      current_messages[[which(idx)[1]]]$pending <- pending
      messages(current_messages)
    }

    reset_messages <- function() {
      message_counter <<- 0L
      messages(list(
        list(
          id = next_message_id(),
          role = "assistant",
          text = paste(
            "Started a new chat.",
            "Describe the next synthetic patient set you want to generate."
          ),
          pending = FALSE
        )
      ))
    }

    output$status <- shiny::renderUI({
      if (!isTRUE(busy())) {
        return(NULL)
      }

      shiny::div(
        id = session$ns("status"),
        class = "small text-muted d-flex align-items-center gap-2",
        shiny::tags$span(class = "spinner-border spinner-border-sm", role = "status", `aria-hidden` = "true"),
        "Generating test set..."
      )
    })

    output$send_ui <- shiny::renderUI({
      shiny::actionButton(
        session$ns("send"),
        if (isTRUE(busy())) "Generating..." else "Send",
        icon = shiny::icon(if (isTRUE(busy())) "spinner" else "paper-plane"),
        class = "btn-primary",
        disabled = if (isTRUE(busy())) "disabled" else NULL
      )
    })

    output$messages <- shiny::renderUI({
      shiny::tagList(lapply(messages(), function(message) {
        role_class <- if (identical(message$role, "user")) "user" else "assistant"

        shiny::div(
          class = paste(session$ns("message"), session$ns(paste0("message_", role_class))),
          shiny::div(
            class = paste(
              session$ns("bubble"),
              session$ns(paste0("bubble_", role_class)),
              if (isTRUE(message$pending)) session$ns("bubble_pending") else NULL
            ),
            if (!identical(role_class, "user")) {
              shiny::div(
                class = session$ns("meta"),
                if (identical(message$text, paste(
                  "Started a new chat.",
                  "Describe the next synthetic patient set you want to generate."
                ))) {
                  "System"
                } else {
                  "Assistant"
                }
              )
            },
            shiny::tags$p(style = "margin: 0; white-space: pre-wrap;", message$text)
          )
        )
      }))
    })

    shiny::observeEvent(input$send, {
      if (isTRUE(busy())) {
        return()
      }

      draft <- trimws(shiny::req(input$draft))

      append_message("user", draft)
      shiny::updateTextAreaInput(session, "draft", value = "")
      pending_id <- append_message(
        "assistant",
        "Generating test set...",
        pending = TRUE
      )
      busy(TRUE)

      session$onFlushed(function() {
        on.exit(busy(FALSE), add = TRUE)

        if (is.null(on_submit)) {
          update_message(
            pending_id,
            paste(
              "Message captured.",
              "No backend is connected yet, so this interface is only storing",
              "the conversation locally in the session."
            ),
            pending = FALSE
          )
          return()
        }

        result <- tryCatch(
          on_submit(draft),
          error = function(e) e
        )

        if (inherits(result, "error")) {
          update_message(pending_id, conditionMessage(result), pending = FALSE)
          return()
        }

        if (is.list(result) && !is.null(result$message)) {
          update_message(pending_id, as.character(result$message), pending = FALSE)
          return()
        }

        if (is.character(result) && length(result) > 0) {
          update_message(pending_id, result[[1]], pending = FALSE)
          return()
        }

        update_message(pending_id, "Generation finished.", pending = FALSE)
      }, once = TRUE)
    })

    if (!is.null(reset_trigger)) {
      shiny::observeEvent(reset_trigger(), {
        reset_messages()
        if (!is.null(on_reset)) {
          on_reset()
        }
      }, ignoreInit = TRUE)
    }
  })
}
