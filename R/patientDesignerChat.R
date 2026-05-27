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
      ".", ns("message"), " { margin-bottom: 0.75rem; display: flex; }",
      ".", ns("message_user"), " { justify-content: flex-end; }",
      ".", ns("message_assistant"), " { justify-content: flex-start; }",
      ".", ns("bubble"), " { max-width: 92%; padding: 0.65rem 0.8rem; border-radius: 0.75rem; line-height: 1.4; }",
      ".", ns("bubble_user"), " { background: #2f6fed; color: #fff; }",
      ".", ns("bubble_assistant"), " { background: #fff; color: #1f2933; border: 1px solid #d9e2ec; }",
      ".", ns("meta"), " { font-size: 0.75rem; color: #6b7280; margin-bottom: 0.35rem; }"
    ))),
    shiny::div(
      id = ns("chat_shell"),
      shiny::div(
        class = "small text-muted",
        "Chat interface only. Backend integration comes later."
      ),
      shiny::uiOutput(ns("messages")),
      shiny::div(
        id = ns("composer"),
        shiny::textAreaInput(
          ns("draft"),
          label = NULL,
          placeholder = "Describe the patient or test case you want to create...",
          rows = 4,
          resize = "vertical"
        ),
        shiny::actionButton(
          ns("send"),
          "Send",
          icon = shiny::icon("paper-plane"),
          class = "btn-primary"
        )
      )
    )
  )
}

#' @param reset_trigger Optional reactive trigger to clear the chat.
#' @noRd
patientDesignerChatServer <- function(id, reset_trigger = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    messages <- shiny::reactiveVal(list(
      list(
        role = "assistant",
        text = paste(
          "Test patients AI assistant"
        )
      )
    ))

    append_message <- function(role, text) {
      current_messages <- messages()
      current_messages[[length(current_messages) + 1L]] <- list(
        role = role,
        text = text
      )
      messages(current_messages)
    }

    reset_messages <- function() {
      messages(list(
        list(
          role = "assistant",
          text = paste(
            "Started a new chat.",
            "This is currently a UI-only placeholder."
          )
        )
      ))
    }

    output$messages <- shiny::renderUI({
      shiny::tagList(lapply(messages(), function(message) {
        role_class <- if (identical(message$role, "user")) "user" else "assistant"

        shiny::div(
          class = paste(session$ns("message"), session$ns(paste0("message_", role_class))),
          shiny::div(
            class = paste(session$ns("bubble"), session$ns(paste0("bubble_", role_class))),
            if (!identical(role_class, "user")) {
              shiny::div(
                class = session$ns("meta"),
                if (identical(message$text, paste(
                  "Started a new chat.",
                  "This is currently a UI-only placeholder."
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
      draft <- trimws(shiny::req(input$draft))

      append_message("user", draft)
      append_message(
        "assistant",
        paste(
          "Message captured.",
          "No backend is connected yet, so this interface is only storing",
          "the conversation locally in the session."
        )
      )
      shiny::updateTextAreaInput(session, "draft", value = "")
    })

    if (!is.null(reset_trigger)) {
      shiny::observeEvent(reset_trigger(), {
        reset_messages()
      }, ignoreInit = TRUE)
    }
  })
}
