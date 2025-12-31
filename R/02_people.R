peopleInput <- function(id) {
  layout_columns(
    card(
      card_header("NPS Authors (email)"),
      selectizeInput("authors",
                     label = NULL,
                     multiple = TRUE,
                     choices = NULL,
                     options = list(create = TRUE),
                     width = "100%"),
      helpText("Authors must be individuals (not organizations) and must ",
               "have ORCIDs. See NPS IMD guidance on ",
               a("best practices for authorship",
               href = paste0("https://doimspp.sharepoint.com/sites/nps-nrss",
                             "-imdiv/data-publication/SitePages/Data-package",
                             "-authorship.aspx?csf=1&web=1&e=ll809Q"),
               target = "_blank"),
               " for additional information."),
      reactable::reactableOutput("valid_authors", width = "100%"),
      fill = FALSE
    ),
    card(
      card_header("Contacts"),
      textInput(NS(id, "contacts"),
                label = NULL,
                width = "100%",
                updateOn = "blur"),
      helpText('Contacts must be NPS employees or partners and should be ',
               'familiar with all aspects of the data package. Contacts are ',
               'almost always one or more of the authors. Consider "contacts"',
               ' similar to "corresponding author" in journal publications.')
    ),
    card(
      card_header("Contributers"),
      textInput(NS(id, "contacts"), 
                label = NULL, 
                width = "100%",
                updateOn = "blur"),
      helpText("Contributors are personell who did not rise to the level of ",
               "authorship but should still be acknowledged. Each contributor ",
               "requires a role. Roles are open format and can be anything ",
               "you choose (e.g. Field Assistant).")
    ),
    card(
      card_header("Editors"),
      textInput(NS(id, "contacts"),
                label = NULL,
                width = "100%",
                updateOn = "blur"),
      helpText("Editors will have the ability to make reasonable updates to ",
               "the DataStore reference such as fixing typos or updating ",
               "permissions. Only editors can access a reference when it is ",
               "in draft status so include potential data package revieres ",
               "as editors (they can be removed later). Editors must be NPS ",
               "employees or partners.")
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

peopleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    #---- Validate emails of reference authors ----
    valid_authors <- reactive({
      if (length(input$authors) > 0) {
        valid_authors <- NPSdatastore::active_directory_lookup(emails = input$authors) |>
          dplyr::mutate(Valid_TF = found & !disabled) |>
          dplyr::mutate(Valid = ifelse(Valid_TF, "\u2713", "\u2715")) |>
          dplyr::select(Valid_TF, Valid, Email = searchTerm, Name = cn)
      } else {
        valid_authors <- NA
      }
      valid_authors
    }) |>
      bindEvent(input$authors)
    
    output$valid_authors <- renderReactable(reactable(valid_authors()[, c("Valid", "Email", "Name")]))
    
  })
}
  