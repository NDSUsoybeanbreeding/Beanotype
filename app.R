# Agriplex Trait Marker Genotype to Phenotype Converter
# Checkbox filters + plots + phenotype selection + multi-trait filter

library(shiny)
library(readxl)
library(tidyr)
library(dplyr)
library(DT)
library(writexl)
library(ggplot2)
library(plotly)

library("PKI")
library("rsconnect")
library(rsconnect)
rsconnect::deployApp(rsconnect::setAccountInfo(name='ndsusoybeanbreeding',
                                               token='8100593667208B3C4ACE880F790F26BB',
                                               secret='e/WNA0HU6Wc4u38W2YkoTtIqTeoI4jQbVCj97NJ3'))

# ================= TEMPLATE =================
template_path <- "ChatGPT_Updated_Trait marker conversion template.xlsx"
template <- read_excel(template_path, sheet = 1, col_names = FALSE)

template_df <- tibble(
  Marker_Type = as.character(unlist(template[1, -1])),
  Agriplex_Trait_Label = as.character(unlist(template[2, -1])),
  Marker_Purpose = as.character(unlist(template[3, -1])),
  Category = as.character(unlist(template[4, -1])),
  Marker_Name = as.character(unlist(template[5, -1])),
  Ref_Pheno = as.character(unlist(template[6, -1])),
  Alt_Pheno = as.character(unlist(template[7, -1])),
  Ref_Alt = as.character(unlist(template[8, -1]))
) %>%
  mutate(
    Ref = substr(Ref_Alt, 1, 1),
    Alt = substr(Ref_Alt, 3, 3)
  )

# ================= UI =================
ui <- fluidPage(
  titlePanel("Agriplex Trait Marker Genotype to Phenotype Converter"),
  sidebarLayout(
    sidebarPanel(
      tags$h4("Instructions"),
      tags$ol(
        tags$li("Download the trait genotype data template outside of this app."),
        tags$li("Copy and paste Sample IDs and trait marker genotype calls into the template."),
        tags$li("Upload your completed file into the app.")
      ),
      fileInput("genofile", "Upload Genotype Excel File", accept = ".xlsx"),
      checkboxGroupInput("category_filter", "Filter by Category:", choices = NULL),
      
      tags$details(
        tags$summary(HTML("<b>&#x25B6; Trait Filters</b>")),
        div(
          style = "border: 1px solid #ccc; padding: 5px; margin-bottom: 10px; background-color: #f9f9f9;",
          actionButton("select_all_traits", "Select All Traits"),
          actionButton("clear_all_traits", "Clear All Traits"),
          uiOutput("trait_filter_ui")
        )
      ),
      
      tags$details(
        tags$summary(HTML("<b>&#x25B6; Sample Filters</b>")),
        div(
          style = "border: 1px solid #ccc; padding: 5px; margin-bottom: 10px; background-color: #f9f9f9;",
          actionButton("select_all_samples", "Select All Samples"),
          actionButton("clear_all_samples", "Clear All Samples"),
          uiOutput("sample_filter_ui")
        )
      ),
      
      tags$details(
        tags$summary(HTML("<b>&#x25B6; Phenotype Filters</b>")),
        div(
          style = "border: 1px solid #ccc; padding: 5px; margin-bottom: 10px; background-color: #f9f9f9;",
          actionButton("select_all_phenotypes", "Select All Phenotypes"),
          actionButton("clear_all_phenotypes", "Clear All Phenotypes"),
          uiOutput("phenotype_filter_ui")
        )
      ),
      
      downloadButton("downloadData", "Download Results"),
      downloadButton("downloadPhenotypeLines", "Download Phenotype Sample Names")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DTOutput("resultTable")),
        tabPanel("Summary", plotlyOutput("summaryPlot"), DTOutput("summaryTable")),
        tabPanel(
          "Phenotype",
          plotlyOutput("phenotypePlot"),
          DTOutput("phenotypeTable"),
          tags$hr(),
          tags$h4("Sample Names by Phenotype"),
          actionButton("clear_selected_bar", "Clear Selected Phenotype Selection"),
          DTOutput("phenotypeLinesTable")
        ),
        tabPanel(
          "Multi-Trait Filter",
          fluidRow(
            column(
              4,
              selectInput("mt_trait", "Trait", choices = NULL),
              selectInput("mt_pheno", "Phenotype", choices = NULL),
              actionButton("add_requirement", "Add Requirement"),
              actionButton("clear_requirements", "Clear Requirements"),
              tags$hr(),
              downloadButton("downloadMultiTrait", "Download Matching Lines")
            ),
            column(
              8,
              tags$h4("Active Requirements"),
              DTOutput("requirementsTable"),
              tags$hr(),
              tags$h4("Lines Matching All Requirements"),
              DTOutput("multiTraitResults")
            )
          )
        )
      )
    )
  )
)

# ================= SERVER =================
server <- function(input, output, session) {
  
  processed_data <- reactiveVal(NULL)
  phenotype_click <- reactiveVal(NULL)
  requirements <- reactiveVal(data.frame(
    Agriplex_Trait_Label = character(),
    Allele_Phenotype = character(),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$genofile, {
    req(input$genofile)
    
    geno <- read_excel(input$genofile$datapath, col_names = FALSE)
    trait_labels_raw <- as.character(unlist(geno[1, -1]))
    marker_names <- as.character(unlist(geno[2, -1]))
    safe_names <- make.names(trait_labels_raw, unique = TRUE)
    
    geno_data <- geno[4:nrow(geno), , drop = FALSE]
    colnames(geno_data) <- c("Sample_ID", safe_names)
    
    geno_long <- pivot_longer(
      geno_data,
      -Sample_ID,
      names_to = "Trait",
      values_to = "Genotype"
    )
    geno_long$Marker_Name <- marker_names[match(geno_long$Trait, safe_names)]
    
    df <- geno_long %>%
      left_join(template_df, by = "Marker_Name") %>%
      mutate(
        Genotype = toupper(trimws(as.character(Genotype))),
        Is_Het = grepl("/", Genotype) & vapply(strsplit(Genotype, "/"), function(x) length(unique(x)) > 1, logical(1)),
        Allele_Phenotype = case_when(
          Genotype == Ref ~ Ref_Pheno,
          Genotype == Alt ~ Alt_Pheno,
          Is_Het ~ paste(Ref_Pheno, "/", Alt_Pheno),
          TRUE ~ "Fail"
        ),
        Phenotype_Class = case_when(
          Genotype == Ref ~ "Ref",
          Genotype == Alt ~ "Alt",
          Is_Het ~ "Het",
          TRUE ~ "Fail"
        ),
        Genotype_Color = case_when(
          Phenotype_Class == "Ref" ~ "background-color: #93c47d;",
          Phenotype_Class == "Alt" ~ "background-color: #ffd966;",
          Phenotype_Class == "Het" ~ "background-color: #f6b26b;",
          Phenotype_Class == "Fail" ~ "background-color: #d9d9d9;",
          TRUE ~ ""
        )
      )
    
    processed_data(df)
    phenotype_click(NULL)
    requirements(data.frame(
      Agriplex_Trait_Label = character(),
      Allele_Phenotype = character(),
      stringsAsFactors = FALSE
    ))
    
    updateSelectInput(session, "mt_trait", choices = sort(unique(df$Agriplex_Trait_Label)))
  })
  
  data <- reactive({
    req(processed_data())
    processed_data()
  })
  
  observeEvent(data(), {
    updateCheckboxGroupInput(session, "category_filter", choices = sort(unique(data()$Category)))
  }, ignoreInit = TRUE)
  
  output$trait_filter_ui <- renderUI({
    req(data())
    checkboxGroupInput(
      "trait_filter",
      "Select Trait(s):",
      choices = sort(unique(data()$Agriplex_Trait_Label)),
      selected = sort(unique(data()$Agriplex_Trait_Label))
    )
  })
  
  output$sample_filter_ui <- renderUI({
    req(data())
    checkboxGroupInput(
      "sample_filter",
      "Select Sample(s):",
      choices = sort(unique(data()$Sample_ID)),
      selected = sort(unique(data()$Sample_ID))
    )
  })
  
  output$phenotype_filter_ui <- renderUI({
    req(data())
    checkboxGroupInput(
      "phenotype_filter",
      "Select Phenotype(s):",
      choices = c("Ref", "Alt", "Het", "Fail"),
      selected = c("Ref", "Alt", "Het", "Fail")
    )
  })
  
  observe({
    req(data())
    updateSelectInput(
      session,
      "mt_trait",
      choices = sort(unique(data()$Agriplex_Trait_Label)),
      selected = sort(unique(data()$Agriplex_Trait_Label))[1]
    )
  })
  
  observe({
    req(data(), input$mt_trait)
    pheno_choices <- data() %>%
      filter(Agriplex_Trait_Label == input$mt_trait) %>%
      pull(Allele_Phenotype) %>%
      unique() %>%
      sort()
    
    updateSelectInput(
      session,
      "mt_pheno",
      choices = pheno_choices,
      selected = if (length(pheno_choices) > 0) pheno_choices[1] else character(0)
    )
  })
  
  observeEvent(input$add_requirement, {
    req(input$mt_trait)
    req(input$mt_pheno)
    
    current <- requirements()
    new_row <- data.frame(
      Agriplex_Trait_Label = as.character(input$mt_trait),
      Allele_Phenotype = as.character(input$mt_pheno),
      stringsAsFactors = FALSE
    )
    
    updated <- bind_rows(current, new_row) %>% distinct()
    requirements(updated)
  })
  
  observeEvent(input$clear_requirements, {
    requirements(data.frame(
      Agriplex_Trait_Label = character(),
      Allele_Phenotype = character(),
      stringsAsFactors = FALSE
    ))
  })
  
  observeEvent(input$select_all_traits, {
    req(data())
    updateCheckboxGroupInput(session, "trait_filter", selected = sort(unique(data()$Agriplex_Trait_Label)))
  })
  
  observeEvent(input$clear_all_traits, {
    updateCheckboxGroupInput(session, "trait_filter", selected = character(0))
  })
  
  observeEvent(input$select_all_samples, {
    req(data())
    updateCheckboxGroupInput(session, "sample_filter", selected = sort(unique(data()$Sample_ID)))
  })
  
  observeEvent(input$clear_all_samples, {
    updateCheckboxGroupInput(session, "sample_filter", selected = character(0))
  })
  
  observeEvent(input$clear_selected_bar, {
    phenotype_click(NULL)
  })
  
  observeEvent(input$select_all_phenotypes, {
    updateCheckboxGroupInput(session, "phenotype_filter", selected = c("Ref", "Alt", "Het", "Fail"))
    phenotype_click(NULL)
  })
  
  observeEvent(input$clear_all_phenotypes, {
    updateCheckboxGroupInput(session, "phenotype_filter", selected = character(0))
    phenotype_click(NULL)
  })
  
  filtered <- reactive({
    df <- data()
    
    if (!is.null(input$category_filter) && length(input$category_filter) > 0) {
      df <- df %>% filter(Category %in% input$category_filter)
    }
    if (!is.null(input$trait_filter) && length(input$trait_filter) > 0) {
      df <- df %>% filter(Agriplex_Trait_Label %in% input$trait_filter)
    }
    if (!is.null(input$sample_filter) && length(input$sample_filter) > 0) {
      df <- df %>% filter(Sample_ID %in% input$sample_filter)
    }
    if (!is.null(input$phenotype_filter) && length(input$phenotype_filter) > 0) {
      df <- df %>% filter(Phenotype_Class %in% input$phenotype_filter)
    }
    
    clicked <- phenotype_click()
    if (!is.null(clicked)) {
      df <- df %>% filter(
        Agriplex_Trait_Label == clicked$trait,
        Allele_Phenotype == clicked$phenotype
      )
    }
    
    df
  })
  
  output$resultTable <- renderDT({
    df <- filtered()
    req(nrow(df) > 0)
    df$Genotype_Colored <- paste0("<div style='", df$Genotype_Color, " padding:4px'>", df$Genotype, "</div>")
    datatable(
      df %>% select(Sample_ID, Marker_Purpose, Ref, Alt, Genotype_Colored, Allele_Phenotype, Category, Agriplex_Trait_Label, Marker_Name, Marker_Type),
      colnames = c("Sample_ID", "Marker_Purpose", "Ref", "Alt", "Genotype_Call", "Allele_Phenotype", "Category", "Agriplex_Trait_Label", "Marker_Name", "Marker_Type"),
      options = list(pageLength = 25, scrollX = TRUE, filter = "top"),
      escape = FALSE
    )
  })
  
  output$summaryPlot <- renderPlotly({
    plot_data <- filtered() %>%
      count(Agriplex_Trait_Label, Phenotype_Class) %>%
      group_by(Agriplex_Trait_Label) %>%
      mutate(Percent = round(100 * n / sum(n), 1)) %>%
      ungroup()
    
    p <- ggplot(plot_data, aes(x = Agriplex_Trait_Label, y = Percent, fill = Phenotype_Class,
                               text = paste0("Trait: ", Agriplex_Trait_Label,
                                             "<br>Allele class: ", Phenotype_Class,
                                             "<br>Percent: ", Percent, "%"))) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("Ref" = "#93c47d", "Alt" = "#ffd966", "Het" = "#f6b26b", "Fail" = "#d9d9d9")) +
      labs(title = "Allele Class Distribution per Trait", x = "Trait", y = "Percentage") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$summaryTable <- renderDT({
    df <- filtered() %>%
      count(Marker_Name, Agriplex_Trait_Label, Ref, Alt, Phenotype_Class) %>%
      group_by(Marker_Name, Agriplex_Trait_Label, Ref, Alt) %>%
      mutate(Percent = round(100 * n / sum(n), 1)) %>%
      pivot_wider(names_from = Phenotype_Class, values_from = c(n, Percent), values_fill = 0) %>%
      ungroup()
    
    datatable(df, options = list(pageLength = 20, scrollX = TRUE))
  })
  
  phenotype_summary <- reactive({
    filtered() %>%
      count(Agriplex_Trait_Label, Allele_Phenotype, name = "Line_Count") %>%
      group_by(Agriplex_Trait_Label) %>%
      mutate(Percent = round(100 * Line_Count / sum(Line_Count), 1)) %>%
      ungroup()
  })
  
  output$phenotypePlot <- renderPlotly({
    df <- phenotype_summary()
    req(nrow(df) > 0)
    
    p <- ggplot(
      df,
      aes(
        x = Agriplex_Trait_Label,
        y = Line_Count,
        fill = Allele_Phenotype,
        key = paste(Agriplex_Trait_Label, Allele_Phenotype, sep = "||"),
        text = paste0(
          "Trait: ", Agriplex_Trait_Label,
          "<br>Phenotype: ", Allele_Phenotype,
          "<br>Lines: ", Line_Count,
          "<br>Percent: ", Percent, "%"
        )
      )
    ) +
      geom_col(position = "dodge") +
      labs(title = "Number of Lines by Phenotype", x = "Trait", y = "Number of Lines", fill = "Phenotype") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text", source = "phenotype_source")
  })
  
  observeEvent(event_data("plotly_click", source = "phenotype_source"), {
    click <- event_data("plotly_click", source = "phenotype_source")
    req(click)
    if (!is.null(click$key)) {
      parts <- strsplit(click$key[[1]], "\\|\\|")[[1]]
      if (length(parts) == 2) {
        phenotype_click(list(trait = parts[1], phenotype = parts[2]))
      }
    }
  })
  
  output$phenotypeTable <- renderDT({
    datatable(phenotype_summary(), options = list(pageLength = 20, scrollX = TRUE))
  })
  
  output$phenotypeLinesTable <- renderDT({
    df <- filtered() %>%
      distinct(Sample_ID, Agriplex_Trait_Label, Marker_Name, Allele_Phenotype) %>%
      arrange(Agriplex_Trait_Label, Allele_Phenotype, Sample_ID)
    datatable(df, options = list(pageLength = 25, scrollX = TRUE, filter = "top"))
  })
  
  output$requirementsTable <- renderDT({
    datatable(
      requirements(),
      options = list(dom = 't', pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  multi_trait_matches <- reactive({
    req(data())
    reqs <- requirements()
    if (nrow(reqs) == 0) {
      return(data.frame(Sample_ID = sort(unique(data()$Sample_ID))))
    }
    
    matched <- data() %>%
      semi_join(reqs, by = c("Agriplex_Trait_Label", "Allele_Phenotype")) %>%
      distinct(Sample_ID, Agriplex_Trait_Label, Allele_Phenotype)
    
    needed <- nrow(reqs)
    
    matched %>%
      count(Sample_ID, name = "Requirements_Met") %>%
      filter(Requirements_Met == needed) %>%
      arrange(Sample_ID)
  })
  
  output$multiTraitResults <- renderDT({
    datatable(multi_trait_matches(), options = list(pageLength = 25, scrollX = TRUE, filter = "top"))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() paste0("allele_phenotype_results_", Sys.Date(), ".xlsx"),
    content = function(file) write_xlsx(filtered(), file)
  )
  
  output$downloadPhenotypeLines <- downloadHandler(
    filename = function() paste0("phenotype_sample_names_", Sys.Date(), ".xlsx"),
    content = function(file) {
      df <- filtered() %>%
        distinct(Sample_ID, Agriplex_Trait_Label, Marker_Name, Allele_Phenotype) %>%
        arrange(Agriplex_Trait_Label, Allele_Phenotype, Sample_ID)
      write_xlsx(df, file)
    }
  )
  
  output$downloadMultiTrait <- downloadHandler(
    filename = function() paste0("multi_trait_matching_lines_", Sys.Date(), ".xlsx"),
    content = function(file) {
      write_xlsx(multi_trait_matches(), file)
    }
  )
}

shinyApp(ui, server)
