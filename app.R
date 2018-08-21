#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(magrittr)
library(data.table)
library(fst)
# library(treemap)
library(hutils)
debug <- FALSE


complications <- alloc.col(read_fst("app-complications.fst",
                                    as.data.table = TRUE),
                           n = 50L)

CHADxTop10_all <-
  alloc.col(read_fst("top10_CHADx_by_SpecialtyAgeSexSameDay.fst",
                     as.data.table = TRUE),
            n = 50L) %>%
  setkeyv(c("Specialty", "sameday_adm", "Age", "Sex"))
stopifnot(haskey(CHADxTop10_all),
          identical(key(CHADxTop10_all),
                    c("Specialty", "sameday_adm", "Age", "Sex")))
MCHADxTop10_all <-
  alloc.col(read_fst("top10_MajorCHADx_by_SpecialtyAgeSexSameDay.fst",
                     as.data.table = TRUE),
            n = 50L) %>%
  setkeyv(c("Specialty", "sameday_adm", "Age", "Sex"))
stopifnot(haskey(MCHADxTop10_all),
          identical(key(MCHADxTop10_all),
                    c("Specialty", "sameday_adm", "Age", "Sex")))

if (identical("hughp", Sys.info()['user'])) {
  fwrite(MCHADxTop10_all, "MCHADxTop10_all.csv")
  fwrite(CHADxTop10_all, "CHADxTop10_all.csv")
  fwrite(complications, "complications.csv")
}

Admissions_by_KEY <- read_fst("n_admissions_by_key.fst", as.data.table = TRUE)






library(shiny)

u1 <- function(v) {
  c("(All)",
    unique(as.character(.subset2(complications, v))))
}

`%||%` <- function(x, y) if (is.null(x)) y else x

tags$head(tags$style("
                     table.dataTable thead th {
                     padding: 8px 10px !important;
                     }
                     "))


# Define UI for application that draws a histogram
heading  <- "Hospital complications calculator"
subheading <- "What is your risk of complication from elective surgery?"

ui <- fluidPage(
  includeCSS("styles.css"),

  tags$head(tags$style("
                       table.dataTable thead th {
                       padding: 8px 10px !important;
                       }
                       ")),
  tags$head(HTML('<link rel="icon" type="image/png" href="https://grattan.edu.au/wp-content/themes/grattan_responsive/images/fav.jpg" sizes="16x16">'),
            HTML('<link rel="icon" type="image/png" href="https://grattan.edu.au/wp-content/themes/grattan_responsive/images/fav.jpg" sizes="32x32">')),

  #tagList(tags$head(HTML('<h1><div style="vertical-align:text-bottom;"><div style="float:left;">Hospital complication calculator</div><div style="float:right;vertical-align:text-bottom;"><img src="GrattanLogo_right.png" type = "image/png" align="right; vertical-align:bottom;"/></div></div></h1>'))),

  tagList(tags$head(tags$title(heading))),

  fluidRow(
    column(7),
    column(3,
           div(img(src = "GrattanLogo_right.png", align = "right", type = "image/png")))
  ),

  fluidRow(
    column(8,
           div(class = "col-sm-12", h1(heading)))
  ),

  fluidRow(
    column(12,
           div(class = "col-sm-12", h2(subheading)))
  ),

  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(2,
           selectInput("Specialty",
                       "Specialty:",
                       u1("Specialty")),
           offset = 1
    ),
    column(2,
           selectInput("Age",
                       "Your age:",
                       u1("Age"))
    ),
    column(2,
           selectInput("Sex",
                       "Your sex:",
                       u1("Sex"))
    ),
    column(2,
           selectInput("Sameday",
                       "Likely length of stay:",
                       c("(All)", "Same day", "Multi day"))
    )

  ),
  # Create a new row for the table.
  fluidRow(
    column(DT::dataTableOutput("TopTable"),
           width = 8,
           offset = 1)
  ),

  br(),
  br(),

  div(class = "row",
      div(class = "col-sm-8 col-sm-offset-1",
          shiny::h4(textOutput("Most_common_complications"))
      )
  ),

  br(),

  fluidRow(
    column(1),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", DT::dataTableOutput("CHADxTop10"))
        # tabPanel("Plot", plotOutput("TreeMapCHADx"))
      )
    )
  ),
  br(),
  htmltools::p("What you should ask your doctor:"),
  tags$ul(
    tags$li("Is there anything I can do to reduce the risk of complications arising from my surgery?"),
    tags$li("Which of these complications are likely for the specific procedure I’m going to have?"),
    tags$li("Which of these complications might still cause me problems after I leave hospital, and what might those problems be?"),
    tags$li("If you have a chronic disease, such as diabetes—Will the rate of complications be higher for me?")
  )
)

server <- function(input, output) {
  # Filter data based on selections

  JJJ <- sample(0:2, size = 1L)
  generate_TopTable <- function(JJ = JJJ) {
    data <- copy(complications)
    data[Admissions < 20L, Admissions := 10L]
    data[, Admissions := Admissions + sample(-5:5, size = .N, replace = TRUE)]
    setnames(data, "Admissions", "N")
    new_key <- key(data)

    if (debug) {
      print(input$Specialty)
      cat(new_key, "\n")
    }

    if (input[["Specialty"]] != "(All)") {
      data <- data[.(input$Specialty)]
    } else {
      new_key <- setdiff(key(data), "Specialty")
      data <- data[,
                   .(Specialty = "(All)",
                     `Rate of complications` = weighted.mean(`Rate of complications`, N),
                     N = sum(N)),
                   keyby = c(new_key)]
    }

    cat(new_key, "\n")

    if (input[["Age"]] != "(All)") {
      data <- data[Age == input$Age]
    } else {
      stopifnot(haskey(data))
      new_key <- setdiff(key(data), c("Specialty", "Age"))
      data <- data[,
                   .(Age = "(All)",
                     `Rate of complications` = weighted.mean(`Rate of complications`, N),
                     N = sum(N)),
                   keyby = c("Specialty", new_key)]
    }

    cat(new_key, "\n")

    if (input[["Sex"]] != "(All)") {
      data <- data[Sex == input$Sex]
    } else {
      new_key <- setdiff(key(data), c("Specialty", "Age", "Sex"))
      data <- data[,
                   .(Sex = "(All)",
                     `Rate of complications` = weighted.mean(`Rate of complications`, N),
                     N = sum(N)),
                   keyby = c("Specialty", "Age", new_key)]
    }

    if (input[["Sameday"]] != "(All)") {
      if (input[["Sameday"]] == "Same day") {
        data <- data[TRUE & sameday_adm]
        data[, `Length of stay` := "Same day"]
      } else {
        data <- data[TRUE & !sameday_adm]
        data[, `Length of stay` := "Multi day"]
      }
      data[, sameday_adm := NULL]
    } else {
      data <- data[,
                   .(`Length of stay` = "(All)",
                     `Rate of complications` = weighted.mean(`Rate of complications`, N),
                     N = sum(N)),
                   keyby = c("Specialty", "Age", "Sex")]
    }
    cat(new_key, "\n")

    if (debug) {
      print(data)
    }
    setcolorder(data, c("Specialty", "Length of stay", "Age", "Sex",
                        "Rate of complications", "N"))
    if ("sameday_adm" %chin% names(data)) {
      setnames(data, "sameday_adm", "Length of stay")
    }
    stopifnot("Length of stay" %chin% names(data))

    data[, N := N + JJ]
    data[, "Rate of complications" := `Rate of complications`]
  }

  output$Most_common_complications <-
    renderText({
      TopTable <- generate_TopTable()
      if (nrow(TopTable)) {
        paste0("There were ", prettyNum(TopTable[["N"]], big.mark = ","),
               " people ",
               "like you who had elective surgery in that specialty ",
               "between 2012 and 2015. Of ",
               "that group, ",
               round(100 * TopTable[["Rate of complications"]]), "% ",
               "had a complication. (People with chronic conditions ",
               "are likely to have higher rates of complications.) ",
               "The most frequent complications were:")
      } else {
        "(No data available.)"
      }
    })

  output$TopTable <-
    DT::renderDataTable(DT::datatable({
    setnames(generate_TopTable(), "N", "Admissions")
  },
  rownames = FALSE,
  options = list(columnDefs = list(list(className = 'dt-right', targets = 3)),
                 columns.contentPadding = "",
                 dom = "t",
                 autoWidth = FALSE),
  selection = "single") %>%
    DT::formatPercentage(., columns = "Rate of complications", digits = 0) %>%
    DT::formatCurrency(., columns = "Admissions", currency = "", digits = 0))


  generate_top10 <- function() {
    data <- copy(CHADxTop10_all)
    input_Specialty <- input[["Specialty"]]
    input_Sex <- input[["Sex"]]
    input_Age <- input[["Age"]]
    input_Sameday <- input[["Sameday"]]
    if (!{SameDayAll <- identical("(All)", input_Sameday)}) {
      input_Sameday <- input_Sameday == "Same day"
    }

    SpecialtyAll <- input_Specialty == "(All)"
    SexAll <- input_Sex == "(All)"
    AgeAll <- input_Age == "(All)"

    if (all(SpecialtyAll, SexAll, AgeAll, SameDayAll)) {
      nAdmissions <- Admissions_by_KEY %$% sum(n_admissions)
      if (debug) print("A")
    } else if (!any(SpecialtyAll, SexAll, AgeAll, SameDayAll)) {
      if (debug) print("B")
      data <- data[.(input_Specialty, input_Sameday, input_Age, input_Sex)]
      nAdmissions <-
        Admissions_by_KEY[.(input_Specialty, input_Sameday, input_Age, input_Sex)] %>%
        .subset2("n_admissions")
    } else {
      N_Admissions <- copy(Admissions_by_KEY)
      if (debug) print("Z")
      if (!SpecialtyAll) {
        data <- data[.(input_Specialty)]
        N_Admissions <- N_Admissions[.(input_Specialty)]
      }
      if (!SameDayAll) {
        if (input_Sameday) {
          data <- data[(sameday_adm)]
          N_Admissions <- N_Admissions[(sameday_adm)]
        } else {
          data <- data[(!sameday_adm)]
          N_Admissions <- N_Admissions[(!sameday_adm)]
        }

      }
      if (!AgeAll) {
        data <- data[Age == input_Age]
        N_Admissions <- N_Admissions[Age == input_Age]
      }
      if (!SexAll) {
        print("Not sex all")
        data <- data[Sex == input_Sex]
        N_Admissions <- N_Admissions[Sex == input_Sex]
      }
      nAdmissions <- N_Admissions %$% sum(n_admissions)

      if (debug) print(data)

    }


    data <-
      data[, .(n_complications = sum(n_complications)),
           keyby = "CHADx_description"]
    data[, n_admissions := nAdmissions]
    data[, `% of patients who had this complication` := n_complications / nAdmissions]
    setnames(data, "CHADx_description", "Top 10 complications")
    setorderv(data, "% of patients who had this complication", -1L)
    if (debug) print(data)
    head(data[, .SD, .SDcols = c("Top 10 complications",
                            "n_admissions",
                            "n_complications",
                            "% of patients who had this complication")],
         10L)
  }

  output$CHADxTop10 <-
    DT::renderDataTable(
      DT::datatable({
        Top10Table <-
          generate_top10() %>%
          .[n_complications > 20L] %>%
          .[,
            .(`% of patients who had this complication` = sum(`% of patients who had this complication`)),
            keyby = c(setdiff(names(.),
                              "% of patients who had this complication"))] %>%
          setorderv("% of patients who had this complication", order = -1L) %>%
          drop_cols(c("n_complications",
                      "n_admissions"))
      },
      rownames = FALSE,
      options = list(dom = "t"),
      selection = "none") %>%
    DT::formatPercentage(., columns = "% of patients who had this complication", digits = 1))
}

# Run the application
shinyApp(ui = ui, server = server)

