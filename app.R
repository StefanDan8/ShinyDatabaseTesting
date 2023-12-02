library(shiny)
library(tidyverse)
library(shinyTree)
library(DT)
source("get_tests.R")
main_ui <- navbarPage(id = "main",
                      title = "Testing Tool",
                      tabPanel("Connection",
                               fluidPage(align = "center",
                                 width = 12,
                                 titlePanel("Connect to a Database"),
                                  textInput("driver", "Driver", value = "ODBC Driver 17 for SQL Server"),
                                  textInput("server", "Server"),
                                  textInput("database", "Database"),
                                  textInput("uid", "User ID"),
                                  passwordInput("password", "Password"),
                                  actionButton("connect", "Connect"),
                                  verbatimTextOutput("connection_status")

                               )

                      )
          )

run_tests_ui <- function(id, con){
  tests <- get_tests(con)
  test_classes <- unname(unlist(tests %>% distinct(ROUTINE_SCHEMA)))
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Run Tests"),

        mainPanel(style = "overflow-x:scroll;max-width:800px", width = 6,
           shinyTree(ns("testTree"), checkbox = TRUE),
          actionButton(ns("runSelected"), "Run selected tests")
        ),
        mainPanel(width = 6,
          DT::dataTableOutput(ns("testReport"), height = "80%")
        )
      )

  )
}

write_tests_ui <- function(id, con){
  database <- con@info$dbname
  test_classes <- get_tests(con) %>% distinct(ROUTINE_SCHEMA)
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Write Tests"),
      selectInput(ns("testClasses"), "Test Class", choices = unname(unlist(test_classes))),
      textInput(ns("testName"), "Test Name", value = "test "),
      textAreaInput(ns("SQLcode"),
                    HTML("CREATE OR ALTER PROCEDURE &ltTest Class&gt.[&ltTest Name&gt] <br/>
                        AS "),
                    width = "700px",
                    height = "300px"),
      actionButton(ns("addTest"), "Add Test")
    )
  )
}

run_tests_server <- function(id, con, available_tests){
  moduleServer(
    id,
    function(input, output, session){
      output$testTree <- renderTree({available_tests})
      observeEvent(input$runSelected, {
        tree_input <- get_selected(input$testTree, format = "slices")
        selected_indices <- tree_input %>% map_int(vec_depth)
        tests <- tree_input[selected_indices==3]
        tests <- cbind(names(sapply(tests, '[', 1)), names(sapply(tests, '[[', 1)))
        #print(tests)
        #print(tests[,2])
       table_f <<- bind_rows(mapply(function(x,y) exec_test(con,x,y), tests[,1], tests[,2], SIMPLIFY = FALSE))
        output$testReport <- DT::renderDataTable(datatable(table_f)%>%
                                                   formatStyle(3, target = 'row',
                                                   backgroundColor = styleEqual(c("Success", "Failure"), c("palegreen", "tomato"))))

      })
    }
  )
}

write_tests_server <- function(id, con){
  moduleServer(
    id,
    function(input, output, session){
      observeEvent(input$addTest, {
        query <- paste0("CREATE OR ALTER PROCEDURE ",
                        input$testClasses,".[",input$testName,"]\nAS \n", input$SQLcode)
        cat(query)
        tryCatch(
          {
            odbc::dbGetQuery(con, query)
          },
          error = function(cond){
            renderText(paste("Could not create test procedure\n", conditionMessage(cond)))
          }
        )
      })
      observeEvent(input$testClasses, {
        updateSelectInput(session, "SQLcode",
                          label = paste0("CREATE OR ALTER PROCEDURE ",
                                         input$testClasses,".[",input$testName,"]\nAS \n"))
      })
      observeEvent(input$testName, {
        updateSelectInput(session, "SQLcode",
                          label = paste0("CREATE OR ALTER PROCEDURE ",
                                         input$testClasses,".[",input$testName,"]\nAS \n"))
      })
    }
  )
}

main_server <- function(input, output, session){
  connected <- reactiveVal(value = FALSE)
  con <- reactiveVal(value = NULL)
  tests <- reactiveVal(value = NULL)
  passTests <- reactiveVal(value = NULL)
  # CONNECT BUTTON
  observeEvent(input$connect, {
    if(connected() == FALSE){
      tryCatch(
        {
          con(odbc::dbConnect(
            odbc::odbc(),
            driver = input$driver,
            server = input$server,
            database = input$database,
            uid = input$uid,
            pwd = input$password))
          output$connection_status <- renderText("Connection Successful")
          connected(TRUE)
          updateActionButton(session, "connect", "Disconnect")
          tests(get_tests(con()))
          passTests(lapply(lapply(lapply(split(tests()$ROUTINE_NAME, tests()$ROUTINE_SCHEMA), as.list),
                              structure, stselected = TRUE),
                       function(x) setNames(x,x)))
        },
        error = function(cond){
          output$connection_status <- renderText(paste("Connection Failed\n", conditionMessage(cond)))
        },
        warning = function(cond){
          output$connection_status <- renderText(conditionMessage(cond))
        }
      ) # end of TryCatch
    } # end of IF NOT CONNECTED
    else{ # IF CONNECTED
      tryCatch(
        {
          odbc::dbDisconnect(conn = con())
          output$connection_status <- renderText("Disconnected")
          connected(FALSE)
          updateActionButton(session, "connect", "Connect")
        },
        error = function(cond){
          output$connection_status <- renderText(paste("Could not disconnect\n", conditionMessage(cond)))
        },
        warning = function(cond){
          output$connection_status <- renderText(conditionMessage(cond))
        }
      ) # end of tryCatch
    } # end of IF CONNECTED

    # OTHER PAGES AFTER CONNECTION
    if(connected()){

      insertTab("main",
                tabPanel("Run Tests",
                         run_tests_ui("runTests", con()))
      )
      insertTab("main",
                tabPanel("Write Tests",
                         write_tests_ui("writeTests", con()))
      )
    }else{
      removeTab("main", "Run Tests")
      removeTab("main", "Write Tests")
    }

  }) # end of CONNECT BUTTON

  # WRITE TESTS SERVER
  write_tests_server("writeTests", con())
  run_tests_server("runTests", con(), passTests())
  session$onSessionEnded( function(){
    if(isolate(connected())){
      odbc::dbDisconnect(conn = isolate(con()))
      print("Disconnected")
    }
  })
}

shinyApp(ui = main_ui, server = main_server)
