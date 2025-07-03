server <- function(input, output){
  current_data <- reactiveVal(NULL)
  current_testdata <- reactiveVal(NULL)
  weights <- reactiveVal(NULL)
  current_fit <- reactiveVal(NULL)

  featureList <- reactive({
    req(input$target)
    dat <- current_data()
    setdiff(names(dat), paste(input$target))
  })
  observeEvent(featureList(), {
    updateSelectInput(inputId = "feature", choices = featureList())
  })
  observeEvent(input$type, {
    updateTabsetPanel(inputId = "klass_panel", selected = input$type)
  })

  rv <- reactiveValues(failure = NULL)

  output$run_txt <- NULL
  output$targetinfo <- renderText({
    if(is.null(input$target)){
      "Targetvariabel endnu ikke valgt."
    } else{
      input$target
    }})

  output$featureinfo <- renderText({
    if(length(input$feature)==0){
      "Featurevariable endnu ikke valgt."
    } else{
      paste(input$feature, collapse = ", ")
    }})

  output$datainfo <- renderText({
    if(is.null(current_data())){
      "Her vises træningsdata når de er indlæst."
    } else{
      if(inherits(current_data(), "try-error")){
        "Kunne ikke indlæse træningsdatafil. Prøv at uploade data i et andet format."
      } else{
        ""
      }}})

  output$testdatainfo <- renderText({
    if(is.null(current_testdata())){
      "Her vises testdata når de er indlæst."
    } else{
      if(inherits(current_testdata(), "try-error")){
        "Kunne ikke indlæse testdatafil. Prøv at uploade data i et andet format."
      } else{
        ""
      }}})

  output$contents <- renderDataTable({
    inFile <- input$file1
    if(is.null(inFile)){
      return(NULL)
    }
    dat <- try(rio::import(inFile$datapath, setclass = "data.frame", check.names = TRUE), silent = TRUE)
    # dat <- try(readxl::read_xlsx(inFile$datapath), silent = TRUE)
    names(dat) <- make.names(names(dat))
    current_data(dat)
    current_data()
  }, options = list(pageLength = 5, dom = "ltp"))

  output$testcontents <- renderDataTable({
    inFileTest <- input$file2
    if(is.null(inFileTest)){
      return(NULL)
    }
    dat_test <- try(rio::import(inFileTest$datapath, setclass = "data.frame", check.names = TRUE), silent = TRUE)
    # dat_test <- try(readxl::read_xlsx(inFileTest$datapath), silent = TRUE)
    names(dat_test) <- make.names(names(dat_test))
    current_testdata(dat_test)
    current_testdata()
  }, options = list(pageLength = 5, dom = "ltp"))

  # output$weightinfo <- renderText({
  #   if(is.null(weights())){
  #     "Her vises vægte når algoritmen er færdig."
  #   } else{
  #     ""
  #   }})
  #
  # output$weights <- renderDataTable({
  #   weights()
  # }, options = list(pageLength = 10, dom = "ltp"))
  #
  observeEvent(current_data(), {
    updateVarSelectizeInput(inputId = "target", data = current_data(), selected = character(0))
    # updateVarSelectizeInput(inputId = "feature", data = current_data(), selected = character(0))
  })

  observeEvent(input$run,{
    if(is.null(input$target) | is.null(input$feature) ){
      output$run_txt <- renderText("Target og feature-variable skal vælges før algoritmen kan køre.")
    } else{
      lhs <- paste(input$target)
      feature_vec <- paste(input$feature)
      m <- length(feature_vec)
      if(lhs %in% feature_vec){
        output$run_txt <- renderText(paste0('Variablen "', lhs, '" kan ikke både være target og feature.
                                           Fjern den fra feature-variablene, og tryk "kør" igen.'))
      } else{
        output$run_txt <- NULL
        rhs <- paste(feature_vec, collapse = " + ")
        form <- as.formula(paste(lhs, "~", rhs, collapse = " "))
        dat <- current_data()
        if(input$type == "klassifikation"){
          dat[[lhs]] <- factor(dat[[lhs]])
        }
        hidden <- c(input$hidden1, input$hidden2)
        # if(!any(hidden>0)){
        #   stop("Mindst et skjult lag skal have neuroner.")
        # }
        # act <- ifelse(input$activation=="Sigmoid", "logistic", "tanh")
        fit <- nn_fun(form, data = dat, weights = input$w_start,
                      eta = input$lr,
                      n_hidden = hidden,
                      # eps = input$eps,
                      iter = input$max_it,
                      lossfun = ifelse(input$loss=="Kvadratisk", "squared", "cross-entropy"),
                      activation = input$activation,
                      type = input$type)
        current_fit(fit)
        output$loss_plot <- renderPlot({
          plot(fit$loss, type = "l", xlab = "Iteration", ylab = "Tab", main = "Tabsfunktion")
        })
        output$network <- visNetwork::renderVisNetwork({
          nn_viz(fit)
        })
      }
    }
  })

  observeEvent(current_testdata(), {
    output$testcontents <- renderDataTable({
      current_testdata()
    }, options = list(pageLength = 5, dom = "ltp"))
  })

  observeEvent(input$run_test,{
    if(is.null(current_testdata())){
      output$run_test_txt <- renderText("Testdata skal uploades først.")
    } else if(is.null(current_fit())){
      output$run_test_txt <- renderText("Algoritmen skal trænes først!")
    } else{
      testfit <- current_fit()
      testdata <- current_testdata()
      fit_var <- all.vars(testfit$formula)
      lhs_var <- as.character(testfit$formula)[2]
      rhs_var <- setdiff(fit_var, lhs_var)
      missing_var <- setdiff(rhs_var, names(testdata))
      if(length(missing_var)>0){
        output$run_test_txt <- renderText(paste("Følgende variable mangler i test data:", paste(missing_var, collapse = ", ")))
      } else{
        output$run_test_txt <- NULL
        pred <- predict(testfit, newdata = testdata, type = "response")
        # X_test <- t(as.matrix(testdata[, rhs_var]))
        # # cat(testfit$params[[length(testfit$params)]]$W3, "/n")
        # fwd <- aimat:::forward_propagation(X_test, params = testfit$params[[length(testfit$params)]])
        # pred <- if(!is.null(fwd$A3)){fwd$A3} else if(!is.null(fwd$A2)){fwd$A2} else{fwd$A1}
        # cat(dim(pred), "\n")
        # cat(testfit$levels, "\n")
        if(is.null(testfit$levels)){
          pred_nam <- paste0("pred_", lhs_var)
        } else{
          pred_nam <- paste0("P(", testfit$levels, ")")
          if(length(pred_nam)==2){
            pred_nam <- pred_nam[1]
          }
        }
        colnames(pred) <- pred_nam
        for(nam in pred_nam){
          testdata[[nam]] <- pred[,nam]
        }
        # testdata <- cbind(testdata, pred)
        # cat(names(testdata), "\n")
        current_testdata(testdata)
      }
    }
  })
}
