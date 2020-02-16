##### RESEARCH GOAL #####

# The goal of this project is to predict a male worker's wage from 
# the Mid-Atlantic region based on their age, marital status, race, 
# education, job class, and year that wage information was recorded.


##### GLOBAL #####

# Load libraries
library(shiny)
library(dplyr) #select function
library(ggplot2)
library(leaps) #regsubsets function
library(boot) #boot function

# Load data
data(Wage, package = "ISLR")

# Define response and predictor variables
response = Wage %>% select(wage, logwage)
predictors = Wage %>% select(year, age, maritl, race, education, jobclass)


##### UI #####

ui <- fluidPage(
   
   # Application title
   titlePanel("Wage Data Analysis"),
   
   # Sidebar with selection inputs for response and predictor variables
   sidebarLayout(
      sidebarPanel(
        
        # User chooses response variable (can only select one)
         selectInput("Y", "Choose response variable:", 
                     c(Choose='', names(response)), selectize = T),
         
         # User chooses predictor variables (can select more than one)
         selectInput("X", "Choose predictor variables:", 
                     c(Choose='', names(predictors)), multiple = T, selectize = T)
      ),
      
      # Main panel for displaying outputs
      mainPanel(
        
        # Designate tabs
         tabsetPanel(type = "tabs",
                     
           # About tab with html rendered by markdown (dillinger.io)          
           tabPanel("About", includeHTML("About.html")),
           
           # Data summary tab for response and predictor variables
           tabPanel("Data Summary", 
                    verbatimTextOutput("summary")),
           
           # Exploratory plots tab for response and predictor variables
           tabPanel("Exploratory Plots", 
                    plotOutput("response.plot", height = 400, width = 600), 
                    uiOutput("plots")),
           
           # Variable selection tab with plot of cv errors vs model size
           # User chooses number of variables based on plot
           # Displays coefficient values for the best subsets model of that size
           tabPanel("Variable Selection",
                    plotOutput("cv.plot", height = 400, width = 600),
                    uiOutput("variables"),
                    verbatimTextOutput("coeffs")),
           
           # Confidence intervals tab for coefficients chosen by best subsets model
           tabPanel("Confidence Intervals",
                    verbatimTextOutput("bootstrap")),
           
           # Prediction interval tab for new response value
           tabPanel("Prediction Interval",
                    br(),
                    p("Below are the median values of the predictors from the chosen model. 
                      This will be considered a new person whose wage we wish to predict."),
                    p("Note: 1 = yes and 0 = no for the dummy variables"),
                    verbatimTextOutput("median"),
                    p("Below is the 95% prediction interval of this new person's wage."),
                    verbatimTextOutput("prediction"))
         )
      )
   )
)

##### SERVER #####

server <- function(input, output) {
  
  # Define reactive values that can be referenced later
  values = reactiveValues(N = NULL, model = NULL, x0 = NULL)
  
  # Print data summaries after response and predictor variables are chosen
  output$summary <- renderPrint({
    validate(need(input$X, 'Choose at least one predictor variable.'), need(input$Y, 'Choose a response variable.'))
    data = Wage[,c(input$Y, input$X)] #define data
    summary.data.frame(data)
  })
  
  # Plot histogram of response variable
  output$response.plot <- renderPlot({
    validate(need(input$Y, 'Choose a response variable.'))
    y = Wage[,c(input$Y)] #define response
    ggplot(Wage, aes(y)) +
      geom_histogram(col = "black", fill = "steelblue") + 
      labs(x = input$Y) +
      theme_bw(base_size = 15)
  })
  
  # Insert the correct number of plot output objects into the web page
  # based on how many predictors the user chose
  output$plots <- renderUI({
    validate(need(input$X, 'Choose at least one predictor variable.'))
    plot_output_list <- lapply(1:length(input$X), function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 400, width = 600)
    })
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly
    do.call(tagList, plot_output_list)
  })
  
  #https://stackoverflow.com/questions/15875786/dynamically-add-plots-to-web-page-using-shiny
  
  observe({
    
    # Loop through each predictor variable and call renderPlot for each plot. 
    # Plots are only actually generated when they are visible on the web page.
    for (i in 1:length(input$X)) {
      
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        
        my_i <- i
        plotname <- paste("plot", my_i, sep="")
        
        x = Wage[,c(input$X[my_i])] #define predictor
        
        # Make a barplot if the variable is categorical and a histogram if its numeric
        if(class(x) == "factor"){
        geom = geom_bar(col = "black", fill = "steelblue")
        } else {
        geom = geom_histogram(col = "black", fill = "steelblue")
        }
        
        # Plot each predictor variable 
        output[[plotname]] <- renderPlot({
          ggplot(Wage, aes(x)) +
            geom + labs(x = input$X[my_i]) +
            theme_bw(base_size = 15)
        })
      })
    }
  })
  
  # Determine the total number of variables R uses in regression based 
  # on the predictor variables chosen. Make this reactive so it changes when 
  # the inputs change and it can be referenced later on.
  
  n.total <- reactive({
    
    # Define output vector
    n.out = c()
    
    # Loop through each predictor variable
    for (i in 1:length(input$X)) {
      
      x = Wage[,c(input$X[i])] #define predictor
      
      # If the variable is categorical, the number of dummy variables assigned will be
      # one less than the number of levels it has. Otherwise, a numeric variable
      # will remain unchanged and only have one variable associated with it.
      if(class(x) == "factor"){
        n = nlevels(x)-1
      } else {
        n = 1
      }
      n.out = c(n.out, n)
    }
    # Calculate total number of variables
    return(sum(n.out))
  })
  
  
  # Perform cross validation for variable selection and determine the mean 
  # cross-validation errors for each model size 
  
  errors <- reactive({
    
    data = Wage[,c(input$Y, input$X)] #define data
    
    # Define model formula to perform variable selection on 
    # Based on the chosen response and predictor variables
    formula = as.formula(paste(input$Y, "~", paste(input$X, collapse = "+", sep = "")))
    
    # Define total number of variables to consider 
    n = n.total()
    
    # Choose k = 10 folds
    k = 10
    set.seed(0)
    
    # Create a vector that allocates each observation to one of 10 folds
    folds = sample(1:k, nrow(predictors), replace = TRUE)
    
    # Create a k x p matrix to store the results
    cv.errors = matrix(NA, k, n, dimnames = list(NULL, paste(1:n)))
    
    # Write a loop that performs cross-validation
    # For the ith fold, the elements of folds that equal i are in the test set,
    # and the remainder are in the training set
    for(i in 1:k){ 
      
      # Perform best subset selection on the training data for each fold
      best.fit = regsubsets(formula, data=data[folds!=i,], nvmax=n)
      for(j in 1:n){
        
        # Build X matrix from test data 
        mat = model.matrix(formula, data[folds==i,])
        # Extract coefficients from the bestsubsets model of size j
        coefi = coef(best.fit, id=j)
        # Get the names of the extracted coefficients
        xvars = names(coefi)
        # Multiply coefficients into the appropriate columns of the test model matrix to
        # Make predictions on the test data for each model size
        pred = mat[,xvars]%*%coefi 
        # Compute the test errors on the appropriate subset and store them in the appropriate slot in the matrix
        cv.errors[i,j] = mean((data[,input$Y][folds==i] - pred)^2)
      }
    }
    # The (i,j)th element of the cv.errors matrix corresponds to the test MSE for the 
    # ith cross-validation fold for the best j-variable model
    
    # Average over the columns of the cv.errors matrix to obtain a vector for which the
    # jth elements is the cross-validation error for the j-variable model
    return(apply(cv.errors, 2, mean))
  })
  
  # Plot the mean cross-validation errors for each model size
  output$cv.plot <- renderPlot({
    
    validate(need(input$Y, 'Choose a response variable.'), need(input$X, 'Choose more than one predictor variable.'))
    
    # Extract errors from reactive function
    errors = errors()
    plot(errors, type='b', xlab = "Number of Variables", ylab = "Mean CV Error", 
         main = "Cross Validation Error vs Model Size", cex = 1.5)
  })
  
  # User chooses best subset size based on the plot
  output$variables <- renderUI({
    
    # Show only after the user chooses the response and predictor variables
    validate(need(input$Y, FALSE), need(input$X, FALSE))
    
    # Choose between 1 and the total number of variables available
    n = n.total()
    selectInput("N", "Choose number of variables to include:", 
                c(Choose='', c(1:n)), selectize = T)
  })
  
  # Determine the coefficients of the best subsets model
  coefficients <- reactive({
    
    data = Wage[,c(input$Y, input$X)] #define data
    formula = as.formula(paste(input$Y, "~", paste(input$X, collapse = "+", sep = "")))
    n = n.total()
    
    # Perform best subset selection on the full dataset
    reg.best = regsubsets(formula, data=data, nvmax=n)
    
    # Returns the model with the number of variables previously chosen by the user
    return(coef(reg.best, input$N))
  })
  
  # Print the best subsets coefficients 
  output$coeffs <- renderPrint({
    # Show only after the user chooses the number of variables
    validate(need(input$N, FALSE))
    Coefficient = coefficients()
    Coefficient = data.frame(Coefficient)
    print(Coefficient)
  })
  
  # Obtain bootstrap distributions for the coefficients of the best subsets model 
  bootstrap <- reactive({
    
    # Define data frame of just the predictors
    x = Wage[,c(input$X)]
    
    # Extract the names of the variables included in the best susbsets model
    coefficients = coefficients()
    variables = names(coefficients)
    
    # Define initial values
    data.out = x
    data = c()
    
    # Loop through predictor names
    for (i in 1:length(input$X)) {
      
      if(class(x[,i]) == "factor"){
        # If the variable is a factor, create a formula needed to
        # extract the design matrix
        formula = as.formula(paste("~", input$X[i]))
        # Store design matrix 
        data = as.data.frame(model.matrix(formula, data = Wage))
      }
      # Combine each design matrix with the original predictors
      data.out = bind_cols(data, data.out)
    }
    # Remove the Intercept columns
    Data = data.out %>% select(-contains("Intercept"))
    
    # Only keep the variables from the best subsets model
    Data = Data[,intersect(variables, names(Data))]
    
    # Remove spaces in variable names 
    names(Data) = gsub(" ", "", names(Data), fixed = TRUE)
    
    # Define formula for best subsets model
    formula = as.formula(paste(input$Y, "~", paste(names(Data), collapse = "+", sep = "")))
    
    # Combine response variable with the predictor variables
    y = Wage %>% select(input$Y)
    Data = bind_cols(y, Data)
    
    # Create a function that returns the coefficients of the linear model for a designated subset
    boot.fn = function(data, index){
      return(coef(lm(formula, data=data[index,])))
    }
    
    # Estimate the standard errors of 1,000 bootstrap estimates for the coefficients
    boot.results = boot(Data, boot.fn, 1000)
    boot.dist = boot.results$t #bootstrap distribution
    
    # Fit a linear model with the variables chosen from best susbsets selection
    # and store it as a reactive value so it can be referenced later.
    values$model = lm(formula, data=Data)
    
    return(boot.dist)
  })
  
  # Calculate 95% bootstrap confidence intervals for the coefficient estimates
  # of the best subsets model
  output$bootstrap <- renderPrint({
    
    validate(need(input$N, 'Perform variable selection first.'))
    
    # Extract bootstrap distribution
    dist = bootstrap()
    
    # Compute the 95% confidence interval for each coefficient
    boot.ci = apply(dist, 2, quantile, probs=c(0.025, 0.975))
    
    # Make confidence intervals into a data frame with appropriate variable names
    boot.ci = as.data.frame(boot.ci)
    names(boot.ci) = names(coef(values$model))
    
    # Transpose results so they are easier to read
    print(t(boot.ci))
  })
  
  # Calculate the median values of the predictor variables
  # This will be the "new" observation in which to predict
  output$median <- renderPrint({
    
    validate(need(values$model, 'Compute confidence intervals first.'))
    
    # extract the design matrix of the best subsets model
    x = model.matrix(values$model)
    
    # calculate medians and store as a reactive value
    values$x0 = apply(x, 2, median) 
    median = values$x0
    
    print(data.frame(median))
    
  })
  
  # Calculate a 95% prediction interval for a new response
  output$prediction <- renderPrint({
    
    validate(need(values$x0, FALSE))
    
    # calculate prediction interval
    predict(values$model, new = data.frame(t(values$x0)), interval="prediction", level = 0.95)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

