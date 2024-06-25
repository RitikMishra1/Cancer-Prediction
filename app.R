# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)



# Load the data
data_lung <- read.csv("survey lung cancer.csv")
data_breast <- read.csv("Cancer_Data.csv")
data_prostate <- read.csv("Prostate_Cancer.csv")

# Convert categorical variables to factors
data_lung$GENDER <- as.factor(data_lung$GENDER)
data_lung$LUNG_CANCER <- as.factor(data_lung$LUNG_CANCER)
data_breast$diagnosis <- as.factor(data_breast$diagnosis)
data_prostate$diagnosis_result <- as.factor(data_prostate$diagnosis_result)

# Create the logistic regression models
model_lung <- glm(LUNG_CANCER ~ AGE + SMOKING + YELLOW_FINGERS + ANXIETY + PEER_PRESSURE + 
                    CHRONIC.DISEASE + FATIGUE + ALLERGY + WHEEZING + ALCOHOL.CONSUMING + 
                    COUGHING + SHORTNESS.OF.BREATH + SWALLOWING.DIFFICULTY + CHEST.PAIN, 
                  data = data_lung, family = binomial)

model_breast <- glm(diagnosis ~ radius_mean + texture_mean + perimeter_mean + area_mean + smoothness_mean +
                      compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + fractal_dimension_mean +
                      radius_se + texture_se + perimeter_se + area_se + smoothness_se + compactness_se + concavity_se +
                      concave.points_se + symmetry_se + fractal_dimension_se + radius_worst + texture_worst +
                      perimeter_worst + area_worst + smoothness_worst + compactness_worst + concavity_worst +
                      concave.points_worst + symmetry_worst + fractal_dimension_worst, 
                    data = data_breast, family = binomial)

model_prostate <- glm(diagnosis_result ~ radius + texture + perimeter + area + smoothness + compactness + symmetry + fractal_dimension, 
                      data = data_prostate, family = binomial)

# Define UI for application
ui <- navbarPage("Cancer Prediction App",
                 tabPanel("Breast Cancer Detection",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Breast Cancer Detection"),
                              p("Breast cancer is one of the most common cancers among women. Early detection can significantly improve survival rates. Enter the following details:"),
                              numericInput("radius_mean", "Radius Mean (0-30):", value = 14, min = 0, max = 30),
                              helpText("Radius Mean: The average distance from the center to the points on the perimeter."),
                              numericInput("texture_mean", "Texture Mean (0-40):", value = 20, min = 0, max = 40),
                              helpText("Texture Mean: The standard deviation of gray-scale values."),
                              numericInput("perimeter_mean", "Perimeter Mean (0-200):", value = 90, min = 0, max = 200),
                              helpText("Perimeter Mean: The mean size of the core tumor."),
                              numericInput("area_mean", "Area Mean (0-2500):", value = 600, min = 0, max = 2500),
                              helpText("Area Mean: The size of the tumor area."),
                              numericInput("smoothness_mean", "Smoothness Mean (0-0.5):", value = 0.1, min = 0, max = 0.5),
                              helpText("Smoothness Mean: The mean of local variation in radius lengths."),
                              numericInput("compactness_mean", "Compactness Mean (0-1):", value = 0.1, min = 0, max = 1),
                              helpText("Compactness Mean: The perimeter squared divided by the area."),
                              numericInput("concavity_mean", "Concavity Mean (0-1):", value = 0.1, min = 0, max = 1),
                              helpText("Concavity Mean: The severity of concave portions of the contour."),
                              numericInput("concave_points_mean", "Concave Points Mean (0-1):", value = 0.1, min = 0, max = 1),
                              helpText("Concave Points Mean: The number of concave portions of the contour."),
                              numericInput("symmetry_mean", "Symmetry Mean (0-1):", value = 0.2, min = 0, max = 1),
                              helpText("Symmetry Mean: The symmetry of the tumor."),
                              numericInput("fractal_dimension_mean", "Fractal Dimension Mean (0-1):", value = 0.1, min = 0, max = 1),
                              helpText("Fractal Dimension Mean: The 'coastline approximation' - 1."),
                              actionButton("predict_breast", "Predict", class = "btn btn-primary")
                            ),
                            mainPanel(
                              textOutput("prediction_breast"),
                              br(),
                              textOutput("probability_breast")
                            )
                          )),
                 tabPanel("Lung Cancer Detection",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Lung Cancer Detection"),
                              p("Lung cancer is the leading cause of cancer death worldwide. Early detection improves the chance of successful treatment. Enter the following details:"),
                              selectInput("gender_lung", "Gender:", choices = c("M" = "M", "F" = "F")),
                              helpText("Gender: Select 'M' for Male and 'F' for Female."),
                              numericInput("age_lung", "Age (1-100):", value = 30, min = 1, max = 100),
                              helpText("Age: Enter your age."),
                              numericInput("smoking", "Smoking (1 = No, 2 = Yes):", value = 1, min = 1, max = 2),
                              helpText("Smoking: Enter 1 if you do not smoke and 2 if you do."),
                              numericInput("yellow_fingers", "Yellow Fingers (1 = No, 2 = Yes):", value = 1, min = 1, max = 2),
                              helpText("Yellow Fingers: Enter 1 if you do not have yellow fingers and 2 if you do."),
                              numericInput("anxiety", "Anxiety (1 = No, 2 = Yes):", value = 1, min = 1, max = 2),
                              helpText("Anxiety: Enter 1 if you do not have anxiety and 2 if you do."),
                              numericInput("peer_pressure", "Peer Pressure (1 = No, 2 = Yes):", value = 1, min = 1, max = 2),
                              helpText("Peer Pressure: Enter 1 if you do not experience peer pressure and 2 if you do."),
                              numericInput("chronic_disease", "Chronic Disease (1 = No, 2 = Yes):", value = 1, min = 1, max = 2),
                              helpText("Chronic Disease: Enter 1 if you do not have a chronic disease and 2 if you do."),
                              numericInput("fatigue", "Fatigue (1 = No, 2 = Yes):", value = 1, min = 1, max = 2),
                              helpText("Fatigue: Enter 1 if you do not experience fatigue and 2 if you do."),
                              numericInput("allergy", "Allergy (1 = No, 2 = Yes):", value = 1, min = 1, max = 2),
                              helpText("Allergy: Enter 1 if you do not have allergies and 2 if you do."),
                              numericInput("wheezing", "Wheezing (1 = No, 2 = Yes):", value = 1, min = 1, max = 2),
                              helpText("Wheezing: Enter 1 if you do not wheeze and 2 if you do."),
                              numericInput("alcohol_consuming", "Alcohol Consuming (1 = No, 2 = Yes):", value = 1, min = 1, max = 2),
                              helpText("Alcohol Consuming: Enter 1 if you do not consume alcohol and 2 if you do."),
                              numericInput("coughing", "Coughing (1 = No, 2 = Yes):", value = 1, min = 1, max = 2),
                              helpText("Coughing: Enter 1 if you do not cough and 2 if you do."),
                              numericInput("shortness_of_breath", "Shortness of Breath (1 = No, 2 = Yes):", value = 1, min = 1, max = 2),
                              helpText("Shortness of Breath: Enter 1 if you do not experience shortness of breath and 2 if you do."),
                              numericInput("swallowing_difficulty", "Swallowing Difficulty (1 = No, 2 = Yes):", value = 1, min = 1, max = 2),
                              helpText("Swallowing Difficulty: Enter 1 if you do not have difficulty swallowing and 2 if you do."),
                              numericInput("chest_pain", "Chest Pain (1 = No, 2 = Yes):", value = 1, min = 1, max = 2),
                              helpText("Chest Pain: Enter 1 if you do not have chest pain and 2 if you do."),
                              actionButton("predict_lung", "Predict", class = "btn btn-primary")
                            ),
                            mainPanel(
                              textOutput("prediction_lung"),
                              br(),
                              textOutput("probability_lung")
                            )
                          )),
                 tabPanel("Prostate Cancer Detection",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Prostate Cancer Detection"),
                              p("Prostate cancer is one of the most common cancers among men. Early detection can lead to better outcomes. Enter the following details:"),
                              numericInput("radius_prostate", "Radius (0-50):", value = 15, min = 0, max = 50),
                              helpText("Radius: The average distance from the center to the points on the perimeter."),
                              numericInput("texture_prostate", "Texture (0-50):", value = 20, min = 0, max = 50),
                              helpText("Texture: The standard deviation of gray-scale values."),
                              numericInput("perimeter_prostate", "Perimeter (0-300):", value = 100, min = 0, max = 300),
                              helpText("Perimeter: The mean size of the core tumor."),
                              numericInput("area_prostate", "Area (0-2500):", value = 600, min = 0, max = 2500),
                              helpText("Area: The size of the tumor area."),
                              numericInput("smoothness_prostate", "Smoothness (0-1):", value = 0.1, min = 0, max = 1),
                              helpText("Smoothness: The mean of local variation in radius lengths."),
                              numericInput("compactness_prostate", "Compactness (0-1):", value = 0.1, min = 0, max = 1),
                              helpText("Compactness: The perimeter squared divided by the area."),
                              numericInput("symmetry_prostate", "Symmetry (0-1):", value = 0.2, min = 0, max = 1),
                              helpText("Symmetry: The symmetry of the tumor."),
                              numericInput("fractal_dimension_prostate", "Fractal Dimension (0-1):", value = 0.1, min = 0, max = 1),
                              helpText("Fractal Dimension: The 'coastline approximation' - 1."),
                              actionButton("predict_prostate", "Predict", class = "btn btn-primary")
                            ),
                            mainPanel(
                              textOutput("prediction_prostate"),
                              br(),
                              textOutput("probability_prostate")
                            )
                          ))
)

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$predict_breast, {
    # Breast Cancer Prediction Logic
    new_data_breast <- data.frame(
      radius_mean = input$radius_mean,
      texture_mean = input$texture_mean,
      perimeter_mean = input$perimeter_mean,
      area_mean = input$area_mean,
      smoothness_mean = input$smoothness_mean,
      compactness_mean = input$compactness_mean,
      concavity_mean = input$concavity_mean,
      concave.points_mean = input$concave_points_mean,
      symmetry_mean = input$symmetry_mean,
      fractal_dimension_mean = input$fractal_dimension_mean
    )
    
    prediction_breast <- predict(model_breast, newdata = new_data_breast, type = "response")
    prediction_class_breast <- ifelse(prediction_breast > 0.5, "Malignant", "Benign")
    
    output$prediction_breast <- renderText({
      paste("Predicted Diagnosis: ", prediction_class_breast)
    })
    
    output$probability_breast <- renderText({
      paste("Probability: ", round(prediction_breast, 4))
    })
  })
  
  observeEvent(input$predict_lung, {
    # Lung Cancer Prediction Logic
    new_data_lung <- data.frame(
      GENDER = factor(input$gender_lung, levels = levels(data_lung$GENDER)),
      AGE = input$age_lung,
      SMOKING = input$smoking,
      YELLOW_FINGERS = input$yellow_fingers,
      ANXIETY = input$anxiety,
      PEER_PRESSURE = input$peer_pressure,
      CHRONIC.DISEASE = input$chronic_disease,
      FATIGUE = input$fatigue,
      ALLERGY = input$allergy,
      WHEEZING = input$wheezing,
      ALCOHOL.CONSUMING = input$alcohol_consuming,
      COUGHING = input$coughing,
      SHORTNESS.OF.BREATH = input$shortness_of_breath,
      SWALLOWING.DIFFICULTY = input$swallowing_difficulty,
      CHEST.PAIN = input$chest_pain
    )
    
    prediction_lung <- predict(model_lung, newdata = new_data_lung, type = "response")
    prediction_class_lung <- ifelse(prediction_lung > 0.5, "YES", "NO")
    
    output$prediction_lung <- renderText({
      paste("Predicted Lung Cancer: ", prediction_class_lung)
    })
    
    output$probability_lung <- renderText({
      paste("Probability: ", round(prediction_lung, 4))
    })
  })
  
  observeEvent(input$predict_prostate, {
    # Prostate Cancer Prediction Logic
    new_data_prostate <- data.frame(
      radius = input$radius_prostate,
      texture = input$texture_prostate,
      perimeter = input$perimeter_prostate,
      area = input$area_prostate,
      smoothness = input$smoothness_prostate,
      compactness = input$compactness_prostate,
      symmetry = input$symmetry_prostate,
      fractal_dimension = input$fractal_dimension_prostate
    )
    
    prediction_prostate <- predict(model_prostate, newdata = new_data_prostate, type = "response")
    prediction_class_prostate <- ifelse(prediction_prostate > 0.5, "Malignant", "Benign")
    
    output$prediction_prostate <- renderText({
      paste("Predicted Diagnosis: ", prediction_class_prostate)
    })
    
    output$probability_prostate <- renderText({
      paste("Probability: ", round(prediction_prostate, 4))
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
