# Handle user input & display for feature selection demo app.
library(shiny)

shinyUI(fluidPage(
    fluidRow(
        column(12,
            h2("Classifier feature selection demo"),
            "Let's construct a classifier to predict whether a patient will get heart disease.",
            br(),
            "This uses data from patients with heart disease, and controls without heart disease.",
            br(),
            "Help the classifier by picking out features that you think will be important.",
            br(),
            "Select two features and click [Compare two features] to show a scatterplot. Use this to decide if two features are highly correlated, so both may not be needed.",
            br(),
            "When you've decided on a set of features, select them and click [Train the classifier] to train the classifier and report its accuracy.",
            br(),
            "Try different combinations of features to get the best accuracy with fewest features.",
            br(),
            hr()
        )
    ),
    # Sidebar with checkboxes for the features.
    fluidRow(
        column(5,
            checkboxGroupInput("features", "Select features",
                               c("Adiposity" = "adiposity",
                                 "Age" = "age",
                                 "Alcohol use" = "alcohol",
                                 "Family history" = "famhist",
                                 "LDL cholesterol" = "ldl",
                                 "Obesity" = "obesity",
                                 "Systolic blood pressure" = "sbp",
                                 "Tobacco use" = "tobacco",
                                 "Type A behavior" = "typea")),
            actionButton("trainButton", "Train the classifier"),
            actionButton("plotButton", "Compare two features")
        ),

        # Show the resulting accuracy.
        column(7,
            #h3("Selected features:"),
            #textOutput("usersFeatures"),
            h3("Classifier accuracy:"),
            textOutput("accuracy"),
            h3("Feature comparison scatterplot:"),
            plotOutput("featurePlot")
        )
    )
))
