# Handle computations for feature selection demo app.
library(shiny)
require(caret, quietly=TRUE)
require(kernlab, quietly=TRUE)
require(ElemStatLearn, quietly=TRUE)
# This package is *not* needed, but when this app is run on shinyapps.io,
# I get an error saying it *is* needed.  The SVM used here is svmPoly,
# which the kernlab docs say explicitly is an *alternative* to svm from
# e1071.
require(e1071, quietly=TRUE)

# We're using the South Africa heart disease data from the ElemStatLearn
# package.  This is chosen because it is small (hence doesn't have long
# training times nor large memory use) and because the features are very
# familar to most people.  (With one exception...no, I don't know the
# difference between obesity and adiposity either.  Have not been able to
# obtain a copy of the original paper to check how they are using the two
# terms.)  Drawback to this dataset is that the feature selection does not
# make a huge difference to the accuracy.  Would be nice to find a dataset
# where there are big gains to be had with the right selection of features.
#
# Training will be done on the entire dataset, since it is so small, and
# accuracy will be estimated by cross-validation rather than saving out a
# validation set.  This does yield a higher accuracy than (say) reserving
# 30% for validation, but that may be due to the small amount of data --
# there really may be an improvement from training on that remaining 30%
# as well, rather than a degradation due to overfitting.
data(SAheart)
SAheart$chd <- as.factor(SAheart$chd)
train.ctrl <- trainControl(method="cv", number=5, returnData=FALSE)
feature.labels <- c(
    "adiposity" = "Adiposity",
    "age" = "Age",
    "alcohol" = "Alcohol use",
    "famhist" = "Family history",
    "ldl" = "LDL cholesterol",
    "obesity" = "Obesity",
    "sbp" = "Systolic blood pressure",
    "tobacco" = "Tobacco use",
    "typea" = "Type A behavior"
)

run.train <- function(users.features) {
    if (!is.null(users.features) & length(users.features) > 0) {
        # Construct the formula.
        users.formula.text <- paste("chd ~ ", paste(users.features, collapse="+"))
        users.formula <- as.formula(users.formula.text)
        # Train the classifier.  Use a support vector machine with polynomial
        # kernel.  This consistently does better than a random forest on this
        # dataset.  Yes, really, try it.
        set.seed(54321)
        sa.model <- train(users.formula, method="svmPoly", data=SAheart,
                          trControl=train.ctrl)
        sa.trainperf <- getTrainPerf(sa.model)
        as.character(round(sa.trainperf$TrainAccuracy, digits=4))
#         sprintf("Accuracy: %s, Formula: %s",
#                 as.character(round(sa.trainperf$TrainAccuracy, digits=4)),
#                 users.formula.text
#                 )
    } else {
        NULL
    }
}

shinyServer(function(input, output) {
    output$featurePlot <- renderPlot({
        input$plotButton
        isolate({
            if (length(input$features >= 2)) {
                par(mar=c(4,4,1,1))
                plot(SAheart[,input$features[1]], SAheart[,input$features[2]],
                     xlab=feature.labels[input$features[1]],
                     ylab=feature.labels[input$features[2]])
            }
        })
    })
#     output$usersFeatures <- renderPrint({
#         input$trainButton
#         isolate(input$features)
#     })
    output$accuracy <- renderText({
        input$trainButton
        isolate(run.train(input$features))
    })
})