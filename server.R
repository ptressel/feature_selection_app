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
SAheart$chd <- factor(SAheart$chd, levels=c(0,1), labels=c("FALSE","TRUE"))
train.ctrl <- trainControl(method="cv", number=5, returnData=FALSE)
column.labels <- c(
    "chd" = "Heart disease",
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
            # Don't complain to the user for minor things.  If they have
            # checked any feature, we can show an appropriate plot.  If the
            # outcome is checked, show that with the first checked feature.
            # Otherwise, show the first two checked features.
            if (length(input$features) >= 1) {
                xcol <- NULL
                ycol <- NULL
                if (input$outcome | length(input$features) == 1) {
                    # Here, the user wants to compare against outcome.
                    x.name <- "chd"
                    y.name <- input$features[1]
                } else {
                    x.name <- input$features[1]
                    y.name <- input$features[2]
                }
                par(mar=c(4,4,1,1))
                x.column <- SAheart[, x.name]
                y.column <- SAheart[, y.name]
                plot(x.column, y.column,
                     xlab=column.labels[x.name],
                     ylab=column.labels[y.name],
                     col="red")
                # If either column is a binary factor, convert it to 0 and 1.
                # For factors with other than 2 levels, omit the correlation.
                cvt.factor <- function(col) {
                    if (is.factor(col)) {
                        level.names <- levels(col)
                        if (length(level.names) == 2) {
                            # Convert the first level to FALSE, second to TRUE.
                            col == level.names[2]
                        } else {
                            NULL
                        }
                    } else {
                        col
                    }
                }
                x.column <- cvt.factor(x.column)
                y.column <- cvt.factor(y.column)
                if (!is.null(x.column) & !is.null(y.column)) {
                    c <- round(cor(x.column, y.column), digits=3)
                    usr <- par("usr")
                    xmid <- (usr[1] + usr[2]) / 2
                    ytop <- usr[4] - 2
                    text(xmid, ytop, sprintf("Correlation = %.3f", c))
                }
            }
        })
    })
    output$accuracy <- renderText({
        input$trainButton
        isolate(run.train(input$features))
    })
})