makeCC = function(learner, order=NULL) {
    #' function creates classifier chain learner
    #' @param learner learner for a binary classifier that will be used by CC
    #' @param order the order of the labels in classifier, default will be taken from task
    #' @return a classifier chain learner
    learner = checkLearner(learner, type = "classif", props = "twoclass")
    id = paste("multilabel.CC", getLearnerId(learner), sep = ".")
    packs = getLearnerPackages(learner)
    type = getLearnerType(learner)
    x = mlr::makeBaseWrapper(id, type, learner, packs, makeParamSet(),
        learner.subclass = c("CCWrapper", "HomogeneusEnsemble"),
        model.subclass = c("CCModel", "HomogeneousEnsembleModel"))
    x$type = "multilabel"
    x$order = order
    return(x)
}

trainLearner.CCWrapper = function(.learner, .task, .subset = NULL, .weights = NULL, ...) {
    #' function creates classifier chain
    #' @param .learner CC learner used to create model
    #' @param .task a task to train classifier chain
    #' @return a CC model
    #' @usage via train(learner, task)
    order = .learner$order
    if (is.null(order)) {
        order = mlr::getTaskTargetNames(task);
    }
    
    models = list()
    .task = subsetTask(.task, subset = .subset)
    data = mlr::getTaskData(.task)
    features = data[!(colnames(data) %in% mlr::getTaskTargetNames(.task))]
    
    for (lab in mlr::getTaskTargetNames(.task))
    {
        ctask = makeClassifTask(id = lab, data = cbind(features, data[lab]), target = lab)
        ctask$task.desc$class.levels = c(FALSE, TRUE)

        models[[lab]] = train(.learner$next.learner, ctask, weights = NULL)

        features = cbind(features, sapply(data[lab], as.numeric))
    }
    return(makeChainModel(next.model = models, cl = c(.learner$model.subclass)))
}

predictLearner.CCWrapper = function(.learner, .model, .newdata, ...) {
    #' function makes prediction for newdata
    #' @param .learner a CC learner
    #' @param .model a CC model
    #' @param .newdata data to be evaluated
    #' @return a prediction for .newdata
    #' @usage via predict(model, task) or predict(model, data)
    
    models = mlr::getLearnerModel(.model, more.unwrap = FALSE)
    out = matrix(nrow = nrow(.newdata), ncol = length(models), dimnames = list(NULL, names(models)))

    for (i in seq_len(length(models)))
    {
        prediction = predict(models[[i]], newdata = .newdata, ...)
        if (.learner$predict.type == "response") {
            prediction = as.logical(getPredictionResponse(prediction))
        } else {
            prediction = getPredictionProbabilities(prediction, cl="TRUE")
        }
        out[, i] = prediction
        .newdata[names(models)[i]] = as.numeric(prediction)
    }
    
    return(out)
}

registerS3method("predictLearner", "<CCWrapper>", 
  trainLearner.CCWrapper)
registerS3method("trainLearner", "<CCWrapper>", 
  predictLearner.CCWrapper)