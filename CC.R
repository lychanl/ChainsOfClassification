MakeCC = function(learner, order=NULL) {
    learner = checkLearner(learner, type = "classif")
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
    order = .learner$order
    if (is.null(order)) {
        order = mlr::getTaskTargetNames(task);
    }
    
    models = list()
    
    for (lab in mlr::getTaskTargetNames(task))
    {
        ctask = makeClassifTask(id = lab, data = cbind(features, data[lab]), target = lab)

        models[[lab]] = train(learner$next.learner, ctask, weights = NULL)

        features = cbind(features, sapply(data[lab], as.numeric))
    }
    return(makeChainModel(next.model = models, cl = c(learner$model.subclass)))
}

predictLearner.CCWrapper = function(.learner, .model, .newdata, ...) {
    
    models = mlr::getLearnerModel(.model, more.unwrap = FALSE)
    out = matrix(nrow = nrow(.newdata), ncol = length(models), dimnames = list(NULL, names(models)))

    for (i in seq_len(length(models)))
    {
        prediction = predict(models[[i]], newdata = .newdata, ...)
        if (.learner$predict.type == "response") {
            prediction = as.logical(getPredictionResponse(prediction))
            print("r")
        } else {
            prediction = getPredictionProbabilities(prediction, cl="TRUE")
            print("p")
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