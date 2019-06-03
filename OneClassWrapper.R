makeOneClassWrapper = function(learner) {
    learner = checkLearner(learner, type='classif')
    id = paste("classif.oneClassWrapper", getLearnerId(learner), sep = ".")
    packs = getLearnerPackages(learner)
    type = getLearnerType(learner)
    x = mlr::makeBaseWrapper(id, type, learner, packs, makeParamSet(),
        learner.subclass = c("OneClassWrapper"),
        model.subclass = c("OneClassWrapperModel"))
    x$type = "classif"
    x$properties = c(learner$properties, 'oneclass')
    return(x)
}

trainLearner.OneClassWrapper = function(.learner, .task, .subset = NULL, .weights = NULL, ...) {
    if (length(getTaskDesc(.task)$class.levels) <= 1) {
        x = list(oneclass=TRUE, value=.task$task.desc$positive)
        class(x) = "OneClassWrapperModel"
        return(makeChainModel(next.model = x, cl = c(.learner$model.subclass)))
    }
    
    model = train(.learner$next.learner, .task, .subset, .weights)
    
    x = list(oneclass=FALSE, model=model)
    class(x) = "OneClassWrapperModel"
    return(makeChainModel(next.model = x, cl = c(.learner$model.subclass)))
}

predictLearner.OneClassWrapper = function(.learner, .model, .newdata, ...) {
    .model = mlr::getLearnerModel(.model, more.unwrap = FALSE)
    
    if (.model$oneclass) {
        out = as.logical(rep(.model$value, nrow(.newdata)))
    }
    else {
        pred = predict(.model$model, newdata=.newdata)
        
        if (.learner$predict.type == "response") {
            out = getPredictionResponse(pred)
        } else {
            out = getPredictionProbabilities(pred, cl="TRUE")
        }
    }
    
    return(as.factor(out))
}

getLearnerProperties.OneClassWrapper = function(.learner) {
    return(.learner$properties)
}

isFailureModel.OneClassWrapperModel = function(model) {
    model = mlr::getLearnerModel(model, more.unwrap = FALSE)

  return(!model$oneclass && isFailureModel(model$model))
}

getFailureModelMsg.OneClassWrapperModel = function(model) {
    model = mlr::getLearnerModel(model, more.unwrap = FALSE)
  if (model$oneclass)
      return("")
  return(getFailureModelMsg(model$model))
}

getFailureModelDump.OneClassWrapperModel = function(model) {
    model = mlr::getLearnerModel(model, more.unwrap = FALSE)
  if (model$oneclass)
      return("")
  return(getFailureModelDump(model$model))
}

registerS3method("trainLearner", "<OneClassWrapper>", 
  trainLearner.OneClassWrapper)
registerS3method("getLearnerProperties", "<OneClassWrapper>", 
  getLearnerProperties.OneClassWrapper)
registerS3method("isFailureModel", "<OneClassWrapperModel>", 
  isFailureModel.OneClassWrapperModel)
registerS3method("getFailureModelMsg", "<OneClassWrapperModel>", 
  getFailureModelMsg.OneClassWrapperModel)
registerS3method("getFailureModelDump", "<OneClassWrapperModel>", 
  getFailureModelDump.OneClassWrapperModel)