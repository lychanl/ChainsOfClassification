source("CC.R")

testCC = function(.learner, .task, .resampleName, .iters) {
    rDesc = makeResampleDesc(.resampleName, iters = .iters)
    
    res = resample(.learner, .task, rDesc, measures = list(multilabel.hamloss, multilabel.subset01, timepredict))

    print(res)
    
    return(res)
}

classifiers = function(.task) {
    svm = makeLearner("classif.svm")
    gbm = makeLearner("classif.gbm", distribution="bernoulli")
    
    our_cc = makeCC(svm)
    our_cc2 = makeCC(gbm)
    
    testCC(our_cc, .task, "CV", 5)
    testCC(our_cc2, .task, "CV", 5)
    
    forest = makeLearner("multilabel.randomForestSRC")
    ferns = makeLearner("multilabel.rFerns")
    cc = makeMultilabelClassifierChainsWrapper(svm)
    cc2 = makeMultilabelClassifierChainsWrapper(gbm)
    br = makeMultilabelBinaryRelevanceWrapper(svm)
    br2 = makeMultilabelBinaryRelevanceWrapper(gbm)
    ns = makeMultilabelNestedStackingWrapper(svm)
    ns2 = makeMultilabelStackingWrapper(gbm)
    dbr = makeMultilabelDBRWrapper(svm)
    dbr2 = makeMultilabelDBRWrapper(gbm)
    
    testCC(forest, .task, "CV", 5)
    testCC(ferns, .task, "CV", 5)
    testCC(cc, .task, "CV", 5)
    testCC(cc2, .task, "CV", 5)
    testCC(br, .task, "CV", 5)
    testCC(br2, .task, "CV", 5)
    testCC(ns, .task, "CV", 5)
    testCC(ns2, .task, "CV", 5)
    testCC(dbr, .task, "CV", 5)
    testCC(dbr2, .task, "CV", 5)
    
}