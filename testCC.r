source("CC.R")
source("OneClassWrapper.R")

testCC = function(.learner, .task, .resampleName, .iters) {
    rDesc = makeResampleDesc(.resampleName, iters = .iters)
    
    res = resample(.learner, .task, rDesc, measures = list(multilabel.hamloss, multilabel.subset01, timepredict))

    print(res)
    
    return(res)
}

classifiers = function(.task) {
    logreg = makeLearner("classif.logreg")
    gbm = makeLearner("classif.gbm", distribution="bernoulli")
    
    logreg = makeOneClassWrapper(logreg)
    gbm = makeOneClassWrapper(gbm)
    
    forest = makeLearner("multilabel.randomForestSRC")
    ferns = makeLearner("multilabel.rFerns")
    cc = makeMultilabelClassifierChainsWrapper(logreg)
    cc2 = makeMultilabelClassifierChainsWrapper(gbm)
    br = makeMultilabelBinaryRelevanceWrapper(logreg)
    br2 = makeMultilabelBinaryRelevanceWrapper(gbm)
    ns = makeMultilabelNestedStackingWrapper(logreg)
    ns2 = makeMultilabelNestedStackingWrapper(gbm)
    s = makeMultilabelStackingWrapper(logreg)
    s2 = makeMultilabelStackingWrapper(gbm)
    dbr = makeMultilabelDBRWrapper(logreg)
    dbr2 = makeMultilabelDBRWrapper(gbm)
    
    print("cc - logreg")
    testCC(cc, .task, "CV", 5)
    print("cc - gbm")
    testCC(cc2, .task, "CV", 5)
    print("forest")
    testCC(forest, .task, "CV", 5)
    print("ferns")
    testCC(ferns, .task, "CV", 5)
    print("binary relevance - logreg")
    testCC(br, .task, "CV", 5)
    print("binary relevance - gbm")
    testCC(br2, .task, "CV", 5)
    print("nested stacking - logreg")
    testCC(ns, .task, "CV", 5)
    print("nested stacking - gbm")
    testCC(ns2, .task, "CV", 5)
    print("stacking - logreg")
    testCC(s, .task, "CV", 5)
    print("stacking - gbm")
    testCC(s2, .task, "CV", 5)
    print("DBR - logreg")
    testCC(dbr, .task, "CV", 5)
    print("DBR - gbm")
    testCC(dbr2, .task, "CV", 5)
    
    our_cc = makeCC(logreg)
    our_cc2 = makeCC(gbm)
    
    print("our cc - logreg")
    testCC(our_cc, .task, "CV", 5)
    print("our cc - gbm")
    testCC(our_cc2, .task, "CV", 5)
}
