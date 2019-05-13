result = testCC(.testData, .learner, .task) {
    model = mlr::train(.learner, .task)
    result_train = mlr::predict(model, .task)
    result_test = mlr::predict(model, newdata = .testData)
    
    as.data.frame(result_train)
    performance(result_train)
    
    as.data.frame(result_test)
    performance(result_test)
    
    dupa = cbind(as.data.frame(result_train), as.data.frame(result_test))
    
    return(dupa)
}