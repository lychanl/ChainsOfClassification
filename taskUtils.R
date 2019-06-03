clean_name = function (str) {
    str = gsub('[^a-zA-Z0-9_]', '_', str)
    
    if(grepl('^[0-9]*$', str))
        str = paste('col', str, sep='')
    
    return(str)
}


create_task = function(mldr_dataset) {
    labels = colnames(as.data.frame(mldr::mldr_to_labels(mldr_dataset)))
                     
    data = mldr_dataset$dataset[mldr_dataset$attributesIndexes]
    
    data = cbind(data, mldr::mldr_to_labels(mldr_dataset) == 1)
    data = tidyr::drop_na(data)
    
    colnames(data) = sapply(colnames(data), clean_name)
    labels = sapply(labels, clean_name)

    labels = labels[labels %in% colnames(data)]
    
    to_map = sapply(data, function(x) all(trimws(x) %in% c("YES", "NO", "TRUE", "FALSE")))
    data[to_map] = sapply(data[to_map], function(x) as.numeric(trimws(x) == "YES" | trimws(x) == "TRUE"))
    
    data[] = sapply(data, as.numeric)
    data[labels] = sapply(data[labels], as.logical)
    
    data = data[sapply(data, function(x) !all(is.na(x)))]
    
    return(makeMultilabelTask(data=data, target=labels))
}