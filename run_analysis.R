library(data.table)


# This function process the data from UCI HAR Dataset and write tidy datas into
# a CSV file. The aggregated tidy dataset is also returned by this function.
processing_data <- function(zip_path, tidy_result_path="tidy_result.csv") {
    if(!file.exists(zip_path))
        print("The zipfile specified doesn't exist")
        return
    

    files <- unzip(zip_path)
    # Search activity_labels in order to have the root path folder
    directory_path <- dirname(files[grep("activity_labels.txt$", files)])
    
    
    # Load the train datas
    train_path <- file.path(directory_path, "train")
    subject_train <- fread(file.path(train_path, "subject_train.txt"))
    x_train <- fread(file.path(train_path, "x_train.txt"))
    y_train <- fread(file.path(train_path, "y_train.txt"))


    # Load the test datas
    test_path <- file.path(directory_path, "test")
    subject_test <- fread(file.path(test_path, "subject_test.txt"))
    x_test <- fread(file.path(test_path, "x_test.txt"))
    y_test <- fread(file.path(test_path, "y_test.txt"))
    
    
    # Merge de the test and the train datas
    subject <- rbind(subject_test, subject_train)
    x <- rbind(x_test, x_train)
    y <- rbind(y_test, y_train)
    
    
    # Get the indexes from the features file
    features <- fread(file.path(directory_path, "features.txt"))
    
    
    # Cath also parenthesis to get only mean (and avoid meanFreq)
    features_index <- grep("(std|mean)\\(\\)", features[[2]])
    
    # Replace features with only std/mean
    features <- features[features_index][[2]]
    
    # Replace mean/std by Mean/Std (in order to have camel case)
    features <- gsub("std\\(\\)", "Std", features)
    features <- gsub("mean\\(\\)", "Mean", features)
    features <- gsub("-", "", features)
    
    
    # Merge the data in one dataset
    data <- cbind(subject, y, x[, features_index, with=FALSE])
    # Rename the columns with appropriates names
    colnames(data) <- c("subject", "activity", features)
    
    
    # Load the activity labels
    activity_labels <- fread(file.path(directory_path, "activity_labels.txt"))
    
    # Factor the activity name and place a proper labels
    data$activity <- factor(data$activity, labels = activity_labels[[2]])
    
    # Factor the subjects
    data$subject <- factor(data$subject)
    
    # Convert to data table
    data_table <- setDT(data)
    
    # Aggregate by subject and activities
    agg <- data_table[, lapply(.SD, mean), by=.(subject, activity)]
    
    # Write to a file, with a coma as separator (CSV format)
    write.table(agg, file=tidy_result_path, sep=",")
    
    agg
}


# Call to the function
processing_data("UCI HAR Dataset.zip", "tidy_result.csv")