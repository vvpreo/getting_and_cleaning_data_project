# loading libraries
library(dplyr)
library(tidyr)

# Fuction that prepares a data frame (test or train)
load_data = function (folder, train = T){
  # Setting paths
  path_to_data = paste("data", folder, sep='/')
  path_subject = paste(path_to_data, ifelse(train, "subject_train.txt", "subject_test.txt"), sep = '/')
  path_features = paste(path_to_data, ifelse(train, "X_train.txt", "X_test.txt"), sep = '/')
  path_activity = paste(path_to_data, ifelse(train, "y_train.txt", "y_test.txt"), sep = '/')
  
  # loading dataframes
  df_subject = read.table(file = path_subject, header = F, col.names=c("subject"))
  df_activity = read.table(file = path_activity, header = F, col.names=c('activity'))
  df_activity_labels = read.table("data/activity_labels.txt")
  df_activity$activity = factor(df_activity$activity, levels=df_activity_labels[,1], labels = df_activity_labels[,2])
  
  # reading features and filtering out features that are not mean or std
  df_features_names = read.table(file = "data/features.txt", header = F, stringsAsFactors = F)
  ptrn = "[Mm][Ee][Aa][Nn]|[Ss][Tt][Dd]"
  df_features = read.table(file = path_features, header = F, col.names = df_features_names[,2])
  df_features = df_features[,grep(pattern = ptrn, colnames(df_features))]
  
  # train/test marker. remains unused.
  df_train = data.frame(train = rep(train, length(df_subject[,1])))
  
  # combining and returning dataframe
  df_data = cbind(df_train,
                  df_subject,
                  df_activity,
                  df_features)
  df_data
}

# concatenating test and train data here
df_data = rbind(load_data("train"), load_data("test", train = F))

# Splitting data on subsets with common activity+object
df_tidy = split(df_data, list(df_data$activity, df_data$subject))

# Counting mean for each variable
df_tidy = sapply(df_tidy, function(x) {colMeans(x[-(1:3)])})
# Transposing a dataframe (just to remain all on it's places)
df_tidy = t(df_tidy)

# separating key variable on two to complain tidy form of data
df_tidy = cbind(data.frame(act.obj = rownames(df_tidy)), df_tidy)
df_tidy = separate(df_tidy, act.obj, c('activity', 'object'), sep = '\\.')

# saving results
write.table(df_tidy, file = "subject-activity_vs_averages.txt", row.names = F)
