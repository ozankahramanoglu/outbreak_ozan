###########################################################################################

######################
#######################
library(RecordLinkage)
#########################################################################################
# here we put a selection of most common column names that users use.
#The first element of each vector should be the best name that
# we suggest users to use and  which our code is based on.
#for example "Retention Time" and "Full Width at Half Maximum" which are the first element
# of each vector in the list, are our suggestion so we wrote them in the fisrt place.
best_colnames <- list(
  c("Date","date"),
  c("Number","number","number of patients","NumberofPatients","numberofpatients","Number_of_patients","number_of_patients","Number_of_Patients"),
  c("Holiday", "holiday")
)
#### camelCaseSplit function ##############################################################################################
camelCaseSplit <- function(x) {
  # This function get a camelCase word and splits it.
  # Ex : camelCaseSplit("myComputerIsHere") ---> my Computer Is Here
  return(gsub("([a-z])([A-Z])", "\\1 \\L\\2", x, perl = TRUE))
}
#### punc_remove function #################################################################################################
punc_remove <- function(x){
  # This function removes any existing punctuation in your sentence or word
  #and transfer it to space.
  # Ex1: punc_remove("Best.RT") --> "Best RT"     #Ex2: punc_remove("Best_RT") --> "Best RT"
  return(gsub("[[:punct:]///' ]", " ", x))
}
#### clearString function ###############################################################################################
clearString <- function(x){
  # This function, gets a word or setence, Splits it (if it is a camelCase),
  #removes any existing punctuations, and transfer
  # all Upper Case letters to lower case letters.
  # Ex: clearString("myName_isJohn.black") --> my name is john black
  return(tolower(punc_remove(camelCaseSplit(x))))
}


#### guessColumnName function ###########################################################################################
# This function receives the data and check the column names of data and changes
#the column names if it is not the
# same names as our suggested sample data to fit our suggested sample data
guessColumnName <- function(x){
  
  a <- clearString(x)
  
  max_index <- 0
  max <- -1
  for(i in seq_along(best_colnames)){
    col <- best_colnames[[i]]
    for(j in seq_along(col)){
      sim <- levenshteinSim(a,clearString(col[j]))
      if(sim > max){
        max <- sim
        max_index <- i
      }
    }
  }
  if (max > 0.6) {
    return(best_colnames[[max_index]][1])
  }
  else {
    return(x)
  }
}

#
##
###


input.date.check <- function(data) {
  
  
  
  error_message <- ""
  #-------------------------
  # Check that all columns are in required format
  # 1. Start with "date" column. All the values of date column should be in date format)

  if(!is.Date(data[,"Date"])) {
    tryCatch({data[,"Date"] <- as.Date(parse_date_time(x = data[,"Date"],
                                                       orders = c("%d-%b-%y", "%d.%m.%Y")))},
             error = function(e){
               print(e)
             })
    

  }
  if(!is.Date(data[,"Date"])) {
    error_message <- paste(error_message, "All the values of date column should be in date format.\n\n")
  }

  if(error_message != "") {
    print(paste(error_message))
  }

  return(data)
}

#################################################################################################
input.sanity.check <- function(data) {
  
  error_message <- ""
  
  # get the column names and convert them into the column names as in required format
  #(For ecample we want Date but a user might use Dat, this function
  #ex:auotomatically change Dat to Date)
  colnames(data) <- unlist(lapply(colnames(data), function(x)guessColumnName(x)))
  
  ############## conditions ##############
  
  required_column_names <- c("Date","Number")
  
  provided_column_names <- colnames(data)
  if(all(required_column_names %in% provided_column_names)==FALSE) {
    missedInput <- which(!(required_column_names %in% provided_column_names))
    error_message <- paste("ERROR : The required input(inputs) : ",
                           paste(required_column_names[missedInput], collapse = ", "),
                           " is(are) not provided in data set. Please add it to your
                           data and try again.\n\n")
  }
  
  if(error_message != "") {
    return(paste(error_message))
  }
  
  if(!("Holiday" %in% colnames(data))) {
    data[,"Holiday"] <- NA
    error_message <- paste("ERROR : The optional input, holiday is not provided in data set.
                           You can get better results if you add it to your data and try again.\n\n")
  }
  
  if(error_message != "") {
    return(paste(error_message))
  }
  data[,"Holiday"] <- factor(data[,"Holiday"])
  
  # date column should be saved as date format.
  
  # 1.

  
  
  # 2. Check the "Number" column. All the values of that column should be in numeric format)
  if(is.numeric(data[ , "Number"]) == FALSE) {
    error_message <- paste(error_message, "All the values of Number column should be numeric and positive.\n\n")
  }
  
  if(error_message != "") {
    return(paste(error_message))
  }
  
  # 3. Check the "holiday" column. All the values of that column should be in numeric format)
  if(!all(data[,"Holiday"] %in% c(0,1))) {
    error_message <- paste(error_message, "All the values of holiday column should be 0 for non-holidays and 1 for holidays.\n\n")
  }
  
  if(error_message != "") {
    return(paste(error_message))
  }
  
  # for custom metrics we are checking them to be numeric in QCMetrics in
  # "find_custom_metrics" function and only accepting numeric columns after Annotation
  
  # if there is any missing value in data replace it with NA
  data[data==""] <- NA
  
  
  print("Your data is ready to go!")
  return(data)
  }

#data <- input.date.check(data)
#data <- input.sanity.check(data)
