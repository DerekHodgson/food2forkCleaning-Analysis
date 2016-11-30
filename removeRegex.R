# removeRegex() takes a two column dataframe and a regex expressions
#   (or vector of regex expressions) and deletes them from the dataframe
# requires stringr

removeRegex <- function(dataframe, regex){
  if(!require("stringr")){
    print("trying to install stringr")
    install.packages("stringr")
    if(require(stringr)){
      print("stringr installed and loaded")
    } else {
      stop("could not install stringr")
    }
  }
  
  # use the stringr package to delete the regex expression where found in each column
  for(i in 1:length(regex)){
    temp <- dataframe[ , 1]
    temp <- str_replace_all(temp, regex[i], "")
    dataframe[ , 1] <- temp
    rm(temp)
    
    temp <- dataframe[ , 2]
    temp <- str_replace_all(temp, regex[i], "")
    dataframe[ , 2] <- temp
  }
  return(dataframe)
}