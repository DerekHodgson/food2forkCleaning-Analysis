# food2forkDataCleaning

Requires dplyr, stringr, and qdapRegex.

This uses a series of ~150 regex expressions to remove much of the extraneous information in the ingredient list. It also removes all of the lines blanks/NAs.

The function removeRegex() takes a vector of Regex expressions and a two column dataframe and uses the expressions to delete them in the dataframe.
