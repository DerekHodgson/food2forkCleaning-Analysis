# food2forkCleaning-Analysis

Requires dplyr, stringr, and qdapRegex.

This uses the data downloaded with the food2fork API. The data for this analysis is arranged in two columns. Each ingredient is combined with each other ingredient in the recipe.

This uses a series of ~150 regex expressions to remove much of the extraneous information in the ingredient list. It also removes all of the lines blanks/NAs.

The function removeRegex() takes a vector of Regex expressions and a two column dataframe and uses the expressions to delete the matching text in the dataframe.
