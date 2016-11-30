library(dplyr)
library(stringr)
library(qdapRegex)

setwd("C:/Users/Derek/Google Drive/R/Recipe Resources/Data Cleaning")
ingredientCombinations <- read.csv("Ingredient Combinations.csv")

# make all of the ingredients lowercase
ingredientCombinations <- mutate_each(ingredientCombinations, funs(tolower)) 

regexExpressions <- c( "\\,.*",
                       "\\s*\\([^\\)]+\\)",
                       "[0-9]",
                       "\\/",
                       "\\.*\\b(g)\\b",
                       "\\.*\\b(gram)\\b",
                       "\\.*\\b(oz)\\b",
                       "\\.*\\b(oz.)\\b",
                       "\\.*\\b(ounce)\\b",
                       "\\.*\\b(ounces)\\b",
                       "\\.*\\b(cup)\\b",
                       "\\.*\\b(cups)\\b",
                       "\\.*\\b(ml)\\b",
                       "\\.*\\b(lb)\\b",
                       "\\.*\\b(lb.)\\b",
                       "\\.*\\b(lbs)\\b",
                       "\\.*\\b(lbs.)\\b",
                       "\\.*\\b(pound)\\b",
                       "\\.*\\b(pounds)\\b",
                       "\\.*\\b(bottle)\\b",
                       "\\.*\\b(t)\\b",
                       "\\.*\\b(t.)\\b",
                       "\\.*\\b(tsp)\\b",
                       "\\.*\\b(teaspoon)\\b",
                       "\\.*\\b(teaspoons)\\b",
                       "\\.*\\b(T)\\b",
                       "\\.*\\b(T.)\\b",
                       "\\.*\\b(tbsp)\\b",
                       "\\.*\\b(tablespoon)\\b",
                       "\\.*\\b(tablespoons)\\b",
                       "\\.*\\b(gallon)\\b",
                       "\\.*\\b(gallons)\\b",
                       "\\.*\\b(quart)\\b",
                       "\\.*\\b(quarts)\\b",
                       "\\.*\\b(ripe)\\b",
                       "\\.*\\b(fl. oz.)\\b",
                       "\\.*\\b(fl oz.)\\b",
                       "\\.*\\b(fl oz)\\b",
                       "\\b(well-crumbled)\\b",
                       "\\b(dried)\\b",
                       "\\b(low-salt)\\b",
                       "\\b(low salt)\\b",
                       "\\b(low sodium)\\b",
                       "\\b(low-sodium)\\b",
                       "\\b(can)\\b",
                       "\\b(cans)\\b",
                       "\\b(bag)\\b",
                       "\\b(bags)\\b",
                       "\\b(medium)\\b",
                       "\\b(fine)\\b",
                       "\\b(finely)\\b",
                       "\\b(chopped)\\b",
                       "\\b(diced)\\b",
                       "\\b(coarse)\\b",
                       "\\b(bunch)\\b",
                       "\\b(small)\\b",
                       "\\b(large)\\b",
                       "\\b(beaten)\\b",
                       "\\b(sprigs)\\b",
                       "\\b(sheet)\\b",
                       "\\b(sheets)\\b",
                       "\\b(frozen)\\b",
                       "\\b(brewed)\\b",
                       "\\b(package)\\b",
                       "\\b(packages)\\b",
                       "\\b(litres)\\b",
                       "\\b(litre)\\b",
                       "\\b(liters)\\b",
                       "\\b(liter)\\b",
                       "\\b(mix)\\b",
                       "\\b(minced)\\b",
                       "\\b(head)\\b",
                       "\\b(heads)\\b",
                       "\\b(chopped)\\b",
                       "\\b(cracked)\\b",
                       "\\b(heaped)\\b",
                       "\\b(a few)\\b",
                       "\\b(few)\\b",
                       "\\b(free-range)\\b",
                       "\\b(chilled)\\b",
                       "\\b(packet)\\b",
                       "\\b(packets)\\b",
                       "\\b(ready-made)\\b",
                       "\\b(all-purpose)\\b",
                       "\\b(freshly)\\b",
                       "\\b(packed)\\b",
                       "\\b(slice)\\b",
                       "\\b(slices)\\b",
                       "\\b(fresh)\\b",
                       "\\b(freshly)\\b",
                       "\\b(shredded)\\b",
                       "\\b(cooked)\\b",
                       "\\b(uncooked)\\b",
                       "\\b(pitted)\\b",
                       "\\b(quarter)\\b",
                       "\\b(cold)\\b",
                       "\\b(ground)\\b",
                       "\\b(self-raising)\\b",
                       "\\b(quartered)\\b",
                       "\\b(gold medal)\\b",
                       "\\b(gold medal)\\b",
                       "\\b(minced)\\b",
                       "\\b(softened)\\b",
                       "\\b(drop)\\b",
                       "\\b(drops)\\b",
                       "\\b(crumbled)\\b",
                       "\\b(stalk)\\b",
                       "\\b(stalks)\\b",
                       "\\b(sliced)\\b",
                       "\\b(bulb)\\b",
                       "\\b(clove)\\b",
                       "\\b(of)\\b",
                       "\\b(handful)\\b",
                       "\\b(stick)\\b",
                       "\\b(good-quality)\\b",
                       "\\b(sticks)\\b",
                       "\\b(salted)\\b",
                       "\\b(unsalted)\\b",
                       "\\b(smooth)\\b",
                       "\\b(coarsely)\\b",
                       "\\b(whole)\\b",
                       "\\b(half)\\b",
                       "\\b(halved)\\b",
                       "\\b(sprinkle)\\b",
                       "\\b(-and-)\\b",
                       "\\b(tub)\\b",
                       "\\b(loaf)\\b",
                       "\\b(-and)\\b",
                       "\\+",
                       "\\b(land o' lakes)\\b",
                       "&frac;",
                       "&nbsp;",
                       "&.*;", # delete anything with simlar constuction to '$frac;'
                       "(; ).*", # delete '; ' and everything to the right
                       "\\b(plus more)\\b",
                       "\\.",
                       "\\(.*",
                       "^(or)\\b", # this will delete any leading 'or'
                       "^(-)", # this will delete any leading '-'
                       "(-)$", # this will delete any trailing '-'
                       ".*\\b(for cooking)\\b.*", # this will delete the whole entry
                       ".*\\b(instructions)\\b.*", # this will delete the whole entry
                       "\\s*\\([^\\)]+\\)",        # seems like some of the parentheses get missed  
                       "\\.*---\\.*",              # delete entries with '---'   
                       "\\)")

ingredientTemp <- removeRegex(ingredientCombinations, regex=regexExpressions)


# replace 'high-gluten' with 'flour' -- it's a mistake
temp <- ingredientTemp[ , 1]
temp <- str_replace_all(temp, "\\b(high-gluten)\\b", "flour")
ingredientTemp[ , 1] <- temp
rm(temp)

# use a qdapRegex function to remove the extraneous whitespace
ingredientTempTemp <- ingredientTemp
temp1 <- rm_white(ingredientTempTemp[ , 1])
temp2 <- rm_white(ingredientTempTemp[ , 2])
ingredientTempTemp[, 1] <- temp1
ingredientTempTemp[, 2] <- temp2
ingredientTemp <- ingredientTempTemp
rm(temp1, temp2, ingredientTempTemp)

# delete lines with blanks or NA
ingredientTemp <- ingredientTemp[!(ingredientTemp$V1=="" | ingredientTemp$V2==""), ]
ingredientTemp <- ingredientTemp[!(ingredientTemp$V1==" " | ingredientTemp$V2==" "), ]
ingredientTemp <- ingredientTemp[!(is.na(ingredientTemp$V1) | is.na(ingredientTemp$V2)), ]

# reassign rownames
rownames(ingredientTemp) <- NULL

saveRDS(ingredientTemp, file="cleaned data.rds")


# delete '-WORD'
# need to figure out what to do with all the dashes -- 40,000/300,000 lines have one

# testing regex
# temp <- ingredientTemp
# text <- temp[139720 ,2]
# text <- str_replace_all(text, "\\+", "")





