# 10.5 Exercises

library(tidyverse)
# 1. How can you tell if an object is a tibble? (Hint: try printing mtcars, which is a regular data frame).

data(mtcars)
as.tibble(mtcars) %>% is.tibble() # Use is.tibble() to check if it is a tibble. 

# 2. Compare and contrast the following operations on a data.frame and equivalent tibble. 
#   What is different? 
#   Why might the default data frame behaviours cause you frustration?
# 
df <- data.frame(abc = 1, xyz = "a")
df_tibble <- tibble::tibble(abc = 1, xyz = "a")

# df does partial matching
df$x
df_tibble$x # tibble doesn't
df[, "xyz"] # returns a factor
df_tibble[, "xyz"] # returns a data frame
# returns the same thing but xyz is a factor in the data farme and a char in the tibble
df[, c("abc", "xyz")]
df_tibble[, c("abc", "xyz")]



# 3. If you have the name of a variable stored in an object, e.g. var <- "mpg", how can you extract the reference variable from a tibble?

dftib <- tibble(
  +     x = runif(5),
  +     y = rnorm(5),
  +     var = c('mpg', 'mpg', 'jpg', 'abd', 'qwe'))

dplyr::filter(dftib, var == 'mpg')

# 4. Practice referring to non-syntactic names in the following data frame by:
#   
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
#   1) Extracting the variable called 1.
annoying$`1`

#   2) Plotting a scatterplot of 1 vs 2.
ggplot(annoying, aes(`1`, `2`)) + geom_point()

#   3) Creating a new column called 3 which is 2 divided by 1.
annoying <- mutate(annoying, `3` = `2` / `1`)

#   4) Renaming the columns to one, two and three.
# 
colnames(annoying) <- c('one', 'two', 'three')

# 5. What does tibble::enframe() do? When might you use it?

# It convert named vector to a tibble. I might use it when I need to do data manupulation involving a named vector.

# 6. What option controls how many additional column names are printed at the footer of a tibble?

# options(tibble.max_extra_cols = n)


# 12.6.1 Exercises
who
# 
# 1. In this case study I set na.rm = TRUE just to make it 
#easier to check that we had the correct values. Is this reasonable? 
# It is reasonable since the initial tibble has too many missing data.

# Think about how missing values are represented in this dataset. 
# Are there implicit missing values? 
# What’s the difference between an NA and zero?
who %>% filter(is.na(new_sp_m014) == FALSE) %>% arrange(new_sp_m014)
who %>% filter(is.na(new_sp_m014) == FALSE) %>% arrange(desc(new_sp_m014))
# there is not other implicit missing values. 
# NA is missing value. zero is no case if not indicated. 

# 2. What happens if you neglect the mutate() step? (mutate(key = stringr::str_replace(key, "newrel", "new_rel")))
# If we neglect the mutate() step, then we can't split by '_' for some "newrel".

# 3. I claimed that iso2 and iso3 were redundant with country. Confirm this claim.
who %>% distinct(country) %>% count()
who %>% distinct(iso2) %>% count()
who %>% distinct(iso3) %>% count()
#the distinct elements have same number, so it is very very likely 

# 4. For each country, year, and sex compute the total number of cases of TB. Make an informative visualisation of the data.
who.tidy <- who %>% 
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE) %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>% 
  separate(key, c("new", "type", "sexage"), sep = "_") %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)
who.tidy %>% count(country, year, type, sex) 

# Tidy Data article:
  
# Use dplyr and tidyr to reproduce


#3)table 4 -> table 6
library(foreign)
library(stringr)
library(plyr)
library(reshape2)
library(xtable)

pew<-read.spss("pew.sav", header=TRUE,stringsAsFactors = FALSE )
pew <- as.data.frame(pew)
religion <- pew[c("q16", "reltrad", "income")]
religion$reltrad <- as.character(religion$reltrad)
religion$reltrad <- str_replace(religion$reltrad, " Churches", "")
religion$reltrad <- str_replace(religion$reltrad, " Protestant", " Prot")
religion$reltrad[religion$q16 == " Atheist (do not believe in God) "] <- "Atheist"
religion$reltrad[religion$q16 == " Agnostic (not sure if there is a God) "] <- "Agnostic"
religion$reltrad <- str_trim(religion$reltrad)
religion$reltrad <- str_replace_all(religion$reltrad, " \\(.*?\\)", "")

religion$income <- c("Less than $10,000" = "<$10k", 
                     "10 to under $20,000" = "$10-20k", 
                     "20 to under $30,000" = "$20-30k", 
                     "30 to under $40,000" = "$30-40k", 
                     "40 to under $50,000" = "$40-50k", 
                     "50 to under $75,000" = "$50-75k",
                     "75 to under $100,000" = "$75-100k", 
                     "100 to under $150,000" = "$100-150k", 
                     "$150,000 or more" = ">150k", 
                     "Don't know/Refused (VOL)" = "Don't know/refused")[religion$income]

religion$income <- factor(religion$income, levels = c("<$10k", "$10-20k", "$20-30k", "$30-40k", "$40-50k", "$50-75k", 
                                                      "$75-100k", "$100-150k", ">150k", "Don't know/refused"))

counts <- count(religion, c("reltrad", "income"))
names(counts)[1] <- "religion"

tidy.clean<-xtable(counts[1:10, ], file = "pew-clean.tex")



#4)table 7 -> table 8
#4)convert table 7 to table 8

options(stringsAsFactors = FALSE)
library(lubridate)
library(reshape2)
library(stringr)
library(plyr)
library(xtable)
library(magrittr)
library(tidyr)

bb <-read_csv("billboard.csv")
bb.4 <- bb %>% gather(key="week", value ="rank", -year, -artist.inverted, -track, -time, -genre, -date.entered, -date.peaked) %>% 
  select(year, artist=artist.inverted, time, track, date=date.entered, week, rank) %>% 
  arrange(track) %>% 
  filter(!is.na(rank))

for (i in 1:nrow(bb.4)) {
  bb.4[i, ]$week <- str_extract_all(bb.4[i, ]$week, '\\d')[[1]] %>% str_c(collapse = "")
}

#must to specified rename in the dplyr
bb.10 <- bb.4 %>% arrange(artist, track) %>% 
  mutate(date = date+(as.numeric(week)-1)*7) %>% 
  mutate(rank = as.integer(rank))

