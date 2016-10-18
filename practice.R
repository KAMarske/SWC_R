#What: SWC workshop
#When: October 18, 2016
#Who: Katharine Marske
#Where: Hatcher library, UM
#Packages necessary

install.packages(c('RSQLite', 'dplyr', 'tidyr', 'ggplot'))
library (RSQLite)


conn<-dbConnect(SQLite(), dbname='C:/Users/vrw239/Desktop/survey.sqlite')
tables<-dbListTables(conn)
tables

class(tables)

#using R to extract data from SQL database
surveys <-dbGetQuery(conn, 'SELECT *FROM surveys')#the middleware will automatically add the ;
head(surveys)
summary(surveys)

#retrieve information from all three tables in database, join based on species_id and plot_id
#species_id and plot_id are called 'foreign keys' because they link the tables
surveys <-dbGetQuery(conn, 'SELECT * FROM surveys 
                     JOIN species ON surveys.species_id=species.species_id
                     JOIN plots ON surveys.plot_id = plots.plot_id;')
names(surveys)
summary(surveys)
head(surveys)
#we end up with species_id and plot_id dublicated
#rather than playing with it now, we will work with the provided csv file

#Once data are extracted, disconnect from database to ease up on hard drive
dbDisconnect(conn)
rm(conn)

surveys<-read.csv('C:/Users/vrw239/Desktop/ecology.csv')
head(surveys)
##data frame is a collection of vectors of equal length (a rectangular table). 
##Vectors can be different data types (unlike in matrix)
class(surveys)

############ Different types of data in R
x1 <- c(1, 2, 3) #a numeric vector
class(x1)
typeof(x1) # Double precision is the default in R, unlike SQL!
x2 <- c("a", "b", "c") #a character vector
class(x2)
typeof(x2)
df <- data.frame(
  x1 = c(TRUE, FALSE, TRUE), #a logical vector
  x2 = c(1, 'red', 2)) #a factor vector, will get turned into a character vector
class(df$x2)
#lists allow storing different types of data, although confusing to work with

str(surveys)
#data frame can hold diff types of vectors, basically a list of vectors

class(surveys$year) #gets out vector
head(surveys$year)
class(surveys['year']) #gets out data frame
head(surveys['year'])
class(surveys[,'year']) #gets out vector (all the rows but just in the year column)
class(surveys[,4]) #gets out vector

##Factors--represent categorical data, can be un/ordered. Stored as integers that code for human-readable labels.
##Treated as integers under the hood-->need to be careful when we work with strings.
##Once created, can only contain a pre-described number of levels
surveys$sex
levels(surveys$sex)
nlevels(surveys$sex)

spice <- factor(c('low', 'med', 'low', 'high'), levels =c('low', 'med', 'high'), ordered=T)
max(spice)
#to reorder
spice <-ordered( spice, levels= c('high', 'med', 'low'))

#to quickly examine our data
tabulation <-table(surveys$taxa)
tabulation
barplot(tabulation)#automatically printed in alpha order (taxa is unordered)
###Reorder the bars based on how frequently the taxa appears in the data

surveys$taxa <-ordered(surveys$taxa, levels=c('Rodent', 'Bird', 'Rabbit', 'Reptile'))
tabulation <-table(surveys$taxa)
tabulation
barplot(tabulation)
#to shorten:
barplot(table(surveys$taxa))
#However, now we have designated them as treatment levels, which may have an effect for a later analysis. 
#Rodent is the 'baseline' if I am doing some sort of regression.
#Better to make a dummy variable rather than reassign the column
mytaxaforplotting<-ordered(surveys$taxa, levels=c('Rodent', 'Bird', 'Rabbit', 'Reptile'))

##Cross-tabulation--a quick way to make sure the data make sense
table(surveys$year, surveys$taxa)
#make this easier with WITH
with(surveys, table(year,taxa)) #look for these columns in surveys, not the global environment

##Check out programming in R lesson on SWC website


#############################################################
############# Subsetting and aggregating data


#What was the median wt of each rodent between 1980-1990?
#start simple and progressively build up to question

#Comparison operator
surveys$taxa=='Rodent' #output is logical vector (list of true and false for every row of dataframe)
length(surveys$taxa=='Rodent')
dim(surveys)

#use this in a conditional statement to select the TRUE rows
surveys[surveys$taxa =='Rodent', 'taxa']#selects just the taxa column so we can take a look
surveys[surveys$taxa =='Rodent',]


#subset to 1980-1990
#integer sequence: 1980:1990 or seq.int(1980,1990)

##My solution, seems to work but throws up some errors
head(surveys)
test<-surveys[surveys$year == 1980:1990, 'year']
#test with max, min, median
test<-surveys[surveys$year == 1980:1990,]
head(test)

##Correct answers:
surveys[surveys$year %in% seq.int(1980,1990),]###need that last comma!!!!
#much safer than putting separate conditionals for ><
surveys[surveys$year %in% seq.int(1980,1990) & surveys$taxa == 'Rodent',]


############################## The package dplyr
#easy and fast tools for working with dataframe (many routines have been compiled in C++)
#can also connect to databases and query

library(dplyr)

output<-select(surveys, year, taxa, weight) #similar to SQL
head(output)

filter(output, taxa=='Rodent')

#if package conflicts use dplyr::filter(output, taxa=='Rodent')

#combine select and filter to do both of those things at once so not having to define intermediate dataframes

##nested function
filter(select(surveys, year, taxa, weight), taxa=='Rodent')

##more efficient/readable to do this with pipes %>%
rodent_surveys<-surveys %>%    ###from here on everything is called from the surveys df
  filter(taxa=='Rodent') %>%
  select(year, taxa, weight)
#each line is a new step in pipeline, pipe symbol goes at the end of lines

####Do the same conditoinal as above (subset surveys to rodents between 1980-1990)
test<-surveys %>% 
  filter(taxa=='Rodent', year %in% seq.int(1980,1990)) %>%
  select(year, taxa, weight)
head(test)

##to write code in different way and see if it gives the same result:
all.equal(rodent_surveys,rodent_surveys2)

##create new col based on existing col
surveys%>%
    mutate(weight_kg = weight/1000) %>% #do not need to provide 1000 as decimal to keep precision as in SQL
    head()  ##can pipe directly into other function

##Split, apply, combine

#sumarize median weight of every species

surveys %>%
    filter(!is.na(weight)) %>%
    group_by(species_id) %>%
    summarize(median(weight))


##we no longer have a dataframe; we have a tibble

##this isnt working
surveys %>%
  filter(!is.na(weight)), taxa=='Rodent', year %in% seq.int(1980,1990)) %>%
  group_by(species_id) %>%
  summarize(median(weight))


###totally lost at this point
surveys_complete<-surveys %>%
  filter(!is.na(weight)),
    species_id != '',
    !is.na(hindfoot_length,)
    sex != '',
    taxa=='Rodent')

head(surveys)

############## 

library(ggplot2)

ggplot(data=common_surveys,
      aes(x=weight, y=hindfoot_length))+
      geom_point()

ggplot(data=common_surveys,
    aes(x=weight, y=hindfoot_length, color=species_id))+  #maps my data onto the aesthetics of the graphic
    geom_point()



