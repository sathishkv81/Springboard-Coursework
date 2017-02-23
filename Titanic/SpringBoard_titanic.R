#1: Port of embarkation
#The embarked column has some missing values, which are known to correspond to passengers who actually embarked at Southampton. 
#Find the missing values and replace them with S. (Caution: Sometimes a missing value might be read into R as a blank or empty string.)

Titanic_DS$embarked[Titanic_DS$embarked == ''] <- "S"

#2: Age
#You’ll notice that a lot of the values in the Age column are missing. 
#While there are many ways to fill these missing values, using the mean or median of the rest of the values is quite common in such cases.
#Calculate the mean of the Age column and use that value to populate the missing values
#Think about other ways you could have populated the missing values in the age column. Why would you pick any of those over the mean (or not)?

Titanic_DS$age[is.na(Titanic_DS$age)] <- mean(Titanic_DS$age,na.rm = TRUE)

#3: Lifeboat
#You’re interested in looking at the distribution of passengers in different lifeboats, but as we know, many passengers did not make it to a boat :-( 
#This means that there are a lot of missing values in the boat column. Fill these empty slots with a dummy value e.g. the string 'None' or 'NA'

gsub("^$","NONE",Titanic_DS$boat)

#4: Cabin
#You notice that many passengers don’t have a cabin number associated with them.
#Does it make sense to fill missing cabin numbers with a value?
#What does a missing value here mean?
#You have a hunch that the fact that the cabin number is missing might be a useful indicator of survival. 
#Create a new column has_cabin_number which has 1 if there is a cabin number, and 0 otherwise.

Titanic_DS$has_cabin_number <- as.numeric(grepl("[[:alnum:]]",Titanic_DS$cabin))

#Alternate way to add a has_cabin_number

titanic3 <- titanic3 %>% mutate(has_cabin_number=ifelse(is.na(cabin)==TRUE,0,1)) %>% as.data.frame()
