require(dplyr)
require(tidyr)
Testing <- read.csv('refine_original.csv', stringsAsFactors=FALSE)

#Assumption: Data is imported into RStudio variable Testing
# 1: Clean up brand names
# Clean up the 'company' column, so all of the misspellings of the brand names are standardized. 
# For example, you can transform the values in the column to be: philips, akzo, van houten and unilever (all lowercase).

Testing$company <- sub(".*ps","philips",Testing$company, ignore.case = TRUE)
Testing$company <- sub(".*ak.*","akzo",Testing$company, ignore.case = TRUE)
Testing$company <- sub(".*ten","van houten",Testing$company, ignore.case = TRUE)
Testing$company <- sub(".*ver","unilever",Testing$company, ignore.case = TRUE)

#2: Separate product code and number
#Separate the product code and product number into separate columns i.e. add two new columns called product_code and product_number, containing the product code and number respectively

Testing <- Testing %>% separate(Product.code...number,c("Product_code","Product_number"),sep = "-")

#3: Add product categories
#You learn that the product codes actually represent the following product categories:
#p = Smartphone
#v = TV
#x = Laptop
#q = Tablet
#In order to make the data more readable, add a column with the product category for each record.

Testing <- Testing %>% mutate(product_category = recode(Product_code,
    p = 'Smartphone',
    v = 'TV',
    x = 'Laptop',
    q = 'Tablet'
))
                             
                             
#4: Add full address for geocoding
#You'd like to view the customer information on a map. In order to do that, the addresses need to be in a form that can be easily geocoded. 
#Create a new column full_address that concatenates the three address fields (address, city, country), separated by commas.
 
Testing <- Testing %>% unite(full_address,address,city,country, sep = ", ")

#5: Create dummy variables for company and product category
#Both the company name and product category are categorical variables i.e. they take only a fixed set of values. 
#In order to use them in further analysis you need to create dummy variables. Create dummy binary variables for each of them with the prefix company_ and product_ i.e.
#Add four binary (1 or 0) columns for company: company_philips, company_akzo, company_van_houten and company_unilever

Testing$company_philips <- as.numeric(Testing$company %in% "philips")
Testing$company_akzo <- as.numeric(Testing$company %in% "akzo")
Testing$company_vanhouten <- as.numeric(Testing$company %in% "van houten")
Testing$company_unilever <- as.numeric(Testing$company %in% "unilever")

#Add four binary (1 or 0) columns for product category: product_smartphone, product_tv, product_laptop and product_tablet

Testing$product_smartphone <- as.numeric(Testing$product_category %in% "Smartphone")
Testing$product_tv <- as.numeric(Testing$product_category %in% "TV") 
Testing$product_laptop <- as.numeric(Testing$product_category %in% "Laptop")
Testing$product_tablet <- as.numeric(Testing$product_category %in% "Tablet")
