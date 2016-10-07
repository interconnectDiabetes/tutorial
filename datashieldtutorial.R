# If it hasnt already been done we have to install all of the datashield dependencies before we can do anything.
# ie we have to download the libraries and make them available to our R sessions

# First are the libraries R depends on
install.packages('RCurl', dependencies = TRUE)
install.packages('rjson', dependencies = TRUE)
install.packages('fields', dependencies = TRUE)

# then install opal and co
install.packages('opal', repos='http://cran.obiba.org', type='source')
install.packages('datashieldclient', repos='http://cran.obiba.org', type='source')

# if we get errors from this we install the dependencies one by time
install.packages('opal', repos='http://cran.obiba.org', type='source')
install.packages('dsBaseClient', repos='http://cran.obiba.org', type='source')
install.packages('dsModellingClient', repos='http://cran.obiba.org', type='source')
install.packages('dsGraphicsClient', repos='http://cran.obiba.org', type='source')
install.packages('dsStatsClient', repos='http://cran.obiba.org', type='source')

# I'm an advocate of programming the hard way, ie not by copy pasting and actually typing things
# down, muscle memory is surprisingly helpful in programming!
library(opal)					# loads a library allowing to
library(dsBaseClient)
library(dsStatsClient)
library(dsGraphicsClient)
library(dsModellingClient)
library(metafor)

# This creates lists of locations and objects we'd like to connect to
server <- c( 'lifelines', 'pauldata')
url <- c('https://192.168.56.110:8843','https://192.168.56.111:8443')
table <- c('lifelines.lifelines_harm', 'tutorial.finrisk_harm')
password <- c('test1234')
user <- c('test')
logindata_all <- data.frame(server,url,user,password, table) # here we take all the lists and put them into a dataframe

# ... and login with those details, giving us an 'opals' object which is kind of like a phone operator we have to
# communicate with to talk to the opal servers each time
opals <- datashield.login(logins=logindata_all, assign=TRUE)

## ** Difference between datashield and normal r commands is the prefix ds (typically) **
# now that we are logged in to the machines we can explore starting with the most difficult concept (sorry)
ds.ls() # this command lists the available variables and dataframes in the workspaces of THE MACHINES OVER THERE
# notice the difference between the workspace in your R environment and those in the machines.

# Each study's data is encapsulated inside a dataframe called 'D' lets look at it in more details
ds.summary('D') # we see some descrptive properties of the dataframe

# lets try looking at some of the variables in more detail
ds.summary('D$SEX')

ds.summary('D$WEIGHT')

ds.summary('D$HEIGHT')

# What else can we do? well whatever datashield tells it can do at the moment.
ls("package:dsBaseClient")  # note how that was not a datashield command but an R command inquiring about this datashield package we've got

# anyhow we've got to understand another difficult concept in datashield itself. Aggregate and Assign functions.
# There are two classes of functions in datashield, aggregate and assign functions

# Aggregate functions, as their name suggests perform calculations and return an aggregate result, which we can often see
# an example of this is ds.mean, and with results we can see we can put them inside our own environment
ds.mean('D$WEIGHT', type='split')
ds.mean('D$WEIGHT', type='combine')

split_weight <- ds.mean('D$WEIGHT', type='split')
combined_weight <- ds.mean('D$WEIGHT', type='combine')

# Assign functions are functions that allow us to assign variables in the study machines to help us analyse things further
# for example we'd like to find the average weight of men and women respectively, at the present we cant do that with
# 'D' because it doesnt distinguish sex, but we can subset it.
