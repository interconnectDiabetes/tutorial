## Welcome to your advanced datashield task of investigating the confounding variables in DNBC

library(opal)
library(dsBaseClient)
library(dsStatsClient)
library(dsGraphicsClient)
library(dsModellingClient)
library(metafor)

server <- c('DNBC')
url <- c('https://193.163.131.62:8443')
table <- c('DNBC_InterConnect.DNBC_harm')
password <- c( 'datashield-test-privatekey.pem')
user <- c('datashield-test-publickey.pem')
logindata_all <- data.frame(server,url,user,password, table)

datashield.logout(opals)	# lets log out in case you forgot
opals <- datashield.login(logins=logindata_all, assign=TRUE)