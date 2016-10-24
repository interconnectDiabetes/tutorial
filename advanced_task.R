## Welcome to your advanced datashield task of investigating the confounding variables in DNBC

setwd('/home/shared/certificates/pa')

library(opal)
library(dsBaseClient)
library(dsStatsClient)
library(dsGraphicsClient)
library(dsModellingClient)
library(metafor)

server <- c('DNBC')
url <- c('https://193.163.131.62:8443')
table <- c('DNBC_InterConnect.DNBC_harm')
password <- c('datashield-test-privatekey.pem')
user <- c('datashield-test-publickey.pem')
logindata_all <- data.frame(server,url,user,password, table)

datashield.logout(opals)	# lets log out in case you forgot
opals <- datashield.login(logins=logindata_all, assign=TRUE)



##########################################################################################
########################### Data Setup and Warm up  ######################################
##########################################################################################
# basic counts
all_infants <- ds.length('D$SEX', type = 'split')

# Make a new subset D1, with no preterm babies
ds.subset(x = 'D', subset = 'D1', logicalOperator = 'GESTATIONAL_AGE>=', threshold = 37) 
no_preterm <- ds.length('D1$SEX', type = 'split')

# Now one without preeclampsia
ds.subset(x = 'D1', subset = 'D2', logicalOperator = 'PREECLAMPSIA==', threshold = 0)
no_preecl <- ds.length('D2$SEX', type = 'split')


# preprocessing to circumvent a small bug in datashield (dont worry about this bit)
for(i in 1:length(opals)){
  work1 <- no_preecl[[i]]
  work2 <- paste0("datashield.assign(opals[",i,"],'temp', quote(c(1:",work1,")))")
  eval(parse(text=work2))
}
ds.cbind(x=c('temp','D2'), newobj='D2a')

# code for filtering out missings
temp <- ds.summary('D$SEX')
num_studies <- length(temp)
study_names <- names(temp)
rm(temp)

# Here we set out the variables we want to work with defined clearly in their groups
my_exp_all = c('MOD_VIG_3', 'LTPA_DUR_3', 'LTPA_EE_3')
my_outcome_all = c('BIRTH_WEIGHT', 'MACROSOMIA', 'BIRTH_WEIGHT_LGA')
my_cov_all = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
               'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY', 'GDM', 'MATERNAL_BMI', 'MATERNAL_OB')

my_vars_all <- c(my_exp_all, my_outcome_all, my_cov_all)
ds.subset(x = 'D2a', subset = 'E3', cols =  my_vars_all)
ds.subset(x = 'E3', subset = 'E4', completeCases = TRUE)


# OPTIONAL
###############################################################################
########################### RUN SUMMARIES  ####################################
###############################################################################

#---------------------------------------------------------
# Summaries for covariates and confounders

summary_sex_temp <- ds.summary('E4$SEX')
summary_sex <- data.frame(matrix(unlist(summary_sex_temp), nrow = num_studies, ncol=6, byrow=TRUE))
rownames(summary_sex) <- study_names
colnames(summary_sex) <- c("type", "N", "male", "female", "count0", "count1")
rm(summary_sex_temp)

#mothers'characteristics
#BMI
mean_bmi <- data.frame(matrix(unlist(ds.mean("E4$MATERNAL_BMI", type = 'split')), nrow = num_studies, ncol = 1, byrow=TRUE))
var_bmi <- data.frame(matrix(unlist(datashield.aggregate(opals, as.symbol("varDS(E4$MATERNAL_BMI)"))), nrow = num_studies, ncol = 1, byrow=TRUE))
summary_bmi <- cbind(mean_bmi,sqrt(var_bmi))
rownames(summary_bmi) <- study_names
colnames(summary_bmi) <- c("mean", "sd")
rm(mean_bmi, var_bmi)

# if you can figure out how this works its really nice helper for regression, how this works wont be immediately clear
# so we press on
do_reg <- function(my_fmla, study, outcome, out_family){
  
  model <- ds.glm(formula= my_fmla, data = ref_table, family = out_family, datasources=opals[i], maxit = 100)
  model_coeffs <- as.data.frame(model$coefficients)
  model_coeffs$study = study
  model_coeffs$outcome = outcome
  model_coeffs$cov = rownames(model_coeffs)
  for (x in 1:3){ model_coeffs <- model_coeffs[,c(ncol(model_coeffs),1:(ncol(model_coeffs)-1))]}
  rownames(model_coeffs) = NULL
  return(model_coeffs)
}


#  /'\_/`\            /\ \        /\_ \       /' \
# /\      \    ___    \_\ \     __\//\ \     /\_, \
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \    \/_/\ \
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_     \ \ \
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\     \ \_\
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/      \/_/

my_exposure = c('MOD_VIG_3', 'LTPA_DUR_3', 'LTPA_EE_3')
my_outcome = c( 'BIRTH_WEIGHT','MACROSOMIA','BIRTH_WEIGHT_LGA')
my_covariate = c('GESTATIONAL_AGE', 'SEX')

ref_table = 'E4'

# normal linear regressions made of formulas, data to act on, an error distribution function
# linear regressions in Data shield not much different for the user they are also are made of formulas, data to act on, 
# an error distribution function, plus the machine it came from 
# fitted_model <- ds.glm(formula= my_fmla, data = ref_table, family = out_family, datasources=opals[i], maxit = 100)
# the maxit just tells the maximum number of iterations we allow the fitting process to repeat itself.

# lets make sure that our linear regression is done on the right family type, ie categorical values will need a binomial distribution, etc
out_class = ds.class('E4$SOMEOUTCOMEGOODNESS')[[1]]
if (out_class == 'factor'){
	outcome_family = 'binomial'
} else if (out_class == 'numeric' | out_class == 'integer') {
	outcome_family = 'gaussian'
}



#  /'\_/`\            /\ \        /\_ \        /'___`\
# /\      \    ___    \_\ \     __\//\ \      /\_\ /\ \
# \ \ \__\ \  / __`\  /'_` \  /'__`\\ \ \     \/_/// /__
#  \ \ \_/\ \/\ \L\ \/\ \L\ \/\  __/ \_\ \_      // /_\ \
#   \ \_\\ \_\ \____/\ \___,_\ \____\/\____\    /\______/
#    \/_/ \/_/\/___/  \/__,_ /\/____/\/____/    \/_____/

my_exposure = c('MOD_VIG_3', 'LTPA_DUR_3','LTPA_EE_3')
my_outcome = c('BIRTH_WEIGHT','MACROSOMIA','BIRTH_WEIGHT_LGA')
my_covariate = c('GESTATIONAL_AGE', 'SEX', 'PARITY', 'MATERNAL_AGE', 'SMOKING',
                 'ALCOHOL', 'MATERNAL_EDU', 'ETHNICITY')

ref_table = 'E4'