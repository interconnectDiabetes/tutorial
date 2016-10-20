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

# gestational age by sex - most of the code is to sort out formatting
summary_ga_temp <- ds.meanByClass(x='E4$GESTATIONAL_AGE~E4$SEX', type = 'split')
summary_ga <- matrix(unlist(summary_ga_temp))
summary_ga <- substr(summary_ga, 1, nchar(summary_ga)-1)
summary_ga <- summary_ga[seq(2, nrow(summary_ga), by = 2),]
summary_ga <- do.call('rbind', strsplit(as.character(summary_ga),'(',fixed=TRUE))
summary_ga <- cbind(summary_ga[seq(1, nrow(summary_ga), by = 2),],summary_ga[seq(2, nrow(summary_ga), by = 2),])
summary_ga <- data.frame(t(apply(summary_ga, 1, as.numeric)))
rownames(summary_ga) <- study_names
colnames(summary_ga) <- c("mean_0", "sd_0", "mean_1", "sd_1")
rm(summary_ga_temp)

#mothers'characteristics
#BMI
mean_bmi <- data.frame(matrix(unlist(ds.mean("E4$MATERNAL_BMI", type = 'split')), nrow = num_studies, ncol = 1, byrow=TRUE))
var_bmi <- data.frame(matrix(unlist(datashield.aggregate(opals, as.symbol("varDS(E4$MATERNAL_BMI)"))), nrow = num_studies, ncol = 1, byrow=TRUE))
summary_bmi <- cbind(mean_bmi,sqrt(var_bmi))
rownames(summary_bmi) <- study_names
colnames(summary_bmi) <- c("mean", "sd")
rm(mean_bmi, var_bmi)


#Age
mean_age <- data.frame(matrix(unlist(ds.mean("E4$MATERNAL_AGE", type = 'split')), nrow = num_studies, ncol = 1, byrow=TRUE))
var_age <- data.frame(matrix(unlist(datashield.aggregate(opals, as.symbol("varDS(E4$MATERNAL_AGE)"))), nrow = num_studies, ncol = 1, byrow=TRUE))
summary_age <- cbind(mean_age,sqrt(var_age))
rownames(summary_age) <- study_names
colnames(summary_age) <- c("mean", "sd")
rm(mean_age, var_age)


# binaries
binary_var <- c('SMOKING', 'GDM')
binary_df <- data.frame()
for (bin in binary_var) {
  summary_temp <- ds.summary(paste0('E4$',bin))
  summary_temp <- data.frame(matrix(unlist(summary_temp), nrow = num_studies, ncol=6, byrow=TRUE))
  rownames(summary_temp) <- paste0(study_names,'_',bin)
  binary_df <- rbind(binary_df, summary_temp)
}
colnames(binary_df) <- c('type', 'n', '0', '1', 'No', 'Yes')
binary_df <- binary_df[,c(5,6)]
rm(summary_temp)

#maternal obesity
summary_ob_temp <- ds.summary('E4$MATERNAL_OB')
summary_ob <- data.frame(matrix(unlist(summary_ob_temp), nrow = num_studies, ncol=8, byrow=TRUE))
rownames(summary_ob) <- study_names
colnames(summary_ob) <- c("type", "N", "0", "1", "2", "Normal", "Overweight", "Obese")
summary_ob <- summary_ob[,c(6,7,8)]
rm(summary_ob_temp)

#---------------------------------------------------------
# Summaries for exposures

#LTPA
summary_ltpa_temp <- ds.summary('E4$LTPA_DUR_3')
summary_ltpa <- data.frame(matrix(unlist(summary_ltpa_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_ltpa) <- study_names
colnames(summary_ltpa) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_ltpa <- summary_ltpa[,c(2,6,5,7)]
rm(summary_ltpa_temp)

# MOD_VIG_3
summary_mod_temp <- ds.summary('E4$MOD_VIG_3')
summary_mod <- data.frame(matrix(unlist(summary_mod_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_mod) <- study_names
colnames(summary_mod) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_mod <- summary_mod[,c(2,6,5,7)]
rm(summary_mod_temp)

# LTPA_EE_3
summary_ee_temp <- ds.summary(x='E4$LTPA_EE_3')
summary_ee <- data.frame(matrix(unlist(summary_ee_temp), nrow = num_studies, ncol=10, byrow=TRUE))
rownames(summary_ee) <- study_names
colnames(summary_ee) <- c("type", "N", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "mean")
summary_ee <- summary_ee[,c(2,6,5,7)]
rm(summary_ee_temp)

#---------------------------------------------------------
# Summaries for outcomes

# Birthweight by sex
summary_bw_temp <- ds.meanByClass(x='E4$BIRTH_WEIGHT~E4$SEX', type = 'split')
summary_bw <- matrix(unlist(summary_bw_temp))
summary_bw <- substr(summary_bw, 1, nchar(summary_bw)-1)
summary_bw <- summary_bw[seq(2, nrow(summary_bw), by = 2),]
summary_bw <- do.call('rbind', strsplit(as.character(summary_bw),'(',fixed=TRUE))
summary_bw <- cbind(summary_bw[seq(1, nrow(summary_bw), by = 2),],summary_bw[seq(2, nrow(summary_bw), by = 2),])
summary_bw <- data.frame(t(apply(summary_bw, 1, as.numeric)))
rownames(summary_bw) <- study_names
colnames(summary_bw) <- c("mean_male", "sd_male", "mean_female", "sd_female")
rm(summary_bw_temp)

# Macrosomia
summary_mac_temp <- ds.summary('E4$MACROSOMIA')
summary_mac <- data.frame(matrix(unlist(summary_mac_temp), nrow = num_studies, ncol=6, byrow=TRUE))
rownames(summary_mac) <- study_names
summary_mac <- summary_mac[,c(1,2,5,6)]
colnames(summary_mac) <- c("class", "length", "No", "Yes")
rm(summary_mac_temp)

# BIRTHWEIGHT_LGA
summary_lga_temp <- ds.summary('E4$BIRTH_WEIGHT_LGA')
summary_lga <- data.frame(matrix(unlist(summary_lga_temp), nrow = num_studies, ncol=6, byrow=TRUE))
rownames(summary_lga) <- study_names
summary_lga <- summary_lga[,c(1,2,5,6)]
colnames(summary_lga) <- c("class", "length", "No", "Yes")
rm(summary_lga_temp)


#--------------- FUNCTIONS TO HELP WITH REGRESSIONS AND REM ------------------#
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

do_REM <- function(coeffs, s_err, labels, fmla, out_family, variable){
  
  res <- rma(yi = coeffs, sei = s_err, method='DL', slab = labels)
  
  #forest plots
  
  if (out_family == 'gaussian') {
    usr <- par("usr")
    forest(res, mlab=bquote(paste('Overall (I'^2*' = ', .(round(res$I2)),'%, p = ',
                                  .(round(res$QEp,3)),')')),
           xlab=bquote(paste('Test of H'[0]*': true mean association = 0, p = ',
                             .(round(res$pval,3)))), ilab = cbind(weights.rma.uni(res)),ilab.xpos = c(usr[1]))
    text(usr[2], usr[4], "Beta [95% CI]", adj = c(1, 8))
    #text(usr[1], usr[4], gsub(paste0(ref_table,"\\$"),"", Reduce(paste, deparse(fmla))), adj = c( 0, 8 ))
    text(usr[1], usr[4], paste0(gsub(paste0(ref_table,"\\$"),"", deparse(fmla)),collapse="\n"), adj = c( 0, 1 ))
    text(usr[1], usr[3], variable, adj = c( 0, 0 ))
  }
  else if (out_family == 'binomial'){
    usr <- par("usr")
    forest(res, digits=3, mlab=bquote(paste('Overall (I'^2*' = ', .(round(res$I2)),'%, p = ',
                                            .(round(res$QEp,3)),')')),
           xlab=bquote(paste('Test of H'[0]*': true relative risk = 1, p = ',
                             .(round(res$pval,3)))),ilab = cbind(weights.rma.uni(res)),ilab.xpos = c(usr[1]), atransf = exp)
    text(usr[2], usr[4], "Relative Risk [95% CI]", adj = c(1, 8))
    #text(usr[1], usr[4], gsub(paste0(ref_table,"\\$"),"", Reduce(paste, deparse(fmla))), adj = c( 0, 8 ))
    text(usr[1], usr[4], paste0(gsub(paste0(ref_table,"\\$"),"", deparse(fmla)),collapse="\n"), adj = c( 0, 1 ))
    text(usr[1], usr[3], variable, adj = c( 0, 0))
  }
  
  return(res)
}
