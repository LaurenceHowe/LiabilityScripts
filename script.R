##1) Population models

##Body size at age 10 

normal <- lm(Bodysize_Tweak ~ GRS_SD + Age + Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data= final)
summary(normal)$coefficients[2, 1]
confint(normal, "GRS_SD")

#Glasses at age 15

normal <- glm(Glasses15 ~ GRS_SD + Age + Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data= final, family = "binomial")
exp(summary(normal)$coefficients[2, 1])
exp(confint(normal, "GRS_SD"))

#Smoking at age 15

normal <- glm(Smk15 ~ GRS_SD + Age + Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data= final, family = "binomial")
exp(summary(normal)$coefficients[2, 1])
exp(confint(normal, "GRS_SD"))

##2) Within-sibship models

phenotypes <- c("Glasses", "Smoking")


output <- data.frame(PHEN=phenotypes, OBS_BETA_TOTAL=NA,  OBS_BETA_WF=NA,
                      OBS_SE_BETA_TOTAL=NA,  OBS_SE_BETA_WF=NA,
                      OBS_P_BETA_TOTAL=NA, OBS_P_BETA_WF=NA)



for (i in 1:2) {

# Run unified regression
if (i == 1) 
{
merge <- data.table(FID= analysis$FID, OUTCOME=analysis$Glasses15,  EXPOSURE=analysis$GRS_SD,
 FAM_MEAN=analysis$FAM_MEAN, Sex = analysis$Sex, Age = analysis$Age)

merge2 <- merge[,CENTREDEXPOSURE:=EXPOSURE-FAM_MEAN]

fit1 <- glm(formula = OUTCOME ~ EXPOSURE + Sex + Age, data = merge2, family = "binomial")
fit2 <- glm(formula = OUTCOME ~ FAM_MEAN + CENTREDEXPOSURE + Sex + Age, data = merge2, family = "binomial")
}

if (i == 2) 
{
merge <- data.table(FID= analysis$FID, OUTCOME=analysis$Smk15,  EXPOSURE=analysis$GRS_SD,
 FAM_MEAN=analysis$FAM_MEAN, Sex = analysis$Sex, Age = analysis$Age)

merge2 <- merge[,CENTREDEXPOSURE:=EXPOSURE-FAM_MEAN]

fit1 <- glm(formula = OUTCOME ~ EXPOSURE + Sex + Age, data = merge2, family = "binomial")
fit2 <- glm(formula = OUTCOME ~ FAM_MEAN + CENTREDEXPOSURE + Sex + Age, data = merge2, family = "binomial")
}



# Save Beta information

output$OBS_BETA_TOTAL[i] <- fit1$coefficients[2]
output$OBS_BETA_WF[i] <- fit2$coefficients[3]

# save the variance covariance matrix
vcv_matrix1 <- vcovCL(fit1, cluster=merge2$FID)
vcv_matrix2 <- vcovCL(fit2, cluster=merge2$FID)


#Derive the clustered SEs for the total effect and P-values

	test_matrix1 <- coeftest(fit1, vcov.=vcv_matrix1)
	test_matrix2 <- coeftest(fit2, vcov.=vcv_matrix2)
	
    output$OBS_SE_BETA_TOTAL[i] <- test_matrix1[2,2]
    output$OBS_P_BETA_TOTAL[i] <- test_matrix1[2,4]
		output$OBS_SE_BETA_WF[i] <- test_matrix2[3,2]
    output$OBS_P_BETA_WF[i] <- test_matrix2[3,4]
	
}
