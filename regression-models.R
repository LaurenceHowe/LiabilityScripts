##1) Genetic analysis - Population models

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

##2) Genetic analysis - Within-sibship models

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

##3) ROSLA - latent analysis

#Defining year before/after reform using year of birth (YoB) , month of birth (MoB) and country of birth (CoB) (to restrict to England/Wales)
YBefore <- data2[which(data2$YoB == "1956" & data2$MoB > 8 & data2$CoB < 3 & data2$CoB > 0 | data2$YoB == "1957" & data2$MoB < 9 & data2$CoB < 3 & data2$CoB > 0), ]
YAfter <- data2[which(data2$YoB == 1957 & data2$MoB > 8 & data2$CoB < 3 & data2$CoB > 0| data2$YoB == 1958 & data2$MoB < 9 & data2$CoB < 3 & data2$CoB > 0), ]


#16 year old leavers from year before reform
Set1 <- YBefore[which(YBefore$Education == 16), ]
Set1$Pre <- 0

#16 year old leavers from year after reform
Set2 <- YAfter[which(YAfter$Education == 16), ]
Set2$Pre <- 1

Set3 <- rbind(Set1, Set2)


#BMI at study baseline
model1 <- lm (BMI ~ Pre + Sex, data = Set3)

#SBP at study baseline
model2 <- lm (SBP ~ Pre + Sex, data = Set3)

#Townsend DI at study baseline
model3 <- lm (TDI ~ Pre + Sex, data = Set3)

#Binary measure of income at study baseline
model4 <- glm (Income ~ Pre + Sex, data = Set3, family = 'binomial')

#Pack years at study baseline
model5 <- lm (PackYears ~ Pre + Sex, data = Set3)

#Glasses (or contacts) use at study baseline
model6 <- glm (Glasses ~ Pre + Sex, data = merge, family = 'binomial')

#Education polygenic score
model7 <- lm (GRS_SD ~ Pre + Sex, data = merge)

