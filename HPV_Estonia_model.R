library(data.table)
library(logbin)

dat1 <- as.data.table(openxlsx::read.xlsx("/data/HPVTests/P07_2021-07-07_3065GD_HPV_proovikogumiseInfo_20210630_Tammesoo_1.0_KRS.xlsx"))
dat2 <- as.data.table(openxlsx::read.xlsx("/data/HPVTests/P07_2021-07-12_3065GD_HPV_Koodiseosed_1.0_KRS.xlsx"))
dat3 <- as.data.table(openxlsx::read.xlsx("/data/HPVTests/P07_2021-08-15_3065GD_HPV_proovivastused_tammesoo_2.0_KRS.xlsx",sheet=2))
dat4 <- as.data.table(openxlsx::read.xlsx("/data/HPVTests/P07_2021-07-22_1422GD_HPV-kõik_A_Puusepp_2.0_KRS.xlsx"))
dat_smoke <- as.data.table(openxlsx::read.xlsx("/data/HPVTests/P07_2021-04-15_2155GD_alcohol_smoking_2276r_20210415_SL_1.0_KRS.xlsx"))
dat_fem_health <- as.data.table(openxlsx::read.xlsx("/data/HPVTests/P07_2021-04-15_2014GD_female_health_2128r_20210407_SL_1.0_KRS.xlsx"))
dat_partners <- as.data.table(openxlsx::read.xlsx("/data/HPVTests/P07_2021-07-22_1422GD_HPV-koik_C_Puusepp_2.0_KRS.xlsx"))

#Add the PRSs
prs <- fread("/data/cancerResults/CervicalCancer.prs")
prs_meta <- fread("/data/HPVTests/HPV_individuals_best_ldpred_score.txt")


#Merge all of the results together
dat4 <- dat4[,c(1,7,10,12,13,15,18,20)]
names(dat4) <- c("SKOOD","Nationality","MaritalStatus","Education","SchoolYears","Employment","Income","Condition")
dat4[Nationality %in% c("eestlane","Eestlane"),Nationality:="Estonian"]
dat4[Nationality !="Estonian" ,Nationality:="notEstonian"]

dat4[MaritalStatus =="Abielus",MaritalStatus:="Married"]
dat4[MaritalStatus =="Lahutatud / elan abikaasast lahus",MaritalStatus:="Divorced"]
dat4[MaritalStatus =="Lesk",MaritalStatus:="Widow"]
dat4[MaritalStatus =="Vabaabielu/elan koos kindla partneriga",MaritalStatus:="Partnered"]
dat4[MaritalStatus =="Vallaline",MaritalStatus:="Single"]

dat4[grepl("rgharidus",Education),isEducation:="Higher"]
dat4[is.na(isEducation),isEducation:="Lower"]
dat4[is.na(Education),isEducation:=NA]

dat4[,Education:=NULL]

dat4[,SchoolYears:=as.numeric(SchoolYears)]
dat4[Condition=="Saan/saame praeguse sissetulekuga hakkama",Condition:="Average"]
dat4[grepl("Elan/elame praeguse sissetulekuga lahedasti",Condition),Condition:="Good"]
dat4[!(Condition %in% c("Average","Good")),Condition:="Bad"]

#Take the smoking variable
dat_smoke_use <- dat_smoke[,list(SKOOD,answerset_date,smoking_status,has_smoked,has_smoked_last12_months)]
dat_smoke_use <- dat_smoke_use[order(SKOOD,answerset_date,decreasing=T),]
dat_smoke_use <- dat_smoke_use[!duplicated(SKOOD),]

dat_smoke_use[smoking_status == 1 | smoking_status ==4 &has_smoked==F,smoking:="Never"]
dat_smoke_use[smoking_status == 2 | smoking_status ==4 &has_smoked_last12_months==F,smoking:="Former"]
dat_smoke_use[smoking_status == 3 | smoking_status ==4 &has_smoked_last12_months==T,smoking:="Current"]
dat_smoke_use <- dat_smoke_use[,list(SKOOD,smoking)]

#Take the female health
dat_fem_health_use <- dat_fem_health[,list(SKOOD,answerset_date,has_been_pregnant,num_of_live_births,num_of_stillbirths,num_of_abortions,has_used_hormonal_contraceptives)]
dat_fem_health_use <- dat_fem_health_use[order(SKOOD,answerset_date,decreasing=T),]
dat_fem_health_use <- dat_fem_health_use[!duplicated(SKOOD),]

dat_fem_health_use[is.na(num_of_live_births) & has_been_pregnant ==F,num_of_live_births:=0]
dat_fem_health_use[is.na(num_of_stillbirths) & has_been_pregnant ==F,num_of_stillbirths:=0]
dat_fem_health_use[,Parity:=num_of_stillbirths + num_of_live_births]
#if miscarriages were not available use simply live births
dat_fem_health_use[is.na(Parity) & !is.na(num_of_live_births), Parity:= num_of_live_births]
dat_fem_health_use[is.na(num_of_abortions) & has_been_pregnant ==F,num_of_abortions:=0]
dat_fem_health_use[Parity==0,ParityClass:="0"]
dat_fem_health_use[Parity==1 | Parity ==2,ParityClass:="1-2"]
dat_fem_health_use[Parity >= 3,ParityClass:="3+"]

dat_fem_health_use <- dat_fem_health_use[,list(SKOOD,ParityClass,Parity,num_of_abortions,has_used_hormonal_contraceptives)]


#Sexual behaviour
dat_partners_use <- dat_partners[,list(SKOOD,ChildrenSexualBehavior.firstIntercourseAge,ChildrenSexualBehavior.partnersTotal)]
dat_partners_use[,firstIntercourseAge:=as.numeric(ChildrenSexualBehavior.firstIntercourseAge)]
dat_partners_use[,partnersTotal:=as.numeric(ChildrenSexualBehavior.partnersTotal)]
dat_partners_use <- dat_partners_use[,list(SKOOD,firstIntercourseAge,partnersTotal)]

dat_use <- merge(dat1,dat2,by="SKOOD",all.x=T)
dat_use <- merge(dat_use,dat3,by="SKOOD",all.x=T,all.y=T)


#Let's assemble the important variables
dat_use <- dat_use[,list(SKOOD,VKOOD,Age,rahvus,MONIT.Maakond,MONIT.proovi.tulemus.y)]
dat_use[Age <=33,ageGroup:="30-33"]
dat_use[Age <=60 & Age >= 57,ageGroup:="57-60"]
dat_use[Age <=70 & Age >= 67,ageGroup:="67-70"]

dat_use[Age <=33,ageGroup2:="30-33"]
dat_use[Age >= 57,ageGroup2:="57+"]


dat_use[rahvus %in% c("EESTI","Eestlane"),nationality:="Estonian"]
dat_use[rahvus %in% c("VENE","Venelane"),nationality:="Russian"]
dat_use[,c("rahvus","Age"):=NULL]


setnames(dat_use,c( "SKOOD", "VCode", "county", "statusHPV" ,"ageGroup","ageGroup2","nationality"))
#County should be a factor
dat_use[,county:=as.factor(county)]
dat_use[,ageGroup:=as.factor(ageGroup)]
dat_use[,ageGroup2:=as.factor(ageGroup2)]

dat_use[,nationality:=as.factor(nationality)]

#merge with the new file from Anna
dat_use <- merge(dat_use,dat4,by.x="SKOOD",by.y="SKOOD",all.x=T,all.y=T)
#Keep only those who have test result
dat_use <- dat_use[!is.na(statusHPV),]

#Fix some nationalities
dat_use[is.na(Nationality) & nationality=="Estonian",Nationality:="Estonian"]
dat_use[is.na(Nationality) & nationality=="Russian",Nationality:="notEstonian"]
dat_use[,nationality:=NULL]

#add the smoking, partners, parity, abortions...
dat_use2 <- merge(dat_use,dat_smoke_use,by="SKOOD",all.x=T)
dat_use2 <- merge(dat_use2,dat_fem_health_use,by="SKOOD",all.x=T)
dat_use2 <- merge(dat_use2,dat_partners_use,by="SKOOD",all.x=T)

#Merge with prs file
dat_use2 <- merge(dat_use2,prs,by="VCode",all.x=T)
dat_use2 <- merge(dat_use2,prs_meta[,list(SKOOD,best_ldpred_score)],by="SKOOD",all.x=T)

dat_use2[,bWICD10:=scale(bWICD10)]
dat_use2[,bRICD10:=scale(bRICD10)]
dat_use2[,best_ldpred_score:=scale(best_ldpred_score)]

#Analysis
andat1 <- copy(dat_use2)
andat1 <- andat1[!is.na(statusHPV),]
#andat1[,c("marginalEst","marginalEst_UK_Kaiser"):=NULL]
#andat2 <- dat_use[,list(statusHPV,county,ageGroup,bRICD10)]

#relevel
andat1[, MaritalStatus:=factor(MaritalStatus, levels = c("Married", "Widow", "Partnered", "Divorced", "Single"))] 
andat1[, Condition:=factor(Condition, levels = c("Bad", "Average", "Good"))] 
andat1[smoking == "Never",smokingNew:="No"]
andat1[smoking != "Never",smokingNew:="Yes"]

andat2 <- copy(andat1)
andat2 <- andat2[!is.na(bRICD10),]

library(mice)
#1. Version with impute all

#imp <- mice(andat1[!is.na(statusHPV),], seed = 123, print = FALSE,m=10)

imp <- mice(andat2, seed = 123, print = FALSE,m=10)

fit_bRAll <- with(imp, glm(statusHPV ~ bRICD10  + MaritalStatus + Condition + partnersTotal + isEducation
                         ,family = binomial))
est_bRAll <- pool(fit_bRAll)
A_bRAll <- as.data.table(summary(est_bRAll))

fit_ldpredAll <- with(imp, glm(statusHPV ~ best_ldpred_score  + MaritalStatus + Condition + partnersTotal + isEducation
                       ,family = binomial))
est_ldpredAll <- pool(fit_ldpredAll)
A_ldpredAll <- as.data.table(summary(est_ldpredAll))

###

fit_bR <- with(imp, glm(statusHPV ~ bRICD10 ,family = binomial))
est_bR <- pool(fit_bR)
A_bR <- as.data.table(summary(est_bR))

fit_ldpred <- with(imp, glm(statusHPV ~ best_ldpred_score ,family = binomial))
est_ldpred <- pool(fit_ldpred)
A_ldpred <- as.data.table(summary(est_ldpred))






A <- as.data.table(summary(est1))


A[,OR:=round(exp(estimate),2)]
A[,OR_lower:=round(exp(estimate-qnorm(0.975)*std.error),2)]
A[,OR_upper:=round(exp(estimate+qnorm(0.975)*std.error),2)]
A[,`p-value`:=as.character(round(p.value,4))]
A[,`95% CI`:=paste0("(",OR_lower,",",OR_upper,")")]
A[`p-value` =="0",`p-value`:="<0.0001"]

printRes <- A[,list(term,OR,`95% CI`,`p-value`)]
printRes <- printRes[-1,]
baseLines <- data.table(term=c("ageGroup30-33","NationalityEstonian","isEducationHigher","MaritalStatusMarried","ConditionBad"), #smokingCurrent
                           OR=c(1,1,1,1,1,1),
                           `95% CI`=c(NA,NA,NA,NA,NA,NA),
                           `p-value`=c(NA,NA,NA,NA,NA,NA))
printRes <- rbind(printRes,baseLines)
printRes[,distFrom1:=abs(OR-1)]
printRes[,prefix:=substr(term,0,3)]
printRes <- printRes[order(as.character(prefix),distFrom1),]
printRes[OR==1,termTmp:=term] 
printRes[,term:=gsub("Condition","",term)]
printRes[,term:=gsub("MaritalStatus","",term)]
printRes[,term:=gsub("Nationality","",term)]
printRes[,term:=gsub("ageGroup","",term)]
printRes[,term:=gsub("bWICD10","Genetic risk",term)]
printRes[,term:=gsub("isEducation","",term)]
printRes[,term:=gsub("smoking","",term)]

for(n in 1:nrow(printRes)){
  printRes[n,termTmp:=gsub(term,"",termTmp)]
}
printRes[termTmp=="isEducation",termTmp:="Education"]
printRes[is.na(termTmp),termTmp:=""]
printRes[OR==1,`Variable (baseline)`:=paste0(termTmp," (",term,")")]
printRes[OR==1,term:=""]
printRes[term=="Genetic risk",`Variable (baseline)`:=term]
printRes[term=="Genetic risk",term:=""]

print(xtable(printRes[,list(`Variable (baseline)`,term,OR,`95% CI`,`p-value`)]),include.rownames=F)



#unadjusted analysis
imp <- mice(andat1[!is.na(statusHPV),], seed = 123, print = FALSE,m=10)
fit <- with(imp, glm(statusHPV ~ bRICD10,family = binomial))
est1 <- pool(fit)
A <- as.data.table(summary(est1))

A[,OR:=round(exp(estimate),2)]
A[,OR_lower:=round(exp(estimate-qnorm(0.975)*std.error),2)]
A[,OR_upper:=round(exp(estimate+qnorm(0.975)*std.error),2)]
A[,`p-value`:=as.character(round(p.value,4))]
A[,`95% CI`:=paste0("(",OR_lower,",",OR_upper,")")]
A[`p-value` =="0",`p-value`:="<0.0001"]

printRes_bR <- A[,list(term,OR,`95% CI`,`p-value`)]
printRes_bR <- printRes_bR[-1,]


imp <- mice(andat1[!is.na(statusHPV),], seed = 123, print = FALSE,m=10)
fit <- with(imp, glm(statusHPV ~ bWICD10,family = binomial))
est1 <- pool(fit)
A <- as.data.table(summary(est1))

A[,OR:=round(exp(estimate),2)]
A[,OR_lower:=round(exp(estimate-qnorm(0.975)*std.error),2)]
A[,OR_upper:=round(exp(estimate+qnorm(0.975)*std.error),2)]
A[,`p-value`:=as.character(round(p.value,4))]
A[,`95% CI`:=paste0("(",OR_lower,",",OR_upper,")")]
A[`p-value` =="0",`p-value`:="<0.0001"]

printRes_bW <- A[,list(term,OR,`95% CI`,`p-value`)]
printRes_bW <- printRes_bW[-1,]

printRes <- rbind(printRes_bR,printRes_bW)
print(xtable(printRes[,list(term,OR,`95% CI`,`p-value`)]),include.rownames=F)


#####
library(mice)
imp <- mice(andat1[!is.na(statusHPV),], seed = 123, print = FALSE,m=10)
fit <- with(imp, lm(statusHPV ~ ageGroup + bRICD10 + Nationality +isEducation +MaritalStatus+Condition))
est1 <- pool(fit)
A <- as.data.table(summary(est1))
dat_samp1 <- complete(imp,1)

a <- lm(statusHPV ~ ageGroup + bRICD10 + Nationality +isEducation +MaritalStatus+Condition,data=dat_samp1)
mm <- model.matrix(a)
mm <- scale(mm)
