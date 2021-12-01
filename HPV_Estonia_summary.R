rm(list=ls())

library(data.table)


dat <- fread("/Users/admin/Downloads/Svenile.csv")

#Take only individuals who belong to the study
dat <- dat[proovolemas =="jah" & küsimustikolemas == "jah",]

table(dat$HPV16)
#High risk HPB
dat[,hrHPV:=HPV16=="jah" | HPV18=="jah" | HPV31=="jah" | HPV33=="jah"| HPV45=="jah" | HPV52=="jah"|
      HPV58=="jah" | HPV35=="jah" | HPV39=="jah" | HPV51=="jah" | HPV56=="jah" | HPV59=="jah"| HPV68=="jah"]


dat_use <- dat[,list(SKOOD,hrHPV,birthYear,maritalStatuscode,educationcode,firstIntercourseAge,firstIntercourseContraceptives4,
                     pregnanciesTotal,gettingPregnantAge1,pregnancyEndcode1,partnersTotal,hasVeneralDisease,
                     lastReproductiveHealthExamcode,awarenessOfPapTestcode,awarenessOfOwnPapTestIntervalc)]


#Separate the numeric and the categorical
dat_use_num <- dat_use[,list(SKOOD,hrHPV,birthYear,firstIntercourseAge,pregnanciesTotal,gettingPregnantAge1,partnersTotal)]

dat_use_num_long <- melt(dat_use_num,id.vars=c("SKOOD","hrHPV"))
dat_use_num_long[,hrHPV:=as.character(hrHPV)]


variables <- unique(dat_use_num_long$variable)
pVals <- NULL
for(var_use in variables){
  xx <- dat_use_num_long[variable==var_use & hrHPV ==T]$value
  yy <- dat_use_num_long[variable==var_use & hrHPV ==F]$value
  pVals <- c(pVals,wilcox.test(xx,yy)$p.value)
}
pvalDat <- data.table(variable=variables,p=round(pVals,3))

dat_use_num_long_extra <- copy(dat_use_num_long)
dat_use_num_long_extra[,hrHPV:="Total"]
dat_use_num_long <- rbind(dat_use_num_long,dat_use_num_long_extra)
dat_use_num_long[,med:=median(value,na.rm=T),by=c("hrHPV","variable")]
dat_use_num_long[,q25:=quantile(value,probs=0.25,na.rm=T),by=c("hrHPV","variable")]
dat_use_num_long[,q75:=quantile(value,probs=0.75,na.rm=T),by=c("hrHPV","variable")]
dat_use_num_long[!is.na(value),n:=.N,by=c("hrHPV","variable")]
dat_use_num_long <- dat_use_num_long[!is.na(n),]
dat_use_num_unique <- unique(dat_use_num_long[,list(hrHPV,variable,med,q25,q75,n)])

dat_use_num_unique[,IQR:=paste0(med," (",q25,"-",q75,")")]
dat_use_num_unique[,c("q25","q75"):=NULL]

dat_use_num_cast <- dcast(dat_use_num_unique,variable~hrHPV,value.var = c("n","IQR"))

dat_use_num_cast <- merge(dat_use_num_cast,pvalDat,by="variable",all.x=T)
headRow <- as.data.table(t(colnames(dat_use_num_cast)))
names(headRow) <- colnames(dat_use_num_cast)

dat_use_num_cast <- rbind(headRow,dat_use_num_cast)
dat_use_num_cast[variable =="variable",variable:=""]
dat_use_num_cast[variable =="birthYear",variable:="Birth year, median (IQR)"]
dat_use_num_cast[variable =="firstIntercourseAge",variable:="Age of first intercourse, median (IQR)"]
dat_use_num_cast[variable =="pregnanciesTotal",variable:="Number of pregnancies, median (IQR)"]
dat_use_num_cast[variable =="gettingPregnantAge1",variable:="Age at pregnancy, median (IQR)"]
dat_use_num_cast[variable =="partnersTotal",variable:="Number of partners, median (IQR)"]
dat_use_num_cast <- dat_use_num_cast[,list(variable,n_TRUE,IQR_TRUE,n_FALSE,IQR_FALSE,n_Total,IQR_Total,p)]
dat_use_num_cast[n_FALSE =="n_FALSE", n_FALSE:="n"]
dat_use_num_cast[n_TRUE =="n_TRUE", n_TRUE:="n"]
dat_use_num_cast[n_Total =="n_Total", n_Total:="n"]
dat_use_num_cast[IQR_FALSE =="IQR_FALSE", IQR_FALSE:=""]
dat_use_num_cast[IQR_TRUE =="IQR_TRUE", IQR_TRUE:=""]
dat_use_num_cast[IQR_Total =="IQR_Total", IQR_Total:=""]
dat_use_num_cast[p=="0",p:="$<0.001$"]
names(dat_use_num_cast) <- c("","","hrHPV-positive","","hrHPV-negative","","hrHPV-total","")

#Calculate percentages for categorical variables
dat_use_cat <- dat_use[,list(SKOOD,hrHPV,maritalStatuscode,educationcode,firstIntercourseContraceptives4,pregnancyEndcode1,hasVeneralDisease,lastReproductiveHealthExamcode,awarenessOfPapTestcode,awarenessOfOwnPapTestIntervalc)]
dat_use_cat[dat_use_cat == ""] <- NA #Create NAs
#Translate variables
dat_use_cat[maritalStatuscode=="Abielus",maritalStatuscode:="Married"]
dat_use_cat[maritalStatuscode=="Lahutatud / elan abikaasast lahus",maritalStatuscode:="Divorced"]
dat_use_cat[maritalStatuscode=="Lesk",maritalStatuscode:="Widowed"]
dat_use_cat[maritalStatuscode=="Vabaabielu/elan koos kindla partneriga",maritalStatuscode:="Partnered"]
dat_use_cat[maritalStatuscode=="Vallaline",maritalStatuscode:="Single"]

dat_use_cat[educationcode=="Põhiharidus (7–9 klassi)",educationcode:="Primary school"]
dat_use_cat[educationcode %in% c("Keskeriharidus","Keskharidus (10–12 klassi)"),educationcode:="Secondary school"]
dat_use_cat[educationcode=="Kõrgharidus",educationcode:="University"]

dat_use_cat[firstIntercourseContraceptives4 %in% c("Ei oska öelda","Ei soovi vastata"),firstIntercourseContraceptives4:=NA]
dat_use_cat[firstIntercourseContraceptives4=="Ei kasutanud midagi",firstIntercourseContraceptives4:="Nothing"]
dat_use_cat[firstIntercourseContraceptives4=="Hormonaalne/emakasisene/hädaabitablett",firstIntercourseContraceptives4:="Hormonal/Intrauterine/Emergency pill"]
dat_use_cat[firstIntercourseContraceptives4=="Kondoomi",firstIntercourseContraceptives4:="Condom"]
dat_use_cat[firstIntercourseContraceptives4=="Muud vahendit/meetodit",firstIntercourseContraceptives4:="Other"]

dat_use_cat[pregnancyEndcode1=="Jah",pregnancyEndcode1:="Yes"]
dat_use_cat[pregnancyEndcode1=="Ei",pregnancyEndcode1:="No"]

dat_use_cat[hasVeneralDisease=="Jah",hasVeneralDisease:="Yes"]
dat_use_cat[hasVeneralDisease=="Ei",hasVeneralDisease:="No"]

dat_use_cat[lastReproductiveHealthExamcode %in% c("Ei tea","Ma pole kunagi läbinud günekoloogilist läbivaatust ega teinud teste günekoloogilise või reproduktiivtervise kohta"),lastReproductiveHealthExamcode:="Never or did not know"]
dat_use_cat[lastReproductiveHealthExamcode=="Üks aasta või vähem",lastReproductiveHealthExamcode:="$<1$ year ago"]
dat_use_cat[lastReproductiveHealthExamcode=="Üle ühe aasta kuni 2 aastat",lastReproductiveHealthExamcode:="1-2 years ago"]
dat_use_cat[lastReproductiveHealthExamcode=="Üle 2 aasta kuni 3 aastat",lastReproductiveHealthExamcode:="2-3 years ago"]
dat_use_cat[lastReproductiveHealthExamcode %in% c("Üle 3 aasta kuni 5 aastat","Üle 5 aasta"),lastReproductiveHealthExamcode:="3+ years ago"]

dat_use_cat[awarenessOfPapTestcode=="Ei",awarenessOfPapTestcode:="No"]
dat_use_cat[awarenessOfPapTestcode=="Jah",awarenessOfPapTestcode:="Yes"]
dat_use_cat[awarenessOfPapTestcode=="Ei tea",awarenessOfPapTestcode:="Did not know"]

dat_use_cat[awarenessOfOwnPapTestIntervalc=="Ei",awarenessOfOwnPapTestIntervalc:="No"]
dat_use_cat[awarenessOfOwnPapTestIntervalc=="Jah",awarenessOfOwnPapTestIntervalc:="Yes"]

dat_use_cat_long <- melt(dat_use_cat,id.vars=c("SKOOD","hrHPV"))
dat_use_cat_long[,hrHPV:=as.character(hrHPV)]

variables <- unique(dat_use_cat_long$variable)
pVals <- NULL
for(var_use in variables){
  dat_tmp <- dat_use_cat_long[var_use == variable,]
  cont_table <- table(dat_tmp$hrHPV,dat_tmp$value)
  if(nrow(cont_table) == 2 & ncol(cont_table) == 2){
    p <- fisher.test(cont_table)$p.value
  }else{
    p <- chisq.test(cont_table)$p.value
  }
  pVals <- c(pVals, p)
}
pvalDat <- data.table(variable=variables,p=round(pVals,3))


dat_use_cat_long_extra <- copy(dat_use_cat_long)
dat_use_cat_long_extra[,hrHPV:="Total"]
dat_use_cat_long <- rbind(dat_use_cat_long,dat_use_cat_long_extra)

#Calculate total counts and percentages
dat_use_cat_long[!is.na(value),nTot:=.N,by=c("hrHPV","variable")]
dat_use_cat_long <- dat_use_cat_long[!is.na(nTot),]
dat_use_cat_long[!is.na(value),nTot:=.N,by=c("hrHPV","variable")]
dat_use_cat_long[!is.na(value),n:=.N,by=c("hrHPV","variable","value")]
dat_use_cat_long[!is.na(value),perc:=round(n*100/nTot)]
dat_use_cat_long[,statCol:=paste0(n," (",perc,"\\%)")]

dat_use_cat_unique <- unique(dat_use_cat_long[,list(hrHPV,variable,value,nTot,statCol)])

dat_use_cat_cast <- dcast(dat_use_cat_unique,variable+value~hrHPV,value.var = c("nTot","statCol"))

dat_use_cat_cast <- merge(dat_use_cat_cast,pvalDat,by="variable",all.x=T)
headRow <- as.data.table(t(colnames(dat_use_cat_cast)))
names(headRow) <- colnames(dat_use_cat_cast)
dat_use_cat_cast <- rbind(headRow,dat_use_cat_cast)
dat_use_cat_cast_tmp1 <- copy(dat_use_cat_cast)
dat_use_cat_cast_tmp1[,c("value","statCol_TRUE","statCol_FALSE","statCol_Total"):=""]
dat_use_cat_cast_tmp1 <- unique(dat_use_cat_cast_tmp1)


dat_use_cat_cast2 <- rbind(dat_use_cat_cast,dat_use_cat_cast_tmp1[-1,])
dat_use_cat_cast2 <- dat_use_cat_cast2[order(variable,value,decreasing=F)]

#order the columns
dat_use_cat_cast2 <- dat_use_cat_cast2[,list(variable,value,nTot_TRUE,statCol_TRUE,nTot_FALSE,statCol_FALSE,nTot_Total,statCol_Total,p)]
dat_use_cat_cast2[value !="",c("nTot_TRUE","nTot_FALSE","nTot_Total"):=""]
dat_use_cat_cast2[value!="",variable:=paste0("\\hskip .5cm",value)]

dat_use_cat_cast2[,value:=NULL]

dat_use_cat_cast2[variable=="maritalStatuscode",variable:="Marital status, n (\\%)"]
dat_use_cat_cast2[variable=="educationcode",variable:="Education, n (\\%)"]
dat_use_cat_cast2[variable=="firstIntercourseContraceptives4",variable:="Contraceptives at first intercourse, n (\\%)"]
dat_use_cat_cast2[variable=="pregnancyEndcode1",variable:="Pregnancy endcode 1, n (\\%)"]
dat_use_cat_cast2[variable=="hasVeneralDisease",variable:="Has veneral diseases, n (\\%)"]
dat_use_cat_cast2[variable=="lastReproductiveHealthExamcode",variable:="Last reproductive health exam, n (\\%)"]
dat_use_cat_cast2[variable=="awarenessOfPapTestcode",variable:="PAP test awareness, n (\\%)"]
dat_use_cat_cast2[variable=="awarenessOfOwnPapTestIntervalc",variable:="PAP test interval awareness, n (\\%)"]
dat_use_cat_cast2[p=="0",p:="$<0.001$"]

dat_use_cat_cast2[nTot_TRUE == "",p:=""]

dat_use_cat_cast2[variable=="\\hskip .5cmvalue",variable:=""]
setnames(dat_use_cat_cast2,c("statCol_TRUE","statCol_FALSE","statCol_Total"),c("hrHPV-positive","hrHPV-negative","hrHPV-total"))
dat_use_cat_cast2[1,p:="p"] 
dat_use_cat_cast2[1,nTot_TRUE:="n"] 
dat_use_cat_cast2[1,nTot_FALSE:="n"] 
dat_use_cat_cast2[1,nTot_Total:="n"] 
dat_use_cat_cast2[1,`hrHPV-positive`:=""] 
dat_use_cat_cast2[1,`hrHPV-negative`:=""] 
dat_use_cat_cast2[1,`hrHPV-total`:=""] 

names(dat_use_cat_cast2) <- c("","","hrHPV-positive","","hrHPV-negative","","hrHPV-total","")

finalDat <- rbind(dat_use_num_cast,dat_use_cat_cast2[-1,])
names(finalDat) <- c("","","hrHPV-positive","","hrHPV-negative","","hrHPV-total","")


library(xtable)
print(xtable(finalDat),include.rownames=F,sanitize.text.function = function(x){x})
