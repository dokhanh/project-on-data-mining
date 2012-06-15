#set default folder
#setwd('/Users/tuanhungvu/Study/M2/Period4/SI Decisionnel/Project/codes');
#Read data from file
data = read.csv('25Promos2009.csv', head = TRUE, sep = ';', stringsAsFactors=FALSE, strip.white=TRUE);


#convert from numeric to factor
data$civilite = factor(data$civilite);
data$diplome = factor(data$diplome);
data$promo = factor(data$promo);
data$admission = factor(data$admission);
data$FormComp = factor(data$FormComp);
data$DeaMsc = factor(data$DeaMsc);
data$Doctorat = factor(data$Doctorat);
data$DESSMBA = factor(data$DESSMBA);
data$AutresForm = factor(data$AutresForm);
data$LieuForm = factor(data$LieuForm);
data$SitPro = factor(data$SitPro);
data$RecchEmp = factor(data$RecchEmp);
data$RecchAutre = factor(data$RecchAutre);
data$NbExp = factor(data$NbExp);
data$NbreEmp = factor(data$NbreEmp);
data$AnnRef = factor(data$AnnRef);
data$TypEntrep = factor(data$TypEntrep);
data$CreaEnt = factor(data$CreaEnt);
data$Dirigeant = factor(data$Dirigeant);
data$EntIndep = factor(data$EntIndep);
data$TailEntrp = factor(data$TailEntrp);




#list of interesting variables
data$civilite								# man or women
#data$nationalite1							# nationality
#data$promo									# year of study
data$Activité.principale.employeur			#
data$DomAct									# professional domain
data$SalBrut								# net salary

# some qualitative variables are considered as number by R automatically
# this step is to convert from number back to factor
data$civilite = factor(data$civilite);
data$nationalite1 = factor(data$nationalite1);
data$promo = factor(data$promo);
data$Activité.principale.employeur = factor(data$Activité.principale.employeur);
data$DomAct = factor(data$DomAct);

# because data$SalBrut is characters, we need to preprocess a little to convert it form characters to number
data$SalBrut = as.numeric(gsub(" ","", data$SalBrut , fixed=TRUE))
# draw distribution of net salary
# we can base on this distribution to decide the intervals (when converting from numeric to factor)
hist(data$SalBrut[data$SalBrut < 400000], nclass = 100)

# convert from numeric to factor
#..............

# create new data frame containing interesting variables
data1 = data.frame(data$civilite, data$Activité.principale.employeur, data$DomAct);

#MCAda
library(FactoMineR);
result_MCA <- MCA(data1);

# interesting variables: diplome, admission, FR.UE.HUE, TailEntrp
data$diplome = factor(data$diplome);
data$admission = factor(data$admission);
data$FR.UE.HUE = factor(data$FR.UE.HUE);
data$TailEntrp = factor(data$TailEntrp);

data2 = data.frame(data$diplome, data$admission, data$FR.UE.HUE, data$TailEntrp);

result_MCA2 <- MCA(data2);
plot(result_MCA2, choix="ind", invisible = "ind", habillage = "quali");
dimdesc(result_MCA2);
print(result_MCA2, sep = ";");
# valeurs propres
result_MCA2$eig;
result_MCA2$var;