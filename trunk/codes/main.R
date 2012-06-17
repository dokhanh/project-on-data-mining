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
nValSal = 4;
ValSal = quantile(data$SalBrut, probs = c(0.25, 0.50, 0.75), na.rm = TRUE);
data$SalBrut2 = rep(nValSal, length(data$SalBrut));
data$SalBrut2[data$SalBrut < ValSal[1]] = 1;
for (i in 2:nValSal)
{
	data$SalBrut2[data$SalBrut < ValSal[i] & data$SalBrut > ValSal[i-1]] = i;
}
data$SalBrut2[is.na(data$SalBrut)] = NA;
data$SalBrut2 = factor(data$SalBrut2);

# create new data frame containing interesting variables
data1 = data.frame(data$civilite, data$Activité.principale.employeur, data$DomAct);

#MCAda
library(FactoMineR);
result_MCA <- MCA(data1);

# interesting variables: diplome, admission, FR.UE.HUE, TailEntrp
data$civilite = factor(data$civilite);
data$diplome = factor(data$diplome);
data$promo = factor(data$promo);
data$admission = factor(data$admission);
data$FR.UE.HUE = factor(data$FR.UE.HUE);
data$TypEntrep = factor(data$TypEntrep);
data$SalBrut = as.numeric(gsub(" ","", data$SalBrut , fixed=TRUE));

data_list1 = data.frame(data$diplome, data$admission, data$FR.UE.HUE, data$TypEntrep, data$promo, data$SalBrut);

result_list1 <- MCA(data_list1, ncp = 5, quanti.sup = 6, quali.sup = 5, graph = TRUE);
plot(result_list1, choix=c("ind", "quanti.sup"), invisible = "ind", habillage = "quali");
dimdesc(result_list1);
print(result_list1, sep = ";");
# valeurs propres
result_MCA2$eig;
result_MCA2$var;

# parcours des étudiants
data$civilite = factor(data$civilite);
data$FormComp = factor(data$FormComp);
data$diplome = factor(data$diplome);
data$promo = factor(data$promo);
data$admission = factor(data$admission);
data$FR.UE.HUE = factor(data$FR.UE.HUE);
data$TypEntrep = factor(data$TypEntrep);
data$SalBrut = as.numeric(gsub(" ","", data$SalBrut , fixed=TRUE));

data_list1 = data.frame(data$civilite, data$FormComp, data$diplome, data$admission, data$FR.UE.HUE, data$TypEntrep, data$promo, data$SalBrut);

result_list1 <- MCA(data_list1, ncp = 5, quanti.sup = 8, quali.sup = 7, graph = TRUE);
plot(result_list1, choix="ind", invisible = c("ind", "quali.sup"), habillage = "quali");
dimdesc(result_list1);
print(result_list1, sep = ";");
# valeurs propres
result_MCA2$eig;
result_MCA2$var;
result_MCA2$var;

#list3
#list4
# interesting variables: TypEntrep, CreaEnt, TailEntrp, Rep, ActivitePrincipale, SalBrut
data$TypEntrep = factor(data$TypEntrep);
data$CreaEnt = factor(data$CreaEnt);
data$TailEntrp = factor(data$TailEntrp);
data$Rep = factor(data$Rep);
data$ActivitÈ.principale.employeur = factor(data$ActivitÈ.principale.employeur);
data$SalBrut2 = factor(data$SalBrut2)

data3 = data.frame(data$TypEntrep, data$CreaEnt, data$TailEntrp, data$Rep, data$ActivitÈ.principale.employeur, data$SalBrut2);
#change to shorter name
colnames(data3)[1] = "Ty";
colnames(data3)[2] = "Cr";
colnames(data3)[3] = "Ta";
colnames(data3)[4] = "Re";
colnames(data3)[5] = "Ac";
colnames(data3)[6] = "Sa";

#in this case, those N/A values are "really missing values", which should be preprocessed before we apply MCA
tab.disj3 <- imputeMCA(data3, ncp = 3);
result_MCA3 <- MCA(data3, graph=FALSE, tab.disj = tab.disj3);
plot(result_MCA3, choix="ind", invisible = "ind", habillage = "quali", xlim=c(-2,2), ylim=c(-0.5,0.75));
dimdesc(result_MCA3);
print(result_MCA3, sep = ";");
# valeurs propres
result_MCA3$eig;
result_MCA3$var;

#lien quan giua viec "tao lap cong ty" va "loai cong ty"
# thuong cac cong ty nhu hat nhan, vu tru thi khong co ai tao cong ty ca !!!!
# thuong chi tao cac cong ty conseil, service ...

#thuong tao cong ty co quy mo nho, type la liberale