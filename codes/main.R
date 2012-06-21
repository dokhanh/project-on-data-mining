#set default folder
#setwd('/Users/tuanhungvu/Study/M2/Period4/SI Decisionnel/Project/codes');
#Read data from file
data = read.csv('25Promos2009.csv', head = TRUE, sep = ';', stringsAsFactors=FALSE, strip.white=TRUE);

# parcours des étudiants
library(FactoMineR);
library(missMDA);
library(sfsmisc);
data$civilite = factor(data$civilite);
data$FormComp = factor(data$FormComp);
data$diplome = factor(data$diplome);
data$rpomo <- as.numeric(data$promo);
data$admission = factor(data$admission);
data$FR.UE.HUE = factor(data$FR.UE.HUE);
data$TypEntrep = factor(data$TypEntrep);
data$SalBrut = as.numeric(gsub(" ","", data$SalBrut , fixed=TRUE));
# convert from numeric to factor
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
# convert promo into nValPro groups
nValPro <- 4;
ValPro <- quantile(data$promo, probs = c(0.25, 0.50, 0.75), na.rm = TRUE);
data$promo2 = rep(nValPro, length(data$promo));
data$promo2[data$promo < ValPro[1]] = 1;
for (i in 2:nValPro)
{
	data$promo2[data$promo < ValPro[i] & data$promo > ValPro[i-1]] = i;
}
data$promo2[is.na(data$promo)] = NA;
data$promo2 = factor(data$promo2);

data_list <- data.frame(data$civilite, data$FormComp, data$diplome, data$promo2, data$admission, data$FR.UE.HUE, data$TypEntrep, data$SalBrut2);

data_list1 <- data.frame(data$diplome, data$admission, data$FR.UE.HUE, data$SalBrut2);
colnames(data_list1)[1] <- "dip";
colnames(data_list1)[2] <- "ad";
colnames(data_list1)[3] <- "ori";
colnames(data_list1)[4] <- "sal";

tab.disj.comp <- imputeMCA(data_list1,ncp=2);
result_list1 <- MCA(data_list1, ncp = 5, graph = TRUE, tab.disj=tab.disj.comp);

plot(result_list1, choix="ind", invisible = c("var"), habillage = "quali");
plot(result_list1, choix="ind", invisible = c("ind"), habillage = "quali");
plot(result_list1, choix="ind", invisible = c("ind"), habillage = "quali", xlim = c(-1.4, 0.537), ylim = c(-0.805, 4.15));
dimdesc(result_list1);
tab1 <- result_list1$eig;
mat2tex(tab1, append = FALSE);
tab2 <- data.frame(result_list1$var$coord[, c(1:2)], result_list1$var$contrib[, c(1:2)], result_list1$var$cos2[, c(1:2)]);
mat2tex(tab2, append = FALSE);

data_list2 <- data.frame(data$admission, data$TypEntrep, data$SalBrut2, data$promo2);
colnames(data_list2)[1] <- "ad";
colnames(data_list2)[2] <- "tep";
colnames(data_list2)[3] <- "sal";
colnames(data_list2)[4] <- "pro";

tab.disj.comp <- imputeMCA(data_list2,ncp=2)
result_list2 <- MCA(data_list2, ncp = 5, graph = TRUE, tab.disj=tab.disj.comp);

tab1 <- result_list2$eig;
mat2tex(tab1, append = FALSE);
tab2 <- data.frame(result_list2$var$coord[, c(1:2)], result_list2$var$contrib[, c(1:2)], result_list2$var$cos2[, c(1:2)]);
mat2tex(tab2, append = FALSE);

plot(result_list2, choix="ind", invisible = c("var"), habillage = "quali");

plot(result_list2, choix="ind", invisible = c("ind"), habillage = "quali", xlim = c(-2.24, 1.62), ylim = c(-0.862, 2.19));
dimdesc(result_list2);

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