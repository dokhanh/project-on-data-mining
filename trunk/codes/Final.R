##################################INITIALIZE######################################################
# Set default folder
#setwd('/Users/tuanhungvu/Study/M2/Period4/SI Decisionnel/Project/codes');
# Read data from file
data = read.csv('25Promos2009_utf8.csv', head = TRUE, sep = ';', stringsAsFactors=FALSE, strip.white=TRUE);
# Load library FactoMineR
library(FactoMineR);

##################################PREPROCESS######################################################
# Because data$SalBrut is characters, we need to preprocess a little to convert it form characters to number
data$SalBrut = as.numeric(gsub(" ","", data$SalBrut , fixed=TRUE))

# We will divide salbrut into intervals (convert it from numeric to factor)
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

##################################ANALYSE 2.1#####################################################
# interesting variables: Taille d'Entreprise, Responsabilite, ActivitePrincipale, SalBrut
data$TailEntrp = factor(data$TailEntrp);
data$Rep = factor(data$Rep);
data$ActivitÈ.principale.employeur = factor(data$ActivitÈ.principale.employeur);
data$SalBrut2 = factor(data$SalBrut2)

# Create data frame containning those variables
data_ana_2_1 = data.frame(data$TailEntrp, data$Rep, data$ActivitÈ.principale.employeur, data$SalBrut2);
# Change to shorter name
colnames(data_ana_2_1)[1] = "ta";
colnames(data_ana_2_1)[2] = "re";
colnames(data_ana_2_1)[3] = "act";
colnames(data_ana_2_1)[4] = "sal";

# Preprocess N/A values before applying MCA
tab.disj_ana_2_1 <- imputeMCA(data_ana_2_1, ncp = 3);
result_MCA_ana_2_1 <- MCA(data_ana_2_1, graph=TRUE, tab.disj = tab.disj_ana_2_1);
plot(result_MCA_ana_2_1, choix="ind", invisible = "ind", habillage = "quali");
dimdesc(result_MCA_ana_2_1);
print(result_MCA_ana_2_1, sep = ";");
# valeurs propres
result_MCA_ana_2_1$eig;
result_MCA_ana_2_1$var;

# Export result to LATEX
mat2tex(result_MCA_ana_2_1$eig, stdout())
colnames(result_MCA_ana_2_1$var$coor) = paste('Crd', colnames(result_MCA_ana_2_1$var$coor))
colnames(result_MCA_ana_2_1$var$contrib) = paste('Contrib', colnames(result_MCA_ana_2_1$var$contrib))
colnames(result_MCA_ana_2_1$var$cos2) = paste('Cos2', colnames(result_MCA_ana_2_1$var$cos2))
mat2tex(data.frame(result_MCA_ana_2_1$var$coor[,1:2], result_MCA_ana_2_1$var$contrib[,1:2], result_MCA_ana_2_1$var$cos2[,1:2]), stdout())

##################################ANALYSE 2.2#####################################################
# interesting variables: Taille d'Entreprise, Creation d'Entreprise, Responsabilite, SalBrut
data$TailEntrp = factor(data$TailEntrp);
data$CreaEnt = factor(data$CreaEnt);
data$Rep = factor(data$Rep);
data$SalBrut2 = factor(data$SalBrut2)

# Create data frame containning those variables
data_ana_2_2 = data.frame(data$TailEntrp, data$CreaEnt, data$Rep, data$SalBrut2);
# Change to shorter name
colnames(data_ana_2_2)[1] = "ta";
colnames(data_ana_2_2)[2] = "cr";
colnames(data_ana_2_2)[3] = "re";
colnames(data_ana_2_2)[4] = "sal";

# Preprocess N/A values before applying MCA
tab.disj_ana_2_2 <- imputeMCA(data_ana_2_2, ncp = 5);
result_MCA_ana_2_2 <- MCA(data_ana_2_2, graph=TRUE, ncp = 5, tab.disj = tab.disj_ana_2_2);
plot(result_MCA_ana_2_2, choix="ind", invisible = "ind", habillage = "quali");
dimdesc(result_MCA_ana_2_2);
print(result_MCA_ana_2_2, sep = ";");
# valeurs propres
result_MCA_ana_2_2$eig;
result_MCA_ana_2_2$var;

# Export result to LATEX
mat2tex(result_MCA_ana_2_2$eig, stdout())
colnames(result_MCA_ana_2_2$var$coor) = paste('Crd', colnames(result_MCA_ana_2_2$var$coor))
colnames(result_MCA_ana_2_2$var$contrib) = paste('Contrib', colnames(result_MCA_ana_2_2$var$contrib))
colnames(result_MCA_ana_2_2$var$cos2) = paste('Cos2', colnames(result_MCA_ana_2_2$var$cos2))
mat2tex(data.frame(result_MCA_ana_2_2$var$coor[,1:2], result_MCA_ana_2_2$var$contrib[,1:2], result_MCA_ana_2_2$var$cos2[,1:2]), stdout())