######################### LOADING LIBRARIES ######################### 

library(tidyverse)
library(mice)
library(VIM)
library("faraway")
library(mice)
library(corrplot)
library(dplyr)    # for data wrangling
library(ggplot2)  # for awesome graphics
library(fields)
library(ranger)   # a c++ implementation of random forest 
library(h2o)      # a java-based implementation of random forest
library(randomForest) # basic implementation
library(caret)        # an aggregator package for performing many machine learning models
library(rsample)      # data splitting 
library(tidyverse)
library(tidymodels)
library(e1071)
library(corrplot)
library(caTools)
#library(extrafont)
library(Metrics)
library('writexl')
library('dplyr')
library('readr')
library('olsrr')
library('leaps')
library('ggplot2')
library('ggpubr') #
library('gridExtra') #create grid of graphs
library('data.table')
library("Hmisc") #correlation matrix and reshaping
library("xlsx")

######################### LOADING DATA ######################### 

data <- read.csv(file = 'LifeExpectancyData1.csv', stringsAsFactors = TRUE)
descriptive_cols = subset(data, select = c(Country.Code, Country.Name))
data = subset(data, select = -c(Country.Code, Country.Name))

######################### EDA (Q1) ######################### 

dim(data)

str(data)

head(data)

colSums(sapply(data, is.na))

summary(data)
sapply(data, mean, na.rm=TRUE)
sapply(data, median, na.rm=TRUE)
sapply(data, sd, na.rm=TRUE)

length(data$SP.DYN.AMRT.FE)

## Missing Values
missing.values <- data %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

levels <-
  (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x =
         'Variable', y = "% of missing values")

percentage.plot

row.plot <- data %>%
  mutate(id = row_number()) %>%
  gather(-id, key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  ggplot(aes(key, id, fill = isna)) +
  geom_raster(alpha=0.8) +
  scale_fill_manual(name = "",
                    values = c('steelblue', 'tomato3'),
                    labels = c("Present", "Missing")) +
  scale_x_discrete(limits = levels) +
  labs(x = "Variable",
       y = "Row Number", title = "Missing values in rows") +
  coord_flip()

row.plot

sapply(data, mean)
sapply(data, median)

## Correlation

correlation <- rcorr(as.matrix(data))

# Reshape matrix
f_matrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
f <- f_matrix(correlation$r, correlation$P) 
write_xlsx(as.data.frame(f), path = "correlation matrix.xlsx")

## Shapiro Test for Normality 

shapiro.test(data$EG.ELC.ACCS.ZS)
shapiro.test(data$NY.ADJ.NNTY.KD.ZG)
shapiro.test(data$NY.ADJ.NNTY.KD)
shapiro.test(data$SE.PRM.UNER.ZS)
shapiro.test(data$SE.XPD.PRIM.ZS)
shapiro.test(data$SP.DYN.IMRT.IN)
shapiro.test(data$SE.ADT.LITR.ZS)
shapiro.test(data$SP.POP.GROW)
shapiro.test(data$SP.POP.TOTL)
shapiro.test(data$SE.PRM.CMPT.ZS)
shapiro.test(data$SH.XPD.CHEX.GD.ZS)
shapiro.test(data$SH.XPD.CHEX.PC.CD)
shapiro.test(data$SL.UEM.TOTL.NE.ZS)
shapiro.test(data$SP.DYN.AMRT.FE)
shapiro.test(data$SP.DYN.AMRT.MA)
shapiro.test(data$NY.GDP.MKTP.KD.ZG)
shapiro.test(data$NY.GDP.PCAP.PP.CD)
shapiro.test(data$SP.DYN.CBRT.IN)
shapiro.test(data$NY.GNP.PCAP.PP.CD)
shapiro.test(data$SL.EMP.TOTL.SP.ZS)

describe <- describe(cbind(
  data$EG.ELC.ACCS.ZS,
  data$NY.ADJ.NNTY.KD.ZG,
  data$NY.ADJ.NNTY.KD,
  data$SE.PRM.UNER.ZS,
  data$SE.XPD.PRIM.ZS,
  data$SP.DYN.IMRT.IN,
  data$SE.ADT.LITR.ZS,
  data$SP.POP.GROW,
  data$SP.POP.TOTL,
  data$SE.PRM.CMPT.ZS,
  data$SH.XPD.CHEX.GD.ZS,
  data$SH.XPD.CHEX.PC.CD,
  data$SL.UEM.TOTL.NE.ZS,
  data$SP.DYN.AMRT.FE,
  data$SP.DYN.AMRT.MA,
  data$NY.GDP.MKTP.KD.ZG,
  data$NY.GDP.PCAP.PP.CD,
  data$SP.DYN.CBRT.IN,
  data$NY.GNP.PCAP.PP.CD,
  data$SL.EMP.TOTL.SP.ZS))
write_xlsx(as.data.frame(describe), path = "/Users/User/describe.xlsx")

## Scatter Plots
v1 <- ggscatter(data, x = "EG.ELC.ACCS.ZS", y = "SP.DYN.LE00.IN", 
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Access to electricity (% of population)", ylab = "Life expectancy at birth total (years)")
v2 <- ggscatter(data, x = "NY.ADJ.NNTY.KD.ZG", y = "SP.DYN.LE00.IN", 
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Adjusted net national income (annual % growth)", ylab = "Life expectancy at birth total (years)")
v3 <- ggscatter(data, x = "NY.ADJ.NNTY.KD", y = "SP.DYN.LE00.IN", 
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Adjusted net national income (constant 2010 US$)", ylab = "Life expectancy at birth total (years)")
v4 <- ggscatter(data, x = "SE.PRM.UNER.ZS", y = "SP.DYN.LE00.IN", 
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Children out of school (% of primary school age)", ylab = "Life expectancy at birth total (years)")
v5 <- ggscatter(data, x = "SE.XPD.PRIM.ZS", y = "SP.DYN.LE00.IN", 
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Expenditure on primary education (% of government expenditure on education)", ylab = "Life expectancy at birth total (years)")
v6 <- ggscatter(data, x = "SP.DYN.IMRT.IN", y = "SP.DYN.LE00.IN", 
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Mortality rate, infant (per 1,000 live births)", ylab = "Life expectancy at birth total (years)")
v7 <- ggscatter(data, x = "SE.ADT.LITR.ZS", y = "SP.DYN.LE00.IN", 
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Literacy rate , adult total (% of people ages 15 and above)", ylab = "Life expectancy at birth total (years)")
v8 <- ggscatter(data, x = "SP.POP.GROW", y = "SP.DYN.LE00.IN", 
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Population growth (annual %)", ylab = "Life expectancy at birth total (years)")
v9 <- ggscatter(data, x = "SP.POP.TOTL", y = "SP.DYN.LE00.IN", 
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Population, total", ylab = "Life expectancy at birth total (years)")
v10 <- ggscatter(data, x = "SE.PRM.CMPT.ZS", y = "SP.DYN.LE00.IN", 
                 add = "reg.line", conf.int = TRUE, 
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "Primary completion rate, total (% of relevant age group)", ylab = "Life expectancy at birth total (years)")
v11 <- ggscatter(data, x = "SH.XPD.CHEX.GD.ZS", y = "SP.DYN.LE00.IN", 
                 add = "reg.line", conf.int = TRUE, 
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "Current health expenditure (% of GDP)", ylab = "Life expectancy at birth total (years)")
v12 <- ggscatter(data, x = "SH.XPD.CHEX.PC.CD", y = "SP.DYN.LE00.IN", 
                 add = "reg.line", conf.int = TRUE, 
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "Current health expenditure per capita, PPP (current international $)", ylab = "Life expectancy at birth total (years)")
v13 <- ggscatter(data, x = "SL.UEM.TOTL.NE.ZS", y = "SP.DYN.LE00.IN", 
                 add = "reg.line", conf.int = TRUE, 
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "Unemployment, total (% of total labor force) (national estimate)", ylab = "Life expectancy at birth total (years)")
v14 <- ggscatter(data, x = "SP.DYN.AMRT.FE", y = "SP.DYN.LE00.IN", 
                 add = "reg.line", conf.int = TRUE, 
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "Mortality rate, adult, female (per 1,000 female adults)", ylab = "Life expectancy at birth total (years)")
v15 <- ggscatter(data, x = "SP.DYN.AMRT.MA", y = "SP.DYN.LE00.IN", 
                 add = "reg.line", conf.int = TRUE, 
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "Mortality rate, adult, male (per 1,000 male adults)", ylab = "Life expectancy at birth total (years)")
v16 <- ggscatter(data, x = "NY.GDP.MKTP.KD.ZG", y = "SP.DYN.LE00.IN", 
                 add = "reg.line", conf.int = TRUE, 
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "GDP growth (annual %)", ylab = "Life expectancy at birth total (years)")
v17 <- ggscatter(data, x = "NY.GDP.PCAP.PP.CD", y = "SP.DYN.LE00.IN", 
                 add = "reg.line", conf.int = TRUE, 
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "GDP per capita, PPP (current international $)", ylab = "Life expectancy at birth total (years)")
v18 <- ggscatter(data, x = "SP.DYN.CBRT.IN", y = "SP.DYN.LE00.IN", 
                 add = "reg.line", conf.int = TRUE, 
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "Birth rate, crue (per 1,000 people)", ylab = "Life expectancy at birth total (years)")
v19 <- ggscatter(data, x = "NY.GNP.PCAP.PP.CD", y = "SP.DYN.LE00.IN", 
                 add = "reg.line", conf.int = TRUE, 
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "GNI per capita, PPP (current international $)", ylab = "Life expectancy at birth total (years)")
v20 <- ggscatter(data, x = "SL.EMP.TOTL.SP.ZS", y = "SP.DYN.LE00.IN", 
                 add = "reg.line", conf.int = TRUE, 
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "Employment to population ratio, 15+, total (%) (modeled ILO estimate)", ylab = "Life expectancy at birth total (years)")
vlog12 <- ggscatter(data, x = "log_SH.XPD.CHEX.PC.CD", y = "SP.DYN.LE00.IN", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "Current health expenditure per capita, PPP (current international $)", ylab = "Life expectancy at birth total (years)")
vlog17 <- ggscatter(data, x = "log_NY.GDP.PCAP.PP.CD", y = "SP.DYN.LE00.IN", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "GDP per capita, PPP (current international $)", ylab = "Life expectancy at birth total (years)")
vlog19 <- ggscatter(data, x = "log_NY.GNP.PCAP.PP.CD", y = "SP.DYN.LE00.IN", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "GNI per capita, PPP (current international $)", ylab = "Life expectancy at birth total (years)")
vlog3 <- ggscatter(data, x = "log_NY.ADJ.NNTY.KD", y = "SP.DYN.LE00.IN", 
                   add = "reg.line", conf.int = TRUE, 
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Adjusted net national income (constant 2010 US$)", ylab = "Life expectancy at birth total (years)")


#arrange scatterplots with high correlation and save
high_corr <- grid.arrange(v1, v6, v7, v14, v15, v18, ncol = 3) 
ggsave(file="MA317_scatter_high_corr.jpg", high_corr)


## Histograms create histograms
hy <- ggplot(data, aes(SP.DYN.LE00.IN)) + geom_histogram() + xlab("Life expectancy at birth total (years)") + ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h1 <- ggplot(data, aes(EG.ELC.ACCS.ZS)) + geom_histogram() + xlab ("Access to electricity (% of population)") + ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h2 <- ggplot(data, aes(NY.ADJ.NNTY.KD.ZG)) + geom_histogram() + xlab ("Adjusted net national income (annual % growth)")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h3 <- ggplot(data, aes(NY.ADJ.NNTY.KD)) + geom_histogram() + xlab ("Adjusted net national income (constant 2010 US$)")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h4 <- ggplot(data, aes(SE.PRM.UNER.ZS)) + geom_histogram() + xlab ("Children out of school (% of primary school age)")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h5 <- ggplot(data, aes(SE.XPD.PRIM.ZS)) + geom_histogram() + xlab ("Expenditure on primary education (% of government expenditure on education)")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h6 <- ggplot(data, aes(SP.DYN.IMRT.IN)) + geom_histogram() + xlab("Mortality rate, infant (per 1,000 live births)")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h7 <- ggplot(data, aes(SE.ADT.LITR.ZS)) + geom_histogram() + xlab("Literacy rate , adult total (% of people ages 15 and above)")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h8 <- ggplot(data, aes(SP.POP.GROW)) + geom_histogram() + xlab("Population growth (annual %)")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h9 <- ggplot(data, aes(SP.POP.TOTL)) + geom_histogram() + xlab("Population, total")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h10 <- ggplot(data, aes(SE.PRM.CMPT.ZS)) + geom_histogram() + xlab("Primary completion rate, total (% of relevant age group)")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h11 <- ggplot(data, aes(SH.XPD.CHEX.GD.ZS)) + geom_histogram() + xlab("Current health expenditure (% of GDP)")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h12 <- ggplot(data, aes(SH.XPD.CHEX.PC.CD)) + geom_histogram() + xlab("Current health expenditure per capita, PPP (current international $)")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h13 <- ggplot(data, aes(SL.UEM.TOTL.NE.ZS)) + geom_histogram() + xlab("Unemployment, total (% of total labor force) (national estimate)")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h14 <- ggplot(data, aes(SP.DYN.AMRT.FE)) + geom_histogram() + xlab("Mortality rate, adult, female (per 1,000 female adults)")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h15 <- ggplot(data, aes(SP.DYN.AMRT.MA)) + geom_histogram() + xlab("Mortality rate, adult, male (per 1,000 male adults)")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h16 <- ggplot(data, aes(NY.GDP.MKTP.KD.ZG)) + geom_histogram() + xlab("GDP growth (annual %)")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h17 <- ggplot(data, aes(NY.GDP.PCAP.PP.CD)) + geom_histogram()+ xlab("GDP per capita, PPP (current international $)")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h18 <- ggplot(data, aes(SP.DYN.CBRT.IN)) + geom_histogram()+ xlab("Birth rate, crue (per 1,000 people)")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h19 <- ggplot(data, aes(NY.GNP.PCAP.PP.CD)) + geom_histogram()+ xlab("GNI per capita, PPP (current international $)")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()
h20 <- ggplot(data, aes(SE.ADT.LITR.ZS)) + geom_histogram() + xlab("Employment to population ratio, 15+, total (%) (modeled ILO estimate)")+ ylab("Frequency") +
  geom_histogram(fill="#0c4c8a") + theme_minimal()

hy
g <-grid.arrange(h1, h2, h3, h4, h5, h6,ncol=3)
g <-grid.arrange(h7, h8, h9, h10, h11, h12, ncol=3)
g <-grid.arrange(h13, h14, h15, h16, h17, h18, ncol=3)
g <-grid.arrange(h19, h20, ncol=3)
ggsave(file="MA317_histograms.jpg", g)



## Outliers
out1 <- boxplot.stats(data$EG.ELC.ACCS.ZSy)$out
out_ind1 <- which(data$EG.ELC.ACCS.ZS %in% c(out1))
length(out2) 
out2 <- boxplot.stats(data$NY.ADJ.NNTY.KD.ZG)$out
out_ind2 <- which(data$NY.ADJ.NNTY.KD.ZG %in% c(out2))
length(out2) 
out3 <- boxplot.stats(data$NY.ADJ.NNTY.KD)$out
out_ind3 <- which(data$NY.ADJ.NNTY.KD %in% c(out3))
length(out3) 
out4 <- boxplot.stats(data$SE.PRM.UNER.ZS)$out
out_ind4 <- which(data$SE.PRM.UNER.ZS %in% c(out4))
length(out4) 
out5 <- boxplot.stats(data$SE.XPD.PRIM.ZS)$out
out_ind5 <- which(data$SE.XPD.PRIM.ZS %in% c(out5))
length(out5) 
out6 <- boxplot.stats(data$SP.DYN.IMRT.IN)$out
out_ind6 <- which(data$SP.DYN.IMRT.IN %in% c(out6))
length(out6) 
out7 <- boxplot.stats(data$SE.ADT.LITR.ZS)$out
out_ind7 <- which(data$EG.ELC.ACCS.ZS %in% c(out7))
length(out7) 
out8 <- boxplot.stats(data$SP.POP.GROW)$out
out_ind8 <- which(data$SP.POP.GROW %in% c(out8))
length(out8) 
out9 <- boxplot.stats(data$SP.POP.TOTL)$out
out_ind9 <- which(data$SP.POP.TOTL %in% c(out9))
length(out9) 
out10 <- boxplot.stats(data$SE.PRM.CMPT.ZS)$out
out_ind10 <- which(data$SE.PRM.CMPT.ZS%in% c(out10))
length(out10) 
out11 <- boxplot.stats(data$SH.XPD.CHEX.GD.ZS)$out
out_ind11 <- which(data$SH.XPD.CHEX.GD.ZS %in% c(out11))
length(out11) 
out12 <- boxplot.stats(data$SH.XPD.CHEX.PC.CD)$out
out_ind12 <- which(data$SH.XPD.CHEX.PC.CD %in% c(out12))
length(out12) 
out13 <- boxplot.stats(data$SL.UEM.TOTL.NE.ZS)$out
out_ind13 <- which(data$SL.UEM.TOTL.NE.ZS %in% c(out13))
length(out13) 
out14 <- boxplot.stats(data$SP.DYN.AMRT.FE)$out
out_ind14 <- which(data$SP.DYN.AMRT.FE %in% c(out14))
length(out14) 
out15 <- boxplot.stats(data$SP.DYN.AMRT.MA)$out
out_ind15 <- which(data$SP.DYN.AMRT.MA %in% c(out15))
length(out15) 
out16 <- boxplot.stats(data$NY.GDP.MKTP.KD.ZG)$out
out_ind16 <- which(data$NY.GDP.MKTP.KD.ZG %in% c(out16))
length(out16) 
out17 <- boxplot.stats(data$NY.GDP.PCAP.PP.CD)$out
out_ind17 <- which(data$NY.GDP.PCAP.PP.CD %in% c(out17))
length(out17) 
out18 <- boxplot.stats(data$SP.DYN.CBRT.IN)$out
out_ind18 <- which(data$SP.DYN.CBRT.IN %in% c(out18))
length(out18) 
out19 <- boxplot.stats(data$NY.GNP.PCAP.PP.CD)$out
out_ind19 <- which(data$NY.GNP.PCAP.PP.CD %in% c(out19))
length(out19) 
out20 <- boxplot.stats(data$SE.ADT.LITR.ZS)$out
out_ind20 <- which(data$SE.ADT.LITR.ZS %in% c(out20))
length(out20) 

######################### PREPROCESSING (Q2) ######################### 

### Missing Values

#number of rows with complete data
sum(complete.cases(data))

#imputing missing values
imputations = mice(data,m = 10,method = "rf",seed = 22)

#summary of imputed values
summary(imputations)

complete_dataset = complete(imputations)

######################### COLLINEARITY ANALYSIS (Q3) ######################### 

# Model 1 -> All predictor variables
model1 <-lm(SP.DYN.LE00.IN ~ ., data=complete_dataset)
summary(model1)

# Create data.frame of all predictors
X<-subset(complete_dataset, select = -c(SP.DYN.LE00.IN))

# Correlation plot of all variables
seatpos.corr<-cor(X)
corrplot.mixed(seatpos.corr, lower.col = "black", number.cex = .5, tl.pos = "lt")

# Variation Inflation Factors of predictors
vif(X)

# Model 2 -> SP.DYN.AMRT.FE excluded
model2 <- lm(SP.DYN.LE00.IN ~ EG.ELC.ACCS.ZS + NY.ADJ.NNTY.KD.ZG + NY.ADJ.NNTY.KD + SE.PRM.UNER.ZS + SE.XPD.PRIM.ZS + SP.DYN.IMRT.IN + SE.ADT.LITR.ZS + SP.POP.GROW + SP.POP.TOTL + SE.PRM.CMPT.ZS + SH.XPD.CHEX.GD.ZS + SH.XPD.CHEX.PC.CD + SL.UEM.TOTL.NE.ZS + SP.DYN.AMRT.MA + NY.GDP.MKTP.KD.ZG + NY.GDP.PCAP.PP.CD + SP.DYN.CBRT.IN, data=complete_dataset)
summary(model2)

model_2_data <- subset(complete_dataset, select = -c(SP.DYN.AMRT.FE)) 
X2<-model_2_data
vif(X2) 

# Model 3 -> SP.DYN.AMRT.FE and SP.DYN.CBRT.IN excluded
model3 <- lm(SP.DYN.LE00.IN ~ EG.ELC.ACCS.ZS + NY.ADJ.NNTY.KD.ZG + NY.ADJ.NNTY.KD + SE.PRM.UNER.ZS + SE.XPD.PRIM.ZS + SP.DYN.IMRT.IN + SE.ADT.LITR.ZS + SP.POP.GROW + SP.POP.TOTL + SE.PRM.CMPT.ZS + SH.XPD.CHEX.GD.ZS + SH.XPD.CHEX.PC.CD + SL.UEM.TOTL.NE.ZS + SP.DYN.AMRT.MA + NY.GDP.MKTP.KD.ZG + NY.GDP.PCAP.PP.CD, data=complete_dataset)
summary(model3)

model_3_data <- subset(complete_dataset, select = -c(SP.DYN.AMRT.FE, SP.DYN.CBRT.IN))
X3<-model_3_data
vif(X3)

# Model 4 ->SP.DYN.AMRT.FE, SP.DYN.CBRT.IN and SP.DYN.IMRT.IN excluded
model4 <- lm(SP.DYN.LE00.IN ~ EG.ELC.ACCS.ZS + NY.ADJ.NNTY.KD.ZG + NY.ADJ.NNTY.KD + SE.PRM.UNER.ZS + SE.XPD.PRIM.ZS + SE.ADT.LITR.ZS + SP.POP.GROW + SP.POP.TOTL + SE.PRM.CMPT.ZS + SH.XPD.CHEX.GD.ZS + SH.XPD.CHEX.PC.CD + SL.UEM.TOTL.NE.ZS + SP.DYN.AMRT.MA + NY.GDP.MKTP.KD.ZG + NY.GDP.PCAP.PP.CD, data=complete_dataset)
summary(model4)

model_4_data <- subset (complete_dataset, select = -c(SP.DYN.AMRT.FE, SP.DYN.CBRT.IN, SP.DYN.IMRT.IN))
X4<-model_4_data
vif(X4)

# Anova tests
anova(model2,model3)
anova(model3, model4)
anova(model2, model4)


######################### MODELLING (Q4a) ######################### 

## Train Test Split

names(complete_dataset)[1] <- "Target"
set.seed(123)
split = sample.split(complete_dataset$Target , SplitRatio = .9)
data_train = subset(complete_dataset, split == TRUE)
data_test = subset(complete_dataset, split == FALSE)

## Random Forest Model

# Training
regressor_rf <- randomForest(Target~ ., 
                             data = data_train, 
                             tree = 300,
                             mtry = 8,
                             proximity = TRUE,
                             importance = TRUE) 

print(regressor_rf)

# Testing
y_pred_rf = predict(regressor_rf, newdata = data_test)

Pred_Actual_rf <- as.data.frame(cbind(Prediction = y_pred_rf, Actual = data_test$Target))

print(Pred_Actual_rf)
summary(Pred_Actual_rf)

# Plot
gg.rf <- ggplot(Pred_Actual_rf, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Random Forest Regression", x = "Actual life expectancy",
       y = "Predicted life expectancy") +
  theme(plot.title = element_text(family = "Lucida Sans", face = "bold", size = (15)), 
        axis.title = element_text(family = "Lucida Sans", size = (10)))
gg.rf

## Multiple Linear Regression

# Training
regressor_lm = lm(formula = Target ~ .,
                  data = data_train)
print(regressor_lm)

summary(regressor_lm)

# Testing
y_pred_lm = predict(regressor_lm, newdata = data_test)


Pred_Actual_lm <- as.data.frame(cbind(Prediction_lm = y_pred_lm, Actual_lm = data_test$Target))

# Plot 
gg.lm <- ggplot(Pred_Actual_lm, aes(Actual_lm, Prediction_lm )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Multiple Linear Regression", x = "Actual life expectancy",
       y = "Predicted life expectancy") +
  theme(plot.title = element_text(family = "Lucida Sans", face = "bold", size = (15)), 
        axis.title = element_text(family = "Lucida Sans", size = (10)))
gg.lm

## Support Vector Regressor (SVR)

# Training
regressor_svr = svm(formula = Target ~ .,
                    data = data_train,
                    type = 'eps-regression',
                    kernel = 'radial')

summary(regressor_svr)


# Test
y_pred_svr = predict(regressor_svr,  newdata = data_test)

Pred_Actual_svr <- as.data.frame(cbind(Prediction_svr = y_pred_svr, Actual_svr = data_test$Target))


# Plot 
gg.svr <- ggplot(Pred_Actual_svr, aes(Actual_svr, Prediction_svr )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "SVR", x = "Actual life expectancy",
       y = "Predicted life expectancy") +
  theme(plot.title = element_text(family = "Lucida Sans", face = "bold", size = (15)), 
        axis.title = element_text(family = "Lucida Sans", size = (10)))
gg.svr


######################### EVALUATION (Q4b) ######################### 

## Mean Square Error of all Models
MSE.rf <- sum((data_test$Target - y_pred_rf)^2)/nrow(data_test)
MSE.lm <- sum((data_test$Target - y_pred_lm)^2)/nrow(data_test)
MSE.svr <- sum((data_test$Target - y_pred_svr)^2)/nrow(data_test)

print(paste("Mean Squared Error (Random Forest regressor):", MSE.rf))
print(paste("Mean Squared Error (Multiple Linear Regression):", MSE.lm))
print(paste("Mean Squared Error (Support vector regressor):", MSE.svr))

## Mean Absolute Error of all models
MAE.rf <- mae(data_test$Target, y_pred_rf)
MAE.lm <- mae(data_test$Target, y_pred_lm)
MAE.svr <- mae(data_test$Target, y_pred_svr)

print(paste("Mean Absolute Error (Random Forest regressor):", MAE.rf))
print(paste("Mean Absolute Error (Multiple Linear Regression):", MAE.lm))
print(paste("Mean Absolute Error (Support vector regressor):", MAE.svr))

## Root Mean square error of all models
RMSE.rf <- rmse(data_test$Target, y_pred_rf)
RMSE.lm <- rmse(data_test$Target, y_pred_lm)
RMSE.svr <- rmse(data_test$Target, y_pred_svr)

print(paste("Root Mean Squared Error (Random Forest regressor):", RMSE.rf))
print(paste("Root Mean Squared Error (Multiple Linear Regression):", RMSE.lm))
print(paste("Root Mean Squared Error (Support vector regressor):", RMSE.svr))

## R-square error of all models
R2.rf <- R2(data_test$Target, y_pred_rf, form = "traditional")
R2.lm <- R2(data_test$Target, y_pred_lm, form = "traditional")
R2.svr <- R2(data_test$Target, y_pred_svr, form = "traditional")

print(paste("R-square error  (Random Forest regressor):", R2.rf))
print(paste("R-square error  (Multiple Linear Regression):", R2.lm))
print(paste("R-square error  (Support vector regressor):", R2.svr))


######################### PREDICTIONS (Q4C) ######################### 

library("readxl")
holdout_data <- read_excel('LifeExpectancyData2.xlsx')

# Switch names to match
colnames(holdout_data)[1] <- "Country.Name"
colnames(holdout_data)[2] <- "Country.Code"

# Store dropped columns
dropped_cols <- subset(holdout_data, select = c(Country.Name, Country.Code))

# Drop columns for now
holdout_data = subset(holdout_data, select = -c(Country.Name, Country.Code))

# Drop target colum
data_drop <- subset(complete_dataset, select = -Target)

# Add the holdout data to the original dataset
data_merged <- rbind(data_drop, holdout_data)

# Convert fields to numeric
data_merged <- sapply(data_merged, as.numeric)

# Impute the missing values using MICE
impute = mice(data_merged,m = 10,method = "rf",seed = 22)

# Provide a summary of the imputed values
summary(impute)

# Provides the merged dataset with the imputed values
holdout_data = complete(impute)

# Selects only the original holdout dataset observations
holdout_data = holdout_data[233:243,]

# Uses the SVR model to predict on the holdout dataset
svr_pred = predict(regressor_svr,  newdata = holdout_data)
# Used the Random Forest model to predict on the holdout dataset
rf_pred = predict(regressor_rf, newdata = holdout_data)
# Used the multiple linear regression model to predict on the holdout dataset
lm_pred = predict(regressor_lm, newdata = holdout_data)

# Adds the predictions from all of the models on to the holdout dataset as columns
holdout_data$SVR_Life_Expectancy_preds = svr_pred
holdout_data$LM_Life_Expectancy_preds = lm_pred
holdout_data$RF_Life_Expectancy_preds = rf_pred


holdout_data <- cbind(dropped_cols, holdout_data)


# Exports the holdout dataset with predictions into an xlsx file
write.xlsx(
  holdout_data,
  file = 'holdout_predictions.xlsx' ,
  sheetName = 'Sheet1',
  col.names = TRUE,
  row.names = FALSE
)

######################### ANOVA (Q5) ######################### 
library(countrycode)

complete_dataset <- cbind(descriptive_cols, complete_dataset)

complete_dataset <- complete_dataset[1:186,]

# Add continent and subcontinent column
complete_dataset$continent <- countrycode(sourcevar = complete_dataset[, "Country.Name"],
                              origin = "country.name",
                              destination = "continent")

complete_dataset$subcont <- countrycode(sourcevar = complete_dataset[, "Country.Name"],
                            origin = "country.name",
                            destination = "region")

complete_dataset <- within(complete_dataset, continent[subcont == 'Latin America & Caribbean'] <- 'South America')
complete_dataset <- within(complete_dataset, continent[subcont == 'North America'] <- 'North America')
complete_dataset <- within(complete_dataset, continent[Country.Name == 'Greenland'] <- 'North America')
complete_dataset <- within(complete_dataset, continent[Country.Name == 'Kosovo'] <- 'Europe')


complete_dataset$continent <- as.factor(complete_dataset$continent)

str(complete_dataset)

anova_data <- complete_dataset[1:186,]

anova_data <- subset(complete_dataset, select = -c(Country.Code, Country.Name))

levels(anova_data$continent)

group_by(anova_data, continent) %>%
  summarise(
    count = n(),
    mean = mean(Target, na.rm = TRUE),
    sd = sd(Target, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(anova_data, x = "continent", y = "Target", 
          color = "continent",
          order = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"),
          ylab = "continent", xlab = "Life Expectancy")



# Compute the analysis of variance
res.aov <- aov(Target ~ continent, data = complete_dataset)
# Summary of the analysis
summary(res.aov)


TukeyHSD(res.aov)
