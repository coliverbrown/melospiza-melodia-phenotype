####### Melospiza melodia phenotypic analysis
####### Caitlyn Oliver Brown
####### First Update: 10 Dec 2022
####### Last Update: 18 Aug 2023

## set working directory
setwd("C:/Users/brown/GitHub/melospiza-melodia-phenotype/")

## install packages and turn them on
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("devtools")
# install_github("fawda123/ggord")
# install.packages("corrplot")
library(corrplot)
library(devtools)
library(tidyverse)
library(ggplot2)
library(ggord)
library(ggpubr)
source("C:/Users/brown/GitHub/CleanORNISFull/skinmeas-split-fun.R") ## function to clean skin measurements column

########## Data clean-up and manipulation ##########

## Import raw data

mm_full <- read.csv("melospiza-melodia-fullflat-raw.csv") ##unedited raw data

## Use the skin.meas.split function to separate SKINMEAS column and add NAs

for (i in 1:dim(mm_full)[1]){
  temp <- skin.meas.split(mm_full[i,"SKINMEAS"]) 
  if(length(temp) >= 1) {
    mm_full[i,29:35] <- temp
    }
  else {
    mm_full[i,29:35]<- c(rep(NA,7))
    }
 
} 

## Select columns of interest
mm_select <- mm_full[,c(1,4,6,7,18,29:35)]
colnames(mm_select) <- c("CATSFX","SUBSPECIES","AGE","SEX","MASS","WCH","TL","TS","BL","BLH","BLW","SKL")

## Filter data frame to only include adults, after hatch years, and males

mm_full_filtered <- mm_select %>% 
  filter(AGE == "AD" | AGE == "AHY") %>% 
  filter(SEX== "M")

## Create separate data frames for each subspecies
mm_maxima_full <- mm_full_filtered %>% filter(SUBSPECIES == "maxima")
mm_sanaka_full <- mm_full_filtered %>% filter(SUBSPECIES == "sanaka")
mm_insignis_full <- mm_full_filtered %>% filter(SUBSPECIES == "insignis")
mm_caurina_full <- mm_full_filtered %>% filter(SUBSPECIES == "caurina")
mm_rufina_full <- mm_full_filtered %>% filter(SUBSPECIES == "rufina")


## Remove outliers to account for human error in measurements

## Make new data frames for each subspecies and pull out numerical data
# maxima
mm_maxima <- mm_full_filtered %>% 
  filter(SUBSPECIES == "maxima") %>% 
  select(MASS, WCH, TL, TS, BL, BLH, BLW, SKL)
z_score_maxima <- scale(mm_maxima)  ## Compute z scores
outliers_maxima <- which(z_score_maxima >=3)    ## tag the rows containing outliers
mm_maxima_clean <- mm_maxima_full[-c(outliers_maxima),]  ## make new dataframe without outliers

## Repeat above steps for each subspecies
# sanaka
mm_sanaka <- mm_full_filtered %>% 
  filter(SUBSPECIES == "sanaka") %>% 
  select(MASS, WCH, TL, TS, BL, BLH, BLW, SKL)
z_score_sanaka <- scale(mm_sanaka)  ## Compute z scores
outliers_sanaka <- which(z_score_sanaka >=3)    ## tag the rows containing outliers
mm_sanaka_clean <- mm_sanaka_full[-c(outliers_sanaka),]  ## make new dataframe without outliers

# insignis
mm_insignis <- mm_full_filtered %>% 
  filter(SUBSPECIES == "insignis") %>% 
  select(MASS, WCH, TL, TS, BL, BLH, BLW, SKL)
z_score_insignis <- scale(mm_insignis)  ## Compute z scores
outliers_insignis <- which(z_score_insignis >=3)    ## tag the rows containing outliers
# mm_insignis_clean <- mm_insignis_full[-c(outliers_insignis),] for some reason, because the outlier is empty, this is making an empty data frame
# going to skip this step and just move all values into the next step

# caurina
mm_caurina <- mm_full_filtered %>% 
  filter(SUBSPECIES == "caurina") %>% 
  select(MASS, WCH, TL, TS, BL, BLH, BLW, SKL)
z_score_caurina <- scale(mm_caurina)  ## Compute z scores
outliers_caurina <- which(z_score_caurina >=3)    ## tag the rows containing outliers
mm_caurina_clean <- mm_caurina_full[-c(outliers_caurina),]  ## make new dataframe without outliers

# rufina
mm_rufina <- mm_full_filtered %>% 
  filter(SUBSPECIES == "rufina") %>% 
  select(MASS, WCH, TL, TS, BL, BLH, BLW, SKL)
z_score_rufina <- scale(mm_rufina)  ## Compute z scores
outliers_rufina <- which(z_score_rufina >=3)    ## tag the rows containing outliers
mm_rufina_clean <- mm_rufina_full[-c(outliers_rufina),]  ## make new dataframe without outliers

## Rejoin the data frames into one

mm_full_clean <- bind_rows(mm_maxima_clean,mm_sanaka_clean,mm_insignis_full,mm_caurina_clean,mm_rufina_clean)

## Finally remove NAs
mm_final <- mm_full_clean %>% 
  filter(!is.na(MASS)) %>% 
  filter(!is.na(WCH)) %>% 
  filter(!is.na(TL)) %>% 
  filter(!is.na(TS)) %>% 
  filter(!is.na(BL)) %>% 
  filter(!is.na(BLW)) %>% 
  filter(!is.na(BLH)) %>% 
  filter(!is.na(SKL))

## export mm_final dataframe as a csv
write.csv(mm_final, "melospiza-melodia-filtered.csv", row.names = FALSE)

## bring in mm_final again (to skip all the above steps)
mm_final <- read.csv("melospiza-melodia-filtered.csv") ##edited raw data


########## Summary Statistics ##########
### Mass statistics
mm_final %>%
  group_by(SUBSPECIES) %>%
  summarize(mean_mass = mean(MASS))

#SUBSPECIES mean_mass
#<chr>          <dbl>
#1 caurina         28.4
#2 insignis        41.0
#3 maxima          46.2
#4 rufina          27.9
#5 sanaka          45.4

mm_final %>%
  group_by(SUBSPECIES) %>% 
  summarise(sd_mass = sd(MASS))
# SUBSPECIES sd_mass
# <chr>        <dbl>
#   1 caurina       1.77
# 2 insignis      2.61
# 3 maxima        2.71
# 4 rufina        1.75
# 5 sanaka        2.06

### WCH statistics 
mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(mean_WCH = mean(WCH))
#SUBSPECIES mean_WCH
#<chr>         <dbl>
#1 caurina        72.6
#2 insignis       80.9
#3 maxima         82.7
#4 rufina         69.7
#5 sanaka         83.3

mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(sd_WCH = sd(WCH))
# SUBSPECIES sd_WCH
# <chr>       <dbl>
#   1 caurina      1.43
# 2 insignis     2.14
# 3 maxima       3.04
# 4 rufina       3.03
# 5 sanaka       2.45

### TL Statistics
mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(mean_TL = mean(TL))
#SUBSPECIES mean_TL
#<chr>        <dbl>
#1 caurina       67.1
#2 insignis      75.4
#3 maxima        78.9
#4 rufina        65.8
#5 sanaka        77.8

mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(sd_TL = sd(TL))
# SUBSPECIES sd_TL
# <chr>      <dbl>
#   1 caurina     4.28
# 2 insignis    4.52
# 3 maxima      4.27
# 4 rufina      3.01
# 5 sanaka      4.28

### TS statistics
mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(mean_TS = mean(TS))
#SUBSPECIES mean_TS
#<chr>        <dbl>
#1 caurina       24.2
#2 insignis      26.3
#3 maxima        27.6
#4 rufina        24.2
#5 sanaka        27.0

mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(sd_TS = sd(TS))
# SUBSPECIES sd_TS
# <chr>      <dbl>
#   1 caurina    1.32 
# 2 insignis   0.530
# 3 maxima     2.04 
# 4 rufina     1.41 
# 5 sanaka     0.985

### BL Statistics
mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(mean_BL = mean(BL))
#SUBSPECIES mean_BL
#<chr>        <dbl>
#1 caurina       10.7
#2 insignis      11.9
#3 maxima        12.4
#4 rufina        10.8
#5 sanaka        12.4

mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(sd_BL = sd(BL))
# SUBSPECIES sd_BL
# <chr>      <dbl>
#   1 caurina    0.600
# 2 insignis   0.570
# 3 maxima     1.34 
# 4 rufina     2.75 
# 5 sanaka     0.509

### BLH Statistics
mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(mean_BLH = mean(BLH))
#SUBSPECIES mean_BLH
#<chr>         <dbl>
#1 caurina        6.09
#2 insignis       6.46
#3 maxima         7.53
#4 rufina         6.37
#5 sanaka         6.89

mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(sd_BLH = sd(BLH))
# SUBSPECIES sd_BLH
# <chr>       <dbl>
#   1 caurina     0.445
# 2 insignis    0.222
# 3 maxima      1.08 
# 4 rufina      0.868
# 5 sanaka      1.04 

### BLW statistics
mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(mean_BLW = mean(BLW))
#SUBSPECIES mean_BLW
#<chr>         <dbl>
#1 caurina        5.06
#2 insignis       5.26
#3 maxima         5.99
#4 rufina         5.1 
#5 sanaka         5.56

mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(sd_BLW = sd(BLW))
# SUBSPECIES sd_BLW
# <chr>       <dbl>
#   1 caurina     0.341
# 2 insignis    0.317
# 3 maxima      0.772
# 4 rufina      0.448
# 5 sanaka      0.559

### SKL Statistics
mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(mean_SKL = mean(SKL))
#SUBSPECIES mean_SKL
#<chr>         <dbl>
#1 caurina        35.7
#2 insignis       38.0
#3 maxima         38.8
#4 rufina         34.8
#5 sanaka         38.8

mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(sd_SKL = sd(SKL))
# SUBSPECIES sd_SKL
# <chr>       <dbl>
#   1 caurina     1.93 
# 2 insignis    0.952
# 3 maxima      1.54 
# 4 rufina      1.41 
# 5 sanaka      0.804

########## STEP 3: Plotting- Box Plots ##########

## Color Pallete (colorblind friendly) - from https://personal.sron.nl/~pault/data/colourschemes.pdf
# maxima = indigo
col.maxima <- c("#332288")
# sanaka = teal
col.sanaka <- c("#44AA99")
# insignis = green
col.insignis <- c("#117733")
# caurina = sand
col.caurina <- c("#DDCC77")
# rufina = rose
col.rufina <- c("#CC6677")
col.pallete <- c(col.maxima, col.sanaka, col.insignis,col.caurina,col.rufina)

## Reorder for West to East distribution
mm_final$order <- factor(mm_final$SUBSPECIES, levels =c("maxima","sanaka","insignis","caurina", "rufina"))


########## Plot boxplots with ggplot ##########


m <- ggplot(data=mm_final, aes(order, MASS))+
      geom_boxplot(color = 'black', fill = col.pallete)+
      labs(x = "Subspecies",y= "Mass (g)") 
    

w <- ggplot(data=mm_final, aes(order, WCH))+
     geom_boxplot(color = 'black', fill = col.pallete)+
     labs(x = "Subspecies", y = "Wing Chord (mm)", show.legend = T) 

t <- ggplot(data=mm_final, aes(order, TL))+
     geom_boxplot(color = 'black', fill = col.pallete)+ 
    labs(y = "Tail Length (mm)") 

s <- ggplot(data=mm_final, aes(order, TS))+
  geom_boxplot(color = 'black', fill = col.pallete)+
  labs(y = "Tarsus Length(mm)") 
 
b <- ggplot(data=mm_final, aes(order, BL))+
  geom_boxplot(color = 'black', fill = col.pallete)+
  labs(y = "Bill Length(mm)") 

h <- ggplot(data=mm_final, aes(order, BLH))+
  geom_boxplot(color = 'black', fill = col.pallete)+
  labs(y = "Bill Height(mm)") 

d <- ggplot(data=mm_final, aes(order, BLW))+
  geom_boxplot(color = 'black', fill = col.pallete)+
  labs(y = "Bill Width (mm)") 

k <- ggplot(data=mm_final, aes(order, SKL))+
  geom_boxplot(color = 'black', fill = col.pallete)+
  labs(y = "Skull Length (mm)") 

# theme_set(theme_bw())
theme_update(panel.background = element_rect(fill = "white", color = "black"),
             axis.title.x = element_blank(),
             axis.text.x = element_text(size = 12),
             axis.title.y = element_text(size =15),
             axis.text.y =element_text(size=12))


figure <-ggarrange(m, w, t, s, b, h, d, k,
                    labels = c("a", "b", "c", "d", "e", "f","g","h"))

figure

dev.copy2pdf(file = "./melospiza-boxplots5.pdf",
             width = 11, height = 8.5, bg = "white", compress = T, out.type = "pdf")


########## Plotting- PCA ##########


mm_final_std <- scale(mm_final[,c(5:12)], scale = TRUE, center = T) ## z-transform data
mm_final_pca <- prcomp(mm_final_std, scale = TRUE) ## pca of transformed data

mm_final_pca$rotation <- mm_final_pca$rotation *9 ## make the arrows on the PCA longer



ggord(mm_final_pca, mm_final$order, 
      cols= col.pallete,
      xlims = c(-6,7), ylims = c(-10, 8), veclsz = 0.75,
      grp_title = "subspecies"
      ) +
  scale_shape_manual(values = c(21,25,22,23,24))



dev.copy2pdf(file = "./pca_plot.pdf", width = 8.5, height = 11, bg = "white", compress = F, out.type= "pdf")

ggsave("pca_plot.png", plot=last_plot(), device=NULL, path=NULL,
       scale=1, width=12, height=8, dpi=300, limitsize=TRUE, bg = "white")


leg <- get_legend(pca)
legend <- as_ggplot(leg)


########## MANOVA ##########

# set dependent and independent variables

dep_vars <- cbind(mm_final$MASS, mm_final$BL, mm_final$BLH, mm_final$BLW)
ind_vars <- mm_final$SUBSPECIES

# Run MANOVA
manova_model <- manova(dep_vars ~ ind_vars, data = mm_final)
summary(manova_model)
#           Df Pillai approx F num Df den Df    Pr(>F)    
#ind_vars    4 1.0286   19.213     16    888 < 2.2e-16 ***
#   Residuals 222                                            
# ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# The p-value is super small (2.26e-16) so I can reject the null hypothesis for the alternative- at least one group differs from the rest

# ANOVA and Tukey's Post-hoc test for all measurements
mass.aov <- aov(MASS ~ SUBSPECIES, data = mm_final) ## anova for mass
summary(mass.aov)

#              Df Sum Sq Mean Sq F value Pr(>F)    
#SUBSPECIES    4  13711    3428   593.3 <2e-16 ***
#Residuals   222   1283       6    

mass.tuk <- TukeyHSD(mass.aov)
mass.tuk

#Tukey multiple comparisons of means
#95% family-wise confidence level

#Fit: aov(formula = MASS ~ SUBSPECIES, data = mm_final)

#$SUBSPECIES
#                     diff        lwr         upr     p adj
#insignis-caurina  12.5147059   9.880108  15.1493042 0.0000000 *
#maxima-caurina    17.7211575  16.011413  19.4309021 0.0000000 *
#rufina-caurina    -0.5621234  -2.469140   1.3448937 0.9271467 
#sanaka-caurina    16.9475630  14.993223  18.9019033 0.0000000 *
#maxima-insignis    5.2064516   3.033257   7.3796458 0.0000000 *
#rufina-insignis  -13.0768293 -15.408408 -10.7452502 0.0000000 *
#sanaka-insignis    4.4328571   2.062416   6.8032985 0.0000059 *
#rufina-maxima    -18.2832809 -19.474238 -17.0923236 0.0000000 *
#sanaka-maxima     -0.7735945  -2.038944   0.4917556 0.4475190
#sanaka-rufina     17.5096864  15.988306  19.0310668 0.0000000 *


wch.aov <- aov(WCH ~ SUBSPECIES, data = mm_final)
summary(wch.aov)

#              Df Sum Sq Mean Sq F value Pr(>F)    
#SUBSPECIES    4   6595    1649   205.5 <2e-16 ***
#Residuals   222   1781       8 

wch.tuk <- TukeyHSD(wch.aov)
wch.tuk

#$SUBSPECIES
#                       diff         lwr         upr     p adj
#insignis-caurina   8.2952941   5.1904031  11.4001851 0.0000000 *
#maxima-caurina    10.1780361   8.1630911  12.1929810 0.0000000 *
#rufina-caurina    -2.8793400  -5.1267720  -0.6319081 0.0046576 *
#sanaka-caurina    10.7495798   8.4463773  13.0527824 0.0000000 *
#maxima-insignis    1.8827419  -0.6783814   4.4438653 0.2589621 
#rufina-insignis  -11.1746341 -13.9224150  -8.4268533 0.0000000 *
#sanaka-insignis    2.4542857  -0.3392947   5.2478661 0.1147610
#rufina-maxima    -13.0573761 -14.4609269 -11.6538253 0.0000000 *
#sanaka-maxima      0.5715438  -0.9196794   2.0627669 0.8296181
#sanaka-rufina     13.6289199  11.8359632  15.4218765 0.0000000 *


tail.aov <- aov(TL~ SUBSPECIES, data = mm_final)
summary(tail.aov)

#             Df Sum Sq Mean Sq F value Pr(>F)    
#SUBSPECIES    4   6628  1657.0   99.27 <2e-16 ***
#Residuals   222   3706    16.7     

tail.tuk <- TukeyHSD(tail.aov)
tail.tuk

#Tukey multiple comparisons of means
#95% family-wise confidence level

#Fit: aov(formula = TL ~ SUBSPECIES, data = mm_final)

#$SUBSPECIES
#diff         lwr        upr     p adj
#insignis-caurina   8.240588   3.7624110  12.718765 0.0000086 *
#maxima-caurina    11.756072   8.8499216  14.662223 0.0000000 *
#rufina-caurina    -1.302582  -4.5440484   1.938883 0.8036788
#sanaka-caurina    10.676303   7.3543988  13.998206 0.0000000 *
#maxima-insignis    3.515484  -0.1784185   7.209386 0.0706244 
#rufina-insignis   -9.543171 -13.5062888  -5.580053 0.0000000 *
#sanaka-insignis    2.435714  -1.5934603   6.464889 0.4592734 
#rufina-maxima    -13.058655 -15.0829927 -11.034316 0.0000000 *
#sanaka-maxima     -1.079770  -3.2305573   1.071018 0.6406453
#sanaka-rufina     11.978885   9.3929078  14.564862 0.0000000 *


tar.aov <- aov(TS ~ SUBSPECIES, data = mm_final)
summary(tar.aov)

#             Df Sum Sq Mean Sq F value Pr(>F)    
#SUBSPECIES    4  459.5  114.87   38.82 <2e-16 ***
#Residuals   222  656.9    2.96

tar.tuk <- TukeyHSD(tar.aov)
tar.tuk

#Tukey multiple comparisons of means
#95% family-wise confidence level

#Fit: aov(formula = TS ~ SUBSPECIES, data = mm_final)

#$SUBSPECIES
#diff        lwr        upr     p adj
#insignis-caurina  2.0188235  0.1333658  3.9042812 0.0291350 *
#maxima-caurina    3.3676945  2.1441109  4.5912780 0.0000000 *
#rufina-caurina   -0.0143472 -1.3791094  1.3504150 0.9999998 
#sanaka-caurina    2.7931092  1.3944801  4.1917384 0.0000011 *
#maxima-insignis   1.3488710 -0.2063816  2.9041236 0.1231446 
#rufina-insignis  -2.0331707 -3.7017719 -0.3645696 0.0083200
#sanaka-insignis   0.7742857 -0.9221274  2.4706988 0.7187650 
#rufina-maxima    -3.3820417 -4.2343536 -2.5297298 0.0000000 *
#sanaka-maxima    -0.5745853 -1.4801366  0.3309661 0.4085809
#sanaka-rufina     2.8074564  1.7186762  3.8962367 0.0000000 *

bl.aov <- aov(BL ~ SUBSPECIES, data = mm_final)
summary(bl.aov)

#             Df Sum Sq Mean Sq F value   Pr(>F)    
#SUBSPECIES    4  118.8   29.71   12.17 5.79e-09 ***
#Residuals   222  541.8    2.44   

bl.tuk <- TukeyHSD(bl.aov)
bl.tuk

#$SUBSPECIES
#diff        lwr        upr     p adj
#insignis-caurina  1.22235294 -0.4899409  2.9346468 0.2874648
#maxima-caurina    1.72590133  0.6146939  2.8371088 0.0002764 *
#rufina-caurina    0.07503587 -1.1643841  1.3144558 0.9998266
#sanaka-caurina    1.71949580  0.4493193  2.9896723 0.0022991 *
#maxima-insignis   0.50354839 -0.9088670  1.9159638 0.8638273
#rufina-insignis  -1.14731707 -2.6626709  0.3680367 0.2313869
#sanaka-insignis   0.49714286 -1.0434686  2.0377543 0.9013005
#rufina-maxima    -1.65086546 -2.4248995 -0.8768314 0.0000002 *
#sanaka-maxima    -0.00640553 -0.8287894  0.8159783 1.0000000
#sanaka-rufina     1.64445993  0.6556753  2.6332446 0.0000774 *



blh.aov <- aov(BLH ~ SUBSPECIES, data = mm_final)
summary(blh.aov)

#             Df Sum Sq Mean Sq F value   Pr(>F)    
#SUBSPECIES    4   67.3   16.83   17.35 2.13e-12 ***
#Residuals   222  215.3    0.97 

blh.tuk <- TukeyHSD(blh.aov)
blh.tuk

#$SUBSPECIES
#                       diff           lwr        upr     p adj
#insignis-caurina  0.36588235 -0.7136310589  1.4453958 0.8841383
#maxima-caurina    1.43733397  0.7367747122  2.1378932 0.0000005 *
#rufina-caurina    0.27905308 -0.5023375986  1.0604438 0.8630936
#sanaka-caurina    0.80016807 -0.0006130246  1.6009492 0.0502820 
#maxima-insignis   1.07145161  0.1809962014  1.9619070 0.0095384 *
#rufina-insignis  -0.08682927 -1.0421820680  0.8685235 0.9991335
#sanaka-insignis   0.43428571 -0.5369907288  1.4055622 0.7340169
#rufina-maxima    -1.15828088 -1.6462696449 -0.6702921 0.0000000 *
#sanaka-maxima    -0.63716590 -1.1556367180 -0.1186951 0.0075680 *
#sanaka-rufina     0.52111498 -0.1022630107  1.1444930 0.1490081

blw.aov <- aov(BLW ~ SUBSPECIES, data= mm_final)
summary(blw.aov)

#             Df Sum Sq Mean Sq F value   Pr(>F)    
#SUBSPECIES    4  34.54   8.635   20.24 3.15e-14 ***
#Residuals   222  94.71   0.427  

blw.tuk <- TukeyHSD(blw.aov)
blw.tuk

#$SUBSPECIES
#                   diff         lwr         upr     p adj
#insignis-caurina  0.19529412 -0.52061672  0.91120496 0.9442315 
#maxima-caurina    0.92803605  0.46343973  1.39263238 0.0000011 *
#rufina-caurina    0.03529412 -0.48290793  0.55349616 0.9997232
#sanaka-caurina    0.49815126 -0.03291010  1.02921262 0.0776003
#maxima-insignis   0.73274194  0.14221043  1.32327344 0.0067882 *
#rufina-insignis  -0.16000000 -0.79357010  0.47357010 0.9575097
#sanaka-insignis   0.30285714 -0.34127319  0.94698747 0.6957230
#rufina-maxima    -0.89274194 -1.21636593 -0.56911794 0.0000000 *
#sanaka-maxima    -0.42988479 -0.77372385 -0.08604573 0.0062225 *
#sanaka-rufina     0.46285714  0.04944583  0.87626846 0.0196036 *


skl.aov <- aov(SKL ~ SUBSPECIES, data = mm_final)
summary(skl.aov)

#             Df Sum Sq Mean Sq F value Pr(>F)    
#SUBSPECIES    4  602.3  150.58   72.84 <2e-16 ***
#Residuals   222  458.9    2.07 

skl.tuk <- TukeyHSD(skl.aov)
skl.tuk

#$SUBSPECIES
#diff        lwr        upr     p adj
#insignis-caurina  2.29647059  0.7205179  3.8724232 0.0007908 *
#maxima-caurina    3.02727704  2.0045494  4.0500047 0.0000000 *
#rufina-caurina   -0.96743185 -2.1081631  0.1732994 0.1385576
#sanaka-caurina    3.09932773  1.9302889  4.2683666 0.0000000 *
#maxima-insignis   0.73080645 -0.5691455  2.0307584 0.5336213
#rufina-insignis  -3.26390244 -4.6585964 -1.8692085 0.0000000 *
#sanaka-insignis   0.80285714 -0.6150833  2.2207976 0.5264478
#rufina-maxima    -3.99470889 -4.7071106 -3.2823072 0.0000000 *
#sanaka-maxima     0.07205069 -0.6848509  0.8289523 0.9989603
#sanaka-rufina     4.06675958  3.1567068  4.9768124 0.0000000 *


########## Correlations ##########
mm_final_num <- mm_final[,5:12]
mm_final_cor <- cor(mm_final_num)

corrplot(mm_final_cor, method = "ellipse")
dev.copy2pdf(file = "./melospiza-correlation-plot.pdf", width = 10, height =10, bg = "white", compress = F, out.type= "pdf")

### because a lot of variables are correlated with mass, I'm going to remove those and use Mass, BL, BLH, BLW in the next PCA

mm_reduced_std <- scale(mm_final[,c(5,9:11)], scale = TRUE, center = T)
mm_reduced_pca <- prcomp(mm_reduced_std, scale = TRUE)
mm_reduced_pca$rotation <- mm_reduced_pca$rotation * 7

ggord(mm_reduced_pca, mm_final$order, 
      cols= col.pallete,
      xlims = c(-7,7), ylims = c(-12, 8),
      grp_title = "subspecies"
) +
  scale_shape_manual(values = c(21,25,22,23,24))

dev.copy2pdf(file = "./melospiza-reduced-vari-pca.pdf", width = 10, height = 12, bg = "white", compress = F, out.type = "pdf")

