####### Melospiza melodia phenotypic analysis
####### Caitlyn Oliver Brown
####### Last Update: 10 Dec 2022

## set working directory
setwd("C:/Users/brown/GitHub/melospiza-melodia-phenotype/")

## install packages and turn them on
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("devtools")
# install_github("fawda123/ggord")
library(devtools)
library(tidyverse)
library(ggplot2)
library(ggord)
source("C:/Users/brown/GitHub/CleanORNISFull/skinmeas-split-fun.R") ## function to clean skin measurements column

#### STEP 1: Data clean-up and manipulation

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
mm_select <- mm_full[,c(4,6,7,18,29:35)]
colnames(mm_select) <- c("SUBSPECIES","AGE","SEX","MASS","WCH","TL","TS","BL","BLH","BLW","SKL")

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

##### STEP 2: Summary Statistics

mm_final %>%
  group_by(SUBSPECIES) %>%
  summarize(mean_mass = mean(MASS))
mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(mean_WCH = mean(WCH))
mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(mean_TL = mean(TL))
mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(mean_TS = mean(TS))
mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(mean_BL = mean(BL))
mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(mean_BLH = mean(BLH))
mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(mean_BLW = mean(BLW))
mm_final %>% 
  group_by(SUBSPECIES) %>% 
  summarize(mean_SKL = mean(SKL))


###### STEP 3: Plotting- Box Plots

## Color Pallete (colorblind friendly) - from https://personal.sron.nl/~pault/data/colourschemes.pdf
# maxima = light blue
col.maxima <- c("#77AADD")
# sanaka = light cyan
col.sanaka <- c("#99DDFF")
# insignis = mint
col.insignis <- c("#44BB99")
# caurina = pear
col.caurina <- c("#BBCC33")
# rufina = olive
col.rufina <- c("#AAAA00")
col.pallete <- c(col.maxima, col.sanaka, col.insignis,col.caurina,col.rufina)

## Reorder for West to East distribution
mm_final$order <- factor(mm_final$SUBSPECIES, levels =c("maxima","sanaka","insignis","caurina", "rufina"))


par(mar = c(5, 5, 3, 1.5), mfrow=c(2,4))

plot(mm_final$order, mm_final$MASS, ylab = "Mass (g)", xlab = NULL, col = col.pallete)
plot(mm_final$order, mm_final$WCH, ylab = "Wing Chord (mm)", xlab = NULL, col = col.pallete)
plot(mm_final$order, mm_final$TL, ylab = "Tail Length (mm)", xlab = NULL, col = col.pallete)
plot(mm_final$order, mm_final$TS, ylab = "Tarsus Length (mm)", xlab = NULL, col = col.pallete)
legend("topright", bty="n",legend = c("maxima","sanaka","insignis","caurina","rufina"),fill = col.pallete,cex=0.8)
plot(mm_final$order, mm_final$BL, ylab = "Bill Length (mm)", xlab = NULL, col = col.pallete)
plot(mm_final$order, mm_final$BLH, ylab = "Bill Length Height (mm)", xlab = NULL, col = col.pallete)
plot(mm_final$order, mm_final$BLW, ylab = "Bill Length Width (mm)",xlab = NULL, col = col.pallete)
plot(mm_final$order, mm_final$SKL, ylab = "Skull Length (mm)",xlab = NULL, col = col.pallete)



dev.copy2pdf(file = "./melospiza-boxplots.pdf",
             width = 12, height = 8, bg = "white", compress = F, out.type = "pdf")


###### STEP 4: Plotting- PCA


mm_final_std <- scale(mm_final[,c(4:11)], scale = TRUE, center = T) ## z-transform data
mm_final_pca <- prcomp(mm_final_std, scale = TRUE) ## pca of transformed data

mm_final_pca$rotation <- mm_final_pca$rotation *9 ## make the arrows on the PCA longer

ggord(mm_final_pca, mm_final$order, 
      cols= col.pallete,
      xlims = c(-6,7), ylims = c(-10, 8),
      grp_title = "subspecies"
      ) +
  scale_shape_manual(values = c(21,25,22,23,24))

dev.copy2pdf(file = "./melospiza-pca.pdf", width = 10, height = 10, bg = "white", compress = F, out.type= "pdf")

