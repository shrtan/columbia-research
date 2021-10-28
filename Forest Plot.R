library(forestplot)
library(dplyr)
library(ggplot2)

setwd('/Users/Shreya/Documents/Research/')


data <- read.csv("Maternal South Africa v Northern Plains Alc and Dev.csv")
data <- as_tibble(data)

## Data Cleaning
data <- na.omit(data)
data <- data[data$Estimate != 0 & data$VariableName != "Education Missing" &
               data$VariableName!="Antenatal Care Visit Missing" & 
               data$VariableName!="Age At Mullen" &
               data$VariableName!="Female", ]
 
names(data)[1] <- "Location"
data$Characteristics <- factor(data$Characteristics, levels = c("Maternal", "Delivery and Child"))

#Tests and Locations
unique(data$Test)
unique(data$Location)

####
SA: Cognitive
SA_cog <- data[data$Location == "South Africa" & data$Test == "Cognitive",]

## FOREST PLOT

ci_temp <- paste("(", format(round(SA_cog$ciLow, 3), nsmall=3), 
                 ",", format(round(SA_cog$ciHigh, 3), nsmall=3), 
                 ")", sep = "")
cat_temp <- as.character(SA_cog$VariableName)
smd_temp <- format(round(SA_cog$Estimate,3), nsmall=3)


cat <- c("Categories", "", "Parity", cat_temp[1:3], "BMI", cat_temp[4:6],
         "Antenatal Care Visit", cat_temp[7:8], "Education", cat_temp[9:12],
         "Drinking", cat_temp[13:15], "Complications", cat_temp[16:18],
         "Birthweight", cat_temp[19:20])

smd <- c("SMD", "", "", smd_temp[1:3], "", smd_temp[4:6],
         "", smd_temp[7:8], "", smd_temp[9:12],
         "", smd_temp[13:15], "", smd_temp[16:18],
         "", smd_temp[19:20])


ci <- c("95% CI", "", "", ci_temp[1:3], "", ci_temp[4:6],
        "", ci_temp[7:8], "", ci_temp[9:12],
        "", ci_temp[13:15], "", ci_temp[16:18],
        "", ci_temp[19:20])

m <- c(NA, NA, NA, SA_cog$Estimate[1:3], NA, SA_cog$Estimate[4:6],
       NA, SA_cog$Estimate[7:8], NA, SA_cog$Estimate[9:12],
       NA, SA_cog$Estimate[13:15], NA, SA_cog$Estimate[16:18],
       NA, SA_cog$Estimate[19:20])

l <- c(NA, NA, NA, SA_cog$ciLow[1:3], NA, SA_cog$ciLow[4:6],
       NA, SA_cog$ciLow[7:8], NA, SA_cog$ciLow[9:12],
       NA, SA_cog$ciLow[13:15], NA, SA_cog$ciLow[16:18],
       NA, SA_cog$ciLow[19:20])

u <- c(NA, NA, NA, SA_cog$ciHigh[1:3], NA, SA_cog$ciHigh[4:6],
       NA, SA_cog$ciHigh[7:8], NA, SA_cog$ciHigh[9:12],
       NA, SA_cog$ciHigh[13:15], NA, SA_cog$ciHigh[16:18],
       NA, SA_cog$ciHigh[19:20])

sum <- c(rep(TRUE, 3), rep(FALSE, 3), TRUE, rep(FALSE, 3), TRUE, rep(FALSE, 2),
         TRUE, rep(FALSE, 4), TRUE, rep(FALSE, 3), TRUE, rep(FALSE, 3),
         TRUE, rep(FALSE, 2))

  
temp <- tibble(mean = m,
       lower = l,
       upper = u,
       study = cat,
       confint=ci,
       smd = smd)

temp %>%
  forestplot(labeltext = c(study, smd, confint),
             clip = c(-0.8, 0.4),
             is.summary = sum,
             new_page = TRUE,
             col = fpColors(box = "royalblue",
                            line = "darkblue"),
             txt_gp = fpTxtGp(cex = 0.9, label = gpar(fontfamily = "serif")),
             graph.pos = 2,
             boxsize = 0.3,
             title = "SA: Cognitive")

#########################################################################################


## ggplot 1
SA_cog$VariableName <- factor(SA_cog$VariableName, levels=rev(unique(SA_cog$VariableName)))
SA_cog$Characteristics <- as.factor(SA_cog$Characteristics)

confint <- paste("[", format(round(SA_cog$ciLow, 5), nsmall=5), ", ", format(round(SA_cog$ciHigh, 5), nsmall=5), "]", sep = "")

ggplot(data=SA_cog, aes(x=VariableName, y=Estimate, ymin=ciLow, ymax=ciHigh, )) +
  geom_pointrange(aes(col = Characteristics)) +
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  ylab("") +
  xlab("") +
  ggtitle("South Africa: Cognitive") +
  theme_classic()  + # use a white background 
  geom_text(aes(y = -1.2, label = format(round(SA_cog$Estimate,5), nsmall = 5), hjust=0), size = 3) +
  geom_text(aes(y = -1, label = confint, hjust=0), size = 3) + 
  coord_flip() +  # flip coordinates (puts labels on y axis)
  theme(axis.line=element_blank(), axis.ticks.x = element_blank(), 
      axis.ticks.y = element_blank(), axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8)) + 
  scale_y_continuous(breaks = c(-0.7, -0.5, -0.3, -0.1, 0.1, 0.3))
  
 #Another way -- ggplot 2
ggplot(SA_cog, aes(y=VariableName, x=Estimate, label=RiskFactor)) +
  geom_point(size=4, shape=19) +
  geom_errorbarh(aes(xmin=ciLow, xmax=ciHigh), height=1) +
  geom_vline(xintercept=0, linetype='longdash') +
  geom_point(aes(colour = factor(Characteristics))) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.text.x= element_text(size = 12)) +
  theme(axis.text.y= element_text(size = 12)) +
  theme(plot.title = element_text(hjust = 0.5))
