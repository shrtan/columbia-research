library(forestplot)
library(dplyr)
library(tidyverse)
library(datamart)

setwd('/Users/Shreya/Documents/Research/')


data <- read.csv("Maternal South Africa v Northern Plains Alc and Dev.csv")
data <- as_tibble(data)

#Data Cleaning
data <- na.omit(data)
data <- data[data$Estimate != 0 & data$VariableName != "Education Missing" &
               data$VariableName!="Antenatal Care Visit Missing" & 
               data$VariableName!="Age At Mullen" & 
               !(data$VariableName == "Female" & data$Characteristics == "Maternal"), ]


names(data)[1] <- "Location"

#Scores and Locations
unique(data$Test)
unique(data$Location)



#confidence intervals formatted
ci_values <- function(low, high) {
  paste("(", format(round(low, 2), nsmall=2), 
        ",", format(round(high, 2), nsmall=2), 
        ")", sep = "")
}

#smd values
smd_value <- function(smd) {
  format(round(smd,2), nsmall=2)
}


#formatting Variables
fv <- function(s) {
  #bmi
  if(grepl("BMI", s, fixed = TRUE)) {
    t <- str_remove(s, "BMI ")
    return(paste("  ", t, sep =""))
  }
  
  #parity
  if(grepl("Parity", s, fixed = TRUE)) {
    t <- substr(s, nchar(s), nchar(s))
    t <- ifelse(t == "3", ">3", t)
    return(paste("  ", t, sep =""))
  }
  
  #antenatal
  if(grepl("Antenatal", s, fixed = TRUE)) {
    t <- str_remove(s, "Antenatal Care Visit ")
    return(paste("  ", t, sep =""))
  }
  
  #birthweight
  if(grepl("Birthweight", s, fixed = TRUE)) {
    t <- gsub("[^0-9<-]", "", s)
    t <- ifelse(t == "<2000", paste("Very low birthweight (", t, "g)", sep =""),
                paste("Low-moderate birthweight (", t, "g)", sep =""))
    t <- strcap(t)
    return(paste("  ", t, sep =""))
  }
  
  #height
  if(grepl("Height", s, fixed = TRUE)) {
    t <- str_remove(s, "Maternal Height ")
    return(paste("  ", t, "cm", sep =""))
  }
  
  #marital status
  if(grepl("Marital", s, fixed = TRUE)) {
    return("Married")
  }
  
  #employment
  if(grepl("Employement", s, fixed = TRUE)) {
    return("Employed")
  }
  
  #depression
  if(grepl("Depression", s, fixed = TRUE)) {
    loc <- gregexpr(pattern ='Score', s)[[1]][1]
    part1 <- strcap(substr(s, 1,loc-3))
    part2 <- substr(s, loc+6, nchar(s))
    t <- paste("  ", part1, " (EPDS score ", part2, ")", sep="")
    return(t)
  }
  
  #age
  if(grepl("Maternal Age", s, fixed = TRUE)) {
    t <- str_remove(s, "Maternal Age ")
    return(paste("  ", t, sep =""))
  }
  
  else {
    if((s != "Resuscitated At Birth") & (s!= "NICU Admission") & (s != "Augmented Labor") &
       (s != "Crowding Index") & (s != "Female") & (s != "Gestational Hypertension") &
       (s != "Gestational Diabetes Mellitus") & (s != "Preeclampsia") &
       (s != "Induced Labor") & (s != "Cesarean Section")) {
      t <- strcap(s)
      return(paste("  ", t, sep =""))
    }
    else {
      if(s == "Female") {
        return("Female sex")
      }
      if(s == "NICU Admission") {
        return("NICU admission")
      }
      else {
        return(strcap(s))
      }
    }
  }
}


#############################################################################

#SA - Cognitive

SA_cog <- data[data$Location == "South Africa" & data$Test == "Cognitive",]


SA_cog$VariableName <- sapply(SA_cog$VariableName, fv)

ci_temp <- ci_values(SA_cog$ciLow, SA_cog$ciHigh)
smd_temp <- smd_value(SA_cog$Estimate)
cat_temp <- as.character(SA_cog$VariableName)



cat <- c("", "Maternal factors", 
         "Prior pregnancies", "  0 (ref)", cat_temp[1:3], 
         "Body Mass Index (kg/m²)", cat_temp[4], "  18.5-25 (ref)", cat_temp[5:6],
         "Antenatal care visit", cat_temp[7:8], "  >6 (ref)",
         "Education", cat_temp[9:11], "  More than high school (ref)",
         cat_temp[12], 
         "Prenatal drinking", cat_temp[13:15], "  No drinking (ref)",
         "", "Delivery and child factors", 
         cat_temp[16:19],
         "Birthweight", cat_temp[20:21], "  Normal birthweight (>2500g) (ref)")

smd <- c("SMD", "", "", "", smd_temp[1:3], "", smd_temp[4], "", smd_temp[5:6],
         "", smd_temp[7:8], "", "", smd_temp[9:11], "", smd_temp[12], 
         "", smd_temp[13:15], "", "", "", smd_temp[16:19],
         "", smd_temp[20:21], "")


ci <- c("95% CI", "", "", "", ci_temp[1:3], "", ci_temp[4], "", ci_temp[5:6],
        "", ci_temp[7:8], "", "", ci_temp[9:11], "", ci_temp[12], 
        "", ci_temp[13:15], "", "", "", ci_temp[16:19], "", ci_temp[20:21], "")

m <- c(NA, NA, NA, NA, smd_temp[1:3], NA, smd_temp[4], NA, smd_temp[5:6],
       NA, smd_temp[7:8], NA, NA, smd_temp[9:11], NA, smd_temp[12], 
       NA, smd_temp[13:15], NA, NA, NA, smd_temp[16:19],
       NA, smd_temp[20:21], NA)

l <- c(NA, NA, NA, NA, SA_cog$ciLow[1:3], NA, SA_cog$ciLow[4], NA, SA_cog$ciLow[5:6],
       NA, SA_cog$ciLow[7:8], NA, NA, SA_cog$ciLow[9:11], NA, SA_cog$ciLow[12], 
       NA, SA_cog$ciLow[13:15], NA, NA, NA, SA_cog$ciLow[16:19],
       NA, SA_cog$ciLow[20:21], NA)

u <- c(NA, NA, NA, NA, SA_cog$ciHigh[1:3], NA, SA_cog$ciHigh[4], NA, SA_cog$ciHigh[5:6],
       NA, SA_cog$ciHigh[7:8], NA, NA, SA_cog$ciHigh[9:11], NA, SA_cog$ciHigh[12], 
       NA, SA_cog$ciHigh[13:15], NA, NA, NA, SA_cog$ciHigh[16:19],
       NA, SA_cog$ciHigh[20:21], NA)



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
             #clip = c(-0.8, 0.4),
             xticks=c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4),
             is.summary = c(TRUE, TRUE, rep(FALSE, 26), TRUE, rep(FALSE, 8)),
             new_page = TRUE,
             col = fpColors(box = "royalblue",
                            line = "darkblue"),
             txt_gp = fpTxtGp(cex = 0.7, label = gpar(fontfamily = "serif")),
             graph.pos = 2,
             boxsize = 0.3,
             title = "Cognitive Scores in Cape Town, South Africa")

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
