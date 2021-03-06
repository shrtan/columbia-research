library(forestplot)

sum <- c(rep(TRUE, 2), rep(FALSE, 25), TRUE, rep(FALSE, 9), TRUE, rep(FALSE, 5),
         TRUE, rep(FALSE, 7))

cat <- c("", "Socio-demographic factors", 
         "Age", "  0-4", "  5-9", "  10-14", "  15-19", "  20-24 (ref)", "  25-29",
         "  30-34", "  35-39", "  40-44", "  45-49", "  50-54", "  55-59", "  60-64",
         "  65-69", "  70-74", "  75-79", "  80+", 
         "Sex", "  Female (ref)", "  Male",
         "Socioeconomic condition", "  High (ref)", "  Low",
         "", "Region", "Dhaka, the capital (ref)", "Barisal", "Chittagong N", "Chittagong S",
         "Khulna", "Rajshahi", "Rangpur", "Sylhet",
         "", "Period of the pandemic", "Period 1 (05/17/2020-09/30/2020) (ref)", "Period 2 (10/01/2020-01/31/2021)",
         "Period 3 (02/01/2021-05/14/2021)", "Period 4 (05/14/2021-06/14/2021)",
         "", "Preexisting conditions", "No comorbidity (ref)", "Diabetes", "Hypertension",
         "Chronic respiratory illness", "Chronic renal disease", "Chronic liver disease", "Other")

smd <- c("OR", "", 
         "", "3.92", "1.53", "1.93", "1.39", "", "1.19", 
         "1.34", "1.73", "2.14", "3.06", "4.60", "6.16", "8.76",
         "13.50", "16.44", "21.61", "30.64", "", "", "1.15", "", "", "1.76",
         "", "", "", "1.07", "1.28", "1.03", "1.23", "1.10", "1.13", "1.61",
         "", "", "", "0.90", "0.84", "9.09",
         "", "", "", "1.05", "1.13", "1.05", "2.33", "2.08", "1.44")


ci <- c("95% CI", "", 
        "", "(2.73, 5.63)", "(0.89, 2.62)", "(1.27, 2.92)", "(1.01, 1.90)", "", "(0.93, 1.54)", 
        "(1.05, 1.73)", "(1.36, 2.2)", "(1.68, 2.73)", "(2.43, 3.87)", "(3.66, 5.78)", "(4.92, 7.72)", "(6.98, 10.99)",
        "(10.79, 16.88)", "(13.07, 20.68)", "(17.06, 27.36)", "(24.22, 38.76)", 
        "", "", "(1.09, 1.22)", "", "", "(1.56, 1.98)",
        "", "", "", "(0.93, 1.24)", "(1.14, 1.44)", "(0.94, 1.14)", "(1.10, 1.38)", "(0.97, 1.25)", "(0.97, 1.32)", "(1.39, 1.86)",
        "", "", "", "(0.85, 0.95)", "(0.77, 0.91)", "(7.63, 10.83)",
        "", "", "", "(0.97, 1.15)", "(1.04, 1.23)", "(0.92, 1.20)", "(2.04, 2.67)", "(1.57, 2.74)", "(1.29, 1.61)")

m <- c(NA, NA, 
       NA, 3.92, 1.53, 1.93, 1.39, NA, 1.19, 
       1.34, 1.73, 2.14, 3.06, 4.60, 6.16, 8.76,
       13.5, 16.4, 21.6, 30.6, NA, NA, 1.15, NA, NA, 1.76,
       NA, NA, NA, 1.07, 1.28, 1.03, 1.23, 1.10, 1.13, 1.61,
       NA, NA, NA, 0.90, 0.84, 9.09,
       NA, NA, NA, 1.05, 1.13, 1.05, 2.33, 2.08, 1.44)

l <- c(NA, NA, 
       NA, 2.73, 0.89, 1.27, 1.01, NA, 0.93, 
       1.05, 1.36, 1.68, 2.43, 3.66, 4.92, 6.98,
       10.79, 13.07, 17.06, 24.22, NA, NA, 1.09, NA, NA, 1.56,
       NA, NA, NA, 0.93, 1.14, 0.94, 1.10, 0.97, 0.97, 1.39,
       NA, NA, NA, 0.85, 0.77, 7.63,
       NA, NA, NA, 0.97, 1.04, 0.92, 2.04, 1.57, 2.74)

u <- c(NA, NA, 
       NA, 5.63, 2.62, 2.92, 1.90, NA, 1.54, 
       1.73, 2.20, 2.73, 3.87, 5.78, 7.72, 10.99,
       16.88, 20.68, 27.36, 38.76, NA, NA, 1.22, NA, NA, 1.98,
       NA, NA, NA, 1.24, 1.44, 1.14, 1.38, 1.25, 1.32, 1.86,
       NA, NA, NA, 0.95, 0.91, 10.83,
       NA, NA, NA, 1.15, 1.23, 1.20, 2.67, 2.74, 1.61)






temp <- tibble(mean = m,
               lower = l,
               upper = u,
               study = cat,
               confint=ci,
               smd = smd)

ticks <- c(0.5, 1, 2, 4, 8, 16, 32)

pdf("C:/Users/Shreya/Documents/Research/Misc/Temp.pdf", onefile=FALSE)

temp %>%
  forestplot(labeltext = c(study, smd, confint),
             #clip = c(0.5, 16),
             xticks=ticks,
             xlog = TRUE,
             is.summary = sum,
             new_page = TRUE,
             col = fpColors(box = "royalblue",
                            line = "darkblue"),
             txt_gp = fpTxtGp(cex = 0.7, label = gpar(fontfamily = "serif"), 
                              ticks=gpar(fontsize=5, cex=1.4)),
             #graphwidth=unit (60, "mm"),
             graph.pos = 2,
             boxsize = 0.3,
             title = "")

dev.off()
