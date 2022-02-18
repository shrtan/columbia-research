library(ggthemes)
library(ggplot2)

######## MEDIAN HEART RATE #########
#sg
sg_medianHR <- data.frame(grp=c("Mod to high continuous", "Low continuous", "Quit early", "Non-smoker"),
                          est=c(126.413015, 126.495186, 127.403700, 126.251242),
                          min=c(125.368909, 125.652523, 125.642273, 125.543046),
                          max=c(127.457121, 127.337849, 129.165127, 126.959438))


level_sg <- c("Mod to high continuous", "Low continuous", "Quit early", "Non-smoker")


pdf("C:/Users/Shreya/Documents/Research/ANS Table/Plots/medianHR_sg.pdf", onefile=FALSE)
ggplot(sg_medianHR, aes(x=factor(grp, level = level_sg), y=est)) +
  geom_point()+
  geom_errorbar(aes(ymin=min, ymax=max), width=0.3, size=0.5) +
  theme_bw() +
  theme(axis.title = element_text(), axis.title.y = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) + 
  ylab(paste("Median Heart Rate ", "\u00B1", " CI")) + xlab("Smoking Groups")
dev.off()


#dg
dg_medianHR <- data.frame(grp=c("Mod to high continuous", "Low continuous", "Quit early", "Non-drinker"),
                          est=c(127.160123, 126.090062, 126.849586, 126.463372),
                          min=c(126.172597, 124.178184, 126.101233, 125.779108),
                          max=c(128.147650, 128.001939, 127.597939, 127.147636))

level_dg <- c("Mod to high continuous", "Low continuous", "Quit early", "Non-drinker")

pdf("C:/Users/Shreya/Documents/Research/ANS Table/Plots/medianHR_dg.pdf", onefile=FALSE)
ggplot(dg_medianHR, aes(x=factor(grp, level = level_dg), y=est)) +
  geom_point()+
  geom_errorbar(aes(ymin=min, ymax=max), width=0.3, size=0.5) +
  theme_bw() +
  theme(axis.title = element_text(), axis.title.y = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) + 
  ylab(paste("Median Heart Rate ", "\u00B1", " CI")) + xlab("Drinking Groups")
dev.off()

#######################################################################################################################








################################ ALTERNATE ###################################
# ggplot(sg_HRmedAb1, aes(x=factor(grp, level = level_order), y=est)) + 
#   geom_errorbar(aes(ymin=min, ymax=max)) + theme(axis.title.x = element_blank())

# plotCI(x = sg_HRmedAb1$grp,               # plotrix plot with confidence intervals
#        y = sg_HRmedAb1$est,
#        li = sg_HRmedAb1$min,
#        ui = sg_HRmedAb1$max)

# ggplot(sg_HRmedAb1, aes(x=factor(grp, level = level_order), y=est)) +
#   geom_pointrange(aes(ymin=min, ymax=max)) +
#   theme_classic() +
#   theme(axis.title = element_text(), axis.title.x = element_blank(), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) + 
#   ylab("Median Heart Rate in Active Sleep")


# #both 
# sg <- factor(c("Mod to high continuous", "Low continuous", "Quit early", "Non-smoker"), 
#              level = c("Mod to high continuous", "Low continuous", "Quit early", "Non-smoker"))
# dg <- factor(c("Mod to high continuous", "Low continuous", "Quit early", "Non-drinker"), 
#              level = c("Mod to high continuous", "Low continuous", "Quit early", "Non-drinker"))

# HRmedAb1 <- data.frame(grp=vector(sg, dg),
#                        fac=factor(c("s", "s", "s", "s", "d", "d", "d", "d")),
#                        est=c(126.413015, 126.495186, 127.403700, 126.251242,
#                              127.160123, 126.090062, 126.849586, 126.463372),
#                        min=c(125.368909, 125.652523, 125.642273, 125.543046,
#                              126.172597, 124.178184, 126.101233, 125.779108),
#                        max=c(127.457121, 127.337849, 129.165127, 126.959438,
#                              128.147650, 128.001939, 127.597939,  127.147636))
# 
# level_order2 <- c("Mod to high continuous", "Low continuous", "Quit early", "Non-smoker",
#                   "Non-drinker")
# 
# ggplot(HRmedAb1, aes(x=grp, y=est, col=fac)) +
#   geom_point()+
#   geom_errorbar(aes(ymin=min, ymax=max), width=0.3, size=0.5) +
#   theme_bw() +
#   theme(axis.title = element_text(), axis.title.y = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) + 
#   ylab(paste("Median Heart Rate ", "\u00B1", " CI")) + xlab("Smoking Groups") +
#   facet_wrap(~fac)
