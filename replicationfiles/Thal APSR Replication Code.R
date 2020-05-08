# Load required packages

library("foreign")
library("ggplot2")
library("Hmisc")
library("psych")
library("Rmisc")
library("texreg")
library(dplyr)
library(tidyverse)

# Set working directory

setwd("")

##########
# Test 1 #
##########

# Load data for Test 1

data <- read.dta("../finalproject1006/dataverse_files/apsrtest1.dta")

# Create income motive subscales

data$needs <- rowMeans(cbind(data$needs1, data$needs2, data$needs3))
data$famsupport <- rowMeans(cbind(data$famsupport1, data$famsupport2, data$famsupport3))
data$security <- rowMeans(cbind(data$security1, data$security2, data$security3))
data$anxiety <- rowMeans(cbind(data$anxiety1, data$anxiety2, data$anxiety3))
data$work <- rowMeans(cbind(data$work1, data$work2, data$work3))
data$leisure <- rowMeans(cbind(data$leisure1, data$leisure2, data$leisure3))
data$charity <- rowMeans(cbind(data$charity1, data$charity2, data$charity3))
data$concon <- rowMeans(cbind(data$concon1, data$concon2, data$concon3))
data$socapprov <- rowMeans(cbind(data$socapprov1, data$socapprov2, data$socapprov3))
data$selfesteem <- rowMeans(cbind(data$selfesteem1, data$selfesteem2, data$selfesteem3))

# Conduct principal component analysis (PCA)

# Scale income motive variables for PCA

df <- data.frame(data$needs, 
                 data$famsupport, 
					       data$security, 
					       data$anxiety, 
					       data$work, 
				         data$leisure, 
			           data$charity, 
			           data$concon, 
					       data$socapprov, 
			           data$selfesteem)

df.scaled <- as.data.frame(scale(df, center = TRUE, scale = TRUE))

# Run PCA

aff.pc <- principal(df.scaled, nfactors = 2, rotate = "promax", scores = T)

# Extract PCA scores and scale by dividing by two standard deviations

data$concrete <- aff.pc$scores[,1]
data$concrete_2sd <- data$concrete/(2*sd(data$concrete)) 

data$status <- aff.pc$scores[,2]
data$status_2sd <- data$status/(2*sd(data$status)) 

############
# Figure 1 #
############

df <- as.data.frame(c("Basic Needs",
                      "Family Support",
                      "Financial Security",
                      "Leisure", 
                      "Philanthropy",
                      "Conspicuous Consumption",
                      "Social Approval",
                      "Anxiety",
                      "Hard Work",
                      "Self-Esteem"))

colnames(df) <- "Motive"

# Note: Some component loadings are slightly shifted to make them visible in Figure 1

df$Concrete <- c(.85, .59, .76, .43, .27, 0, -.26, .73, .56, -.06)
df$Status <- c(-.12, -.05, -.2, .39, .03, .81, .94, .03, .21, .87)

df$Concrete.p <- df$Concrete*3
df$Status.p <- df$Status*3

df$Concrete.a <- df$Concrete.p - .1
df$Status.a <- df$Status.p - .1
df[3,]$Status.a <- -.5
df[1,]$Concrete.a <- df[1,]$Concrete.a -.3
df[3,]$Concrete.a <- df[3,]$Concrete.a -.4

write_rds(df, "../finalproject1006/readata/df.rds")

ggplot(df, aes(x=Concrete, y=Status)) +
  theme_bw() +
  labs(x = "PC1: Concrete", y = "PC2: Status") +
  scale_x_continuous(limits = c(-1,1), breaks = seq(-1, 1, .25)) +
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1, 1, .25)) +
  coord_cartesian(xlim=c(-.95, .95), ylim=c(-.95, .95)) +
  geom_segment(aes(x=-1,xend=1,y=0,yend=0), colour="dark grey", linetype="solid") +
  geom_segment(aes(x=0,xend=0,y=-1,yend=1), colour="dark grey", linetype="solid") +
  geom_text(data=df, aes(x=Concrete, y=Status, 
                         label = Motive, fontface = "bold"),
            size = 6) +
  theme(text = element_text(size=25),
        plot.margin=unit(c(1,1,1,1.2),"cm"))

############
# Figure 2 #
############

write_rds(data, "../finalproject1006/readata/data.rds")

ggplot() +
  geom_point(data=data, aes(x=concrete, y=status), size = 3, color = 'grey') +
  theme_bw() +
  labs(x = "Concrete Component Score", y = "Status Component Score") +
  scale_x_continuous(breaks = seq(-3, 3, .5),
                     sec.axis = dup_axis(name = "", 
                                         breaks = seq(-3, 3, length.out = 9),
                                         labels = sprintf("%.2f", seq(-1, 1, .25)))) +
  scale_y_continuous(breaks = seq(-3, 3, .5),
                     sec.axis = dup_axis(name = "", 
                                         breaks = seq(-3, 3, length.out = 9),
                                         labels = sprintf("%.2f", seq(-1, 1, .25)))) +
  coord_cartesian(xlim=c(-2.95, 2.95), ylim=c(-2.95, 2.95)) +
  geom_segment(aes(x=-3,xend=3,y=0,yend=0), colour="grey", linetype="dashed") +
  geom_segment(aes(x=0,xend=0,y=-3,yend=3), colour="grey", linetype="dashed") +
       theme(text = element_text(size=30),
        		 axis.text.x=element_text(size=20),
        		 axis.text.y=element_text(size=20)) +
  geom_text(data=df, aes(x=Concrete.p, y=Status.p, 
                         label = Motive, fontface = "bold"),
            						size = 5.5) +
   geom_segment(data=df, aes(xend = Concrete.a,yend=Status.a),
              x=0, y=0, colour="black",
              arrow=arrow(angle=25, length=unit(0.5, "cm")),
              linetype="dashed",
   						size=1) 

# Create Economic Conservatism Index

data$econcon <- rowMeans(cbind(data$taxdechi, data$capgains, data$regindustry))

write_rds(data, "../finalproject1006/readata/dataf1.rds")

###########
# Table 2 #  
###########

econcon.aff.lm <- lm(econcon ~ 
					      status_2sd +
					      concrete_2sd +
					      COLI_2sd +
                male + 
                asian +
					      hispanic +
					      nhblack +
					      other +
                age3044 + 
                age4554 + 
                age55up + 
                college + 
                grad, 
					      weights = weight, 
                data = data)
summary(econcon.aff.lm)

econcon.aff.lmg <- lm(econcon ~
					      status_2sd*male +
					      concrete_2sd*male +
					      COLI_2sd +
                asian +
					      hispanic +
					      nhblack +
					      other +
                age3044 + 
                age4554 + 
                age55up + 
                college + 
                grad, 
					      weights = weight, 
                data = data)
summary(econcon.aff.lmg)

texreg(list(econcon.aff.lm, econcon.aff.lmg),
       single.row = FALSE,
       caption.above = TRUE, stars = c(0.001, 0.01, 0.05), 
       custom.model.names = c("Main Model", "Gender Interaction Model"),
       custom.coef.names = c("Intercept",
        										"Status",
        										"Concrete",
        										"Regional Cost of Living",
        										"Male",
        										"Asian",
        										"Latino",
        										"Black",
        										"Other",
        										"Age 30-44",
        										"Age 45-54",
        										"Age 55 and up",
        										"College degree",
        										"Graduate degree",
														"Status X Male",
														"Concrete X Male"))

############
# Figure 3 #
############

# Measure predicted effects with all other variables held at means

concrete.pred.df <- data.frame(concrete=NA,
											male=wtd.mean(data$male, weights=data$weight),
											status=wtd.mean(data$status, weights=data$weight),
									    COLI=wtd.mean(data$COLI, weights=data$weight),
									    asian=wtd.mean(data$asian, weights=data$weight),
									    hispanic=wtd.mean(data$hispanic, weights=data$weight),
									    nhblack=wtd.mean(data$nhblack, weights=data$weight),
									    other=wtd.mean(data$other, weights=data$weight),
									    age3044=wtd.mean(data$age3044, weights=data$weight),
									    age4554=wtd.mean(data$age4554, weights=data$weight),
									    age55up=wtd.mean(data$age55up, weights=data$weight),
                      college=wtd.mean(data$college, weights=data$weight),
									    grad=wtd.mean(data$grad, weights=data$weight))

status.pred.df <- data.frame(status=NA,
											male=wtd.mean(data$male, weights=data$weight),
											concrete=wtd.mean(data$concrete, weights=data$weight),
									    COLI=wtd.mean(data$COLI, weights=data$weight),
									    asian=wtd.mean(data$asian, weights=data$weight),
									    hispanic=wtd.mean(data$hispanic, weights=data$weight),
									    nhblack=wtd.mean(data$nhblack, weights=data$weight),
									    other=wtd.mean(data$other, weights=data$weight),
									    age3044=wtd.mean(data$age3044, weights=data$weight),
									    age4554=wtd.mean(data$age4554, weights=data$weight),
									    age55up=wtd.mean(data$age55up, weights=data$weight),
                      college=wtd.mean(data$college, weights=data$weight),
									    grad=wtd.mean(data$grad, weights=data$weight))

# Model for predicted effects

econcon.aff.lm <- lm(econcon ~
					      concrete +
					      status +
					      male +
					      COLI +
                asian +
					      hispanic +
					      nhblack +
					      other +
                age3044 + 
                age4554 + 
                age55up + 
                college + 
                grad, 
					      weights = weight, 
                data=data)
summary(econcon.aff.lm)

# Concrete predicted effects

temp.lo <- concrete.pred.df
temp.lo$concrete <- -1
conc.lo.pred <- predict(econcon.aff.lm, temp.lo, interval = "confidence", level = 0.95)[1] 
conc.lo.lci <- predict(econcon.aff.lm, temp.lo, interval = "confidence", level = 0.95)[2] 
conc.lo.uci <- predict(econcon.aff.lm, temp.lo, interval = "confidence", level = 0.95)[3] 

temp.hi <- concrete.pred.df
temp.hi$concrete <- 1
conc.hi.pred <- predict(econcon.aff.lm, temp.hi, interval = "confidence", level = 0.95)[1] 
conc.hi.lci <- predict(econcon.aff.lm, temp.hi, interval = "confidence", level = 0.95)[2] 
conc.hi.uci <- predict(econcon.aff.lm, temp.hi, interval = "confidence", level = 0.95)[3] 

# Status predicted effects

temp.lo <- status.pred.df
temp.lo$status <- -1
stat.lo.pred <- predict(econcon.aff.lm, temp.lo, interval = "confidence", level = 0.95)[1] 
stat.lo.lci <- predict(econcon.aff.lm, temp.lo, interval = "confidence", level = 0.95)[2] 
stat.lo.uci <- predict(econcon.aff.lm, temp.lo, interval = "confidence", level = 0.95)[3] 

temp.hi <- status.pred.df
temp.hi$status <- 1
stat.hi.pred <- predict(econcon.aff.lm, temp.hi, interval = "confidence", level = 0.95)[1] 
stat.hi.lci <- predict(econcon.aff.lm, temp.hi, interval = "confidence", level = 0.95)[2] 
stat.hi.uci <- predict(econcon.aff.lm, temp.hi, interval = "confidence", level = 0.95)[3] 

# Create figure

df <- data.frame(rbind(conc.lo.pred,
											 conc.hi.pred,
											 stat.lo.pred,
											 stat.hi.pred))

colnames(df) <- "pred"

df$lwr <- rbind(conc.lo.lci,
								conc.hi.lci,
						    stat.lo.lci,
						    stat.hi.lci)

df$upr <- rbind(conc.lo.uci,
	  			     conc.hi.uci,
					     stat.lo.uci,
					     stat.hi.uci) 

df$level <- rbind("-1 S.D.",
								  "+1 S.D.",
                  "-1 S.D.",
								  "+1 S.D.")

df$type <- rbind("Concrete",
								 "Concrete",
                 "Status",
								 "Status")

df$level <- factor(df$level, levels = rev(c("+1 S.D.", "-1 S.D.")), 
									           labels = rev(c("+1 S.D.", "-1 S.D.")))

df$type <- factor(df$type, levels = c("Status", 
                                      "Concrete"))

dodge <- position_dodge(width=-.1)

ggplot(df, aes(x = level, y = pred, ymin = lwr, ymax = upr, 
							 shape = type, group = type, colour = type, linetype = type)) + 
	geom_errorbar(width=.1, size=1, position=dodge, linetype = "solid") +
  geom_point(size=5, position=dodge, aes(colour = type)) +
  geom_line(aes(group=type), position=dodge) +
	theme_bw() +
  scale_colour_manual(values=c("grey35", "grey65"))  +
  scale_linetype_manual(values=c("solid", "dashed"))  +
  ylab("Economic Conservatism") +
	xlab("Level") +
  scale_y_continuous(limits=c(.45, .75),
                     breaks = seq(0,1,.05)) +
  theme(text = element_text(size=25),
        axis.text.x=element_text(size=25)) +
	guides(shape = guide_legend(reverse=F, title="Motive"),
				 colour = guide_legend(reverse=F, title="Motive"),
				 linetype = guide_legend(reverse=F, title="Motive"))

write_rds(df, "../finalproject1006/readata/df3.rds")

############
# Figure 4 #
############

# Measure predicted effects with all other variables held at means

concrete.pred.df <- data.frame(concrete=NA,
											male=NA,
											status=wtd.mean(data$status, weights=data$weight),
									    COLI=wtd.mean(data$COLI, weights=data$weight),
									    asian=wtd.mean(data$asian, weights=data$weight),
									    hispanic=wtd.mean(data$hispanic, weights=data$weight),
									    nhblack=wtd.mean(data$nhblack, weights=data$weight),
									    other=wtd.mean(data$other, weights=data$weight),
									    age3044=wtd.mean(data$age3044, weights=data$weight),
									    age4554=wtd.mean(data$age4554, weights=data$weight),
									    age55up=wtd.mean(data$age55up, weights=data$weight),
                      college=wtd.mean(data$college, weights=data$weight),
									    grad=wtd.mean(data$grad, weights=data$weight))

status.pred.df <- data.frame(status=NA,
											male=NA,
											concrete=wtd.mean(data$concrete, weights=data$weight),
									    COLI=wtd.mean(data$COLI, weights=data$weight),
									    asian=wtd.mean(data$asian, weights=data$weight),
									    hispanic=wtd.mean(data$hispanic, weights=data$weight),
									    nhblack=wtd.mean(data$nhblack, weights=data$weight),
									    other=wtd.mean(data$other, weights=data$weight),
									    age3044=wtd.mean(data$age3044, weights=data$weight),
									    age4554=wtd.mean(data$age4554, weights=data$weight),
									    age55up=wtd.mean(data$age55up, weights=data$weight),
                      college=wtd.mean(data$college, weights=data$weight),
									    grad=wtd.mean(data$grad, weights=data$weight))

# Model for predicted effects

econcon.aff.lm <- lm(econcon ~
					      concrete*male +
					      status*male +
					      COLI +
                asian +
					      hispanic +
					      nhblack +
					      other +
                age3044 + 
                age4554 + 
                age55up + 
                college + 
                grad, 
					      weights = weight, 
                data=data)
summary(econcon.aff.lm)

# Affluent Male

# Concrete predicted effects

temp.lo <- concrete.pred.df
temp.lo$male <- 1
temp.lo$concrete <- -1
conc.lo.pred <- predict(econcon.aff.lm, temp.lo, interval = "confidence", level = 0.95)[1] 
conc.lo.lci <- predict(econcon.aff.lm, temp.lo, interval = "confidence", level = 0.95)[2] 
conc.lo.uci <- predict(econcon.aff.lm, temp.lo, interval = "confidence", level = 0.95)[3] 

temp.hi <- concrete.pred.df
temp.hi$male <- 1
temp.hi$concrete <- 1
conc.hi.pred <- predict(econcon.aff.lm, temp.hi, interval = "confidence", level = 0.95)[1] 
conc.hi.lci <- predict(econcon.aff.lm, temp.hi, interval = "confidence", level = 0.95)[2] 
conc.hi.uci <- predict(econcon.aff.lm, temp.hi, interval = "confidence", level = 0.95)[3] 

# Status predicted effects

temp.lo <- status.pred.df
temp.lo$male <- 1
temp.lo$status <- -1
stat.lo.pred <- predict(econcon.aff.lm, temp.lo, interval = "confidence", level = 0.95)[1] 
stat.lo.lci <- predict(econcon.aff.lm, temp.lo, interval = "confidence", level = 0.95)[2] 
stat.lo.uci <- predict(econcon.aff.lm, temp.lo, interval = "confidence", level = 0.95)[3] 

temp.hi <- status.pred.df
temp.hi$male <- 1
temp.hi$status <- 1
stat.hi.pred <- predict(econcon.aff.lm, temp.hi, interval = "confidence", level = 0.95)[1] 
stat.hi.lci <- predict(econcon.aff.lm, temp.hi, interval = "confidence", level = 0.95)[2] 
stat.hi.uci <- predict(econcon.aff.lm, temp.hi, interval = "confidence", level = 0.95)[3] 

# Store data

df <- data.frame(rbind(conc.lo.pred,
											 conc.hi.pred,
											 stat.lo.pred,
											 stat.hi.pred))

colnames(df) <- "pred"

df$lwr <- rbind(conc.lo.lci,
								conc.hi.lci,
						    stat.lo.lci,
						    stat.hi.lci)

df$upr <- rbind(conc.lo.uci,
	  			     conc.hi.uci,
					     stat.lo.uci,
					     stat.hi.uci) 

df$level <- rbind("-1 S.D.",
								  "+1 S.D.",
                  "-1 S.D.",
								  "+1 S.D.")

df$type <- rbind("Concrete",
								 "Concrete",
                 "Status",
								 "Status")

df$sex <- rbind("Affluent Men",
								"Affluent Men",
								"Affluent Men",
								"Affluent Men")

male.df <- df

# Affluent Female

# Concrete predicted effects

temp.lo <- concrete.pred.df
temp.lo$male <- 0
temp.lo$concrete <- -1
conc.lo.pred <- predict(econcon.aff.lm, temp.lo, interval = "confidence", level = 0.95)[1] 
conc.lo.lci <- predict(econcon.aff.lm, temp.lo, interval = "confidence", level = 0.95)[2] 
conc.lo.uci <- predict(econcon.aff.lm, temp.lo, interval = "confidence", level = 0.95)[3] 

temp.hi <- concrete.pred.df
temp.hi$male <- 0
temp.hi$concrete <- 1
conc.hi.pred <- predict(econcon.aff.lm, temp.hi, interval = "confidence", level = 0.95)[1] 
conc.hi.lci <- predict(econcon.aff.lm, temp.hi, interval = "confidence", level = 0.95)[2] 
conc.hi.uci <- predict(econcon.aff.lm, temp.hi, interval = "confidence", level = 0.95)[3] 

# Status predicted effects

temp.lo <- status.pred.df
temp.lo$male <- 0
temp.lo$status <- -1
stat.lo.pred <- predict(econcon.aff.lm, temp.lo, interval = "confidence", level = 0.95)[1] 
stat.lo.lci <- predict(econcon.aff.lm, temp.lo, interval = "confidence", level = 0.95)[2] 
stat.lo.uci <- predict(econcon.aff.lm, temp.lo, interval = "confidence", level = 0.95)[3] 

temp.hi <- status.pred.df
temp.hi$male <- 0
temp.hi$status <- 1
stat.hi.pred <- predict(econcon.aff.lm, temp.hi, interval = "confidence", level = 0.95)[1] 
stat.hi.lci <- predict(econcon.aff.lm, temp.hi, interval = "confidence", level = 0.95)[2] 
stat.hi.uci <- predict(econcon.aff.lm, temp.hi, interval = "confidence", level = 0.95)[3] 

# Store data

df <- data.frame(rbind(conc.lo.pred,
											 conc.hi.pred,
											 stat.lo.pred,
											 stat.hi.pred))

colnames(df) <- "pred"

df$lwr <- rbind(conc.lo.lci,
								conc.hi.lci,
						    stat.lo.lci,
						    stat.hi.lci)

df$upr <- rbind(conc.lo.uci,
	  			     conc.hi.uci,
					     stat.lo.uci,
					     stat.hi.uci) 

df$level <- rbind("-1 S.D.",
								  "+1 S.D.",
                  "-1 S.D.",
								  "+1 S.D.")

df$type <- rbind("Concrete",
								 "Concrete",
                 "Status",
								 "Status")

df$sex <- rbind("Affluent Women",
								"Affluent Women",
								"Affluent Women",
								"Affluent Women")

female.df <- df

# Create figure

dodge <- position_dodge(width=-.2)

df <- rbind(male.df, female.df)

df$level <- factor(df$level, levels = rev(c("+1 S.D.", "-1 S.D.")), 
									           labels = rev(c("+1 S.D.", "-1 S.D.")))

df$type <- factor(df$type, levels = c("Status", "Concrete"))

df$sex <- factor(df$sex, levels = c("Affluent Men",
                                    "Affluent Women"))

write_rds(df, "../finalproject1006/readata/df4.rds")

ggplot(df, aes(x = level, y = pred, ymin = lwr, ymax = upr, 
							 shape = type, group = type, colour = type, linetype = type)) + 
  facet_grid(.~sex) +
	geom_errorbar(width=.1, size=1, position=dodge, linetype="solid") +
  geom_point(size=5, position=dodge, aes(colour = type)) +
  geom_line(aes(group=type), position=dodge) +
	theme_bw() +
  scale_colour_manual(values=c("grey35", "grey65"))  +
  scale_linetype_manual(values=c("solid", "dashed"))  +
  ylab("Economic Conservatism") +
	xlab("Level") +
  scale_y_continuous(limits=c(.45, .75),
                     breaks = seq(0,1,.05)) +
  theme(text = element_text(size=25),
        axis.text.x=element_text(size=25)) +
	guides(shape = guide_legend(reverse=F, title="Motive"),
				 colour = guide_legend(reverse=F, title="Motive"),
				 linetype = guide_legend(reverse=F, title="Motive"))

##########
# Test 2 #
##########

# Load data for Test 2

data2 <- read.dta("../finalproject1006/dataverse_files/apsrtest2.dta")

# Create Economic Conservatism Index

data2$econcon <- rowMeans(cbind(data2$taxdechi, data2$capgains, data2$regindustry))

###########
# Table 4 #
###########

# Run regressions with one-sided p-values

aff.lm <- lm(econcon ~ condition, data = data2)
aff.pval <- pt(coef(summary(aff.lm))[, 3], summary(aff.lm)$df[2], lower=FALSE)

aff.lmg <- lm(econcon ~ condition*male, data = data2)
aff.pvalg <- pt(coef(summary(aff.lmg))[, 3], summary(aff.lmg)$df[2], lower=FALSE)

write_rds(data2, "../finalproject1006/readata/data2.rds")

texreg(list(aff.lm, aff.lmg),
			 override.pvalues = list(c(aff.pval),
			                         c(aff.pvalg)),
       caption.above = TRUE, stars = c(0.001, 0.01, 0.05),
			 custom.model.names = c("Main Model", 
			                        "Gender Interaction Model"),
       custom.coef.names = c("Intercept",
        										"Concrete",
       											"Conspicuous Consumption",
       											"Self-Esteem",
       											"Social Approval",
        										"Affluent Male",
        										"Concrete X Affluent Male",
       											"Conspicuous Consumption X Affluent Male",
       											"Self-Esteem X Affluent Male",
       											"Social Approval X Affluent Male"))

############
# Figure 6 #
############

plot <- summarySE(data2, measurevar="econcon", 
									groupvars=c("condition"), 
									na.rm = T, conf.interval = .95)

plot$condition <- factor(plot$condition, levels = c("Placebo", 
																							 	    "Conspicuous Consumption", 
																										"Self-Esteem", 
																										"Social Approval",
																										"Concrete"))

plot$label <- paste(round(plot$econcon,2))

write_rds(plot, "../finalproject1006/readata/plot.rds")


ggplot(plot, aes(x=condition, y=econcon)) + 
    geom_errorbar(aes(ymin=econcon-ci, ymax=econcon+ci), 
                  width=.15, position = position_dodge(width = 0.5), size = .75) +
    geom_point(position = position_dodge(width = 0.5), size=6,
               shape=18) + 
	  geom_text(aes(label=label),hjust=-.25, vjust=.35, size=8) +
    ylab("Economic Conservatism") +
	  xlab("") +
    theme_bw() +
       theme(text = element_text(size=20),
          legend.position="none",
          axis.text.x = element_text(size = 25),
          axis.text.y = element_text(size = 25),
    			axis.title.y = element_text(size = 25)) +
    scale_y_continuous(limits=c(.55, .75),
                      breaks = seq(.55,.75,.05),
                      labels = c(".55",".60",".65",".70",".75")) +
    scale_x_discrete(labels = c("Placebo",
                                "Conspicuous\nConsumption",
                                "Self-\nEsteem",
                                "Social\nApproval",
    														"Concrete"))

##########################################
# Effects Relative to Concrete Condition #
##########################################

# Run regressions with one-sided p-values

aff.lm2 <- lm(econcon ~ moneycon, data = data2)
aff.pval2 <- pt(coef(summary(aff.lm2))[, 3], summary(aff.lm2)$df[2], lower=FALSE)

texreg(list(aff.lm2),
			 override.pvalues = list(c(aff.pval2)),
       caption.above = TRUE, stars = c(0.001, 0.01, 0.05),
       custom.coef.names = c("Intercept",
       											"Conspicuous Consumption",
       											"Self-Esteem",
       											"Social Approval"))

######################
# Top 10% vs. Top 5% #
######################

# Run regressions with one-sided p-values

top10.lm <- lm(econcon ~ condition, data = data2)
top10.pval <- pt(coef(summary(top10.lm))[, 3], summary(top10.lm)$df[2], lower=FALSE)

top10.lm2 <- lm(econcon ~ moneycon, data = data2)
top10.pval2 <- pt(coef(summary(top10.lm2))[, 3], summary(top10.lm2)$df[2], lower=FALSE)

top5.lm <- lm(econcon ~ condition, data = data2[data2$top5 == 1,])
top5.pval <- pt(coef(summary(top5.lm))[, 3], summary(top5.lm)$df[2], lower=FALSE)

top5.lm2 <- lm(econcon ~ moneycon, data = data2[data2$top5 == 1,])
top5.pval2 <- pt(coef(summary(top5.lm2))[, 3], summary(top5.lm2)$df[2], lower=FALSE)

texreg(list(top10.lm, top10.lm2, top5.lm, top5.lm2),
			 override.pvalues = list(c(top10.pval),
			                         c(top10.pval2),
			                         c(top5.pval),
			                         c(top5.pval2)),
       caption.above = TRUE, stars = c(0.001, 0.01, 0.05),
       custom.coef.names = c("Intercept",
                            "Concrete",
       											"Conspicuous Consumption",
       											"Self-Esteem",
       											"Social Approval",
       											"Conspicuous Consumption",
       											"Self-Esteem",
       											"Social Approval"))

############
# Figure 7 #
############

# Affluent Men (panel a)

plot <- summarySE(data2[data2$male == 1,], measurevar="econcon", 
									groupvars=c("condition"), 
									na.rm = T, conf.interval = .95)

plot$condition <- factor(plot$condition, levels = c("Placebo", 
																							 	    "Conspicuous Consumption", 
																										"Self-Esteem", 
																										"Social Approval",
																										"Concrete"))

plot$label <- paste(round(plot$econcon,2))

write_rds(plot, "../finalproject1006/readata/plotm.rds")

ggplot(plot, aes(x=condition, y=econcon)) + 
    geom_errorbar(aes(ymin=econcon-ci, ymax=econcon+ci), 
                  width=.15, position = position_dodge(width = 0.5), size = .75) +
    geom_point(position = position_dodge(width = 0.5), size=5,
               shape=16) + 
	  geom_text(aes(label=label),hjust=-.25, vjust=.35, size=8) +
    ylab("Economic Conservatism") +
	  xlab("") +
    theme_bw() +
       theme(text = element_text(size=20),
          legend.position="none",
          axis.text.x = element_text(size = 25),
          axis.text.y = element_text(size = 25),
    			axis.title.y = element_text(size = 25)) +
    scale_y_continuous(limits=c(.55, .8),
                      breaks = seq(.5,.8,.05),
                      labels = c(".50", ".55",".60",".65",".70",".75", ".80")) +
    scale_x_discrete(labels = c("Placebo",
                               "Conspicuous\nConsumption",
                               "Self-\nEsteem",
                               "Social\nApproval",
    													 "Concrete"))
# Affluent Women (panel b)

plot <- summarySE(data2[data2$male == 0,], measurevar="econcon", 
									groupvars=c("condition"), 
									na.rm = T, conf.interval = .95)

plot$condition <- factor(plot$condition, levels = c("Placebo", 
																							 	    "Conspicuous Consumption", 
																										"Self-Esteem", 
																										"Social Approval",
																										"Concrete"))

plot$label <- paste(round(plot$econcon,2))

write_rds(plot, "../finalproject1006/readata/plotw.rds")

ggplot(plot, aes(x=condition, y=econcon)) + 
    geom_errorbar(aes(ymin=econcon-ci, ymax=econcon+ci), 
                  width=.15, position = position_dodge(width = 0.5), size = .75) +
    geom_point(position = position_dodge(width = 0.5), size=5,
               shape=17) + 
	  geom_text(aes(label=label),hjust=-.25, vjust=.35, size=8) +
    ylab("Economic Conservatism") +
	  xlab("") +
    theme_bw() +
       theme(text = element_text(size=20),
          legend.position="none",
          axis.text.x = element_text(size = 25),
          axis.text.y = element_text(size = 25),
    			axis.title.y = element_text(size = 25)) +
    scale_y_continuous(limits=c(.55, .8),
                      breaks = seq(.55,.8,.05),
                      labels = c(".55",".60",".65",".70",".75", ".80")) +
    scale_x_discrete(labels = c("Placebo",
                               "Conspicuous\nConsumption",
                               "Self-\nEsteem",
                               "Social\nApproval",
    													 "Concrete"))
