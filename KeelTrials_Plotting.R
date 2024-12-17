library(car)
library(tidyr)
library(ggplot2)
library(patchwork)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(pracma)
library(dplyr)
library(scales)

data02 <- read.csv("Flume_Forces_Sum_rotated_new.csv")
data02$model <- as.factor(paste(data02$Body, data02$Keel))
data02$Frequency <- as.numeric(data02$Frequency)


data02$model <- factor(data02$model, levels = c("E no", "E 40", "E 60", 
                                                        "M no", "M 40", "M 60",
                                                        "D no", "D 40", "D 60"))

colors = c("hotpink", "gold","dodgerblue2")
#Colors for inverting
#colors = c("green3", "magenta", "orange")

data02$Area <- with(data02, ifelse(Body == 'D', 0.000984,
                              ifelse(Body == 'M', 0.000670, 0.000437)))
data02$DC <- 2*data02$Fx2Mean/(1000*data02$Speed^2*data02$Area)
  

#Subseting
thirty <- data02[data02$Speed == 30,]
twenty <- data02[data02$Speed == 20,]
ten <- data02[data02$Speed == 10,]
D30 <- data02[data02$Speed == 30 & data02$Body == "D",]
D20 <- data02[data02$Speed == 20 & data02$Body == "D",]
D10 <- data02[data02$Speed == 10 & data02$Body == "D",]
E30 <- data02[data02$Speed == 30 & data02$Body == "E",]
E20 <- data02[data02$Speed == 20 & data02$Body == "E",]
E10 <- data02[data02$Speed == 10 & data02$Body == "E",]
M30 <- data02[data02$Speed == 30 & data02$Body == "M",]
M20 <- data02[data02$Speed == 20 & data02$Body == "M",]
M10 <- data02[data02$Speed == 10 & data02$Body == "M",]

#Define ANOVA model
D30.A.thrust <- lm(Fx2Mean~Keel,data=D30)
D30.A.torque <- lm(abs(MyMean)~Keel,data=D30)
D20.A.thrust <- lm(Fx2Mean~Keel,data=D20)
D20.A.torque <- lm(abs(MyMean)~Keel,data=D20)
D10.A.thrust <- lm(Fx2Mean~Keel,data=D10)
D10.A.torque <- lm(abs(MyMean)~Keel,data=D10)
E30.A.thrust <- lm(Fx2Mean~Keel,data=E30)
E30.A.torque <- lm(abs(MyMean)~Keel,data=E30)
E20.A.thrust <- lm(Fx2Mean~Keel,data=E20)
E20.A.torque <- lm(abs(MyMean)~Keel,data=E20)
E10.A.thrust <- lm(Fx2Mean~Keel,data=E10)
E10.A.torque <- lm(abs(MyMean)~Keel,data=E10)
M30.A.thrust <- lm(Fx2Mean~Keel,data=M30)
M30.A.torque <- lm(abs(MyMean)~Keel,data=M30)
M20.A.thrust <- lm(Fx2Mean~Keel,data=M20)
M20.A.torque <- lm(abs(MyMean)~Keel,data=M20)
M10.A.thrust <- lm(Fx2Mean~Keel,data=M10)
M10.A.torque <- lm(abs(MyMean)~Keel,data=M10)

loadModel <- lm(MaxLoad ~ Shape+Keel+Speed, fullData)

#View ANOVA results

##bonferoni correction
pairwise.t.test(D30$Fx2Mean, D30$Keel, p.adj = "bonf")
false discovery rate - benjamini-hochberg

Anova(D30.A.thrust)
Anova(D30.A.torque)
Anova(D20.A.thrust)
Anova(D20.A.torque)
Anova(D10.A.thrust)
Anova(D10.A.torque)
Anova(E30.A.thrust)
Anova(E30.A.torque)
Anova(E20.A.thrust)
Anova(E20.A.torque)
Anova(E10.A.thrust)
Anova(E10.A.torque)
Anova(M30.A.thrust)
Anova(M30.A.torque)
Anova(M20.A.thrust)
Anova(M20.A.torque)
Anova(M10.A.thrust)
Anova(M10.A.torque)

#Thrust plot for 30m/s
D30.thrust <- ggplot(D30, aes(x = model, y = as.numeric(Fx2Mean), fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  ylab("Thrust (N)")+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  ylim(-1.5,1.5)+
  theme(plot.title = element_text(hjust=0.5), axis.title.x = element_blank(), legend.position="none")
E30.thrust <- ggplot(E30, aes(x = model, y = as.numeric(Fx2Mean), fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  ylab("Thrust (N)")+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  ylim(-1.5,1.5)+
  theme(plot.title = element_text(hjust=0.5), axis.title.x = element_blank(), legend.position="none")
M30.thrust <- ggplot(M30, aes(x = model, y = as.numeric(Fx2Mean), fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  ylab("Thrust (N)")+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  ylim(-1.5,1.5)+
  theme(plot.title = element_text(hjust=0.5), axis.title.x = element_blank(), legend.position="none")
thrust.30 <- grid.arrange(E30.thrust, M30.thrust, D30.thrust, ncol= 3)

#Torque plot for 30m/s
D30.torque <- ggplot(D30, aes(x = model, y = abs(MxMean)*1000, fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  ylab("X Torque (Nmm)")+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  ylim(0,5)+
  theme(plot.title = element_text(hjust=0.5), axis.title.x = element_blank(), legend.position="none")
E30.torque <- ggplot(E30, aes(x = model, y = abs(MxMean)*1000, fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  ylab("X Torque (Nmm)")+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  ylim(0,5)+
  theme(plot.title = element_text(hjust=0.5), axis.title.x = element_blank(), legend.position="none")
M30.torque <- ggplot(M30, aes(x = model, y = abs(MxMean)*1000, fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  ylab("X Torque (Nmm)")+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  ylim(0,5)+
  theme(plot.title = element_text(hjust=0.5), axis.title.x = element_blank(), legend.position="none")
torque.30 <- grid.arrange(E30.torque, M30.torque, D30.torque, ncol= 3)

#Plot for 30
grid.arrange(thrust.30, torque.30, nrow = 2)

#Thrust plot for 20m/s
D20.thrust <- ggplot(D20, aes(x = model, y = as.numeric(Fx2Mean), fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  ylab("Thrust (N)")+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  ylim(-0.5,1)+
  theme(plot.title = element_text(hjust=0.5), axis.title.x = element_blank(), legend.position="none")
E20.thrust <- ggplot(E20, aes(x = model, y = as.numeric(Fx2Mean), fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  ylab("Thrust (N)")+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  ylim(-0.5,1)+
  theme(plot.title = element_text(hjust=0.5), axis.title.x = element_blank(), legend.position="none")
M20.thrust <- ggplot(M20, aes(x = model, y = as.numeric(Fx2Mean), fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  ylab("Thrust (N)")+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  ylim(-0.5,1)+
  theme(plot.title = element_text(hjust=0.5), axis.title.x = element_blank(), legend.position="none")
thrust.20 <- grid.arrange(E20.thrust, M20.thrust, D20.thrust, ncol= 3)

#Torque plot for 20m/s
D20.torque <- ggplot(D20, aes(x = model, y = abs(MyMean)*1000, fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  ylab("X Torque (Nmm)")+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  ylim(0,20)+
  theme(plot.title = element_text(hjust=0.5), axis.title.x = element_blank(), legend.position="none")
E20.torque <- ggplot(E20, aes(x = model, y = abs(MyMean)*1000, fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  ylab("X Torque (Nmm)")+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  ylim(0,20)+
  theme(plot.title = element_text(hjust=0.5), axis.title.x = element_blank(), legend.position="none")
M20.torque <- ggplot(M20, aes(x = model, y = abs(MyMean)*1000, fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  ylab("X Torque (Nmm)")+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  ylim(0,20)+
  theme(plot.title = element_text(hjust=0.5), axis.title.x = element_blank(), legend.position="none")
torque.20 <- grid.arrange(E20.torque, M20.torque, D20.torque, ncol= 3)

#Plot for 20
grid.arrange(thrust.20, torque.20, nrow = 2)

#Thrust plot for 10m/s
D10.thrust <- ggplot(D10, aes(x = model, y = as.numeric(Fx2Mean), fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  ylab("Thrust (N)")+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  ylim(-1,0.8)+
  theme(plot.title = element_text(hjust=0.5), axis.title.x = element_blank(), legend.position="none")
E10.thrust <- ggplot(E10, aes(x = model, y = as.numeric(Fx2Mean), fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  ylab("Thrust (N)")+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  ylim(-1,0.8)+
  theme(plot.title = element_text(hjust=0.5), axis.title.x = element_blank(), legend.position="none")
M10.thrust <- ggplot(M10, aes(x = model, y = as.numeric(Fx2Mean), fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  ylab("Thrust (N)")+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  ylim(-1,0.8)+
  theme(plot.title = element_text(hjust=0.5), axis.title.x = element_blank(), legend.position="none")
thrust.10 <- grid.arrange(E10.thrust, M10.thrust, D10.thrust, ncol= 3)

#Torque plot for 10m/s
D10.torque <- ggplot(D10, aes(x = model, y = abs(MyMean)*1000, fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  ylab("X Torque (Nmm)")+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  ylim(0,16)+
  theme(plot.title = element_text(hjust=0.5), axis.title.x = element_blank(), legend.position="none")
E10.torque <- ggplot(E10, aes(x = model, y = abs(MyMean)*1000, fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  ylab("X Torque (Nmm)")+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  ylim(0,16)+
  theme(plot.title = element_text(hjust=0.5), axis.title.x = element_blank(), legend.position="none")
M10.torque <- ggplot(M10, aes(x = model, y = abs(MyMean)*1000, fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  ylab("X Torque (Nmm)")+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  ylim(0,16)+
  theme(plot.title = element_text(hjust=0.5), axis.title.x = element_blank(), legend.position="none")
torque.10 <- grid.arrange(E10.torque, M10.torque, D10.torque, ncol= 3)

#Plot for 10
grid.arrange(thrust.10, torque.10, nrow = 2)


#Torque Frequency plot for 30m/s
ggplot(thirty, aes(x = Frequency, y = MyMean, color = model))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_manual(values=colors)+
  theme_classic()+
  xlab("Frequency")+
  ylab("X Torque (Nm)")

#Torque Frequency plot for 20m/s
ggplot(twenty, aes(x = Frequency, y = MyMean, color = model))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_manual(values=colors)+
  theme_classic()+
  xlab("Frequency")+
  ylab("X Torque (Nm)")

#Torque Frequency plot for 10m/s
ggplot(ten, aes(x = Frequency, y = MyMean, color = model))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_manual(values=colors)+
  theme_classic()+
  xlab("Frequency")+
  ylab("X Torque (Nm)")

#Frequency plot for 30m/s
ggplot(thirty, aes(x = Frequency, y = as.numeric(Fx2Mean), color = model))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_manual(values=colors)+
  theme_classic()+
  xlab("Frequency")+
  ylab("Thrust (N)")

#Frequency plot for 20m/s
ggplot(twenty, aes(x = Frequency, y = Fx2Mean, color = model))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_manual(values=colors)+
  theme_classic()+
  xlab("Frequency")+
  ylab("Thrust (N)")

#Frequency plot for 10m/s
ggplot(ten, aes(x = Frequency, y = Fx2Mean, color = model))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_manual(values=colors)+
  theme_classic()+
  xlab("Frequency")+
  ylab("Thrust (N)")