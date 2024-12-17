library(tidyr)
library(ggplot2)
library(patchwork)
library(ggplot2)
library(pracma)
library(dplyr)

colMax <- function(data) sapply(data, max, na.rm=TRUE)

path <- "Flume Data" # path to the folder with all of the txt files

##Get all the arm position files
armFiles <- list.files(path = ".", pattern = "*arm.csv", all.files = FALSE, full.names = FALSE, 
                       recursive = TRUE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

## Get all the force files
forceFiles <- list.files(path = ".", pattern = "*FT.csv", all.files = FALSE, full.names = FALSE, 
                         recursive = TRUE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

# files <- files[grep("rotated", files)]
# files <- files[grep("test", files, invert = T)]

#### START Test Data  ####
# armData = as.data.frame(read.csv(armFiles[5]))
# forceData = as.data.frame(read.csv(forceFiles[5]))
# armData$theta_z_rad = (armData$theta_z*3.141592)/180
# 
# armData=armData[- 1, ]
# forceData=forceData[- 1, ]
# 
# x = linspace(min(armData$t), max(armData$t), length(forceData$t))
# y = 20*sin(2*pi*-1.5*x)
# yRad = y*pi/180
# 
# forceDataTest = forceData
# forceDataTest$x = forceDataTest$Fx*cos(yRad) + forceDataTest$Fy*sin(yRad)
# forceDataTest$y = forceDataTest$Fx*sin(yRad) + forceDataTest$Fy*cos(yRad)
# 
# forceDataTest = as.data.frame(colMeans(forceDataTest[2:7]))
# forceDataTest = as.data.frame(t(forceDataTest))

# matplot(forceDataTest$t, cbind(forceDataTest$x, forceDataTest$y,forceDataTest$Fz),
#         type = "l", lty = 1, col = c("red", "blue", "green"),
#         xlab = "Time", ylab = "Force(N)")
# 
# matplot(forceData$t, cbind(forceData$Fx, forceData$Fy,forceData$Fz),
#         type = "l", lty = 1, col = c("red", "blue", "green"),
#         xlab = "Time", ylab = "Force(N)")
# 
# matplot(cbind(forceData01$t, forceData02$t), cbind(forceData01$Fx, forceData02$Fx),
#         type = "l", lty = 1, col = c("red", "blue", "green"),
#         xlab = "Time", ylab = "Force(N)")
#### END Test Data  ####

#concat all data
data02 <- data.frame()
for (i in 1:length(forceFiles)) {
  fileName = forceFiles[i]  # get the file name of the i-th force file
  tmp_data = as.data.frame(read.csv(forceFiles[i]))  # read in the i-th force file 
  armData = as.data.frame(read.csv(armFiles[i]))     # read in the i-th arm file
  tmp_data=tmp_data[- 1, ]  # remove the first row (noise)
  name <- stringr::str_split(fileName,"_")[[1]]  # break up file name by underscores
  freq = as.numeric(name[4])  # convert frequency into a number
  
  # create a time matrix based on the min and max time
  x = linspace(min(armData$t), max(armData$t), length(tmp_data$t))
  # create a matrix based on set frequency to calculate angle
  y = 20*sin(2*pi*-freq*x)
  # convert to radians
  yRad = y*pi/180
  
  # Rotate Fx and Fy based on arm position
  tmp_data$Fx = tmp_data$Fx*cos(yRad) + tmp_data$Fy*sin(yRad)
  tmp_data$Fy = tmp_data$Fx*sin(yRad) + tmp_data$Fy*cos(yRad)

  # Take the mean of all the columns and convert to data frame
  
  ## Maximum force and torque
  #tmp_data = as.data.frame(colMax(tmp_data[2:7]))
  #tmp_data = as.data.frame(t(tmp_data))

  ## Average force and torque
  #tmp_data = as.data.frame(colMeans(tmp_data[2:7]))
  #tmp_data = as.data.frame(t(tmp_data))
  
  ## Work
  
  ## Thrust 
  
  
  
  # use file name to get meta data
  tmp_data$Body <- name[1]
  tmp_data$Keel <- name[2]
  tmp_data$Frequency <- name[4]
  tmp_data$Speed <- name[8]
  # tmp_data <- tmp_data[c(8:10,1:7)]
  # tmp_data[, c(2:10)] <- sapply(tmp_data[, c(2:10)], as.numeric)
  
  data02 <- rbind(data02,tmp_data) # add row to concatenaetd data
  # colnames(data) <- c("Model","Angle","Speed","t","Fx","Fy","Fz",
  #                     "Filtered.x","Filtered.y","Filtered.z")
  print(fileName) # tell us where we are
}

data02 <- data02[data02$Speed != 0,]
data02$model <- as.factor(paste(data02$Body, data02$Keel))
data02$Frequency <- as.numeric(data02$Frequency)

#For max
write.csv(data02, "Flume_Forces_Max_rotated.csv", row.names = F)

#For average
write.csv(data02, "Flume_Forces_Sum_rotated.csv", row.names = F)



dataSubset$model <- factor(dataSubset$model, levels = c("E no", "E 40", "E 60", 
                                                        "M no", "M 40", "M 60",
                                                        "D no", "D 40", "D 60"))

colors = c("palegreen", "green3", "darkgreen",
           "lightblue1", "dodgerblue", "dodgerblue4",
           "burlywood1", "orange", "darkorange4")

threeHz <- filter(dataSubset, Speed == "30")


ggplot(threeHz, aes(x = Frequency, y = Fy, color = model))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_manual(values=colors)+
  theme_classic()+
  xlab("Model")+
  ylab("Thrust (N)")

ggplot(threeHz, aes(y = Fy, fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  xlab("Model")+
  ylab("Thrust (N)")

ggplot(threeHz, aes(y = Mx, fill = model))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  theme_classic()+
  xlab("Model")+
  ylab("X Torque (Nm)")
