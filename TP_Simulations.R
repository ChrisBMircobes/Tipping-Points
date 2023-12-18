
###Change to 500 time units, change sensitivity to 1/f*e and resistance to e-f
###Change min tipping point to first extinction.
###Change aye to 0.01
###Change freq dependent a to max(1,afreq) instead of + min

##Play around with the max() and set it to 1, see what changes?  Not much, reduces initial a little bit
###Change the range from 0.61 to 0.81 


###Get changes in alpha, K, r and res., get both sides, change to double the environmental change..


library(deSolve)
library(ggplot2)
library(segmented)
library(dplyr)
#### Standard conditions
getwd()
setwd("~/Desktop")

x=0
i=0
f=0 
eN <- 0
sN <- 0
alpha<-vector()
ratio<-data.frame()

for(x in 1:1501){
  
  for(i in 1:50){
    
    Lokta <- function(t, n, parms){
      with(as.list(parms),{
        dn1dt <- (r1*(1/(1+e))) * n[1] * (1 - ((n[1] + (max(1,((n[2]/(n[1]+n[2]))*a12))) * n[2])/((1/(1+e))*k1)))
        dn2dt <- r2 * n[2] * (1 - ((n[2] +  ((n[1]/(n[1]+n[2]))*a21) * n[1])/k2))
        list(c(dn1dt, dn2dt))
      })
    }
    eN <- 1000 +(i-1)*2000
    sN <- 100000-eN
    initialN <- c(eN, sN)
    parms <- c(r1 = 0.75 , r2 = 0.5, k1 = 4*10^7, a21 = 0.01, k2 = 10^7, a12 = 3.5, e = -0.92+0.002*x)
    out <- ode(y = initialN, times = 1:500, func = Lokta, parms = parms)
    f <- i+50*(x-1)
    ratio[f,1] <- out[500,2]/(out[500,2]+ out[500,3])
    ratio[f,2] <- out[1,2]/(out[1,2]+ out[1,3])
    ratio[f,3] <- parms[7] 
    print(f)
  }
}

ratio

colnames(ratio) <- c("Ratio","InitialFrequency","Environment")

ratio$e <- as.factor(ratio$Environment)

##Getting the tipping point line

# Step 1: Filter the dataframe to include only the rows where the final ratio is smaller than 0.1%
zero_ratios <- ratio[ratio$Ratio <= 0.001, ]


# Step 2: Group the filtered dataframe by the environmental stress and get the max ratio, min ratio and tipping point (highest ratio that goes extinct)

tipping_data <- zero_ratios %>% group_by(Environment) %>% summarize(Ratio = max(InitialFrequency))
max_data <- ratio %>% group_by(Environment) %>% summarize(Ratio = max(Ratio))
min_data <-  ratio %>% group_by(Environment) %>% summarize(Ratio = min(Ratio))

mintipping_data <- tipping_data  %>% group_by(Ratio) %>% summarize(Environment = min(Environment))
tipping_data <- tipping_data  %>% group_by(Ratio) %>% summarize(Environment = mean(Environment))
tipping_data <- data.frame(Environment = tipping_data$Environment, Ratio = tipping_data$Ratio)


tipping_data2 <- filter(tipping_data, Ratio < 0.99)
tipping_data2 <- filter(tipping_data2, Environment <= 0.4660)

trial <- rbind(max_data, min_data, tipping_data)

plot(trial$Environment,trial$Ratio)

##Step 3: Filter data to remove unwanted values and get backbend curve.
max_data <- filter(max_data, Environment <= 0.465)
max_data$ID <- seq(1:nrow(max_data))
max_data$group <- rep("max", nrow(max_data))
min_data <- filter(min_data, Environment >=  0.174)
min_data$ID <- seq(nrow(max_data) + nrow(tipping_data) + 1, nrow(max_data) + nrow(tipping_data) + nrow(min_data))
min_data$group <- rep("min", nrow(min_data))
tipping_data <- filter(tipping_data, Environment <= 0.465)
tipping_data <- arrange(tipping_data, desc(tipping_data$Environment))
tipping_data$ID <- seq(nrow(max_data) + 1, nrow(max_data) + nrow(tipping_data))
tipping_data$group <- rep("tipping", nrow(tipping_data))







##Step 4: Bind all dataframes
grouped_data <- rbind(max_data, tipping_data, min_data)



tipping_data_low <- data.frame(Environment = seq(-0.9, 0.174, by = 0.002),Ratio = rep(0,538))
tipping_data_high <-  data.frame(Environment = seq(0.465, 2, by = 0.002),Ratio = rep(1,768))



tipping_data3 <- rbind(tipping_data_low,tipping_data2,tipping_data_high)

tipping_data4 <- filter(tipping_data3, Environment >= 0.1724)
tipping_data4 <- filter(tipping_data4, Environment <= 0.475)

grouped_data2 <- filter(grouped_data, group == "tipping")
test <- data.frame(Environment = 0.4641, Ratio = 0.5,ID = 233,group = "tipping")
test2 <- rbind(test,grouped_data2)

anc_data <- grouped_data
anc_data$Environment <- anc_data$Environment + 1

ratior <- ratio
ratior$Environment <- ratio$Environment + 1
tipping_data3$Environment <- tipping_data3$Environment + 1
tipping_data4$Environment <- tipping_data4$Environment + 1
test2$Environment <- test2$Environment + 1


ratioplot2b <- ggplot(ratior, aes(x = Environment, y = Ratio)) +
  theme_bw(base_size = 30) +
  theme(axis.title = element_blank(), legend.position = 0,panel.border = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_ribbon(data = tipping_data3, aes(x = Environment, ymin = Ratio, ymax = 1), fill = "chartreuse3", alpha = 0.4)  +
  geom_ribbon(data = tipping_data3, aes(x = Environment, ymax = Ratio, ymin = 0), fill = "lightgrey", alpha = 0.3)  +
  geom_ribbon(data = tipping_data3, aes(x = Environment, ymin = Ratio, ymax = 1),inherit.aes = FALSE, fill = "darkolivegreen4", alpha = 0.3)  +
  geom_path(data = tipping_data4, color = "darkorange", alpha = 1, size = 3 ) + 
  geom_point(size = 2.5, color = "black") +
  geom_path(data = anc_data, aes(x = Environment, y = Ratio), color = "cornflowerblue", size = 1, linetype = "dashed") +
  geom_path(data = test2, aes(x = Environment, y = Ratio), color = "#EF5350", size = 2, linetype = "dashed") +
# geom_ribbon(data = tipping_data2, aes(x = Environment, ymax = Ratio+0.05, ymin = Ratio-0.05), fill = "orange", alpha = 0.5)  +
  scale_y_continuous(limits = c(0,1),expand = c(0,0)) +
  scale_x_continuous(limits = c(0.1,3),  breaks = c(0.1,1,2,3) , expand = c(0,0)) +
  xlab("Environmental Stress") +
  ylab("Ecosystem State")
# scale_x_reverse(limits = c(0,1), expand = c(0,0))

ratioplot2b


ggsave(filename = "~/Desktop/AESPlots/Coexistence_Anc.png", plot = ratioplot2b, width = 10, height = 10, dpi = 300)









######

##Getting the base point line

# Step 1: Filter the dataframe to include only the rows where the final ratio is smaller than 0.1%
zero_ratios <- ratio[ratio$Ratio <= 0.001, ]


# Step 2: Group the filtered dataframe by the environmental stress and get the max ratio, min ratio and base point (highest ratio that goes extinct)

base_data <- zero_ratios %>% group_by(Environment) %>% summarize(Ratio = max(InitialFrequency))
max_data <- ratio %>% group_by(Environment) %>% summarize(Ratio = max(Ratio))
min_data <-  ratio %>% group_by(Environment) %>% summarize(Ratio = min(Ratio))

minbase_data <- base_data  %>% group_by(Ratio) %>% summarize(Environment = min(Environment))
base_data <- base_data  %>% group_by(Ratio) %>% summarize(Environment = mean(Environment))
base_data <- data.frame(Environment = base_data$Environment, Ratio = base_data$Ratio)


base_data2 <- filter(base_data, Ratio < 0.99)
base_data2 <- filter(base_data2, Environment <= 0.4660)

trial <- rbind(max_data, min_data, base_data)

plot(trial$Environment,trial$Ratio)

##Step 3: Filter data to remove unwanted values and get backbend curve.
max_data <- filter(max_data, Environment <= 0.465)
max_data$ID <- seq(1:nrow(max_data))
max_data$group <- rep("max", nrow(max_data))
min_data <- filter(min_data, Environment >=  0.174)
min_data$ID <- seq(nrow(max_data) + nrow(base_data) + 1, nrow(max_data) + nrow(base_data) + nrow(min_data))
min_data$group <- rep("min", nrow(min_data))
base_data <- filter(base_data, Environment <= 0.465)
base_data <- arrange(base_data, desc(base_data$Environment))
base_data$ID <- seq(nrow(max_data) + 1, nrow(max_data) + nrow(base_data))
base_data$group <- rep("base", nrow(base_data))







##Step 4: Bind all dataframes
grouped_data <- rbind(max_data, base_data, min_data)



base_data_low <- data.frame(Environment = seq(-0.9, 0.174, by = 0.002),Ratio = rep(0,538))
base_data_high <-  data.frame(Environment = seq(0.465, 2, by = 0.002),Ratio = rep(1,768))



base_data3 <- rbind(base_data_low,base_data2,base_data_high)

base_data4 <- filter(base_data3, Environment >= 0.1724)
base_data4 <- filter(base_data4, Environment <= 0.475)

grouped_data2 <- filter(grouped_data, group == "base")
test <- data.frame(Environment = 0.4641, Ratio = 0.5,ID = 233,group = "base")
test2 <- rbind(test,grouped_data2)






###Get changes in growthrate min 0.215, max 0.465

library(deSolve)
library(ggplot2)
library(segmented)
library(dplyr)
#### Standard conditions
getwd()
setwd("~/Desktop")

x=0
i=0
f=0 
eN <- 0
sN <- 0
alpha<-vector()
ratio2<-data.frame()

for(x in 1:1501){
  
  for(i in 1:50){
    
    Lokta <- function(t, n, parms){
      with(as.list(parms),{
        dn1dt <- (r1*(1/(1+e))) * n[1] * (1 - ((n[1] + (max(1,((n[2]/(n[1]+n[2]))*a12))) * n[2])/((1/(1+e))*k1)))
        dn2dt <- r2 * n[2] * (1 - ((n[2] +  ((n[1]/(n[1]+n[2]))*a21) * n[1])/k2))
        list(c(dn1dt, dn2dt))
      })
    }
    eN <- 1000 +(i-1)*2000
    sN <- 100000-eN
    initialN <- c(eN, sN)
    parms <- c(r1 = 0.9 , r2 = 0.5, k1 = 4*10^7, a21 = 0.01, k2 = 10^7, a12 = 3.5, e = -0.92+0.002*x)
    out <- ode(y = initialN, times = 1:500, func = Lokta, parms = parms)
    f <- i+50*(x-1)
    ratio2[f,1] <- out[500,2]/(out[500,2]+ out[500,3])
    ratio2[f,2] <- out[1,2]/(out[1,2]+ out[1,3])
    ratio2[f,3] <- parms[7] 
    print(f)
  }
}

ratio2

colnames(ratio2) <- c("ratio2","InitialFrequency","Environment")

ratio2$e <- as.factor(ratio2$Environment)


# Step 1: Filter the dataframe to include only the rows where the final ratio2 is smaller than 0.1%
zero_ratio2s <- ratio2[ratio2$ratio2 <= 0.001, ]


# Step 2: Group the filtered dataframe by the environmental stress and get the max ratio2, min ratio2 and tipping point (highest ratio2 that goes extinct)

tipping_data <- zero_ratio2s %>% group_by(Environment) %>% summarize(ratio2 = max(InitialFrequency))
max_data <- ratio2 %>% group_by(Environment) %>% summarize(ratio2 = max(ratio2))
min_data <-  ratio2 %>% group_by(Environment) %>% summarize(ratio2 = min(ratio2))

mintipping_data <- tipping_data  %>% group_by(ratio2) %>% summarize(Environment = min(Environment))
tipping_data <- tipping_data  %>% group_by(ratio2) %>% summarize(Environment = mean(Environment))
tipping_data <- data.frame(Environment = tipping_data$Environment, ratio2 = tipping_data$ratio2)


tipping_data2 <- filter(tipping_data, ratio2 < 0.99)
tipping_data2 <- filter(tipping_data2, Environment <= 0.4660)

trial <- rbind(max_data, min_data, tipping_data)

plot(trial$Environment,trial$ratio2)

##Step 3: Filter data to remove unwanted values and get backbend curve.
max_data <- filter(max_data, Environment <= 0.465)
max_data$ID <- seq(1:nrow(max_data))
max_data$group <- rep("max", nrow(max_data))
min_data <- filter(min_data, Environment >=  0.215)
min_data$ID <- seq(nrow(max_data) + nrow(tipping_data) + 1, nrow(max_data) + nrow(tipping_data) + nrow(min_data))
min_data$group <- rep("min", nrow(min_data))
tipping_data <- filter(tipping_data, Environment <= 0.465)
tipping_data <- arrange(tipping_data, desc(tipping_data$Environment))
tipping_data$ID <- seq(nrow(max_data) + 1, nrow(max_data) + nrow(tipping_data))
tipping_data$group <- rep("tipping", nrow(tipping_data))







##Step 4: Bind all dataframes
grouped_data <- rbind(max_data, tipping_data, min_data)



tipping_data_low <- data.frame(Environment = seq(-0.9, 0.2155, by = 0.002),ratio2 = rep(0,558))
tipping_data_high <-  data.frame(Environment = seq(0.465, 2, by = 0.002),ratio2 = rep(1,768))



tipping_data3 <- rbind(tipping_data_low,tipping_data2,tipping_data_high)

tipping_data4 <- filter(tipping_data3, Environment >= 0.2135)
tipping_data4 <- filter(tipping_data4, Environment <= 0.475)

grouped_data2 <- filter(grouped_data, group == "tipping")
test <- data.frame(Environment = 0.4641, ratio2 = 0.5,ID = 233,group = "tipping")
test2 <- rbind(test,grouped_data2)



ratio2$Environment <- ratio2$Environment + 1
tipping_data3$Environment <- tipping_data3$Environment + 1
tipping_data4$Environment <- tipping_data4$Environment + 1
test2$Environment <- test2$Environment + 1
base_data3$Environment <- base_data3$Environment + 1





ratio2plot2b <- ggplot(ratio2, aes(x = Environment, y = ratio2)) +
  theme_bw(base_size = 30) +
  theme(axis.title = element_blank(), legend.position = 0,panel.border = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_ribbon(data = tipping_data3, aes(x = Environment, ymin = ratio2, ymax = 1), fill = "chartreuse3", alpha = 0.4)  +
  geom_ribbon(data = tipping_data3, aes(x = Environment, ymax = ratio2, ymin = 0), fill = "lightgrey", alpha = 0.3)  +
  geom_ribbon(data = base_data3, aes(x = Environment, ymin = Ratio, ymax = 1),inherit.aes = FALSE, fill = "darkolivegreen4", alpha = 0.3)  +
  geom_path(data = tipping_data4, color = "darkorange", alpha = 1, size = 3 ) + 
  geom_point(size = 2.5) +
  geom_path(data = anc_data, aes(x = Environment, y = Ratio), color = "cornflowerblue", size = 1, linetype = "dashed") +
  geom_path(data = test2, aes(x = Environment, y = ratio2), color = "#EF5350", size = 2.5, linetype = "dashed") +
   # geom_ribbon(data = tipping_data2, aes(x = Environment, ymax = ratio2+0.05, ymin = ratio2-0.05), fill = "orange", alpha = 0.5)  +
  scale_y_continuous(limits = c(0,1),expand = c(0,0)) +
  scale_x_continuous(limits = c(0.1,3),  breaks = c(0.1,1,2,3) , expand = c(0,0)) +
  xlab("Environmental Stress") +
  ylab("Ecosystem State")
# scale_x_reverse(limits = c(0,1), expand = c(0,0))

ratio2plot2b


ggsave(filename = "~/Desktop/AESPlots/Coexistence_Growthrate.png", plot = ratio2plot2b, width = 10, height = 10, dpi = 300)















##changes in capacity, min 0.395, max 0.725


library(deSolve)
library(ggplot2)
library(segmented)
library(dplyr)
#### Standard conditions
getwd()
setwd("~/Desktop")

x=0
i=0
f=0 
eN <- 0
sN <- 0
alpha<-vector()
ratio3<-data.frame()

for(x in 1:1501){
  
  for(i in 1:50){
    
    Lokta <- function(t, n, parms){
      with(as.list(parms),{
        dn1dt <- (r1*(1/(1+e))) * n[1] * (1 - ((n[1] + (max(1,((n[2]/(n[1]+n[2]))*a12))) * n[2])/((1/(1+e))*k1)))
        dn2dt <- r2 * n[2] * (1 - ((n[2] +  ((n[1]/(n[1]+n[2]))*a21) * n[1])/k2))
        list(c(dn1dt, dn2dt))
      })
    }
    eN <- 1000 +(i-1)*2000
    sN <- 100000-eN
    initialN <- c(eN, sN)
    parms <- c(r1 = 0.75 , r2 = 0.5, k1 = 4.8*10^7, a21 = 0.01, k2 = 10^7, a12 = 3.5, e = -0.92+0.002*x)
    out <- ode(y = initialN, times = 1:500, func = Lokta, parms = parms)
    f <- i+50*(x-1)
    ratio3[f,1] <- out[500,2]/(out[500,2]+ out[500,3])
    ratio3[f,2] <- out[1,2]/(out[1,2]+ out[1,3])
    ratio3[f,3] <- parms[7] 
    print(f)
  }
}

ratio3

colnames(ratio3) <- c("ratio3","InitialFrequency","Environment")

ratio3$e <- as.factor(ratio3$Environment)






##Getting the tipping point line

# Step 1: Filter the dataframe to include only the rows where the final ratio3 is smaller than 0.1%
zero_ratio3s <- ratio3[ratio3$ratio3 <= 0.001, ]


# Step 2: Group the filtered dataframe by the environmental stress and get the max ratio3, min ratio3 and tipping point (highest ratio3 that goes extinct)

tipping_data <- zero_ratio3s %>% group_by(Environment) %>% summarize(ratio3 = max(InitialFrequency))
max_data <- ratio3 %>% group_by(Environment) %>% summarize(ratio3 = max(ratio3))
min_data <-  ratio3 %>% group_by(Environment) %>% summarize(ratio3 = min(ratio3))

mintipping_data <- tipping_data  %>% group_by(ratio3) %>% summarize(Environment = min(Environment))
tipping_data <- tipping_data  %>% group_by(ratio3) %>% summarize(Environment = mean(Environment))
tipping_data <- data.frame(Environment = tipping_data$Environment, ratio3 = tipping_data$ratio3)


tipping_data2 <- filter(tipping_data, ratio3 < 0.99)
tipping_data2 <- filter(tipping_data2, Environment <= 0.7515)

trial <- rbind(max_data, min_data, tipping_data)

plot(trial$Environment,trial$ratio3)

##Step 3: Filter data to remove unwanted values and get backbend curve.
max_data <- filter(max_data, Environment <= 0.730)
max_data$ID <- seq(1:nrow(max_data))
max_data$group <- rep("max", nrow(max_data))
min_data <- filter(min_data, Environment >=  0.395)
min_data$ID <- seq(nrow(max_data) + nrow(tipping_data) + 1, nrow(max_data) + nrow(tipping_data) + nrow(min_data))
min_data$group <- rep("min", nrow(min_data))
tipping_data <- filter(tipping_data, Environment <= 0.730)
tipping_data <- arrange(tipping_data, desc(tipping_data$Environment))
tipping_data$ID <- seq(nrow(max_data) + 1, nrow(max_data) + nrow(tipping_data))
tipping_data$group <- rep("tipping", nrow(tipping_data))







##Step 4: Bind all dataframes
grouped_data <- rbind(max_data, tipping_data, min_data)

tipping_data_low <- data.frame(Environment = seq(-0.9, 0.395, by = 0.002),ratio3 = rep(0,648))
tipping_data_high <-  data.frame(Environment = seq(0.730, 2, by = 0.002),ratio3 = rep(1,636))



tipping_data3 <- rbind(tipping_data_low,tipping_data2,tipping_data_high)

tipping_data4 <- filter(tipping_data3, Environment >= 0.394)
tipping_data4 <- filter(tipping_data4, Environment <= 0.73)

grouped_data2 <- filter(grouped_data, group == "tipping")
test <- data.frame(Environment = 0.71, ratio3 = 0.5,ID = 233,group = "tipping")
test2 <- rbind(test,grouped_data2)


ratio3$Environment <- ratio3$Environment + 1
tipping_data3$Environment <- tipping_data3$Environment + 1
tipping_data4$Environment <- tipping_data4$Environment + 1
test2$Environment <- test2$Environment + 1
scale_x_continuous(limits = c(0.1,3),  breaks = c(0.1,1,2,3) , expand = c(0,0)) 
  


ratio3plot2b <- ggplot(ratio3, aes(x = Environment, y = ratio3)) +
  theme_bw(base_size = 30) +
  theme(axis.title = element_blank(), legend.position = 0,panel.border = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_ribbon(data = tipping_data3, aes(x = Environment, ymin = ratio3, ymax = 1), fill = "chartreuse3", alpha = 0.4)  +
  geom_ribbon(data = tipping_data3, aes(x = Environment, ymax = ratio3, ymin = 0), fill = "lightgrey", alpha = 0.3)  +
  geom_ribbon(data = base_data3, aes(x = Environment, ymin = Ratio, ymax = 1),inherit.aes = FALSE, fill = "darkolivegreen4", alpha = 0.3)  +
  geom_path(data = tipping_data4, color = "darkorange", alpha = 1, size = 3 ) + 
  geom_point(size = 2.5) +
  geom_path(data = anc_data, aes(x = Environment, y = Ratio), color = "cornflowerblue", size = 1, linetype = "dashed") +
  geom_path(data = test2, aes(x = Environment, y = ratio3), color = "#EF5350", size = 2.5, linetype = "dashed") +
  # geom_ribbon(data = tipping_data2, aes(x = Environment, ymax = ratio3+0.05, ymin = ratio3-0.05), fill = "orange", alpha = 0.5)  +
  scale_y_continuous(limits = c(0,1),expand = c(0,0)) +
  scale_x_continuous(limits = c(0.1,3),  breaks = c(0.1,1,2,3) , expand = c(0,0)) +
  xlab("Environmental Stress") +
  ylab("Ecosystem State")
# scale_x_reverse(limits = c(0,1), expand = c(0,0))

ratio3plot2b


ggsave(filename = "~/Desktop/AESPlots/Capacity_Growthrate.png", plot = ratio3plot2b, width = 10, height = 10, dpi = 300)








###changes in ecoli comp ae, min 0.175,max 0.49 


library(deSolve)
library(ggplot2)
library(segmented)
library(dplyr)
#### Standard conditions
getwd()
setwd("~/Desktop")

x=0
i=0
f=0 
eN <- 0
sN <- 0
alpha<-vector()
ratio4<-data.frame()

for(x in 1:1501){
  
  for(i in 1:50){
    
    Lokta <- function(t, n, parms){
      with(as.list(parms),{
        dn1dt <- (r1*(1/(1+e))) * n[1] * (1 - ((n[1] + (max(1,((n[2]/(n[1]+n[2]))*a12))) * n[2])/((1/(1+e))*k1)))
        dn2dt <- r2 * n[2] * (1 - ((n[2] +  ((n[1]/(n[1]+n[2]))*a21) * n[1])/k2))
        list(c(dn1dt, dn2dt))
      })
    }
    eN <- 1000 +(i-1)*2000
    sN <- 100000-eN
    initialN <- c(eN, sN)
    parms <- c(r1 = 0.75 , r2 = 0.5, k1 = 4*10^7, a21 = 0.05, k2 = 10^7, a12 = 3.5, e = -0.92+0.002*x)
    out <- ode(y = initialN, times = 1:500, func = Lokta, parms = parms)
    f <- i+50*(x-1)
    ratio4[f,1] <- out[500,2]/(out[500,2]+ out[500,3])
    ratio4[f,2] <- out[1,2]/(out[1,2]+ out[1,3])
    ratio4[f,3] <- parms[7] 
    print(f)
  }
}

ratio4

colnames(ratio4) <- c("ratio4","InitialFrequency","Environment")

ratio4$e <- as.factor(ratio4$Environment)



##Getting the tipping point line

# Step 1: Filter the dataframe to include only the rows where the final ratio4 is smaller than 0.1%
zero_ratio4s <- ratio4[ratio4$ratio4 <= 0.001, ]


# Step 2: Group the filtered dataframe by the environmental stress and get the max ratio4, min ratio4 and tipping point (highest ratio4 that goes extinct)

tipping_data <- zero_ratio4s %>% group_by(Environment) %>% summarize(ratio4 = max(InitialFrequency))
max_data <- ratio4 %>% group_by(Environment) %>% summarize(ratio4 = max(ratio4))
min_data <-  ratio4 %>% group_by(Environment) %>% summarize(ratio4 = min(ratio4))

mintipping_data <- tipping_data  %>% group_by(ratio4) %>% summarize(Environment = min(Environment))
tipping_data <- tipping_data  %>% group_by(ratio4) %>% summarize(Environment = mean(Environment))
tipping_data <- data.frame(Environment = tipping_data$Environment, ratio4 = tipping_data$ratio4)


tipping_data2 <- filter(tipping_data, ratio4 < 0.99)
tipping_data2 <- filter(tipping_data2, Environment <= 0.49)

trial <- rbind(max_data, min_data, tipping_data)

plot(trial$Environment,trial$ratio4)

##Step 3: Filter data to remove unwanted values and get backbend curve.
max_data <- filter(max_data, Environment <= 0.49)
max_data$ID <- seq(1:nrow(max_data))
max_data$group <- rep("max", nrow(max_data))
min_data <- filter(min_data, Environment >=  0.175)
min_data$ID <- seq(nrow(max_data) + nrow(tipping_data) + 1, nrow(max_data) + nrow(tipping_data) + nrow(min_data))
min_data$group <- rep("min", nrow(min_data))
tipping_data <- filter(tipping_data, Environment <= 0.49)
tipping_data <- arrange(tipping_data, desc(tipping_data$Environment))
tipping_data$ID <- seq(nrow(max_data) + 1, nrow(max_data) + nrow(tipping_data))
tipping_data$group <- rep("tipping", nrow(tipping_data))


###changes in ecoli comp ae, min 0.175,max 0.49 




##Step 4: Bind all dataframes
grouped_data <- rbind(max_data, tipping_data, min_data)

tipping_data_low <- data.frame(Environment = seq(-0.9, 0.175, by = 0.002),ratio4 = rep(0,538))
tipping_data_high <-  data.frame(Environment = seq(0.495, 2, by = 0.002),ratio4 = rep(1,753))



tipping_data3 <- rbind(tipping_data_low,tipping_data2,tipping_data_high)

tipping_data4 <- filter(tipping_data3, Environment >= 0.174)
tipping_data4 <- filter(tipping_data4, Environment <= 0.495)

grouped_data2 <- filter(grouped_data, group == "tipping")
test <- data.frame(Environment = 0.495, ratio4 = 0.5,ID = 233,group = "tipping")
test2 <- rbind(test,grouped_data2)



ratio4$Environment <- ratio4$Environment + 1
tipping_data3$Environment <- tipping_data3$Environment + 1
tipping_data4$Environment <- tipping_data4$Environment + 1
test2$Environment <- test2$Environment + 1
scale_x_continuous(limits = c(0.1,3),  breaks = c(0.1,1,2,3) , expand = c(0,0)) 
  
ratio4

ratio4plot2b <- ggplot(ratio4, aes(x = Environment, y = ratio4)) +
  theme_bw(base_size = 30) +
  theme(axis.title = element_blank(), legend.position = 0,panel.border = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_ribbon(data = tipping_data3, aes(x = Environment, ymin = ratio4, ymax = 1), fill = "chartreuse3", alpha = 0.4)  +
  geom_ribbon(data = tipping_data3, aes(x = Environment, ymax = ratio4, ymin = 0), fill = "lightgrey", alpha = 0.3)  +
  geom_ribbon(data = base_data3, aes(x = Environment, ymin = Ratio, ymax = 1),inherit.aes = FALSE, fill = "darkolivegreen4", alpha = 0.3)  +
  geom_path(data = tipping_data4, color = "darkorange", alpha = 1, size = 3 ) + 
  annotate("rect", xmin = 0.1, xmax = 0.195, ymin = 0, ymax = 1, fill = "darkgrey", alpha = 1 ) + 
  geom_point(size = 2.5) +
  geom_path(data = anc_data, aes(x = Environment, y = Ratio), color = "cornflowerblue", size = 1, linetype = "dashed") +
  geom_path(data = test2, aes(x = Environment, y = ratio4), color = "#EF5350", size = 2.5, linetype = "dashed") +
  # geom_ribbon(data = tipping_data2, aes(x = Environment, ymax = ratio4+0.05, ymin = ratio4-0.05), fill = "orange", alpha = 0.5)  +
  scale_y_continuous(limits = c(0,1),expand = c(0,0)) +
  scale_x_continuous(limits = c(0.1,3),  breaks = c(0.1,1,2,3) , expand = c(0,0)) +
  xlab("Environmental Stress") +
  ylab("Ecosystem State")
# scale_x_reverse(limits = c(0,1), expand = c(0,0))

ratio4plot2b


ggsave(filename = "~/Desktop/AESPlots/aye.png", plot = ratio4plot2b, width = 10, height = 10, dpi = 300)















##changes in yeast competition aey, min  0.45,max 0.69


library(deSolve)
library(ggplot2)
library(segmented)
library(dplyr)
#### Standard conditions
getwd()
setwd("~/Desktop")

x=0
i=0
f=0 
eN <- 0
sN <- 0
alpha<-vector()
ratio5<-data.frame()

for(x in 1:1501){
  
  for(i in 1:50){
    
    Lokta <- function(t, n, parms){
      with(as.list(parms),{
        dn1dt <- (r1*(1/(1+e))) * n[1] * (1 - ((n[1] + (max(1,((n[2]/(n[1]+n[2]))*a12))) * n[2])/((1/(1+e))*k1)))
        dn2dt <- r2 * n[2] * (1 - ((n[2] +  ((n[1]/(n[1]+n[2]))*a21) * n[1])/k2))
        list(c(dn1dt, dn2dt))
      })
    }
    eN <- 1000 +(i-1)*2000
    sN <- 100000-eN
    initialN <- c(eN, sN)
    parms <- c(r1 = 0.75 , r2 = 0.5, k1 = 4*10^7, a21 = 0.01, k2 = 10^7, a12 = 2.8, e = -0.92+0.002*x)
    out <- ode(y = initialN, times = 1:500, func = Lokta, parms = parms)
    f <- i+50*(x-1)
    ratio5[f,1] <- out[500,2]/(out[500,2]+ out[500,3])
    ratio5[f,2] <- out[1,2]/(out[1,2]+ out[1,3])
    ratio5[f,3] <- parms[7] 
    print(f)
  }
}

ratio5

colnames(ratio5) <- c("ratio5","InitialFrequency","Environment")

ratio5$e <- as.factor(ratio5$Environment)






##Getting the tipping point line

# Step 1: Filter the dataframe to include only the rows where the final ratio5 is smaller than 0.1%
zero_ratio5s <- ratio5[ratio5$ratio5 <= 0.001, ]


# Step 2: Group the filtered dataframe by the environmental stress and get the max ratio5, min ratio5 and tipping point (highest ratio5 that goes extinct)

tipping_data <- zero_ratio5s %>% group_by(Environment) %>% summarize(ratio5 = max(InitialFrequency))
max_data <- ratio5 %>% group_by(Environment) %>% summarize(ratio5 = max(ratio5))
min_data <-  ratio5 %>% group_by(Environment) %>% summarize(ratio5 = min(ratio5))

mintipping_data <- tipping_data  %>% group_by(ratio5) %>% summarize(Environment = min(Environment))
tipping_data <- tipping_data  %>% group_by(ratio5) %>% summarize(Environment = mean(Environment))
tipping_data <- data.frame(Environment = tipping_data$Environment, ratio5 = tipping_data$ratio5)


tipping_data2 <- filter(tipping_data, ratio5 < 0.99)
tipping_data2 <- filter(tipping_data2, Environment <= 0.7)

trial <- rbind(max_data, min_data, tipping_data)

plot(trial$Environment,trial$ratio5)

##changes in yeast competition aey, min  0.45,max 0.69



##Step 3: Filter data to remove unwanted values and get backbend curve.
max_data <- filter(max_data, Environment <= 0.69)
max_data$ID <- seq(1:nrow(max_data))
max_data$group <- rep("max", nrow(max_data))
min_data <- filter(min_data, Environment >=  0.45)
min_data$ID <- seq(nrow(max_data) + nrow(tipping_data) + 1, nrow(max_data) + nrow(tipping_data) + nrow(min_data))
min_data$group <- rep("min", nrow(min_data))
tipping_data <- filter(tipping_data, Environment <= 0.69)
tipping_data <- arrange(tipping_data, desc(tipping_data$Environment))
tipping_data$ID <- seq(nrow(max_data) + 1, nrow(max_data) + nrow(tipping_data))
tipping_data$group <- rep("tipping", nrow(tipping_data))


###changes in ecoli comp ae, min 0.175,max 0.49 




##Step 4: Bind all dataframes
grouped_data <- rbind(max_data, tipping_data, min_data)

tipping_data_low <- data.frame(Environment = seq(-0.9, 0.45, by = 0.002),ratio5 = rep(0,676))
tipping_data_high <-  data.frame(Environment = seq(0.69, 2, by = 0.002),ratio5 = rep(1,656))



tipping_data3 <- rbind(tipping_data_low,tipping_data2,tipping_data_high)

tipping_data4 <- filter(tipping_data3, Environment >= 0.445)
tipping_data4 <- filter(tipping_data4, Environment <= 0.69)

grouped_data2 <- filter(grouped_data, group == "tipping")
test <- data.frame(Environment = 0.685, ratio5 = 0.5,ID = 233,group = "tipping")
test2 <- rbind(test,grouped_data2)

 


ratio5$Environment <- ratio5$Environment + 1
tipping_data3$Environment <- tipping_data3$Environment + 1
tipping_data4$Environment <- tipping_data4$Environment + 1
test2$Environment <- test2$Environment + 1
scale_x_continuous(limits = c(0.1,3),  breaks = c(0.1,1,2,3) , expand = c(0,0)) 



ratio5plot2b <- ggplot(ratio5, aes(x = Environment, y = ratio5)) +
  theme_bw(base_size = 30) +
  theme(axis.title = element_blank(), legend.position = 0,panel.border = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_ribbon(data = tipping_data3, aes(x = Environment, ymin = ratio5, ymax = 1), fill = "chartreuse3", alpha = 0.4)  +
  geom_ribbon(data = tipping_data3, aes(x = Environment, ymax = ratio5, ymin = 0), fill = "lightgrey", alpha = 0.3)  +
  geom_ribbon(data = base_data3, aes(x = Environment, ymin = Ratio, ymax = 1),inherit.aes = FALSE, fill = "darkolivegreen4", alpha = 0.3)  +
  geom_path(data = tipping_data4, color = "darkorange", alpha = 1, size = 3 ) + 
  geom_point(size = 2.5) +
  geom_path(data = anc_data, aes(x = Environment, y = Ratio), color = "cornflowerblue", size = 1, linetype = "dashed") +
  geom_path(data = test2, aes(x = Environment, y = ratio5), color = "#EF5350", size = 2.5, linetype = "dashed") +
  # geom_ribbon(data = tipping_data2, aes(x = Environment, ymax = ratio5+0.05, ymin = ratio5-0.05), fill = "orange", alpha = 0.5)  +
  scale_y_continuous(limits = c(0,1),expand = c(0,0)) +
  scale_x_continuous(limits = c(0.1,3),  breaks = c(0.1,1,2,3) , expand = c(0,0)) +
  xlab("Environmental Stress") +
  ylab("Ecosystem State")
# scale_x_reverse(limits = c(0,1), expand = c(0,0))

ratio5plot2b


ggsave(filename = "~/Desktop/AESPlots/aey.png", plot = ratio5plot2b, width = 10, height = 10, dpi = 300)











###changes in resistance, min 0.325,max 0.615











###changes in resistance, min 0.325,max 0.615


library(deSolve)
library(ggplot2)
library(segmented)
library(dplyr)
#### Standard conditions
getwd()
setwd("~/Desktop")

x=0
i=0
f=0 
eN <- 0
sN <- 0
alpha<-vector()
ratio7<-data.frame()

for(x in 1:1501){
  
  for(i in 1:50){
    
    Lokta <- function(t, n, parms){
      with(as.list(parms),{
        dn1dt <- (r1*(1/(1+max(0,(e-f))))) * n[1] * (1 - ((n[1] + (max(1,((n[2]/(n[1]+n[2]))*a12))) * n[2])/((1/max(1,(1+(e-f))))*k1)))
        dn2dt <- r2 * n[2] * (1 - ((n[2] +  ((n[1]/(n[1]+n[2]))*a21) * n[1])/k2))
        list(c(dn1dt, dn2dt))
      })
    }
    eN <- 1000 +(i-1)*2000
    sN <- 100000-eN
    initialN <- c(eN, sN)
    parms <- c(r1 = 0.75 , r2 = 0.5, k1 = 4*10^7, a21 = 0.01, k2 = 10^7, a12 = 3.5, e = -0.92+0.002*x, f = 0.2325)
    out <- ode(y = initialN, times = 1:500, func = Lokta, parms = parms)
    f <- i+50*(x-1)
    ratio7[f,1] <- out[500,2]/(out[500,2]+ out[500,3])
    ratio7[f,2] <- out[1,2]/(out[1,2]+ out[1,3])
    ratio7[f,3] <- parms[7] 
    print(f)
  }
}

ratio7

colnames(ratio7) <- c("ratio7","InitialFrequency","Environment")

ratio7$e <- as.factor(ratio7$Environment)





##Getting the tipping point line

# Step 1: Filter the dataframe to include only the rows where the final ratio7 is smaller than 0.1%
zero_ratio7s <- ratio7[ratio7$ratio7 <= 0.001, ]


# Step 2: Group the filtered dataframe by the environmental stress and get the max ratio7, min ratio7 and tipping point (highest ratio7 that goes extinct)

tipping_data <- zero_ratio7s %>% group_by(Environment) %>% summarize(ratio7 = max(InitialFrequency))
max_data <- ratio7 %>% group_by(Environment) %>% summarize(ratio7 = max(ratio7))
min_data <-  ratio7 %>% group_by(Environment) %>% summarize(ratio7 = min(ratio7))

mintipping_data <- tipping_data  %>% group_by(ratio7) %>% summarize(Environment = min(Environment))
tipping_data <- tipping_data  %>% group_by(ratio7) %>% summarize(Environment = mean(Environment))
tipping_data <- data.frame(Environment = tipping_data$Environment, ratio7 = tipping_data$ratio7)


tipping_data2 <- filter(tipping_data, ratio7 < 0.99)
tipping_data2 <- filter(tipping_data2, Environment <= 0.6975)

trial <- rbind(max_data, min_data, tipping_data)

plot(trial$Environment,trial$ratio7)

##changes in yeast competition aey, min  0.45,max 0.69

###changes in resistance, min 0.325,max 0.615 0.465 0.174
0.465 + 0.2325
0.174 + 0.2325
##Step 3: Filter data to remove unwanted values and get backbend curve.
max_data <- filter(max_data, Environment <= 0.6975)
max_data$ID <- seq(1:nrow(max_data))
max_data$group <- rep("max", nrow(max_data))
min_data <- filter(min_data, Environment >=  0.4065)
min_data$ID <- seq(nrow(max_data) + nrow(tipping_data) + 1, nrow(max_data) + nrow(tipping_data) + nrow(min_data))
min_data$group <- rep("min", nrow(min_data))
tipping_data <- filter(tipping_data, Environment <= 0.6975)
tipping_data <- arrange(tipping_data, desc(tipping_data$Environment))
tipping_data$ID <- seq(nrow(max_data) + 1, nrow(max_data) + nrow(tipping_data))
tipping_data$group <- rep("tipping", nrow(tipping_data))





##Step 4: Bind all dataframes
grouped_data <- rbind(max_data, tipping_data, min_data)

tipping_data_low <- data.frame(Environment = seq(-0.9, 0.4065, by = 0.002),ratio7 = rep(0,654))
tipping_data_high <-  data.frame(Environment = seq(0.6975, 2, by = 0.002),ratio7 = rep(1,652))



tipping_data3 <- rbind(tipping_data_low,tipping_data2,tipping_data_high)

tipping_data4 <- filter(tipping_data3, Environment >= 0.406)
tipping_data4 <- filter(tipping_data4, Environment <= 0.6975)

grouped_data2 <- filter(grouped_data, group == "tipping")
test <- data.frame(Environment = 0.6975, ratio7 = 0.5,ID = 233,group = "tipping")
test2 <- rbind(test,grouped_data2)



ratio7$Environment <- ratio7$Environment + 1
tipping_data3$Environment <- tipping_data3$Environment + 1
tipping_data4$Environment <- tipping_data4$Environment + 1
test2$Environment <- test2$Environment + 1
scale_x_continuous(limits = c(0.1,3),  breaks = c(0.1,1,2,3) , expand = c(0,0)) 





ratio7plot2b <- ggplot(ratio7, aes(x = Environment, y = ratio7)) +
  theme_bw(base_size = 30) +
  theme(axis.title = element_blank(), legend.position = 0,panel.border = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_ribbon(data = tipping_data3, aes(x = Environment, ymin = ratio7, ymax = 1), fill = "chartreuse3", alpha = 0.4)  +
  geom_ribbon(data = tipping_data3, aes(x = Environment, ymax = ratio7, ymin = 0), fill = "lightgrey", alpha = 0.3)  +
  geom_ribbon(data = base_data3, aes(x = Environment, ymin = Ratio, ymax = 1),inherit.aes = FALSE, fill = "darkolivegreen4", alpha = 0.3)  +
  geom_path(data = tipping_data4, color = "darkorange", alpha = 1, size = 3 ) + 
  geom_point(size = 2.5) +
  geom_path(data = anc_data, aes(x = Environment, y = Ratio), color = "cornflowerblue", size = 1, linetype = "dashed") +
  geom_path(data = test2, aes(x = Environment, y = ratio7), color = "#EF5350", size = 2.5, linetype = "dashed") +
  # geom_ribbon(data = tipping_data2, aes(x = Environment, ymax = ratio7+0.05, ymin = ratio7-0.05), fill = "orange", alpha = 0.5)  +
  scale_y_continuous(limits = c(0,1),expand = c(0,0)) +
  scale_x_continuous(limits = c(0.1,3),  breaks = c(0.1,1,2,3) , expand = c(0,0)) + 
  xlab("Environmental Stress") +
  ylab("Ecosystem State")
# scale_x_reverse(limits = c(0,1), expand = c(0,0))

ratio7plot2b


ggsave(filename = "~/Desktop/AESPlots/res.png", plot = ratio7plot2b, width = 10, height = 10, dpi = 300)








































### Include more concept steps to rounden the edges.



concept <- data.frame(Environment = c(0,0.8,0.87,0.9,1,1.2,1.4,1.6,1.8,1.94,1.97,2,2.04,2.09,2.1,2.2,2.3,2.31,3), Ratio = c(0,0,0,0.1,0.15,0.2,0.25,0.3,0.35,0.38,0.39,0.4,0.43,0.47,0.48,0.6,0.9,1,1),ID = seq(1,19))
concept2 <- data.frame(Environment = c(0.87,0.9,1,1.2,1.4,1.6,1.8,1.94,1.97,2,2.04,2.09,2.1,2.2,2.3,2.31), Ratio = c(0,0.1,0.15,0.2,0.25,0.3,0.35,0.38,0.39,0.4,0.43,0.47,0.48,0.6,0.9,1),ID = seq(1,16))

concept3 <- concept

concept4 <- concept2

concept4$Environment <- concept2$Environment + 0.72

concept3$Environment <- concept3$Environment + 0.72

concept3$Environment[1] = 0


trialplot <- ggplot(concept, aes(x = Environment, y = Ratio)) + 
  theme_bw(base_size = 25) +
  theme(legend.position = 0,panel.border = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_ribbon(data = concept3, aes(x = Environment, ymin = Ratio, ymax = 1), fill = "chocolate2", alpha = 0.4)  +
  geom_ribbon(aes(x = Environment, ymin = Ratio, ymax = 1), fill = "darkolivegreen4", alpha = 0.25)  +
  geom_ribbon(data = concept3, aes(x = Environment, ymax = Ratio, ymin = 0), fill = "lightgrey", alpha = 0.3)  +
  geom_path(data = concept4, color = "#FFC107", alpha = 1, size = 7) + 
  xlab("Environmental Stress") +
  ylab("Ecosystem State") + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(limits = c(0,1),expand = c(0,0)) 

trialplot





























