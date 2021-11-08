# options(max.print=1000000)

# Libraries
library(ggplot2)
library(rcompanion)
library(FSA)
library(stringr)
library(DescTools)
library(nortest)
library(tidyverse)
library(psych)
library(rstatix)
# Load csv's

# memory [KB], Power_raw [uW] "power delta" [uW], "cpu norm" [%]
trepn=read.csv(file="../experiments/processed/trepn.csv", header=TRUE)
wireshark=read.csv(file="../experiments/processed/wireshark.csv", header=TRUE)

trepn$app = as.factor(trepn$app)
trepn$treatment = as.factor(trepn$treatment)
trepn$participants = as.factor(trepn$participants)
trepn$microphone = as.factor(trepn$microphone)

wireshark$app = as.factor(wireshark$app)
wireshark$treatment = as.factor(wireshark$treatment)
wireshark$participants = as.factor(wireshark$participants)
wireshark$microphone = as.factor(wireshark$microphone)

head(trepn)
head(wireshark)

# memory, power.delta, cpu.norm, packet.size
colors = c("darkorange1", "deeppink3", "firebrick3", "darkblue")

col_count = 1
############################# RQ1 #############################

# 1. Grouping 

## aggregate treatment data for all three apps
trepn_f = trepn[trepn$app == "facebook", ]  
trepn_i = trepn[trepn$app == "instagram", ]  
trepn_w = trepn[trepn$app == "whatsapp", ]  

wireshark_f = wireshark[wireshark$app == "facebook", ]  
wireshark_i = wireshark[wireshark$app == "instagram", ]  
wireshark_w = wireshark[wireshark$app == "whatsapp", ]  



############ TODO: add wrireshark data ############
# 2. data exploration

## 2.1 boxplots (violin)

for (col in colnames(trepn)){
  if (!is.factor(trepn[[col]]) & !grepl('ms', col, fixed = TRUE)) {
    print(col)
    X = 'app'
    print(p <- ggplot(trepn, aes_string(x=X, y=col, fill=X))
          + scale_fill_manual(values = c("#4267B2", "#bc2a8d", "#25D366"))
          + geom_violin() + geom_boxplot(width = .1) + labs(y = toString(col))
          + theme(text = element_text(size=25),
                  legend.text = element_text(angle=270, margin = margin(t = 15)),
                  legend.position = "none",
                  legend.direction = "vertical")
          + guides(fill = guide_legend(title = "",
                                       label.position = "bottom",
                                       title.position = "left", title.vjust = 1))
    )
    
    ggsave(paste("../results/Violin_", X, "-", col, ".png", sep = ""))
    if (col == 'cpu.norm'){
      X = 'app'
      col = 'packet.size'
      print(col)
      print(p <- ggplot(wireshark, aes_string(x=X, y=col, fill=X)) 
            + scale_fill_manual(values = c("#4267B2", "#bc2a8d", "#25D366"))
            + geom_violin() + geom_boxplot(width = .1, show.legend = FALSE) + labs(y = toString(col))
            + theme(text = element_text(size=25),
                    legend.text = element_text(angle=270, margin = margin(t = 15)),
                    legend.position = "none",
                    legend.direction = "vertical")
            + guides(fill = guide_legend(title = "",
                                         label.position = "bottom",
                                         title.position = "left", title.vjust = 1))
      )
      
      ggsave(paste("../results/Violin_", X, "-", col, ".png", sep = ""))
    }
  }
};dev.off()


## 2.2 qqplots

############## COMBINED ############## 

png(paste("../results/app_qqplots.png"), width = 9.4, height = 8, units = "in", res = 300);
par(mfrow = c(4,3), omi=c(0.3,0.2,0,0.2), plt=c(0.2,0.9,0.05,0.7));
for (col in colnames(trepn)){
  if (!is.factor(trepn[[col]]) & !grepl('ms', col, fixed = TRUE)) {
    print(col)
    app = 'Facebook'
    X = col
    qqnorm(trepn_f[[col]], col = '#4267B2', main=paste("QQplot of ", X, " for ", app, sep= ""), xlab=X, cex.lab=1.3)
    app = "Instagram"
    qqnorm(trepn_i[[col]], col = '#bc2a8d', main=paste("QQplot of ", X, " for ", app, sep= ""), xlab=X, cex.lab=1.3)
    app = "Whatsapp"
    qqnorm(trepn_w[[col]], col = '#25D366', main=paste("QQplot of ", X, " for ", app, sep= ""), xlab=X, cex.lab=1.3)
    
    if (col == 'cpu.norm'){
      print('packet.size')
      app = 'Facebook'
      X = 'packet.size'
      qqnorm(wireshark_f[[X]], col = '#4267B2', main=paste("QQplot of ", X, " for ", app, sep= ""), xlab=X, cex.lab=1.3)
      app = "Instagram"
      qqnorm(wireshark_i[[X]], col = '#bc2a8d', main=paste("QQplot of ", X, " for ", app, sep= ""), xlab=X, cex.lab=1.3)
      app = "Whatsapp"
      qqnorm(wireshark_w[[X]], col = '#25D366', main=paste("QQplot of ", X, " for ", app, sep= ""), xlab=X, cex.lab=1.3)
    }
  }
};dev.off()

############## SOLO ############## 


col_count = 1
for (col in colnames(trepn)){
  if (!is.factor(trepn[[col]]) & !grepl('ms', col, fixed = TRUE)) {
    
    X = col
    Xlab = sub(".", " ", X, fixed = TRUE)
    
    print(Xlab)
    
    app = 'Facebook'
    png(paste("../results/facebook_", col, "_qqplot.png", sep=""), res = 100);
    qqnorm(trepn_f[[col]], col = '#4267B2', main=paste("QQplot of ", Xlab, " for \n", app, sep= ""), xlab=X, cex.lab=1.3)
    dev.off()
    
    app = "Instagram"
    png(paste("../results/instagram_", col, "_qqplot.png", sep=""), res = 100);
    qqnorm(trepn_i[[col]], col = '#bc2a8d', main=paste("QQplot of ", Xlab, " for \n", app, sep= ""), xlab=X, cex.lab=1.3)
    dev.off()
    
    app = "Whatsapp"
    png(paste("../results/whatsapp_", col, "_qqplot.png", sep=""), res = 100);
    qqnorm(trepn_w[[col]], col = '#25D366', main=paste("QQplot of ", Xlab, " for \n", app, sep= ""), xlab=X, cex.lab=1.3)
    dev.off()
    
    app = "all applications"
    Xlab = paste(sub(".", " ", col, fixed = TRUE), " residuals", sep="")
    png(paste("../results/all_apps_", col, "_qqplot.png", sep=""), res = 100);
    qqnorm(lm(trepn[[col]]~trepn$app)$residuals, col = colors[col_count], main=paste("QQplot of ", Xlab, " for \n", app, sep= ""), xlab=Xlab, cex.lab=1.3)
    dev.off()
    
    col_count = col_count + 1
    
    if (col == 'cpu.norm'){
      
      X = 'packet.size'
      Xlab = sub(".", " ", X, fixed = TRUE)
      
      print(Xlab)
      
      app = 'Facebook'
      
      png(paste("../results/facebook_packetsize_qqplot.png", sep=""), res = 100);
      qqnorm(wireshark_f[[X]], col = '#4267B2', main=paste("QQplot of ", Xlab, " for \n", app, sep= ""), xlab=X, cex.lab=1.3)
      dev.off()
      
      app = "Instagram"
      png(paste("../results/instagram_packetsize_qqplot.png", sep=""), res = 100);
      qqnorm(wireshark_i[[X]], col = '#bc2a8d', main=paste("QQplot of ", Xlab, " for \n", app, sep= ""), xlab=X, cex.lab=1.3)
      dev.off()
      
      app = "Whatsapp"
      png(paste("../results/whatsapp_packetsize_qqplot.png"), res = 100);
      qqnorm(wireshark_w[[X]], col = '#25D366', main=paste("QQplot of ", Xlab, " for \n", app, sep= ""), xlab=X, cex.lab=1.3)
      dev.off()
      
      app = "all applications"
      Xlab = paste(sub(".", " ", X, fixed = TRUE), " residuals", sep="")
      png(paste("../results/all_apps_", X, "_qqplot.png", sep=""), res = 100);
      qqnorm(lm(wireshark[[X]]~wireshark$app)$residuals, col = colors[col_count], main=paste("QQplot of ", Xlab, " for \n", app, sep= ""), xlab=Xlab, cex.lab=1.3)
      dev.off()
    }
  }
}

## 2.3 histograms

############## COMBINED ############## 

png(paste("../results/app_histograms.png"), width = 9.4, height = 10, units = "in", res = 300);
par(mfrow = c(4,3), omi=c(0.3,0.2,0,0.2), plt=c(0.2,0.9,0.05,0.7));
for (col in colnames(trepn)){
  if (!is.factor(trepn[[col]]) & !grepl('ms', col, fixed = TRUE)) {
    print(col)
    
    app = 'Facebook'
    X = col
    hist(trepn_f[[col]], col = '#4267B2', main=paste("Histogram of ", X, " for ", app, sep= ""), xlab=X, cex.lab=1.3)
    
    app = "Instagram"
    hist(trepn_i[[col]], col = '#bc2a8d', main=paste("Histogram of ", X, " for ", app, sep= ""), xlab=X, cex.lab=1.3)
    
    app = "Whatsapp"
    hist(trepn_w[[col]], col = '#25D366', main=paste("Histogram of ", X, " for ", app, sep= ""), xlab=X, cex.lab=1.3)
    
    if (col == 'cpu.norm'){
      print('packet.size')
      app = 'Facebook'
      X = 'packet.size'
      hist(wireshark_f[[X]], col = '#4267B2', main=paste("Histogram of ", X, " for ", app, sep= ""), xlab=X, cex.lab=1.3)
      
      app = "Instagram"
      hist(wireshark_i[[X]], col = '#bc2a8d', main=paste("Histogram of ", X, " for ", app, sep= ""), xlab=X, cex.lab=1.3)
      
      app = "Whatsapp"
      hist(wireshark_w[[X]], col = '#25D366', main=paste("Histogram of ", X, " for ", app, sep= ""), xlab=X, cex.lab=1.3)
    }
  }
};dev.off()

############## SOLO ##############
col_count = 1
for (col in colnames(trepn)){
  if (!is.factor(trepn[[col]]) & !grepl('ms', col, fixed = TRUE)) {
    X = col
    Xlab = sub(".", " ", X, fixed = TRUE)
    
    print(Xlab)
    
    app = 'Facebook'
    png(paste("../results/facebook_", col, "_histogram.png", sep=""), res = 100);
    hist(trepn_f[[col]], col = '#4267B2', main=paste("Histogram of ", Xlab, " for \n", app, sep= ""), xlab=Xlab, cex.lab=1.3)
    dev.off()
  
    app = "Instagram"
    png(paste("../results/instagram_", col, "_histogram.png", sep=""), res = 100);
    hist(trepn_i[[col]], col = '#bc2a8d', main=paste("Histogram of ", Xlab, " for \n", app, sep= ""), xlab=Xlab, cex.lab=1.3)
    dev.off()
    
    app = "Whatsapp"
    png(paste("../results/whatsapp_", col, "_histogram.png", sep=""), res = 100);
    hist(trepn_w[[col]], col = '#25D366', main=paste("Histogram of ", Xlab, " for \n", app, sep= ""), xlab=Xlab, cex.lab=1.3)
    dev.off()
    
    Xlab = paste(Xlab, " residuals", sep="")
    
    app = "all applications"
    png(paste("../results/all_apps_", col, "_histogram.png", sep=""), res = 100);
    hist(lm(trepn[[col]]~trepn$app)$residuals, col = colors[col_count], main=paste("Histogram of ", Xlab, " for \n", app, sep= ""), xlab=Xlab, cex.lab=1.3)
    dev.off()
    col_count = col_count + 1
    
    if (col == 'cpu.norm'){
      X = 'packet.size'
      Xlab = sub(".", " ", X, fixed = TRUE)
      
      print(Xlab)
      
      app = 'Facebook'
      png(paste("../results/facebook_packetsize_histogram.png", sep=""), res = 100);
      hist(wireshark_f[[X]], col = '#4267B2', main=paste("Histogram of ", Xlab, " for \n", app, sep= ""), xlab=Xlab, cex.lab=1.3)
      dev.off()
      
      app = "Instagram"
      png(paste("../results/instagram_packetsize_histogram.png", sep=""), res = 100);
      hist(wireshark_i[[X]], col = '#bc2a8d', main=paste("Histogram of ", Xlab, " for \n", app, sep= ""), xlab=Xlab, cex.lab=1.3)
      dev.off()
      
      app = "Whatsapp"
      png(paste("../results/whatsapp_packetsize_histogram.png", sep=""), res = 100);
      hist(wireshark_w[[X]], col = '#25D366', main=paste("Histogram of ", Xlab, " for \n", app, sep= ""), xlab=Xlab, cex.lab=1.3)
      dev.off()
      
      Xlab = paste(Xlab, " residuals", sep="")
      
      app = "all applications"
      png(paste("../results/all_apps_", X, "_histogram.png", sep=""), res = 100);
      hist(lm(wireshark[[X]]~wireshark$app)$residuals, col = colors[col_count], main=paste("Histogram of ", Xlab, " for \n", app, sep= ""), xlab=Xlab, cex.lab=1.3)
      dev.off()
    }
  }
}

## 2.4 summary

t_columns = c("memory", "cpu.norm")
w_columns

### Whatsapp
summary(trepn_w[, t_columns])

### Instagram
summary(trepn_i[, t_columns])

### Facebook
summary(trepn_f[, t_columns])


############################# RQ2 ############################# 

# 1. Grouping 

trepn_2 = trepn[trepn$participants == 2, ]
trepn_5 = trepn[trepn$participants == 5, ]

wireshark_2 = wireshark[wireshark$participants == 2, ]
wireshark_5 = wireshark[wireshark$participants == 5, ]


# 2. data exploration

## 2.1 boxplots (violin)

for (col in colnames(trepn)){
  if (!is.factor(trepn[[col]]) & !grepl('ms', col, fixed = TRUE)) {
    print(col)
    X = 'participants'
          print(p <- ggplot(trepn, aes_string(x=X, y=col, fill=X)) 
                + scale_fill_manual(values = c("#8ECCFF", "#1997FE"))
                + geom_violin() + geom_boxplot(width = .1, show.legend = FALSE) + labs(y = toString(col))
                + theme(text = element_text(size=25),
                        legend.text = element_text(angle=270, margin = margin(t = 15)),
                        legend.position = "none",
                        legend.direction = "vertical")
                + guides(fill = guide_legend(title = "",
                                             label.position = "bottom",
                                             title.position = "left", title.vjust = 1))
          )
    
    ggsave(paste("../results/Violin_", X, "-", col, ".png", sep = ""))
    if (col == 'cpu.norm'){
      X = 'participants'
      col = 'packet.size'
      print(col)
      print(p <- ggplot(wireshark, aes_string(x=X, y=col, fill=X)) 
            + scale_fill_manual(values = c("#8ECCFF", "#1997FE"))
            + geom_violin() + geom_boxplot(width = .1, show.legend = FALSE) + labs(y = toString(col))
            + theme(text = element_text(size=25),
                    legend.text = element_text(angle=270, margin = margin(t = 15)),
                    legend.position = "none",
                    legend.direction = "vertical")
            + guides(fill = guide_legend(title = "",
                                         label.position = "bottom",
                                         title.position = "left", title.vjust = 1))
            )
      ggsave(paste("../results/Violin_", X, "-", col, ".png", sep = ""))
    }
  }
};dev.off()

## 2.2 qqplots

############## COMBINED ############## 

png(paste("../results/par_qqplots.png"), width = 6, height = 8, units = "in", res = 300);
par(mfrow = c(4,2), omi=c(0.3,0.2,0,0.2), plt=c(0.2,0.9,0.05,0.7));
for (col in colnames(trepn)){
  if (!is.factor(trepn[[col]]) & !grepl('ms', col, fixed = TRUE)) {
    print(col)
    treat = 'P2'
    X = col
    qqnorm(trepn_2[[col]], col = '#8ECCFF', main=paste("QQplot of ", X, " for ", treat, sep= ""), xlab=X, cex.lab=1.3)
    treat = 'P5'
    qqnorm(trepn_5[[col]], col = '#1997FE', main=paste("QQplot of ", X, " for ", treat, sep= ""), xlab=X, cex.lab=1.3)
    
    if (col == 'cpu.norm'){
      print('packet.size')
      treat = 'P2'
      X = 'packet.size'
      qqnorm(wireshark_2[[X]], col = '#8ECCFF', main=paste("QQplot of ", X, " for ", treat, sep= ""), xlab=X, cex.lab=1.3)
      treat = 'P5'
      qqnorm(wireshark_5[[X]], col = '#1997FE', main=paste("QQplot of ", X, " for ", treat, sep= ""), xlab=X, cex.lab=1.3)
    }
  }
};dev.off()

############## SOLO ############## 
col_count = 1
for (col in colnames(trepn)){
  if (!is.factor(trepn[[col]]) & !grepl('ms', col, fixed = TRUE)) {
    X = col
    Xlab = sub(".", " ", X, fixed = TRUE)
    
    print(Xlab)
    
    treat = 'P2'
    png(paste("../results/p2_", X, "_qqplot.png", sep=""), res = 100);
    qqnorm(trepn_2[[col]], col = '#8ECCFF', main=paste("QQplot of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
    dev.off()
    
    treat = 'P5'
    png(paste("../results/p5_", X, "_qqplot.png", sep=""), res = 100);
    qqnorm(trepn_5[[col]], col = '#1997FE', main=paste("QQplot of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
    dev.off()
    
    Xlab = paste(Xlab, " residuals", sep="")
    
    treat = "all participant configurations"
    png(paste("../results/all_par_", X, "_qqplot.png", sep=""), res = 100);
    qqnorm(lm(trepn[[col]]~trepn$participants)$residuals, col = colors[col_count], main=paste("QQplot of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
    dev.off()
    
    col_count = col_count + 1
    
    if (col == 'cpu.norm'){
      X = 'packet.size'
      Xlab = sub(".", " ", X, fixed = TRUE)
      
      print(Xlab)
      
      
      treat = 'P2'
      png(paste("../results/p2_packetsize_qqplot.png", sep=""), res = 100);
      qqnorm(wireshark_2[[X]], col = '#8ECCFF', main=paste("QQplot of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
      dev.off()
      
      treat = 'P5'
      png(paste("../results/p5_packetsize_qqplot.png", sep=""), res = 100);
      qqnorm(wireshark_5[[X]], col = '#1997FE', main=paste("QQplot of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
      dev.off()
      
      Xlab = paste(Xlab, " residuals", sep="")
      
      treat = "all participant configurations"
      png(paste("../results/all_par_", X, "_qqplot.png", sep=""), res = 100);
      qqnorm(lm(wireshark[[X]]~wireshark$participants)$residuals, col = colors[col_count], main=paste("QQplot of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
      dev.off()
    }
  }
}


## 2.3 histograms

############## COMBINED ############## 

# png(paste("../results/par_histograms.png"), width = 6, height = 8, units = "in", res = 300);
# par(mfrow = c(4,2), omi=c(0.3,0.2,0,0.2), plt=c(0.2,0.9,0.05,0.7));

for (col in colnames(trepn)){
  if (!is.factor(trepn[[col]]) & !grepl('ms', col, fixed = TRUE)) {
    print(col)
    treat = 'P2'
    X = col
    hist(trepn_2[[col]], col = '#8ECCFF', main=paste("Histograms of ", X, " for ", treat, sep= ""), xlab=X, cex.lab=1.3)
    treat = 'P5'
    hist(trepn_5[[col]], col = '#1997FE', main=paste("Histograms of ", X, " for ", treat, sep= ""), xlab=X, cex.lab=1.3)
    
    if (col == 'cpu.norm'){
      print('packet.size')
      treat = 'P2'
      X = 'packet.size'
      hist(wireshark_2[[X]], col = '#8ECCFF', main=paste("Histogram of ", X, " for ", treat, sep= ""), xlab=X, cex.lab=1.3)
      treat = 'P5'
      hist(wireshark_5[[X]], col = '#1997FE', main=paste("Histogram of ", X, " for ", treat, sep= ""), xlab=X, cex.lab=1.3)
    }
  }
};dev.off()

############## SOLO ############## 
col_count = 1
for (col in colnames(trepn)){
  if (!is.factor(trepn[[col]]) & !grepl('ms', col, fixed = TRUE)) {
    X = col
    Xlab = sub(".", " ", X, fixed = TRUE)
    
    print(Xlab)
    
    treat = 'P2'
    png(paste("../results/p2_", col, "_histogram.png", sep=""), res = 100);
    hist(trepn_2[[col]], col = '#8ECCFF', main=paste("Histogram of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
    dev.off()
    
    treat = 'P5'
    png(paste("../results/p5_", col, "_histogram.png", sep=""), res = 100);
    hist(trepn_5[[col]], col = '#1997FE', main=paste("Histogram of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
    dev.off()
    
    Xlab = paste(Xlab, " residuals", sep="")
    
    treat = "all participant configurations"
    png(paste("../results/all_par_", col, "_histogram.png", sep=""), res = 100);
    hist(lm(trepn[[col]]~trepn$participants)$residuals, col = colors[col_count], main=paste("Histogram of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
    dev.off()
    col_count = col_count + 1
    
    if (col == 'cpu.norm'){
      X = 'packet.size'
      Xlab = sub(".", " ", X, fixed = TRUE)
      
      print(Xlab)
      
      treat = 'P2'
      png(paste("../results/p2_packetsize_histogram.png", sep=""), res = 100);
      hist(wireshark_2[[X]], col = '#8ECCFF', main=paste("Histogram of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
      dev.off()
      
      treat = 'P5'
      png(paste("../results/p5_packetsize_histogram.png", sep=""), res = 100);
      hist(wireshark_5[[X]], col = '#1997FE', main=paste("Histogram of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
      dev.off()
      
      Xlab = paste(Xlab, " residuals", sep="")
      
      treat = "all participant configurations"
      png(paste("../results/all_par_", X, "_histogram.png", sep=""), res = 100);
      hist(lm(wireshark[[X]]~wireshark$participants)$residuals, col = colors[col_count], main=paste("Histogram of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
      dev.off()
    }
  }
}



############################# RQ3 #############################


# 1. Grouping 

trepn_M = trepn[trepn$microphone == 'muted', ]  
trepn_U = trepn[trepn$microphone == 'unmuted', ]  

wireshark_M = wireshark[wireshark$microphone == 'muted', ]  
wireshark_U = wireshark[wireshark$microphone == 'unmuted', ]  

head(trepn_M)
head(trepn_U)

head(wireshark_M)
head(wireshark_U)

# 2. data exploration

## 2.1 boxplots (violin)

for (col in colnames(trepn)){
  if (!is.factor(trepn[[col]]) & !grepl('ms', col, fixed = TRUE)) {
    print(col)
    X = 'microphone'
    print(p <- ggplot(trepn, aes_string(x=X, y=col, fill=X)) 
          + scale_fill_manual(values = c("#F96462", "#77D850"))
          + geom_violin() + geom_boxplot(width = .1, show.legend = FALSE) + labs(y = toString(col))
          + theme(text = element_text(size=25),
                  legend.text = element_text(angle=270, margin = margin(t = 15)),
                  legend.position = "none",
                  legend.direction = "vertical")
          + guides(fill = guide_legend(title = "",
                                       label.position = "bottom",
                                       title.position = "left", title.vjust = 1))
          )
    
    ggsave(paste("../results/Violin_", X, "-", col, ".png", sep = ""))
    if (col == 'cpu.norm'){
      X = 'microphone'
      col = 'packet.size'
      print(col)
      print(p <- ggplot(wireshark, aes_string(x=X, y=col, fill=X)) 
            + scale_fill_manual(values = c("#F96462", "#77D850"))
            + geom_violin() + geom_boxplot(width = .1, show.legend = FALSE) + labs(y = toString(col))
            + theme(text = element_text(size=25),
                    legend.text = element_text(angle=270, margin = margin(t = 15)),
                    legend.position = "none",
                    legend.direction = "vertical")
            + guides(fill = guide_legend(title = "",
                                         label.position = "bottom",
                                         title.position = "left", title.vjust = 1))
            )
      ggsave(paste("../results/Violin_", X, "-", col, ".png", sep = ""))
    }
  }
};dev.off()



## 2.2 qqplots

############## COMBINED ############## 

png(paste("../results/mic_qqplots.png"), width = 6, height = 8, units = "in", res = 300);
par(mfrow = c(4,2), omi=c(0.3,0.2,0,0.2), plt=c(0.2,0.9,0.05,0.7));
for (col in colnames(trepn)){
  if (!is.factor(trepn[[col]]) & !grepl('ms', col, fixed = TRUE)) {
    print(col)
    
    treat = 'MM'
    X = col
    qqnorm(trepn_M[[col]], col = '#F96462', main=paste("QQplot of ", X, " for ", treat, sep= ""), xlab=X, cex.lab=1.3)
    treat = 'MU'
    qqnorm(trepn_U[[col]], col = '#77D850', main=paste("QQplot of ", X, " for ", treat, sep= ""), xlab=X, cex.lab=1.3)
    
    if (col == 'cpu.norm'){
      print('packet.size')
      
      treat = 'MM'
      X = 'packet.size'
      qqnorm(wireshark_M[[X]], col = '#F96462', main=paste("QQplot of ", X, " for ", treat, sep= ""), xlab=X, cex.lab=1.3)
      
      treat = 'MU'
      qqnorm(wireshark_U[[X]], col = '#77D850', main=paste("QQplot of ", X, " for ", treat, sep= ""), xlab=X, cex.lab=1.3)
    }
  }
};dev.off()

############## SOLO ############## 
col_count = 1
for (col in colnames(trepn)){
  if (!is.factor(trepn[[col]]) & !grepl('ms', col, fixed = TRUE)) {
    X = col
    Xlab = sub(".", " ", X, fixed = TRUE)
    
    print(Xlab)
    
    treat = 'MU'
    png(paste("../results/mu_", X, "_qqplot.png", sep=""), res = 100);
    qqnorm(trepn_M[[col]], col = '#8ECCFF', main=paste("QQplot of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
    dev.off()
    
    treat = 'MM'
    png(paste("../results/mm_", X, "_qqplot.png", sep=""), res = 100);
    qqnorm(trepn_U[[col]], col = '#1997FE', main=paste("QQplot of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
    dev.off()
    
    Xlab = paste(Xlab, " residuals", sep="")
    
    treat = "all microphone configurations"
    png(paste("../results/all_mic_", X, "_qqplot.png", sep=""), res = 100);
    qqnorm(lm(trepn[[col]]~trepn$microphone)$residuals, col = colors[col_count], main=paste("QQplot of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
    dev.off()
    
    col_count = col_count + 1
    
    if (col == 'cpu.norm'){
      X = 'packet.size'
      Xlab = sub(".", " ", X, fixed = TRUE)
      
      print(Xlab)
      
      treat = 'mu'
      png(paste("../results/mu_packetsize_qqplot.png", sep=""), res = 100);
      qqnorm(wireshark_M[[X]], col = '#8ECCFF', main=paste("QQplot of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
      dev.off()
      
      treat = 'mm'
      png(paste("../results/mm_packetsize_qqplot.png", sep=""), res = 100);
      qqnorm(wireshark_U[[X]], col = '#1997FE', main=paste("QQplot of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
      dev.off()
      
      Xlab = paste(Xlab, " residuals", sep="")
      
      treat = "all microphone configurations"
      png(paste("../results/all_mic_", X, "_qqplot.png", sep=""), res = 100);
      qqnorm(lm(wireshark[[X]]~wireshark$microphone)$residuals, col = colors[col_count], main=paste("QQplot of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
      dev.off()
    }
  }
}


## 2.3 histograms

############## COMBINED ############## 

png(paste("../results/mic_histograms.png"), width = 6, height = 8, units = "in", res = 300);
par(mfrow = c(4,2), omi=c(0.3,0.2,0,0.2), plt=c(0.2,0.9,0.05,0.7));
for (col in colnames(trepn)){
  if (!is.factor(trepn[[col]]) & !grepl('ms', col, fixed = TRUE)) {
    print(col)
    treat = 'MM'
    X = col
    hist(trepn_M[[col]], col = '#F96462', main=paste("Histograms of ", X, " for ", treat, sep= ""), xlab=X, cex.lab=1.3)
    treat = 'MU'
    hist(trepn_U[[col]], col = '#77D850', main=paste("Histograms of ", X, " for ", treat, sep= ""), xlab=X, cex.lab=1.3)
    
    if (col == 'cpu.norm'){
      print('packet.size')
      treat = 'MM'
      X = 'packet.size'
      hist(wireshark_M[[X]], col = '#F96462', main=paste("Histograms of ", X, " for ", treat, sep= ""), xlab=X, cex.lab=1.3)
      treat = 'MU'
      hist(wireshark_U[[X]], col = '#77D850', main=paste("Histograms of ", X, " for ", treat, sep= ""), xlab=X, cex.lab=1.3)
    }
  }
};dev.off()

############## SOLO ############## 
col_count = 1
for (col in colnames(trepn)){
  if (!is.factor(trepn[[col]]) & !grepl('ms', col, fixed = TRUE)) {
    X = col
    Xlab = sub(".", " ", X, fixed = TRUE)
    
    print(Xlab)
    
    treat = 'MU'
    png(paste("../results/mu_", X, "_histogram.png", sep=""), res = 100);
    hist(trepn_M[[col]], col = '#8ECCFF', main=paste("Histogram of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
    dev.off()
    
    treat = 'MM'
    png(paste("../results/mm_", X, "_histogram.png", sep=""), res = 100);
    hist(trepn_U[[col]], col = '#1997FE', main=paste("Histogram of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
    dev.off()
    
    Xlab = paste(Xlab, " residuals", sep="")
    
    treat = "all microphone configurations"
    png(paste("../results/all_mic_", X, "_histogram.png", sep=""), res = 100);
    hist(lm(trepn[[col]]~trepn$microphone)$residuals, col = colors[col_count], main=paste("Histogram of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
    dev.off()
    
    col_count = col_count + 1
    
    if (col == 'cpu.norm'){
      X = 'packet.size'
      Xlab = sub(".", " ", X, fixed = TRUE)
      
      print(Xlab)
      
      treat = 'mu'
      png(paste("../results/mu_packetsize_histogram.png", sep=""), res = 100);
      hist(wireshark_M[[X]], col = '#8ECCFF', main=paste("Histogram of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
      dev.off()
      
      treat = 'mm'
      png(paste("../results/mm_packetsize_histogram.png", sep=""), res = 100);
      hist(wireshark_U[[X]], col = '#1997FE', main=paste("Histogram of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
      dev.off()
      
      Xlab = paste(Xlab, " residuals", sep="")
      
      treat = "all microphone configurations"
      png(paste("../results/all_mic_", X, "_histogram.png", sep=""), res = 100);
      hist(lm(wireshark[[X]]~wireshark$microphone)$residuals, col = colors[col_count], main=paste("Histogram of ", Xlab, " for \n", treat, sep= ""), xlab=Xlab, cex.lab=1.3)
      dev.off()
    }
  }
}


###################### TODO ############################################################
## Hypothesis testing (with blocking factors)
## Include effect size measurement (Cohen's d) if result is statisticallysignificant. 
########################################################################################



length(trepn[['cpu.norm']])
res = residuals(lm(trepn[['cpu.norm']] ~ trepn[['app']]))

AT = ad.test(res); print(AT)


# 1 hypothesis testing

## 1.1 APP

kruskal.test(cpu.norm ~ app, data = trepn)
kruskal.test(memory ~ app, data = trepn)
kruskal.test(packet.size  ~ app, data = wireshark)
kruskal.test(power.delta ~ app, data = trepn)


NT = NemenyiTest(x = trepn$cpu.norm,
                 g = trepn$app,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn, cpu.norm ~ app)

NT = NemenyiTest(x = trepn$memory,
                 g = trepn$app,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn, memory ~ app)

NT = NemenyiTest(x = trepn$power.delta,
                 g = trepn$app,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn, power.delta ~ app)

NT = NemenyiTest(x = wireshark$packet.size,
                 g = wireshark$app,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(wireshark, packet.size ~ app)

## 1.2 PAR

### 1.2.1 facebook

kruskal.test(cpu.norm ~ participants, data = trepn_f)
kruskal.test(memory ~ participants, data = trepn_f)
kruskal.test(packet.size  ~ participants, data = wireshark_f)
kruskal.test(power.delta ~ participants, data = trepn_f)


NT = NemenyiTest(x = trepn_f$cpu.norm,
                 g = trepn_f$participants,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn_f, cpu.norm ~ participants)

NT = NemenyiTest(x = trepn_f$memory,
                 g = trepn_f$participants,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn_f, memory ~ participants)

NT = NemenyiTest(x = trepn_f$power.delta,
                 g = trepn_f$participants,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn_f, power.delta ~ participants)

NT = NemenyiTest(x = wireshark_f$packet.size,
                 g = wireshark_f$participants,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(wireshark_f, packet.size ~ participants)


### 1.2.2 instagram


kruskal.test(cpu.norm ~ participants, data = trepn_i)
kruskal.test(memory ~ participants, data = trepn_i)
kruskal.test(packet.size  ~ participants, data = wireshark_i)
kruskal.test(power.delta ~ participants, data = trepn_i)


NT = NemenyiTest(x = trepn_i$cpu.norm,
                 g = trepn_i$participants,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn_i, cpu.norm ~ participants)

NT = NemenyiTest(x = trepn_i$memory,
                 g = trepn_i$participants,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn_i, memory ~ participants)

NT = NemenyiTest(x = trepn_i$power.delta,
                 g = trepn_i$participants,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn_i, power.delta ~ participants)

NT = NemenyiTest(x = wireshark_i$packet.size,
                 g = wireshark_i$participants,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(wireshark_i, packet.size ~ participants)


### 1.2.3 whatsapp

kruskal.test(cpu.norm ~ participants, data = trepn_w)
kruskal.test(memory ~ participants, data = trepn_w)
kruskal.test(packet.size  ~ participants, data = wireshark_w)
kruskal.test(power.delta ~ participants, data = trepn_w)


NT = NemenyiTest(x = trepn_w$cpu.norm,
                 g = trepn_w$participants,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn_w, cpu.norm ~ participants)

NT = NemenyiTest(x = trepn_w$memory,
                 g = trepn_w$participants,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn_w, memory ~ participants)

NT = NemenyiTest(x = trepn_w$power.delta,
                 g = trepn_w$participants,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn_w, power.delta ~ participants)

NT = NemenyiTest(x = wireshark_w$packet.size,
                 g = wireshark_w$participants,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(wireshark_w, packet.size ~ participants)

## 1.3 MIC


### 1.3.1 facebook

kruskal.test(cpu.norm ~ microphone, data = trepn_f)
kruskal.test(memory ~ microphone, data = trepn_f)
kruskal.test(packet.size  ~ microphone, data = wireshark_f)
kruskal.test(power.delta ~ microphone, data = trepn_f)


NT = NemenyiTest(x = trepn_f$cpu.norm,
                 g = trepn_f$microphone,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn_f, cpu.norm ~ microphone)

NT = NemenyiTest(x = trepn_f$memory,
                 g = trepn_f$microphone,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn_f, memory ~ microphone)

NT = NemenyiTest(x = trepn_f$power.delta,
                 g = trepn_f$microphone,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn_f, power.delta ~ microphone)

NT = NemenyiTest(x = wireshark_f$packet.size,
                 g = wireshark_f$microphone,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(wireshark_f, packet.size ~ microphone)


### 1.3.2 instagram


kruskal.test(cpu.norm ~ microphone, data = trepn_i)
kruskal.test(memory ~ microphone, data = trepn_i)
kruskal.test(packet.size  ~ microphone, data = wireshark_i)
kruskal.test(power.delta ~ microphone, data = trepn_i)


NT = NemenyiTest(x = trepn_i$cpu.norm,
                 g = trepn_i$microphone,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn_i, cpu.norm ~ microphone)

NT = NemenyiTest(x = trepn_i$memory,
                 g = trepn_i$microphone,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn_i, memory ~ microphone)

NT = NemenyiTest(x = trepn_i$power.delta,
                 g = trepn_i$microphone,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn_i, power.delta ~ microphone)

NT = NemenyiTest(x = wireshark_i$packet.size,
                 g = wireshark_i$microphone,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(wireshark_i, packet.size ~ microphone)


### 1.3.3 whatsapp

kruskal.test(cpu.norm ~ microphone, data = trepn_w)
kruskal.test(memory ~ microphone, data = trepn_w)
kruskal.test(packet.size  ~ microphone, data = wireshark_w)
kruskal.test(power.delta ~ microphone, data = trepn_w)


NT = NemenyiTest(x = trepn_w$cpu.norm,
                 g = trepn_w$microphone,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn_w, cpu.norm ~ microphone)

NT = NemenyiTest(x = trepn_w$memory,
                 g = trepn_w$microphone,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn_w, memory ~ microphone)

NT = NemenyiTest(x = trepn_w$power.delta,
                 g = trepn_w$microphone,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(trepn_w, power.delta ~ microphone)

NT = NemenyiTest(x = wireshark_w$packet.size,
                 g = wireshark_w$microphone,
                 dist="tukey", p.adjust.methods= 'bonferroni'); print(NT)
kruskal_effsize(wireshark_w, packet.size ~ microphone)



###########################################################


print('------------------------------------------------------------------------------------')
cat('\n facebook - trepn\n'); summary(select(trepn_f, memory, power.delta, cpu.norm)); cat('\n')
cat('\n facebook - trepn\n'); summary(select(wireshark_f, packet.size)); cat('\n')
cat('\n instagram - trepn\n'); summary(select(trepn_i, memory, power.delta, cpu.norm)); cat('\n')
cat('\n instagram - trepn\n'); summary(select(wireshark_i, packet.size)); cat('\n')
cat('\n whatsapp - trepn\n'); summary(select(trepn_w, memory, power.delta, cpu.norm)); cat('\n')
cat('\n whatsapp - trepn\n'); summary(select(wireshark_w, packet.size)); cat('\n')

cat('\n 2 participants - trepn\n'); summary(select(trepn_2, memory, power.delta, cpu.norm)); cat('\n')
cat('\n 2 participants - wireshark\n'); summary(select(wireshark_2, packet.size)); cat('\n')
cat('\n 5 participants - trepn\n'); summary(select(trepn_5, memory, power.delta, cpu.norm)); cat('\n')
cat('\n 5 participants - wireshark\n'); summary(select(wireshark_5, packet.size)); cat('\n')

cat('\n mic muted - trepn\n'); summary(select(trepn_M, memory, power.delta, cpu.norm)); cat('\n')
cat('\n mic muted - wireshark\n'); summary(select(wireshark_M, packet.size)); cat('\n')
cat('\n mic unmuted - trepn\n'); summary(select(trepn_U, memory, power.delta, cpu.norm)); cat('\n')
cat('\n mic unmuted - wireshark\n'); summary(select(wireshark_U, packet.size)); cat('\n')

print('------------------------------------------------------------------------------------')



exp_vars = c("app", "participants", "microphone")

for (f in exp_vars){
  for (col in colnames(trepn)){
    if (!is.factor(trepn[[col]]) & !grepl('ms', col, fixed = TRUE)) {
      explanatory_var = f
      X = col
      
      print(paste(X, " ~ ", explanatory_var , sep=""))
      
      NT = NemenyiTest(x = trepn[[X]],
                       g = trepn[[explanatory_var]],
                       dist="tukey"); print(NT)
      
      cat("\n")
      
      if (col == 'cpu.norm'){
        X = 'packet.size'
        
        print(paste(X, " - ", explanatory_var ,sep=""))
        
        NT = NemenyiTest(x = wireshark[[X]],
                         g = wireshark[[explanatory_var]],
                         dist="tukey"); print(NT)
    
        cat("-----------------------------------------------------\n")
      }
    }  
  }
}

X = 'cpu.norm'; Exp_var = 'microphone'
ad.test(residuals(lm(trepn[[X]] ~ trepn[[Exp_var]])))




ad.test(data$variable)


for (f in exp_vars){
  for (col in colnames(trepn)){
    if (!is.factor(trepn[[col]]) & !grepl('ms', col, fixed = TRUE)) {
      explanatory_var = f
      X = col
      
      print(paste(X, " ~ ", explanatory_var , sep=""))
      
      res = residuals(lm(trepn[[X]] ~ trepn[[explanatory_var]]))
      
      
      ST = shapiro.test(res[1:5000]); print(ST)
      AT = ad.test(res); print(AT)
      
      cat("\n")
      
      if (col == 'cpu.norm'){
        X = 'packet.size'
        
        print(paste(X, " - ", explanatory_var ,sep=""))
        
        res = residuals(lm(wireshark[[X]] ~ wireshark[[explanatory_var]]))
        
        
        ST = shapiro.test(res[1:5000]); print(ST)
        AT = ad.test(res); print(AT)
        
        cat("-----------------------------------------------------\n")
      }
    }  
  }
}



mean(wireshark$avg.packets)
mean(wireshark_f$avg.packets)
mean(wireshark_i$avg.packets)
mean(wireshark_w$avg.packets)
mean(wireshark_2$avg.packets)
mean(wireshark_5$avg.packets)
mean(wireshark_M$avg.packets)
mean(wireshark_U$avg.packets)

