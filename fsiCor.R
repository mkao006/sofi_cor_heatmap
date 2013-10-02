########################################################################
## Title: Visualizing Food Security Correlation Matrix
## Date: 2013-06-24
########################################################################

## Load library
library(scales)
library(foreign)
library(ggplot2)

## Read the data
fsiIndicator.df = read.spss("analysis indicators2.sav",
  to.data.frame = TRUE)
fsiIndicator.df$year = as.numeric(as.character(fsiIndicator.df$year))
colnames(fsiIndicator.df) = ifelse(attr(fsiIndicator.df,
          "variable.labels") == "", colnames(fsiIndicator.df),
          attr(fsiIndicator.df, "variable.labels"))
fsiIndicatorSub.df = subset(fsiIndicator.df, year %in% c(1996:2008))

## Select variables to be used
fsiVar = colnames(fsiIndicatorSub.df)[!colnames(fsiIndicatorSub.df) %in%
  c("Cou", "Country", "region", "year", "Depth of the food deficit",
    "Prevalence of food inadequacy")]

## Compute the correlation
fsiCor.df = expand.grid(fsiVar, fsiVar, stringsAsFactors = FALSE)
for(i in 1:NROW(fsiCor.df)){
  fsiCor.df[i, "pearsonRho"] =
    cor.test(fsiIndicatorSub.df[, fsiCor.df[i, "Var1"]],
             fsiIndicatorSub.df[, fsiCor.df[i, "Var2"]],
             method = "pearson")$estimate    
  fsiCor.df[i, "spearmanRho"] =
    cor.test(fsiIndicatorSub.df[, fsiCor.df[i, "Var1"]],
             fsiIndicatorSub.df[, fsiCor.df[i, "Var2"]],
             method = "spearman")$estimate
}


## Create dictionary and abbreviation label 
dict.df = data.frame(varName = unique(fsiCor.df$Var1),
  varLab = factor(paste("Ind-", 1:length(unique(fsiCor.df$Var1)),
    sep = ""),
    levels = paste("Ind-", length(unique(fsiCor.df$Var1)):1, sep = "")))

for(i in 1:NROW(fsiCor.df)){
  fsiCor.df[i, "var1Lab"] = factor(dict.df[which(dict.df$varName ==
    fsiCor.df[i, "Var1"]), "varLab"], levels = levels(dict.df$varLab))
  fsiCor.df[i, "var2Lab"] = factor(dict.df[which(dict.df$varName ==
    fsiCor.df[i, "Var2"]), "varLab"], levels = rev(levels(dict.df$varLab)))
}

## Hack to remove upper triangle
fsiCor.df[duplicated(fsiCor.df$pearsonRho), "pearsonRho"] = NA
fsiCor.df[which(fsiCor.df$var1Lab == fsiCor.df$var2Lab), "pearsonRho"] = NA
fsiCor.df[duplicated(fsiCor.df$spearmanRho), "spearmanRho"] = NA
fsiCor.df[which(fsiCor.df$var1Lab == fsiCor.df$var2Lab), "spearmanRho"] = NA


## Drop unused level
fsiCor.df = subset(fsiCor.df,
  !(var1Lab == c("Ind-1")) &
  !(var2Lab == c("Ind-24")), droplevels = TRUE)

fsiCor.df$var1Lab = droplevels(fsiCor.df$var1Lab)
fsiCor.df$var2Lab = droplevels(fsiCor.df$var2Lab)


## Scale correlation
fsiCor.df$pearsonRho = round(fsiCor.df$pearsonRho * 100)
fsiCor.df$plabel = ifelse(abs(fsiCor.df$pearsonRho) >= 75,
  as.character(fsiCor.df$pearsonRho), "")
fsiCor.df$spearmanRho = round(fsiCor.df$spearmanRho * 100)
fsiCor.df$slabel = ifelse(abs(fsiCor.df$spearmanRho) >= 75,
  as.character(fsiCor.df$spearmanRho), "")  

## Generate the pdf plot
pdf(file = "fsiCor.pdf", width = 10, height = 10)
print(ggplot(data = fsiCor.df, aes(x = var2Lab, y = var1Lab)) +
      geom_tile(aes(fill = pearsonRho), size = 10) +
      scale_fill_gradient2(na.value = "white", mid = "grey90",
                           low = muted("red", c = 300),
                           high = "darkgreen", breaks = c(-100, 0, 100),
                           limits = c(-100, 100)) +
      geom_text(aes(label = plabel), size = 2.5, col = "white") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.text.y = element_text(size = 10),
              legend.position = "top",
            panel.background = element_rect(fill = "white"),
            axis.ticks.y = element_line(colour = "white")) +
      labs(x = NULL, y = NULL, fill = "Strength of Correlation",
           title = "Pearson's Correlation Coefficient"))
print(ggplot(data = fsiCor.df, aes(x = var2Lab, y = var1Lab)) +
      geom_tile(aes(fill = spearmanRho), size = 10) +
      scale_fill_gradient2(na.value = "white", mid = "grey90",
                           low = muted("red", c = 300),
                           high = "darkgreen", breaks = c(-100, 0, 100),
                           limits = c(-100, 100)) +
      geom_text(aes(label = slabel), size = 2.5, col = "white") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.text.y = element_text(size = 10),
              legend.position = "top",
            panel.background = element_rect(fill = "white"),
            axis.ticks.y = element_line(colour = "white")) +
      labs(x = NULL, y = NULL, fill = "Strength of Correlation",
           title = "Spearman's Rho"))
graphics.off()
system("evince fsiCor.pdf&")


## Generate the eps plot
postscript(file = "fsiCor.eps", width = 10, height = 10,
           paper = "special", horizontal = FALSE)
print(ggplot(data = fsiCor.df, aes(x = var2Lab, y = var1Lab)) +
      geom_tile(aes(fill = pearsonRho), size = 10) +
      scale_fill_gradient2(na.value = "white", mid = "grey90",
                           low = muted("red", c = 300),
                           high = "darkgreen", breaks = c(-100, 0, 100),
                           limits = c(-100, 100)) +
      geom_text(aes(label = plabel), size = 2.5, col = "white") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.text.y = element_text(size = 10),
              legend.position = "top",
            panel.background = element_rect(fill = "white"),
            axis.ticks.y = element_line(colour = "white")) +
      labs(x = NULL, y = NULL, fill = "Strength of Correlation",
           title = "Pearson's Correlation Coefficient"))
print(ggplot(data = fsiCor.df, aes(x = var2Lab, y = var1Lab)) +
      geom_tile(aes(fill = spearmanRho), size = 10) +
      scale_fill_gradient2(na.value = "white", mid = "grey90",
                           low = muted("red", c = 300),
                           high = "darkgreen", breaks = c(-100, 0, 100),
                           limits = c(-100, 100)) +
      geom_text(aes(label = slabel), size = 2.5, col = "white") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.text.y = element_text(size = 10),
              legend.position = "top",
            panel.background = element_rect(fill = "white"),
            axis.ticks.y = element_line(colour = "white")) +
      labs(x = NULL, y = NULL, fill = "Strength of Correlation",
           title = "Spearman's Rho"))
graphics.off()
system("evince fsiCor.eps&")

write.csv(dict.df, file = "indicator_dictionary.csv", row.names = FALSE)




## Color clustered heat map
## ---------------------------------------------------------------------

## Compute the correlation
fsiCor.df = expand.grid(fsiVar, fsiVar, stringsAsFactors = FALSE)
for(i in 1:NROW(fsiCor.df)){
  fsiCor.df[i, "pearsonRho"] =
    cor.test(fsiIndicatorSub.df[, fsiCor.df[i, "Var1"]],
             fsiIndicatorSub.df[, fsiCor.df[i, "Var2"]],
             method = "pearson")$estimate    
  fsiCor.df[i, "spearmanRho"] =
    cor.test(fsiIndicatorSub.df[, fsiCor.df[i, "Var1"]],
             fsiIndicatorSub.df[, fsiCor.df[i, "Var2"]],
             method = "spearman")$estimate
}



## Calculate the average correlation
fsiAvgCor.df = ddply(fsiCor.df, ~Var1, summarise,
  avg_pearson_cor = mean(pearsonRho, na.rm = TRUE))
fsiAvgCorSort.df = na.omit(arrange(fsiAvgCor.df,
  by = desc(avg_pearson_cor)))

## Drop unused level
fsiCor.df$Var2 = factor(fsiCor.df$Var2,
  levels = unique(fsiAvgCorSort.df$Var1))
fsiCor.df$Var1 = factor(fsiCor.df$Var1,
  levels = rev(unique(fsiAvgCorSort.df$Var1)))
fsiCor.df$Var1 = droplevels(fsiCor.df$Var1)
fsiCor.df$Var2 = droplevels(fsiCor.df$Var2)
fsiCor.df = subset(fsiCor.df,
  !(Var1 %in% c("Percent of paved roads over total roads")) &
  !(Var2 %in% c("Domestic Food Price Level Index")), droplevels = TRUE)


fsiCor.df = arrange(fsiCor.df, Var1)
## Hack to remove upper triangle
fsiCor.df[duplicated(fsiCor.df$pearsonRho), "pearsonRho"] = NA
fsiCor.df[which(fsiCor.df$Var1 == fsiCor.df$Var2), "pearsonRho"] = NA
fsiCor.df[duplicated(fsiCor.df$spearmanRho), "spearmanRho"] = NA
fsiCor.df[which(fsiCor.df$Var1 == fsiCor.df$Var2), "spearmanRho"] = NA

## Scale correlation
fsiCor.df$pearsonRho = round(fsiCor.df$pearsonRho * 100)
fsiCor.df$plabel = ifelse(abs(fsiCor.df$pearsonRho) >= 75,
  as.character(fsiCor.df$pearsonRho), "")
fsiCor.df$spearmanRho = round(fsiCor.df$spearmanRho * 100)
fsiCor.df$slabel = ifelse(abs(fsiCor.df$spearmanRho) >= 75,
  as.character(fsiCor.df$spearmanRho), "")  


## Generate the pdf plot
pdf(file = "fsiCorCluster.pdf", width = 10, height = 10)
print(ggplot(data = fsiCor.df, aes(x = Var1, y = Var2)) +
      geom_tile(aes(fill = pearsonRho), size = 10) +
      scale_fill_gradient2(na.value = "white", mid = "grey90",
                           low = muted("red", c = 300),
                           high = "darkgreen", breaks = c(-100, 0, 100),
                           limits = c(-100, 100)) +
      geom_text(aes(label = plabel), size = 2.5, col = "white") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.text.y = element_text(size = 10),
              legend.position = "top",
            panel.background = element_rect(fill = "white"),
            axis.ticks.y = element_line(colour = "white")) +
      labs(x = NULL, y = NULL, fill = "Strength of Correlation",
           title = "Pearson's Correlation Coefficient"))
print(ggplot(data = fsiCor.df, aes(x = Var1, y = Var2)) +
      geom_tile(aes(fill = spearmanRho), size = 10) +
      scale_fill_gradient2(na.value = "white", mid = "grey90",
                           low = muted("red", c = 300),
                           high = "darkgreen", breaks = c(-100, 0, 100),
                           limits = c(-100, 100)) +
      geom_text(aes(label = slabel), size = 2.5, col = "white") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.text.y = element_text(size = 10),
              legend.position = "top",
            panel.background = element_rect(fill = "white"),
            axis.ticks.y = element_line(colour = "white")) +
      labs(x = NULL, y = NULL, fill = "Strength of Correlation",
           title = "Spearman's Rho"))
graphics.off()
system("evince fsiCorCluster.pdf&")


## Generate the eps plot
postscript(file = "fsiCorCluster.eps", width = 10, height = 10,
           paper = "special", horizontal = FALSE)
print(ggplot(data = fsiCor.df, aes(x = Var1, y = Var2)) +
      geom_tile(aes(fill = pearsonRho), size = 10) +
      scale_fill_gradient2(na.value = "white", mid = "grey90",
                           low = muted("red", c = 300),
                           high = "darkgreen", breaks = c(-100, 0, 100),
                           limits = c(-100, 100)) +
      geom_text(aes(label = plabel), size = 2.5, col = "white") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.text.y = element_text(size = 10),
              legend.position = "top",
            panel.background = element_rect(fill = "white"),
            axis.ticks.y = element_line(colour = "white")) +
      labs(x = NULL, y = NULL, fill = "Strength of Correlation",
           title = "Pearson's Correlation Coefficient"))
print(ggplot(data = fsiCor.df, aes(x = Var1, y = Var2)) +
      geom_tile(aes(fill = spearmanRho), size = 10) +
      scale_fill_gradient2(na.value = "white", mid = "grey90",
                           low = muted("red", c = 300),
                           high = "darkgreen", breaks = c(-100, 0, 100),
                           limits = c(-100, 100)) +
      geom_text(aes(label = slabel), size = 2.5, col = "white") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.text.y = element_text(size = 10),
              legend.position = "top",
            panel.background = element_rect(fill = "white"),
            axis.ticks.y = element_line(colour = "white")) +
      labs(x = NULL, y = NULL, fill = "Strength of Correlation",
           title = "Spearman's Rho"))
graphics.off()
system("evince fsiCorCluster.eps&")
