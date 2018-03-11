library(dplyr)
library(data.table)
library(ggplot2)
library(plotly)
library(scales)
library(car)


setwd('/Users/danny/git_proj/housingPrices/Kaggle_Predict_Ames_Iowa_Housing_Prices/Datasets/')

HomeSales <- fread('train.csv')

monthSales <- HomeSales %>% group_by(YrSold, MoSold) %>% summarise('MonthlySales' = sum(SalePrice), 'MeanPrice' 
                                                                   = mean(SalePrice), count = n())


monthSales$Month <- seq.int(nrow(monthSales))

MeanPlot <- ggplot(monthSales, aes(x=Month, y=MeanPrice)) + geom_point()
MeanPlot

SumPlot <- ggplot(monthSales, aes(x=Month, y=MonthlySales)) + geom_point()
SumPlot

July2006 <- HomeSales %>% select(MoSold, YrSold, SalePrice) %>% filter(MoSold == 
                          7, YrSold == 2006) %>% select(-MoSold)
July2007 <- HomeSales %>% select(MoSold, YrSold, SalePrice) %>% filter(MoSold == 
                          7, YrSold == 2007) %>% select(-MoSold)
July2008 <- HomeSales %>% select(MoSold, YrSold, SalePrice) %>% filter(MoSold == 
                          7, YrSold == 2008) %>% select(-MoSold)
July2009 <- HomeSales %>% select(MoSold, YrSold, SalePrice) %>% filter(MoSold == 
                          7, YrSold == 2009) %>% select(-MoSold)

HomeSales_Yr <- rbind(July2006, July2007, July2008, July2009)

Home_hist <- ggplot(HomeSales_Yr, aes(SalePrice, group = YrSold,
  color = YrSold, fill = YrSold)) + geom_histogram(alpha = 0.3) + 
  facet_grid(YrSold~.) + xlab('Home Sale Price ($)') + ylab('Counts') + 
  theme(legend.position="none") + scale_x_continuous(labels = comma)
Home_hist

Home_box <- ggplot(HomeSales_Yr) + geom_boxplot(aes(x = as.factor(YrSold), y = SalePrice, group = YrSold, fill = YrSold)) + 
  scale_y_continuous(labels = comma) + xlab('Year') + ylab('Home Sale Price ($)') 

Home_box

VarTest <- leveneTest(SalePrice ~ as.factor(YrSold), data = HomeSales_Yr)
MedTest <- kruskal.test(SalePrice ~ as.factor(YrSold), data = HomeSales_Yr)

sprintf('The p value for ')
