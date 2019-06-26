  ### Libraries & working directory -----
library(tidyverse)
library(rvest)
library(rstudioapi)
library(quantmod)
library(ggplot2)
library(ggcorrplot)
library(cluster)
library(fpc)
library(xtable)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('D:/Škola/5. semestr/Data Science with R/Seminar work/R codes/')

  ### Defining functions -----
##### Web scraping from Wikipedia and Yahoo Finance:
data_scraping <- function() { 
    #Wikipedia scraping
  wiki <- 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
  wikipedia1 <- wiki %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
    html_table(fill = TRUE)
  dates <- wikipedia1[[1]]$`Date first added[3][4]`
  dates <- sub('2018', NA_character_, dates)
  firms <- wikipedia1[[1]]$Symbol[which(!is.na(dates))]
  added1 <- wikipedia1[[1]]$Symbol[which(is.na(dates))]
  
  wikipedia2 <- wiki %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
    html_table(fill = TRUE)
  added2 <- wikipedia2[[1]]$Added[-1]
  removed <- wikipedia2[[1]]$Removed[-1]
  
  for (i in added1){
    firms <- append(firms, removed[which(added2 == i)])
  }
    #manual correction!
  firms <- firms[-which(firms == 'SNI')]
  firms <- firms[-which(firms == 'GGP')]
  firms <- firms[-which(firms == 'WYN')]
  
    #YAHOO scraping - Income statements
  financials <- vector('list', length(firms))
  for (firm in firms) {
    url <- paste0('https://finance.yahoo.com/quote/', firm, '/financials?p=', firm)
    
    finances <- url %>%
      read_html() %>%
      html_table(header = T) %>%
      map_df(bind_cols) %>%
      as_tibble()
    
    finances <- mutate_all(finances, funs(str_replace_all(., ',', '')))
    for (a in 1:9){
      finances <- mutate_all(finances, funs(str_replace(., paste0('-', a), paste0('%', a))))
    }
    finances <- mutate_all(finances, funs(str_replace(., '-', NA_character_)))
    for (b in 1:9){
      finances <- mutate_all(finances, funs(str_replace(., paste0('%', b), paste0('-', b))))
    }
    
    finances <- finances %>%
      mutate_at(vars(-Revenue), funs(str_remove_all(., '[a-zA-Z]'))) %>%
      mutate_at(vars(-Revenue), funs(as.numeric)) %>%
      drop_na() %>%
      as.data.frame()
    
    financials[[which(firms == firm)]] <- finances
    finances <- NULL
  }
  names(financials) <- firms
  
    #YAHOO scraping - Balance sheets
  balance_sheets <- vector('list', length(firms))

  for (firm in firms) {
    url <- paste0('https://finance.yahoo.com/quote/', firm, '/balance-sheet?p=', firm)
    
    BS <- url %>%
      read_html() %>%
      html_table(header = T) %>%
      map_df(bind_cols) %>%
      as_tibble()
    
    BS <- mutate_all(BS, funs(str_replace_all(., ',', '')))
    for (c in 1:9){
      BS <- mutate_all(BS, funs(str_replace(., paste0('-', c), paste0('%', c))))
    }
    BS <- mutate_all(BS, funs(str_replace(., '-', NA_character_)))
    for (d in 1:9){
      BS <- mutate_all(BS, funs(str_replace(., paste0('%', d), paste0('-', d))))
    }
    
    BS <- BS %>%
      mutate_at(vars(-`Period Ending`), funs(str_remove_all(., '[a-zA-Z]'))) %>%
      mutate_at(vars(-`Period Ending`), funs(str_remove_all(., "'"))) %>%
      mutate_at(vars(-`Period Ending`), funs(as.numeric)) %>%
      drop_na() %>%
      as.data.frame()
    
    balance_sheets[[which(firms == firm)]] <- BS
    BS <- NULL
  }
  names(balance_sheets) <- firms
  
  ## Cash flow
  cash_flows <- vector('list', length(firms))
  for (firm in firms) {
    url <- paste0('https://finance.yahoo.com/quote/', firm, '/cash-flow?p=', firm)
    
    CF <- url %>%
      read_html() %>%
      html_table(header = T) %>%
      map_df(bind_cols) %>%
      as_tibble()

    CF <- mutate_all(CF, funs(str_replace_all(., ',', '')))
    for (c in 1:9){
      CF <- mutate_all(CF, funs(str_replace(., paste0('-', c), paste0('%', c))))
    }
    CF <- mutate_all(CF, funs(str_replace(., '-', NA_character_)))
    for (d in 1:9){
      CF <- mutate_all(CF, funs(str_replace(., paste0('%', d), paste0('-', d))))
    }
    
    CF <- CF %>%
      mutate_at(vars(-`Period Ending`), funs(str_remove_all(., '[a-zA-Z]'))) %>%
      mutate_at(vars(-`Period Ending`), funs(str_remove_all(., "'"))) %>%
      mutate_at(vars(-`Period Ending`), funs(as.numeric)) %>%
      drop_na() %>%
      as.data.frame()
    
    cash_flows[[which(firms == firm)]] <- CF
    CF <- NULL
  } 
  names(cash_flows) <- firms
  
  saveRDS(balance_sheets, 'balance_sheets.rds')
  saveRDS(financials, 'financials.rds')
  saveRDS(firms, 'firms.rds')
  saveRDS(cash_flows, 'cash_flows.rds')
}

##### Data transformation:
data_transformation <- function(){
  company_financials <- list(financials, balance_sheets, cash_flows)
  for (statement in 1:length(company_financials)){
    for (firm in 1:length(firms)){
      if (dim(company_financials[[statement]][[firm]])[2] == 5){
        rownames(company_financials[[statement]][[firm]]) <- company_financials[[statement]][[firm]][,1]
        company_financials[[statement]][[firm]] <- company_financials[[statement]][[firm]][-1]
        colnames(company_financials[[statement]][[firm]]) <- substring(colnames(company_financials[[statement]][[firm]]), nchar(colnames(company_financials[[statement]][[firm]])) - 3, nchar(colnames(company_financials[[statement]][[firm]])))
        if( (colnames(company_financials[[statement]][[firm]])[1] == '2017' & colnames(company_financials[[statement]][[firm]])[3] == '2015') |
            (colnames(company_financials[[statement]][[firm]])[2] == '2017' & colnames(company_financials[[statement]][[firm]])[4] == '2015') ){
          company_financials[[statement]][[firm]] <- company_financials[[statement]][[firm]][,c('2017', '2016', '2015')]
        } else company_financials[[statement]][[firm]] <- NA
        next  
      }
      if (dim(company_financials[[statement]][[firm]])[2] == 4) {
        rownames(company_financials[[statement]][[firm]]) <- company_financials[[statement]][[firm]][,1]
        company_financials[[statement]][[firm]] <- company_financials[[statement]][[firm]][-1]
        colnames(company_financials[[statement]][[firm]]) <- substring(colnames(company_financials[[statement]][[firm]]), nchar(colnames(company_financials[[statement]][[firm]])) - 3, nchar(colnames(company_financials[[statement]][[firm]])))
        if(colnames(company_financials[[statement]][[firm]])[1] == '2017' & colnames(company_financials[[statement]][[firm]])[3] == '2015'){
          company_financials[[statement]][[firm]] <- company_financials[[statement]][[firm]][,c('2017', '2016', '2015')]
        } else company_financials[[statement]][[firm]] <- NA
        next
      } else company_financials[[statement]][[firm]] <- NA
    }
  }
  company_financials <- company_financials
  for (statement in 1:length(company_financials)){
    for (firm in firms){
      if (length(company_financials[[statement]][[firm]]) == 1) {
        company_financials[[statement]][[firm]] <- NULL
      }
    }
  }
  names <- list(financials, balance_sheets, cash_flows)
  for (statement in 1:length(company_financials)){
    names[[statement]] <- names(company_financials[[statement]]) 
  }
  company_financials <- lapply(company_financials, function(x) x[intersect(intersect(names[[1]], names[[2]]), names[[3]])])
}

##### Extracting financial ratios:
financial_ratios <- function() {
  current_ratio <- sapply(company_financials[["balance_sheets"]], function(x) apply(x['Total Current Assets',], 1, mean)) / 
    sapply(company_financials[["balance_sheets"]], function(x) apply(x['Total Current Liabilities',], 1, mean))
  names(current_ratio) <- paste0(firms, '.Current_ratio')
  
  ROA <- sapply(company_financials[["financials"]], function(x) apply(x['Net Income',], 1, mean)) /
    sapply(company_financials[["balance_sheets"]], function(x) apply(x['Total Assets',], 1, mean))
  names(ROA) <- paste0(firms, '.ROA')
  
  profit_margin <- sapply(company_financials[["financials"]], function(x) apply(x['Net Income',], 1, mean)) /
    sapply(company_financials[["financials"]], function(x) apply(x['Total Revenue',], 1, mean))
  names(profit_margin) <- paste0(firms, '.Profit_margin')
  
  debt_to_equity <- sapply(company_financials[["balance_sheets"]], function(x) apply(x['Total Liabilities',], 1, mean)) /
    sapply(company_financials[["balance_sheets"]], function(x) apply(x['Total Stockholder Equity',], 1, mean))
  names(debt_to_equity) <- paste0(firms, '.Debt_to_Equity')
  
  cash_flow_to_debt <- sapply(company_financials[["cash_flows"]], function(x) apply(x['Total Cash Flow From Operating Activities',], 1, mean)) / 
    sapply(company_financials[["balance_sheets"]], function(x) apply(x['Long Term Debt',], 1, mean))
  names(cash_flow_to_debt) <- paste0(firms, '.Cash_flow_to_Debt')
  
  AR_to_Sales <- sapply(company_financials[["balance_sheets"]], function(x) apply(x['Net Receivables',], 1, mean)) /
    sapply(company_financials[["financials"]], function(x) apply(x['Total Revenue',], 1, mean))
  names(AR_to_Sales) <- paste0(firms, '.AR_to_Sales')
  
  Asset_turnover <- (sapply(company_financials[["financials"]], function(x) apply(x['Total Revenue',1, drop = F], 1, mean)) /
                       sapply(company_financials[["balance_sheets"]], function(x) apply(x['Total Assets', 1:2], 1, mean)) + 
                       sapply(company_financials[["financials"]], function(x) apply(x['Total Revenue',2, drop = F], 1, mean)) /
                       sapply(company_financials[["balance_sheets"]], function(x) apply(x['Total Assets', 2:3], 1, mean))) / 2
  
  missed1 <- as.numeric(which(is.na(cash_flow_to_debt))) ## the indicies of NA values from the ratios above are the subset of this one!
  missed2 <- as.numeric(which(is.na(AR_to_Sales)))
  missed3 <- as.numeric(which(is.na(Asset_turnover)))
  missed <- unique(c(missed1, missed2, missed3))
  firms <- firms[-missed]
  
  current_ratio <- current_ratio[-missed]
  ROA <- ROA[-missed]
  profit_margin <- profit_margin[-missed]
  debt_to_equity <- debt_to_equity[-missed]
  cash_flow_to_debt <- cash_flow_to_debt[-missed]
  AR_to_Sales <- AR_to_Sales[-missed]
  Asset_turnover <- Asset_turnover[-missed]
  
  Financial_ratios <- list(current_ratio, ROA, profit_margin, debt_to_equity, cash_flow_to_debt, AR_to_Sales, Asset_turnover,firms)
}


  ### Body -----
#data_scraping() ## this operation may take ca 30 minutes, therefore, attached downloaded files can be found below. We obtain financial statements,
                ## balance sheets and statements of cash flows of all S&P500 companies as of 2017-12-31 using this function
balance_sheets <- readRDS('balance_sheets.rds')
financials <- readRDS('financials.rds')
firms <- readRDS('firms.rds')
cash_flows <- readRDS('cash_flows.rds')

company_financials <- data_transformation() ## This function transforms out dataset to the more conveninet format. Furthermore, this returns
                                            ## only the set of firms which all three statements are sufficient for. (i.e. all three statements
                                            ## contains information for years 2017, 2016 and 2015.)
names(company_financials) <- c('financials', 'balance_sheets', 'cash_flows')
firms <- names(company_financials$financials)

Financial_ratios <- financial_ratios() ## This function provides us with five 3-year-average financial ratios for 416 firms, which this information
                                       ## is available for.
firms <- Financial_ratios[[8]]
Financial_ratios <- Financial_ratios[-8]
names(Financial_ratios) <- c('Current_ratio', 'ROA', 'Profit_margin', 'Debt_to_Equity', 'Cash_Flow_to_Debt', 'AR_to_Sales', 'Asset_turnover')

ratios_df <- as.data.frame(sapply(Financial_ratios, function(x) x))
rownames(ratios_df) <- firms

  # Initial exploratory analysis
par(mfrow = c(4,2))
hist(ratios_df$Current_ratio)
hist(ratios_df$ROA)
hist(ratios_df$Profit_margin)
hist(ratios_df$Debt_to_Equity)
hist(ratios_df$Cash_Flow_to_Debt)
hist(ratios_df$AR_to_Sales)
hist(ratios_df$Asset_turnover)

boxplot(ratios_df$Current_ratio)
boxplot(ratios_df$ROA)
boxplot(ratios_df$Profit_margin)
boxplot(ratios_df$Debt_to_Equity)
boxplot(ratios_df$Cash_Flow_to_Debt)
boxplot(ratios_df$AR_to_Sales)
boxplot(ratios_df$Asset_turnover)

  # Filtering data to avoid choosing too much risk investments, thresholds are rather weaker as we do not want to narrow the pool of potential stocks too much
ratios_df <- ratios_df[-which(ratios_df$AR_to_Sales > 2),]
ratios_df <- ratios_df[-which(ratios_df$Debt_to_Equity > 20),]
ratios_df <- ratios_df[-which(ratios_df$Debt_to_Equity < -5),]
ratios_df <- ratios_df[-which(ratios_df$Cash_Flow_to_Debt > 20),] # Two companies are deleted due to the very outstanding (really high) CF-to-debt ratio
ratios_df <- ratios_df[-which(ratios_df$Profit_margin < -0.25),]
firms <- rownames(ratios_df)
  # Deleting duplicated rows companies
ratios_df <- unique(ratios_df)
firms <- rownames(ratios_df)

  # Exploratory analysis again
hist(ratios_df$Current_ratio)
hist(ratios_df$ROA)
hist(ratios_df$Profit_margin)
hist(ratios_df$Debt_to_Equity)
hist(ratios_df$Cash_Flow_to_Debt)
hist(ratios_df$AR_to_Sales)
hist(ratios_df$Asset_turnover)

  # Downloading stock market data, computing average returns, standard deviation and ratios
ratios_df <- ratios_df[-which(firms == 'JEF'),] # data are not downloadable for a given company
firms <- rownames(ratios_df)
ratios_df <- ratios_df[-which(firms == 'LIN'),] # data are not downloadable for a given company
firms <- rownames(ratios_df)
ratios_df <- ratios_df[-which(firms == 'PPG'),] # data are not downloadable for a given company
firms <- rownames(ratios_df)
ratios_df <- ratios_df[-which(firms == 'DGX'),] # data are not downloadable for a given company
firms <- rownames(ratios_df)

#stocks <- list()
#for (i in 1:length(ratios_df)){
  #symbols <- new.env()
  #getSymbols(Symbols = firms[i], env = symbols, from = '2014-12-29', to = '2017-12-31', periodicity = 'weekly')
  #stocks <- append(stocks, as.list(symbols))
#} # I use this iterative method in order to detect stock which information is not available for, and to avoid warnings that I download more than 5 indices in one batch
#saveRDS(stocks, 'stocks.rds')
stocks <- readRDS('stocks.rds')

dataset <- do.call(merge, stocks)
dataset <- as.data.frame(dataset)
adjusted <- 6 * (1:nrow(ratios_df))
returns <- (dataset[2:nrow(dataset),adjusted] - dataset[1:(nrow(dataset)-1),adjusted]) / dataset[1:(nrow(dataset)-1),adjusted]
rownames(returns) <- NULL
colnames(returns) <- firms

geometric_mean_returns <- apply(returns, 2, function(x) (prod(x + 1, na.rm = T))^(1/nrow(returns)) - 1)
standard_deviations <- apply(returns, 2, function(x) sd(x, na.rm = T))

statistics <- as.data.frame(geometric_mean_returns)
statistics$SD <- standard_deviations
rownames(statistics) <- firms
colnames(statistics) <- c('Avg.returs', 'Std.dev')
statistics$Ratio <- statistics$Avg.returs / statistics$Std.dev

  # Clustering part
par(mfrow = c(1,1))
ratios_df <- as.data.frame(sapply(ratios_df, function(x) (x - mean(x) / sd(x)))) ## standardization
  # Even though all measures are obtained in the same metrics, they may slightly vary in a degree of variance and thus I consider standardization as a reasonable choice
distance <- dist(ratios_df, method = 'euclidian')
fit <- hclust(distance, method = 'ward.D')
CH <- rep(NA, 90)
for (k in 6:95) {
  labels <- cutree(fit, k)
  CH[k-5] <- calinhara(ratios_df, labels)
}
maxCH <- which.max(CH) + 5
plot(6:95, CH, xlab = 'Number of clusters', ylab = 'Calinski-Harabasz values')
abline(v = maxCH, col = 'blue')
labels <- cutree(fit, maxCH)
  # Bootstraping to verify the reliability of the clusters
cboot.hclust <- clusterboot(ratios_df,
                            clustermethod = hclustCBI,
                            method = 'ward.D',
                            k = maxCH,
                            B = 1000,
                            count = T)
mean_Jaccard_index <- cboot.hclust$bootmean
mean_Jaccard_index
sort(mean_Jaccard_index, decreasing = T)

ratios_df$labels <- factor(labels)
ratios_df$Return.Ratio <- statistics$Ratio

  # Selecting one stock with the highest avg.return/std.dev ratio from each group
selected_stocks <- rep(NA, maxCH)
for (i in 1:maxCH){
  selected_stocks[i] <- as.integer(rownames(ratios_df[which(ratios_df$labels == i),"Return.Ratio",drop = F])[which.max(ratios_df$Return.Ratio[which(labels == i)])])
}
selected_stocks <- firms[selected_stocks]
  
  # Aggregated cluster information containing a number of firms in each cluster, corresponding Mean Jaccard index and the selected stock with the best ratio
cluster_information <- rbind(as.vector(summary(factor(labels))), round(mean_Jaccard_index, 3), selected_stocks)
cluster_information <- as.data.frame(cluster_information)
rownames(cluster_information) <- c('# of stocks', 'Mean Jaccard index', 'Selected stock')
colnames(cluster_information) <- 1:length(selected_stocks)
cluster_information
  # Correlation matrix
selected_returns <- returns[,selected_stocks]
corrs <- matrix(NA, nrow = ncol(selected_returns), ncol = ncol(selected_returns))
##Deriving correlation matrix
for (i in 1:ncol(selected_returns)){
  for (j in 1:ncol(selected_returns)){
    corrs[i,j] <- cor(selected_returns[,i], selected_returns[,j])
  }
}
corrs <- as.data.frame(corrs)
corrs <- round(corrs, 2)
colnames(corrs) <- selected_stocks
rownames(corrs) <- selected_stocks
cor_pmat <- cor_pmat(corrs)
ggcorrplot(corrs, type = 'lower', lab = TRUE, lab_size = 2, outline.color = 'gray') ## correlation plot without considering statistical significance
corplot <- ggcorrplot(corrs, type = 'lower', lab = FALSE, outline.color = 'gray', p.mat = cor_pmat,
              insig = c('pch'), pch = 4, pch.cex = 5, pch.col = 'black',sig.level = 0.1,
              tl.col = 'black', tl.cex = 15, tl.srt = 90,
              colors = c('#2E86C1', 'white', '#E74C3C')) ##corrplot considering stat. significance) ##corrplot considering stat. significance
corplot

png('corplot.png', width = 600, height = 600)
corplot
dev.off()

  # Projecting weekly returns of S&P500 against my portfolio in the period from 2018-02-01 to 2018-11-30
  # In this stage, no intervention regarding statistically significant (at the 5% level) is executed
stocks1 <- list()
#for (i in 1:39){
  #symbols <- new.env()
  #getSymbols(Symbols = selected_stocks[i], env = symbols, from = '2018-02-01', to = '2018-11-30', periodicity = 'weekly')
  #stocks1 <- append(stocks1, as.list(symbols))
#} # I use this iterative method in order to detect stock which information is not available for, and to avoid warnings that I download more than 5 indices in one batch
#saveRDS(stocks1, 'stocks1.rds')
stocks1 <- readRDS('stocks1.rds')

dataset <- do.call(merge, stocks1)
dataset <- as.data.frame(dataset)
adjusted <- 6 * (1:39)
test_returns <- (dataset[2:nrow(dataset),adjusted] - dataset[1:(nrow(dataset)-1),adjusted]) / dataset[1:(nrow(dataset)-1),adjusted]

my_portfolio <- rowSums(test_returns) / 39 # In this task, I consider weight of all stock is the same
cumulative_returns_of_my_portfolio <- rep(NA, 43)
for (i in 1:43) {
  cumulative_returns_of_my_portfolio[i] <- prod(my_portfolio[1:i] + 1) 
} # I use cummulative returns + 1 in order to depict the $1000 investment during the period stated above
cumulative_returns_of_my_portfolio <- append(1 ,cumulative_returns_of_my_portfolio)

getSymbols(Symbols = '^GSPC', env = .GlobalEnv, from = '2018-02-01', to = '2018-11-30', periodicity = 'weekly')
sp500 <- as.data.frame(GSPC)[,6]
sp500_returns <- (sp500[-1] - sp500[-44]) / sp500[-44]
cumulative_sp500_returns <- rep(NA, 43)
for (i in 1:43) {
  cumulative_sp500_returns[i] <- prod(sp500_returns[1:i] + 1)
}
cumulative_sp500_returns <- append(1, cumulative_sp500_returns)

dataset <- as.data.frame(cbind(1:44, 1000 * cumulative_returns_of_my_portfolio, 1000 * cumulative_sp500_returns))
colnames(dataset) <- c('Week', 'My.portfolio', 'SP500')

  ## Graph of weekly returns comparing the performance of my portfolio against the S&P500
ggplot <- ggplot(data = dataset, aes(x = Week)) +
  geom_line(aes(y = My.portfolio, colour = 'My portfolio'), lwd = 1.2) +
  geom_line(aes(y = SP500, colour = 'S&P500'), lwd = 1.1) +
  xlab('Weeks') +
  ylab('Dollars') +
  ggtitle('My portfolio vs. SP500') +
  scale_colour_manual(name = '$1000 investment', values = c('My portfolio' = 'Darkblue', 'S&P500' = 'Red')) + 
  theme(axis.title.x = element_text(colour = "Black", size = 12),
        axis.title.y = element_text(colour = "Black", size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = c(0.105, 0.905),
        legend.background = element_rect(colour = 'white', fill = 'gray', linetype='solid'),
        legend.title = element_text(size = 12))  
ggplot

png('ggplot.png', width = 600, height = 400)
ggplot
dev.off()


  ##### Alternative approach consider dissolved clusters as big one
new_labels <- ifelse(mean_Jaccard_index[labels] < 0.5, factor(1), labels)
new_labels <- factor(new_labels)
summary(new_labels)

ratios_df$new_labels <- new_labels

# Selecting one stock with the highest avg.return/std.dev ratio from each group
new_selected_stocks <- rep(NA, length(summary(new_labels)))
for (i in 1:length(summary(new_labels))){
  new_selected_stocks[i] <- as.integer(rownames(ratios_df[which(ratios_df$labels == i),"Return.Ratio",drop = F])[which.max(ratios_df$Return.Ratio[which(labels == i)])])
}
new_selected_stocks <- firms[new_selected_stocks]
selected_stocks
new_selected_stocks

new_selected_returns <- returns[,new_selected_stocks]
new_corrs <- matrix(NA, nrow = ncol(new_selected_returns), ncol = ncol(new_selected_returns))
##Deriving correlation matrix
for (i in 1:ncol(new_selected_returns)){
  for (j in 1:ncol(new_selected_returns)){
    new_corrs[i,j] <- cor(new_selected_returns[,i], new_selected_returns[,j])
  }
}
new_corrs <- as.data.frame(new_corrs)
new_corrs <- round(new_corrs, 2)
colnames(new_corrs) <- new_selected_stocks
rownames(new_corrs) <- new_selected_stocks
new_cor_pmat <- cor_pmat(new_corrs)
ggcorrplot(new_corrs, type = 'lower', lab = TRUE, lab_size = 2, outline.color = 'gray') ## correlation plot without considering statistical significance
new_corplot <- ggcorrplot(new_corrs, type = 'lower', lab = FALSE, outline.color = 'gray', p.mat = new_cor_pmat,
                  insig = c('pch'), pch = 4, pch.cex = 9, pch.col = 'black',sig.level = 0.1,
                  tl.col = 'black', tl.cex = 15, tl.srt = 90,
                  colors = c('#2E86C1', 'white', '#E74C3C')) ##corrplot considering stat. significance
new_corplot

png('new_corplot.png', width = 600, height = 600)
new_corplot
dev.off()

#new_stocks1 <- list()
#for (i in 1:28){
  #new_symbols <- new.env()
  #getSymbols(Symbols = new_selected_stocks[i], env = new_symbols, from = '2018-02-01', to = '2018-11-30', periodicity = 'weekly')
  #new_stocks1 <- append(new_stocks1, as.list(new_symbols))
#} # I use this iterative method in order to detect stock which information is not available for, and to avoid warnings that I download more than 5 indices in one batch
#saveRDS(new_stocks1, 'new_stocks1.rds')
new_stocks1 <- readRDS('new_stocks1.rds')
new_dataset <- do.call(merge, new_stocks1)
new_dataset <- as.data.frame(new_dataset)
new_adjusted <- 6 * 1:28
new_test_returns <- (new_dataset[2:nrow(new_dataset),new_adjusted] - new_dataset[1:(nrow(new_dataset)-1),new_adjusted]) / new_dataset[1:(nrow(new_dataset)-1),new_adjusted]

new_my_portfolio <- rowSums(new_test_returns) / length(summary(new_labels)) # In this task, I consider weight of all stock is the same
new_cumulative_returns_of_my_portfolio <- rep(NA, 43)
for (i in 1:43) {
  new_cumulative_returns_of_my_portfolio[i] <- prod(new_my_portfolio[1:i] + 1) 
} # I use cummulative returns + 1 in order to depict the $1000 investment during the period stated above
new_cumulative_returns_of_my_portfolio <- append(1 ,new_cumulative_returns_of_my_portfolio)

new_dataset <- as.data.frame(cbind(1:44, 1000 * new_cumulative_returns_of_my_portfolio, 1000 * cumulative_sp500_returns))
colnames(new_dataset) <- c('Week', 'My.new.portfolio', 'SP500')

## Graph of weekly returns comparing the performance of my portfolio against the S&P500
new_ggplot <- ggplot(data = new_dataset, aes(x = Week)) +
  geom_line(aes(y = My.new.portfolio, colour = 'My portfolio'), lwd = 1.2) +
  geom_line(aes(y = SP500, colour = 'S&P500'), lwd = 1.1) +
  xlab('Weeks') +
  ylab('Dollars') +
  ggtitle('My portfolio vs. SP500') +
  scale_colour_manual(name = '$1000 investment', values = c('My portfolio' = 'Darkblue', 'S&P500' = 'Red')) + 
  theme(axis.title.x = element_text(colour = "Black", size = 12),
        axis.title.y = element_text(colour = "Black", size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = c(0.105, 0.905),
        legend.background = element_rect(colour = 'white', fill = 'gray', linetype='solid'),
        legend.title = element_text(size = 12))  
new_ggplot

png('new_ggplot.png', width = 600, height = 400)
new_ggplot
dev.off()

## Graph of both versions of portfolio against the S&P
presented_plot <- ggplot(data = new_dataset, aes(x = Week)) +
  geom_line(aes(y = My.new.portfolio, colour = 'Restricted portfolio'), lwd = 1.2) +
  geom_line(aes(y = My.portfolio, colour = 'Complete portfolio'), lwd = 1.2, data = dataset) +
  geom_line(aes(y = SP500, colour = 'S&P500'), lwd = 1.1) +
  xlab('Weeks') +
  ylab('Dollars') +
  scale_colour_manual(name = '$1000 investment', values = c('Complete portfolio' = 'Darkblue',  'Restricted portfolio' = 'Green', 'S&P500' = 'Red')) + 
  theme(axis.title.x = element_text(colour = "Black", size = 14, face = 'bold'),
        axis.title.y = element_text(colour = "Black", size = 14, face = 'bold'),
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.text.y = element_text(size = 12, face = 'bold'),
        legend.position = c(0.105, 0.922),
        legend.background = element_rect(colour = 'white', fill = 'gray', linetype='solid'),
        legend.title = element_text(size = 18),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'Black'),
        panel.grid.major.y = element_line(colour = 'Gray', size = 0.25))
presented_plot

png('presented_plot.png', width = 800, height = 600)
presented_plot
dev.off()

## Complementary table
comp_table <- matrix(NA, nrow = 3, ncol = 5)
comp_table[,1] <- c('Complete portfolio', 'Restricted portfolio', 'S&P500')
comp_table  <- as.data.frame(comp_table)
colnames(comp_table) <- c('Index', 'Return', 'Std. dev.', 'Cor. with S&P500', 'Size')
comp_table[,2] <- paste(c( round((dataset$My.portfolio[nrow(dataset)] - 1000) / 10, 2),
                           round((new_dataset$My.new.portfolio[nrow(new_dataset)] - 1000) / 10, 2),
                           round((dataset$SP500[nrow(dataset)] - 1000) / 10, 2)), '%')
comp_table[,3] <- paste(c(round(sd(my_portfolio) * 100,2), round(sd(new_my_portfolio) * 100, 2), round(sd(sp500_returns) * 100, 2)),'%')
comp_table[,4] <- c(round(cor(my_portfolio, sp500_returns), 2), round(cor(new_my_portfolio, sp500_returns),2), 1)
comp_table[,5] <- c(39, 31, 500)
comp_table

#<<results=tex>>
  #xtable(comp_table, caption = 'Investment information')
#@