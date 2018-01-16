library(Rblpapi)
library(dplyr)
library(readxl)
library(foreach)
library(xts)
library(magrittr)
library(tidyquant)
library(tibble)
library(ggplot2)
library(foreach)
library(gridExtra)
library(randomForest)
library(caret)

start.date<-'1962-01-31' #'1962-01-31' #'1960-01-01'
end.date<-Sys.Date()
Symbols_2Y<-read_excel('T:/taolu/Fixed Income/Yield Curve Model/Time Series.xlsx', sheet='Ticker_2y')
Fields<-read_excel('T:/taolu/Fixed Income/Yield Curve Model/Time Series.xlsx', sheet='Field')

blpConnect()
Info<-bdp(Symbols_2Y$Ticker, Fields$Fields)
Info %<>% cbind(Ticker=rownames(.), stringsAsFactors=FALSE) %>% mutate(Ticker.dot=make.names(Ticker))

reftable<-select(Info, Ticker, Ticker.dot) %>% left_join(Symbols_2Y) %>% as.matrix
Series<-bdh(Symbols_2Y$Ticker, 'PX_LAST', as.Date(start.date))


Series<-foreach(i=1:length(Series), .combine=cbind) %do% {
  tseries<-xts(Series[[i]]$PX_LAST, Series[[i]]$date) %>% set_colnames(names(Series)[i])
}

cut_off = '2007-01-01'
Series_2007 = Series['2007-01-03/'] %>% na.locf()

# week.tickers<-dplyr::filter(Info, INDX_FREQ %>% startsWith('Weekly'))$Ticker %>% make.names
# 
# week.freqs <- Series[,week.tickers] %>% na.locf %>% na.trim(sides='left') %>% to.daily(indexAt='lastof', OHLC=FALSE)
# nrow(Series[,-which(names(Series) %in% week.tickers)])
# nrow(week.freqs)
# 
# data = cbind(coredata(week.freqs), coredata(Series[,-which(names(Series) %in% week.tickers)]) )
roll_width = 80

Series_2007 %>% as.data.frame() %>% rollapply(width = roll_width, 
                                              FUN = function(x) summary(lm(USGG2YR.Index ~ S0042FS.3M3M.BLC.Curncy + ARDITRBM.Index + CBT42NCN.Index, data = as.data.frame(x))), 
                                              by.column = FALSE) -> Series_2007_temp
Series_2007_temp %>% tibble::as_tibble() %>% mutate(Coef_number = .$coefficients,
                                                    rsq = .$r.squared %>% unlist) %>% select(Coef_number, rsq) -> temp3

avg_rsq = mean(temp3$rsq)
opt_i = roll_width

for (i in seq(100, 1000, 20))
{
  Series_2007 %>% as.data.frame() %>% rollapply(width = i, 
                                                FUN = function(x) summary(lm(USGG2YR.Index ~ S0042FS.3M3M.BLC.Curncy + ARDITRBM.Index + CBT42NCN.Index, data = as.data.frame(x))), 
                                                by.column = FALSE) -> Series_2007_temp
  Series_2007_temp %>% tibble::as_tibble() %>% mutate(Coef_number = .$coefficients,
                                                      rsq = .$r.squared %>% unlist) %>% select(Coef_number, rsq) -> temp3
  if(mean(temp3$rsq) > avg_rsq){
    avg_rsq = temp3$rsq
    opt_i = i
  }
  
}


roll_width = opt_i
Series_2007 %>% as.data.frame() %>% rollapply(width = roll_width, 
                                              FUN = function(x) summary(lm(USGG2YR.Index ~ S0042FS.3M3M.BLC.Curncy + ARDITRBM.Index + CBT42NCN.Index, data = as.data.frame(x))), 
                                              by.column = FALSE) -> Series_2007_temp
Series_2007_temp %>% tibble::as_tibble() %>% mutate(Coef_number = .$coefficients,
                                                    rsq = .$r.squared %>% unlist) %>% select(Coef_number, rsq) -> temp3

avg_rsq = mean(temp3$rsq)



temp3 %>% mutate(Intercept = lapply(.$Coef_number, "[", c(1)) %>% unlist(),
                 S0042FS.3M3M.BLC.Curncy.coef = lapply(.$Coef_number, "[", c(2)) %>% unlist(),
                 ARDITRBM.Index.coef = lapply(.$Coef_number, "[", c(3)) %>% unlist(),
                 CBT42NCN.Index.coef = lapply(.$Coef_number, "[", c(4)) %>% unlist()
) %>% select(-Coef_number) -> temp4

temp4 %>% dim()


bind_cols(Series_2007[-(1:(roll_width-1)),] %>% tibble::as.tibble(), temp4) %>% 
    mutate(pred_2y = Intercept + S0042FS.3M3M.BLC.Curncy * S0042FS.3M3M.BLC.Curncy.coef + ARDITRBM.Index*ARDITRBM.Index.coef + CBT42NCN.Index * CBT42NCN.Index.coef,
           Date = Series_2007[-(1:(roll_width-1)),] %>% index())-> Series_combind

#plot the predictions vs the actual
cols <- c("Pred"="#f04546","Actual"="#3591d1")
Series_combind %>% ggplot(aes(x = Date)) + geom_line(aes(y = pred_2y_rf,colour="Pred")) + geom_line(aes(y=USGG2YR.Index, colour="Actual")) + 
  labs( y = "Yield Level", title = "2 Year Yield Prediction vs Actual") + scale_y_continuous(breaks = seq(0, 3.5, by = 0.1), position = c("right")) + scale_colour_manual(name="Actual vs Pred", values = cols) +theme_bw()
##000099 is blue and CC0000 is red
p1 <- Series_combind %>% ggplot(aes(y = rsq, x = Date)) + labs( y = "R Square",x = "", title = "") + geom_line() + theme_bw()
p2 <- Series_combind %>% ggplot(aes(y = S0042FS.3M3M.BLC.Curncy.coef, x = Date)) + labs( y = "Forward OIS 3M3M Coefficient",x = "", title = "") + geom_line() + theme_bw()
p3 <- Series_combind %>% ggplot(aes(y = ARDITRBM.Index.coef*1000000, x = Date)) + labs( y = "Fed Excess Reserve (Trillion) Coefficient",x = "", title = "") + geom_line() + theme_bw()
p4 <- Series_combind %>% ggplot(aes(y = CBT42NCN.Index.coef, x = Date)) + labs( y = "Speculative Futures Coefficient",x = "", title = "") + geom_line() + theme_bw()

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol=2)

p5 <- Series_combind %>% ggplot(aes(x= Date)) + geom_line(aes(y= USGG2YR.Index)) + labs( y = "2Y",x = "", title = "") + theme_bw()
p6 <- Series_combind %>% ggplot(aes(x= Date)) + geom_line(aes(y= S0042FS.3M3M.BLC.Curncy)) + labs( y = "Forward OIS 3M3M",x = "", title = "") + theme_bw()
p7 <- Series_combind %>% ggplot(aes(x= Date)) + geom_line(aes(y= ARDITRBM.Index)) + labs( y = "Fed Excess Reserve",x = "", title = "") + theme_bw()
p8 <- Series_combind %>% ggplot(aes(x= Date)) + geom_line(aes(y= CBT42NCN.Index)) + labs( y = "Speculative Futures",x = "", title = "") + theme_bw()
grid.arrange(p5, p6, p7, p8, nrow = 2, ncol=2)

start_date_change = '2017-06-30'
end_date_change = '2017-08-29'
# bind_rows(Series_combind %>% select(Date,S0042FS.3M3M.BLC.Curncy,ARDITRBM.Index,CBT4TNCN.Index,USGG2YR.Index) %>%  filter(Date == start_date_change),
#            Series_combind %>% select(Date,S0042FS.3M3M.BLC.Curncy,ARDITRBM.Index,CBT4TNCN.Index,USGG2YR.Index) %>%  filter(Date == end_date_change)) -> changes
changes = (Series_combind %>% select(Date,S0042FS.3M3M.BLC.Curncy,ARDITRBM.Index,CBT42NCN.Index,USGG2YR.Index) %>%  filter(Date == end_date_change)) - (Series_combind %>% select(Date,S0042FS.3M3M.BLC.Curncy,ARDITRBM.Index,CBT42NCN.Index,USGG2YR.Index) %>%  filter(Date == start_date_change))

(changes %>% select(S0042FS.3M3M.BLC.Curncy,ARDITRBM.Index,CBT42NCN.Index)) * (Series_combind %>% filter(Date == end_date_change) %>% 
  select(S0042FS.3M3M.BLC.Curncy.coef, ARDITRBM.Index.coef,CBT42NCN.Index.coef)) %>% tibble::as_tibble() -> contributions
residuals_changes = changes$USGG2YR.Index - sum(contributions)
summary_changes = tibble(x = c("Change_2y","ForwardOIS3m3m","Fed_excess_reserve","Future_positions","Residuals"), y = c(changes$USGG2YR.Index,contributions[[1]],contributions[[2]], 
                          contributions[[3]], residuals_changes))
summary_changes %>% ggplot(aes(x, y)) + geom_bar(stat = "identity") + labs(x = "", y = "", title = paste0("Breakdown of the change of 2Y yield Between ", start_date_change, " and " ,end_date_change))

Series_combind_2017 = Series_combind %>% filter(Date >= "2017-01-01")
Performance_2017 <- mean(abs((Series_combind_2017$pred_2y[-length(Series_combind_2017$pred_2y)] - Series_combind_2017$USGG2YR.Index[-1])/ Series_combind_2017$USGG2YR.Index[-1]))
Performance_2017_sqrt <- mean(abs((Series_combind_2017$pred_2y[-length(Series_combind_2017$pred_2y)] - Series_combind_2017$USGG2YR.Index[-1]))^2)

#testing randomforest regression
rf = randomForest(USGG2YR.Index ~ S0042FS.3M3M.BLC.Curncy + ARDITRBM.Index + CBT42NCN.Index, data = Series_2007['2007-01-09/2016-12-31'], ntree = 1000, nodesize = 4, importance = TRUE)
pred = predict(rf, Series_2007['2017-01-01/'])
Performance_rf_2017 <- mean(abs((pred - Series_combind_2017$USGG2YR.Index)/ Series_combind_2017$USGG2YR.Index))
Performance_rf_2017_sqrt <- mean(abs((pred - Series_combind_2017$USGG2YR.Index))^2)

Series_2007_2016 = Series_2007['2007-01-09/2016-12-31']
Series_2007_2016_rf = bind_cols(coredata(Series_2007_2016) %>% tibble::as.tibble(), pred_rf = rf$predicted) %>% 
  mutate(Date = Series_2007_2016 %>% index())
importance(rf)
cols <- c("Pred"="#f04546","Actual"="#3591d1")
Series_2007_2016_rf %>% ggplot(aes(x = Date)) + geom_line(aes(y = pred_rf,colour="Pred")) + geom_line(aes(y=USGG2YR.Index, colour="Actual")) + 
  labs( y = "Yield Level", title = "2 Year Yield Prediction vs Actual") + scale_y_continuous(breaks = seq(0, 3.5, by = 0.1), position = c("right")) + scale_colour_manual(name="Actual vs Pred", values = cols) +theme_bw()
print(rf)
plot(rf)



bind_cols(Series_2007[-(1:(roll_width-1)),] %>% tibble::as.tibble(), temp4) %>% 
  mutate(pred_2y = Intercept + S0042FS.3M3M.BLC.Curncy * S0042FS.3M3M.BLC.Curncy.coef + ARDITRBM.Index*ARDITRBM.Index.coef + CBT42NCN.Index * CBT42NCN.Index.coef,
         Date = Series_2007[-(1:(roll_width-1)),] %>% index())-> Series_combind

fitControl <- trainControl(
  ## Repeated 5-fold CV 
  method = "cv",
  number = 5
  ## repeated 10 times
  #repeats = 10,
  #verboseIter = TRUE,
  #returnResamp = "all"
  )

rrfFit <- train(USGG2YR.Index ~ ., 
                data = Series_2007['2007-01-09/2016-12-31'],
                method = 'rf',
                metric = "RMSE",
                trControl = fitControl,
                num.trees = 200
                )

testPred <- predict(rrfFit, Series_2007['2017-01-01/'])
RMSE(testPred, Series_2007['2017-01-01/','USGG2YR.Index'])
