library(tidyverse)
library(lubridate)
holdout<- read_csv("/Users/malorie/Library/CloudStorage/Dropbox/GITHUB/2022_NYR/model_progression.csv")
holdout<- holdout%>%select(!c(Group,Dataset))

# add numbers to the Method names
holdout<- holdout %>% mutate(x_label=paste0(row_number()-1,": ",Method))
holdout$x_label=gsub('Logistic','Logit',holdout$x_label)
holdout$x_label=gsub('RF Gini','RFG',holdout$x_label)
holdout$x_label=gsub('feature','',holdout$x_label)
holdout$x_label=gsub('Filtered DT','FDT',holdout$x_label)
datatable(holdout,  class = "compact",options = list(pageLength = 18))


recall_baseline<-holdout$Recall[1]
precision_baseline<- holdout$Precision[1]
holdout <- holdout[-1,]



performance_threshold_dash= 'shortdashdot'
precision_color='maroon'
recall_color= 'slateblue'
    
below_baseline_color='lightgrey'
above_goal_color='green'
      
#####  what do you want to name the values
x <- c("Method","Precision", "Recall", "Accuracy")  

#####  column names of the values & formatting to cut off numerics at 2 decimal places
y <- sprintf("{point.%s:.2f}", c("Method", "Precision", "Recall", "Accuracy"))   

#####  creates the tooltip
tltip <- tooltip_table(x, y)            

highchart() %>%
      hc_add_series(holdout, "line", hcaes( x_label , Precision )
                    , name = "Precision" 
                    , color = precision_color 
                    # conditional line coloring: let the line plot change colors and dash style based on y-values
                    , zones = list(  
                                 list( value = precision_baseline , color= below_baseline_color)
                              ,  list( value = 0.8  , color = precision_color )
                              ,  list( value = 1.01 , color = above_goal_color , dashStyle='shortdashdot' , dashWidth=3 )
                    ) 
      ) %>% 
      hc_add_series(holdout , "line",  hcaes( x_label , Recall )  
                    , name = "Recall"  , color = recall_color
                    , zones = list(
                                 list(value=recall_baseline,color= below_baseline_color)
                               , list(value = 0.8  , color = recall_color)
                               , list(value = 1.01 , color = above_goal_color ) 
                    )
      ) %>% 
      hc_xAxis(  type="category"
               , tickWidth = 1
               , labels = list( rotation = -30)                                              # try: staggerLines = 2 instead of rotation
               , crosshair = list( width = 40 ,color = rgb(0,0,1,.1) )
               , plotBands = list(
                                 list( from = 6.75
                                     , to = 7.25 
                                     , color = '#FCFFC5'
                                     , label = list(text='Best Combined Performance'
                                                  , style = list(color='grey')
                                                  , verticalAlign = "bottom"
                                                  , y = -20)
                                 )
                )
      ) %>%   
      hc_yAxis( max = 1
         , plotLines = list(
              list(value = recall_baseline
                , label = list(text = "Recall Baseline", y = 13 , style = list(color = 'grey' , fontFamily = 'monospace'))
                , color = "slateblue"
                , width = 1.75
                , dashStyle = performance_threshold_dash
                   )
            , list(value = precision_baseline
                 , label = list(text = "Precision Baseline", y = 13 ,style = list(color = 'grey' , fontFamily = 'monospace') )
                 , color = "maroon"
                 , width = 2
                 , dashStyle = performance_threshold_dash
                   )
            , list(value = 0.8
                 , label = list(text = "GOAL",style = list(fontWeight = 'bold' , fontFamily = 'monospace') )
                 , color = above_goal_color 
                 , width = 2.25
                 , dashStyle = performance_threshold_dash )
          )
      ) %>%    
      hc_legend(align = "right"
               , verticalAlign = "top"
               , layout = "vertical") %>% 
      hc_tooltip(useHTML = TRUE
               , headerFormat = NULL
               , pointFormat =tltip
               , distance = 30) %>%  # distance of the hover box from the data point. use this when hover 
                                     # box is coverering up important data that you want to see while looking at the tooltip                                
      hc_title(text = "Precision/Recall Tradeoff") %>% 
      hc_subtitle(text = "ML Classification Model Progression") %>%
      hc_size(height = 600 , width = 1000)
