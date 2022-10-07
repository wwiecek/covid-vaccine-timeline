
# if(!is.na(seed)){
#   set.seed(seed)
# }

cfs <- c("1-days-earlier", "2-days-earlier",  "4-days-earlier",  "8-days-earlier",
                  "16-days-earlier", "32-days-earlier", "64-days-earlier")

###Load data:

table1_df_ind <- loadCounterfactualData(cfs,
                                 group_by = "iso3c") %>%
  select(!country) %>%
  mutate(
      days = map(counterfactual,function(cf){
      d <- str_split(cf,'-')[[2]]
        if(~is.na(d)){
          return(as.integer(d))
          } else{
            return(-1)
          }
        }) %>%
  mutate(
    efficacy_length = map(counterfactual,function(cf){
        return(as.integer(gsub("d","",str_split(cf,'-')[[1]])))
      })
    )

### Plot deaths averted
da_plot <- ggplot(table1_df_ind,aes(x=reorder(counterfactual,days),y=averted_deaths_avg,fill=efficacy_length))
  +geom_bar(stat="identity",position=position_dodge()) 
  + theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))

ggsave("deaths_averted_plot.pdf",da_plot,device='pdf')

table2_df_ind <- loadCounterfactualData(cfs,
                                 group_by = c("iso3c","date"))

ts_plots <- lapply(cfs,function(cf{
  deaths_timeseries_plot <- ggplot(table2_df_ind %>% filter(counterfactual == cf), aes(x = date)) +
        geom_line(aes(y = deaths_avg), colour = colour_counterfactual) +
        geom_line(aes(y = baseline_deaths_avg), colour = colour_baseline) +
        labs(x = "Date", y = "Daily Deaths",title=cf) +
        theme_pubr() +
        theme(legend.position = "none")
  fn = paste0(cf,"_tsplot.pdf")
  ggsave(fn,deaths_timeseries_plot,device='pdf')
  })
)