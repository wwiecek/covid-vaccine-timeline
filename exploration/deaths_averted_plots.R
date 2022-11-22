require(ggplot2)
require(ggpubr)
require(dplyr)
require(gridExtra)
require(squire.page)
require(tidyr)
require(purrr)
require(readr)
require(stringr)
require(countrycode)


# cfs <- apply(expand.grid(2^(1:6),c(446,1000)),1,function(n){return(paste0("d",n[2],"-",n[1],"-days-earlier"))})
cfs <- c('owid-raw','owid-none')



###Load data:

# table1_df_ind <- loadCounterfactualData(cfs,
#     group_by = "iso3c") %>%
#   select(!country) %>%
#   filter( iso3c != "USA2" ) %>%
#   mutate(
#     days = as.numeric(gsub(".*-([0-9]+)-.*$","\\1", counterfactual))
#        ) %>%
#   mutate(
#     efficacy_length = map(counterfactual,function(cf){
#         return(as.integer(gsub("d","",str_split(cf,'-')[[1]][1])))
#       })
#     )


#colours
colour_direct <- "#98df8a"
colour_total <- "#17becf"
colour_baseline <- "black"
colour_counterfactual <- "#d62728"

# ### Plot deaths averted
# da_plot <- ggplot(table1_df_ind,aes(x=reorder(counterfactual,days),y=averted_deaths_avg,fill=efficacy_length))+
#   geom_bar(stat="identity",position=position_dodge()) +
#   theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1)) + facet_wrap(~iso3c,ncol=2)

# ggsave("deaths_averted_plot.pdf",da_plot,device='pdf')

table2_df_ind <- loadCounterfactualData(cfs,
                                 group_by = c("iso3c","date"))  %>%
  # mutate(
  #   days = map(counterfactual,function(cf){
  #     d <- str_split(cf,'-')[[1]][2]
  #       if(!is.na(d)){
  #         return(as.integer(d))
  #         } else{
  #            return(-1)
  #          }
  #        })
  #      ) %>%
  # filter(
  #   days > 0
  #   ) %>%
  mutate(
      cumulative_deaths = ave(deaths_avg,counterfactual,iso3c,FUN=cumsum)
    ) %>%
  mutate(
      baseline_cumulative_deaths = ave(baseline_deaths_avg,counterfactual,iso3c,FUN=cumsum)
     )

ts_plots <- lapply(cfs,function(cf){
  deaths_timeseries_plot <- ggplot(table2_df_ind %>% filter(counterfactual == cf), aes(x = date)) +
        geom_line(aes(y = baseline_deaths_avg, colour="baseline")) +
        geom_line(aes(y = deaths_avg, colour = "counterfactual")) +
        facet_wrap(~iso3c,nrow=2) +
        scale_colour_manual(labels=c('baseline','counterfactual'),values=c(colour_baseline,colour_counterfactual))
        labs(x = "Date", y = "Daily Deaths",title=cf) +
        theme_pubr() +
        theme(legend.position = "right")

  fn = paste0(cf,"_tsplot.pdf")
  ggsave(fn,deaths_timeseries_plot,device='pdf')
  })

cum_ts_plots <- lapply(cfs,function(cf){
  cum_deaths_timeseries_plot <- ggplot(table2_df_ind %>% filter(counterfactual == cf), aes(x = date)) +
        geom_line(aes(y = baseline_cumulative_deaths, colour="baseline")) +
        geom_line(aes(y = cumulative_deaths, colour = "counterfactual")) +
        facet_wrap(~iso3c,nrow=2) +
        scale_colour_manual(labels=c('baseline','counterfactual'),values=c(colour_baseline,colour_counterfactual))
        labs(x = "Date", y = "Daily Deaths",title=cf) +
        theme_pubr() +
        theme(legend.position = "right")

  fn = paste0(cf,"_cum_tsplot.pdf")
  ggsave(fn,cum_deaths_timeseries_plot,device='pdf')
  })