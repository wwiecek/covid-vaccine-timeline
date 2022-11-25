
# Counterfactual selection, manually specified

cfs <- c('owid-raw','no-vaccines')

###Load data:

table1_df_ind <- loadCounterfactualData(cfs,
    group_by = "iso3c") %>%
  select(!country)

#colours
colour_direct <- "#98df8a"
colour_total <- "#17becf"
colour_baseline <- "black"
colour_counterfactual <- "#d62728"

### Plot deaths averted
da_plot <- ggplot(table1_df_ind,aes(x=counterfactual,y=averted_deaths_avg))+
  geom_bar(stat="identity",position=position_dodge()) +
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))

ggsave("deaths_averted_plot.pdf",da_plot,device='pdf')

table2_df_ind <- loadCounterfactualData(cfs,
                                 group_by = c("iso3c","date")) %>%
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

  fn <- paste0(cf,"_tsplot.pdf")

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

  fn <- paste0(cf,"_cum_tsplot.pdf")

  ggsave(fn,cum_deaths_timeseries_plot,device='pdf')

  })