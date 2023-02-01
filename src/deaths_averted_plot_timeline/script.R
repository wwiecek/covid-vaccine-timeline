
# Counterfactual selection, manually specified

cfs <- c('no_vaccines','counterfactual_vaccination')

###Load data:

table1_df_ind <- loadCounterfactualDataSingle(cfs,
    group_by = "iso3c") %>%
  select(!country)

#colours
colour_direct <- "#98df8a"
colour_total <- "#17becf"
colour_baseline <- "black"
colour_counterfactual <- "#d62728"
colour_vaccinated <- "#17becf"
colour_baseline_vaccinated <- "#98df8a"
colour_waned <- "#ff1235"

### Plot deaths averted
da_plot <- ggplot(table1_df_ind,aes(x=counterfactual,y=averted_deaths_avg))+
  geom_bar(stat="identity",position=position_dodge()) +
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))

ggsave("deaths_averted_plot.pdf",da_plot,device='pdf')

table2_df_ind <- loadCounterfactualDataSingle(cfs,
                                 group_by = c("iso3c","date")) %>%
    mutate(
        cumulative_deaths = ave(deaths_avg,counterfactual,iso3c,FUN=cumsum)
      ) %>%
    mutate(
        baseline_cumulative_deaths = ave(baseline_deaths_avg,counterfactual,iso3c,FUN=cumsum)
       ) %>%
    mutate (
        vaccinated = vaccinated_avg /2.5e4 - vaccinated_second_waned_avg /2.5e4
      ) %>%
    mutate(
        baseline_vaccinated = baseline_vaccinated_avg /2.5e4 - baseline_vaccinated_second_waned_avg /2.5e4
      ) %>%
    mutate(
        percent_susceptible = percent_susceptible_avg*3000
        ) %>%
    mutate(
        baseline_percent_susceptible = baseline_percent_susceptible_avg*3000
        )

ts_plots <- lapply(cfs,function(cf){
    deaths_timeseries_plot <- ggplot(table2_df_ind %>% filter(counterfactual == cf), aes(x = date)) +
        geom_line(aes(y = baseline_deaths_avg, colour="baseline")) +
        geom_line(aes(y = deaths_avg, colour = "counterfactual")) +
        geom_line(aes(y = percent_susceptible, colour = "counterfactual percent susceptible")) +
        geom_line(aes(y = baseline_percent_susceptible, colour = "baseline percent susceptible")) +
        facet_wrap(~iso3c,nrow=2)
        scale_colour_manual(labels=c('baseline','counterfactual','percent susceptible','baseline percent susceptible'),
          values=c(colour_baseline,colour_counterfactual,colour_vaccinated,colour_baseline_vaccinated, colour_waned))
        labs(x = "Date", y = "Daily Deaths",title=cf) +
        theme_pubr() +
        theme(legend.position = "right")

    fn <- paste0(cf,"_tsplot.pdf")

    ggsave(fn,deaths_timeseries_plot,device='pdf')

    infections_timeseries_plot <- ggplot(table2_df_ind %>% filter(counterfactual == cf), aes(x = date)) +
        geom_line(aes(y = baseline_infections_avg, colour="baseline")) +
        geom_line(aes(y = infections_avg, colour = "counterfactual")) +
        geom_line(aes(y = percent_susceptible*300, colour = "counterfactual percent susceptible")) +
        geom_line(aes(y = baseline_percent_susceptible*300, colour = "baseline percent susceptible")) +
        facet_wrap(~iso3c,nrow=2)
        scale_colour_manual(labels=c('baseline','counterfactual','vaccinated','baseline_vaccinated'),
          values=c(colour_baseline,colour_counterfactual,colour_vaccinated,colour_baseline_vaccinated))
        labs(x = "Date", y = "Daily Deaths",title=cf) +
        theme_pubr() +
        theme(legend.position = "right")

    fn <- paste0(cf,"_infection_tsplot.pdf")

    ggsave(fn,infections_timeseries_plot,device='pdf')


    log_infections_timeseries_plot <- ggplot(table2_df_ind %>% filter(counterfactual == cf), aes(x = date)) +
        geom_line(aes(y = log(infections_avg)/max(log(infections_avg)), colour="log infections")) +
        geom_line(aes(y = percent_susceptible/3000, colour = "counterfactual percent susceptible")) +
        facet_wrap(~iso3c,nrow=2)
        scale_colour_manual(labels=c('baseline','counterfactual','vaccinated','baseline_vaccinated'),
          values=c(colour_baseline,colour_counterfactual,colour_vaccinated,colour_baseline_vaccinated))
        labs(x = "Date", y = "Percent",title=cf) +
        theme_pubr() +
        theme(legend.position = "right")

    fn <- paste0(cf,"_log_infection_tsplot.pdf")
    ggsave(fn,log_infections_timeseries_plot,device='pdf')

    log_baseline_infections_timeseries_plot <- ggplot(table2_df_ind %>% filter(counterfactual == cf), aes(x = date)) +
            geom_line(aes(y = log(baseline_infections_avg)/max(log(infections_avg)), colour="log baseline infections")) +
            geom_line(aes(y = baseline_percent_susceptible/3000, colour = "baseline percent susceptible")) +
            facet_wrap(~iso3c,nrow=2)
            scale_colour_manual(labels=c('baseline','counterfactual','vaccinated','baseline_vaccinated'),
              values=c(colour_baseline,colour_counterfactual,colour_vaccinated,colour_baseline_vaccinated))
            labs(x = "Date", y = "Percent",title=cf) +
            theme_pubr() +
            theme(legend.position = "right")

      fn <- paste0(cf,"_log_baseline_infection_tsplot.pdf")
      ggsave(fn,log_baseline_infections_timeseries_plot,device='pdf')

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