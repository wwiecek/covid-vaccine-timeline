# # Use to switch into this file's directory for development
# root = here::here()
# setwd(file.path(root, "src/deaths_averted_plot_timeline"))
# setwd(root) # Use to get back to root

# Counterfactual selection, manually specified

cfs <- c('owid_raw','no_vaccines', 'counterfactual_vaccination')

###Load data:

table1_df_ind <- loadCounterfactualDataSingle(group_by = "iso3c") %>%
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

table2_df_ind <- loadCounterfactualDataSingle(group_by = c("iso3c","date")) %>%
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

# source
# https://data.worldbank.org/indicator/SP.POP.TOTL
population = data.frame(
  iso3c = c("GBR", "USA"),
  pop10k = c(67081000, 331501080) / 10000
)

table1_df = table1_df_ind %>%
  left_join(population, by = "iso3c") %>%
  mutate(averted_deaths_perpop_avg = averted_deaths_avg / pop10k,
         averted_deaths_perpop_025 = averted_deaths_025 / pop10k,
         averted_deaths_perpop_975 = averted_deaths_975 / pop10k)

digits = 0

differences_table = table1_df %>%
  mutate(delta_deaths = paste0(
    format(round(averted_deaths_avg, digits), big.mark=",", trim = TRUE),
    " [",
    format(round(averted_deaths_025, digits), big.mark=",", trim = TRUE),
    "; ",
    format(round(averted_deaths_975, digits), big.mark=",", trim = TRUE),
    "]"
  ), delta_deaths_perpop = paste0(
    format(round(averted_deaths_perpop_avg, digits), big.mark=",", trim = TRUE),
    " [",
    format(round(averted_deaths_perpop_025, digits), big.mark=",", trim = TRUE),
    "; ",
    format(round(averted_deaths_perpop_975, digits), big.mark=",", trim = TRUE),
    "]"
  ),
  counterfactual = as.factor(counterfactual),
  iso3c = as.factor(iso3c)) %>%
  select(
    iso3c,
    counterfactual,
    delta_deaths,
    delta_deaths_perpop,
  )

table = tabular((iso3c) ~ counterfactual * (delta_deaths + delta_deaths_perpop) * (identity),
                data = differences_table)
colLabels(table) = colLabels(table)[-c(1,4),]

colLabels(table)[2,] = c("Change in deaths", "Change in deaths per 10,000")

counterfactuals_table = toLatex(table)
cat(counterfactuals_table$text, file="counterfactuals_table.tex")
