library(magrittr)

Rblpapi::blpConnect()

ticker <- 't' %>% stringr::str_to_upper() %>% paste('US Equity')

ovr <- c(
   'FUND_PER' = 'Q',
   'BEST_FPERIOD_OVERRIDE' = '-FQ'
)

f <- function(ticker, type, fields){
   
   dat_actual <- ticker %>%
      Rblpapi::bdh(
         fields     = fields[1], 
         overrides  = ovr,
         start.date = '2018-01-01' %>% as.Date()
      ) %>%
      tail(5) %>%
      dplyr::pull()
   
   dat_consensus <- ticker %>%
      Rblpapi::bdp(
         fields    = fields[2], 
         overrides = ovr
      ) %>%
      dplyr::pull()
   
   dat <- data.frame(
      actual    = dat_actual[5],
      consensus = dat_consensus,
      prior     = dat_actual[1]
   ) %>%
      set_rownames(type)
   
   return(dat)
}

last_period <- Rblpapi::bdp(
   ticker, 
   'LATEST_ANNOUNCEMENT_PERIOD', 
   overrides = ovr
) %>% 
   dplyr::pull() %>%
   stringr::str_remove(':')

revenue    <- f(ticker, 'revenue', c('IS_COMP_SALES','BEST_SALES'))
ebitda     <- f(ticker, 'ebitda', c('IS_COMPARABLE_EBITDA', 'BEST_EBITDA'))
net.income <- f(ticker, 'net income', c('IS_COMP_NET_INCOME_ADJUST', 'BEST_NET_INCOME'))
eps        <- f(ticker, 'eps', c('IS_COMP_EPS_ADJUSTED', 'BEST_EPS'))  

dat <- dplyr::rbind_all(revenue, ebitda, net.income, eps)
   
   p <- dat %>%
   tibble::rownames_to_column('metric') %>%
   reshape2::melt(id.vars = 'metric') %>%
   dplyr::mutate(
      metric = metric %>%
         stringr::str_to_title() %>%
         stringr::str_replace_all('Ebitda', 'EBITDA') %>%
         stringr::str_replace_all('Eps', 'EPS'),
      
      variable = variable %>% 
         stringr::str_to_title(),
      
      period = last_period
   ) %>%
   dplyr::mutate(
      metric = metric %>% factor(
         levels = c('Revenue','EBITDA','Net Income','EPS'))
   ) %>%
   ggplot2::ggplot(ggplot2::aes(period, value, color = variable)) +
   ggplot2::geom_point(size = 4) +
   ggplot2::scale_color_manual(
      values = c('black','#850237','gray')
   ) +
   ggplot2::facet_grid(cols   = ggplot2::vars(metric),
                       scales = 'free') +
   ggplot2::coord_flip() +
   ggplot2::labs(title = 'Earnings Analysis') +
   ggplot2::theme(
      axis.title      = ggplot2::element_blank(),
      legend.position = 'bottom',
      legend.title    = ggplot2::element_blank()
   )

ggplot2::ggsave(
   filename = paste0(
      './analysis/charts/',
      last_period %>% stringr::str_to_lower(), 
      '_results_versus_consensus.png'),
   width  = 6.5,
   height = 2,
   units  = 'in'
)