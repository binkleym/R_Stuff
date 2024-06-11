################################################################################
### Average Investor Equity Allocation - a measure of stock market valuation
###
### This is comparable to, but more accurate than similar valuation metrics like
### CAPE, Tobin's Q, the modified Buffett Ratio, or detrended log(SP500)
################################################################################
library(tidyverse)
library(fredr)
library(rvest)
library(purrr)
library(tsibble)
library(broom)
library(scales)

# Set your FRED API key here.  You may request an API key at:
# https://research.stlouisfed.org/useraccount/apikeys
fredr_set_key("<PUT_YOUR_FRED_API_KEY_HERE>")

# Fetch data series from FRED, normalize units, and rename as needed
fredr_fetch <- function(series_id, frequency, units, scale) {
  val <- 
    fredr(series_id = series_id,  frequency = frequency, units = units) %>%
    mutate(
      series_id = paste(series_id, "_", units, sep = ""),
      series_id = if_else(grepl("_lin$", series_id), gsub("_lin$", "", series_id), series_id),
      series_id = if_else(grepl("_$", series_id), gsub("_$", "", series_id), series_id),
      value = case_when(
        scale == "p" ~ value / 100,
        scale == "k" ~ value * 1000,
        scale == "m" ~ value * 1000000,
        scale == "b" ~ value * 1000000000,
        scale == "t" ~ value * 1000000000000,
        TRUE         ~ value,
      )
    )
  return(val)
}

# Data series we need to compute AIEA
fred_series <-
  tribble(
    ~series_id,    ~frequency,  ~units,  ~scale, ~name,
    
    # Equities
    "NCBEILQ027S", "q",         "lin",   "m",    "Nonfinancial Corporate Business; Corporate Equities; Liability, Level",
    "FBCELLQ027S", "q",         "lin",   "m",    "Domestic Financial Sectors; Corporate Equities; Liability, Level",
    
    # Debt
    "BCNSDODNS",   "q",         "lin",   "b",    "Nonfinancial Corporate Business; Debt Securities and Loans; Liability, Level",
    "CMDEBT",      "q",         "lin",   "b",    "Households and Nonprofit Organizations; Debt Securities and Loans; Liability, Level",
    "FGSDODNS",    "q",         "lin",   "b",    "Federal Government; Debt Securities and Loans; Liability, Level",
    "SLGSDODNS",   "q",         "lin",   "b",    "State and Local Governments; Debt Securities and Loans; Liability, Level",
    "DODFFSWCMI",  "q",         "lin",   "b",    "Rest of the World; Debt Securities and Loans; Liability, Level"
  )

# Fetch raw data from FRED
data_fred <-
  purrr::pmap_dfr(.l = fred_series %>% select(-name), .f = fredr_fetch) %>%
  select(date, series_id, value) %>% 
  pivot_wider(id_cols = "date", names_from = "series_id", values_from = "value") %>%
  arrange(date)

# Compute AIEA
data_aiea <-
  data_fred %>%
  mutate(
    yearmonth = yearmonth(date),
    
    # AIEA is total market value of stocks divided by total market value of stocks + bonds
    stocks = NCBEILQ027S + FBCELLQ027S,

    bonds  = BCNSDODNS + CMDEBT + FGSDODNS + SLGSDODNS + DODFFSWCMI,
    
    AIEA = stocks / (stocks + bonds)
    
    ) %>%
  filter(!is.na(AIEA))


# Compute mean/sd of AIEA so we can draw tick markets on our graph
mean <- data_aiea$AIEA %>% mean()
sd   <- data_aiea$AIEA %>% sd()

# Read Excel spreadsheets from URL's since the "readxl" package can't yet
read_excel_url <- function(url, ...) {
  tf <- tempfile(fileext = ".xls")
  curl::curl_download(url, tf)
  return(readxl::read_excel(tf, ...))
}

# Read Robert Shiller's dataset on stock market returns so we can compare
# AIEA to subsequent 10 year stock market returns

# Column names in Shiller's data
shiller_cols <-
  c("Year.Month",              "SP_Comp",           "Dividend",
    "Earnings",                "CPI",               "Date_Fraction", 
    "Long_Interest_Rate",      "Real_Price",        "Real_Dividend", 
    "Real_Total_Return_Price", "Real_Earnings",     "Real_TR_Scaled_Earnings", 
    "CAPE",                    NA,                  "TR_CAPE", 
    NA,                        "Excess_CAPE_Yield", "Monthly_Total_Bond_Returns",
    "Real_Total_Bond_Returns", "10Yr_Annualized_Stock_Real_Return", 
    "10Yr_Annualized_Bond_Real_Return", "10Yr_Annualized_Excess_Real_Return")


# Shiller's website has changed the URL for the spreadsheet from a static 
# link to a dynamic link.   So scrape the page to find the latest URL.
url <-
  "https://shillerdata.com" %>%
  read_html() %>%
  html_nodes("a") %>% 
  html_attr("href") %>%
  as_tibble() %>%
  filter(grepl("ie_data.xls", value)) %>%
  pull() %>%
  paste("https:", ., sep = "")

spreadsheet_raw <- 
  url %>%
  read_excel_url(., sheet = "Data", col_names = shiller_cols, skip = 8) 

spreadsheet <-
  spreadsheet_raw %>%
  select(Date_Fraction, `10Yr_Annualized_Stock_Real_Return`) %>%
  mutate(Date = as.Date(date_decimal(Date_Fraction))) %>%
  rename(StockReturns_10Yr = `10Yr_Annualized_Stock_Real_Return`) %>%
  select(Date, StockReturns_10Yr) %>%
  filter(Date >= as.Date("1951-10-01")) %>%
  filter(!is.na(StockReturns_10Yr)) %>%
  mutate(yearmonth = yearmonth(Date))

# Let's examine the correlation between AIEA and 10 year stock returns
combo <- 
  data_aiea %>% 
  select(yearmonth, AIEA) %>% 
  left_join(spreadsheet %>% select(yearmonth, StockReturns_10Yr), by = "yearmonth") %>% 
  filter(!is.na(StockReturns_10Yr)) 

# Compute correlation coefficients
fit <- lm(StockReturns_10Yr ~ AIEA, data = combo)
c0 <- fit %>% tidy() %>% filter(term == "(Intercept)") %>% pull(estimate)
c1 <- fit %>% tidy() %>% filter(term == "AIEA")        %>% pull(estimate)

# Draw a graph of AIEA to highlight the expected 10 year stock market return
g_aiea_10yr <- 
  ggplot(data = data_aiea) +
  theme_bw() + 
  geom_line(aes(x = date, y = AIEA)) +

  geom_hline(yintercept = -c0/c1, linetype = "solid", color = "red") +
  geom_hline(yintercept = mean + 2 * sd, linetype = "dotted", color = "darkred") +
  geom_hline(yintercept = mean + 0 * sd, linetype = "dashed", color = "black") +
  geom_hline(yintercept = mean - 2 * sd, linetype = "dotted", color = "darkblue") +
  
  labs(
    x = "", 
    y = "Aggregate Investor Equity Allocation",
    title = "Aggregate Investor Equity Allocation",
    subtitle = "Solid line = Expected 10 yr avg stock market return = 0%\nDashed line = mean AIEA\nDotted lines = mean +/- 2 standard deviations",
    caption = "Data for computing AIEA from FRED [https://fred.stlouisfed.org/]\n10 Year Total Stock Returns from Robert Shiller's website [https://shillerdata.com/]",
  ) +
  scale_x_date(breaks = pretty_breaks(9)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = pretty_breaks(10),
                     limits = c(0.20, 0.55),
                     sec.axis = sec_axis(~ c0 + c1 *., 
                                         name = "S&P 500 Avg Real Return per year over the next 10 years",
                                         labels = scales::percent_format(accuracy = 1), 
                                         breaks = pretty_breaks(10)))

print(g_aiea_10yr)
