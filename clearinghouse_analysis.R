# Analysis of the National Student Clearinghouse Data Resource Center
# on student enrollment for Fall 2020
# Originally published September 24th 2020
#
# Source: https://nscresearchcenter.org/stay-informed/
# Additional Sources:
#   https://fred.stlouisfed.org/

# LOAD PACKAGES AND SETUP -------------------------------------------------

require(tidyverse)
require(openxlsx)
require(lubridate)
require(scales)

# LOAD ENROLLMENT DATA ----------------------------------------------------

# Data sourced from IPEDS `Most Current Digest Tables`. To run analysis,
# download each table's corresponding excel. Table reference name will
# be commented out next to code.

# Table 303.70.
# Total undergraduate fall enrollment in degree-granting postsecondary institutions,
# by attendance status, sex of student, and control and level of institution:
# Selected years, 1970 through 2029
# https://nces.ed.gov/programs/digest/d19/tables/dt19_303.70.asp

public_2yr_enrol <- readxl::read_excel(
  "~/clearinghouse_analysis_data/table_data/tabn303.70.xls",
  skip = 53,
  n_max = 37
) %>%
  rename(
    year = "2-year insti-\n   tutions\\2\\",
    enrollment = "...11"
  ) %>%
  mutate(
    level = "public 2 year inst",
    pct_change = (enrollment - lag(enrollment)) / lag(enrollment),
    year = parse_date(as.character(year), format = "%Y"),
    source = "IPEDS"
  ) %>%
  select(year, enrollment, pct_change, level, source) %>%
  filter(year >= "1990-01-01")

# Table 322.20.
# Bachelor's degrees conferred by postsecondary institutions, by race/ethnicity and sex of student:
# Selected years, 1976-77 through 2017-18
# https://nces.ed.gov/programs/digest/d19/tables/dt19_322.20.asp?current=yes

public_4yr_enrol <- readxl::read_excel(
  "~/clearinghouse_analysis_data/table_data/tabn303.70.xls",
  skip = 102,
  n_max = 37
) %>%
  rename(
    year = "4-year insti-\n   tutions",
    enrollment = "...11"
  ) %>%
  arrange(as.numeric(year)) %>%
  mutate(
    level = "public 4 year inst",
    enrollment = as.numeric(enrollment),
    pct_change = (enrollment - lag(enrollment)) / lag(enrollment),
    year = parse_date(as.character(year), format = "%Y"),
    source = "IPEDS"
  ) %>%
  select(year, enrollment, pct_change, level, source) %>%
  filter(year >= "1990-01-01")

# Table 322.20.
# Bachelor's degrees conferred by postsecondary institutions, by race/ethnicity and sex of student:
# Selected years, 1976-77 through 2017-18
# https://nces.ed.gov/programs/digest/d19/tables/dt19_322.20.asp?current=yes

private_4yr_enrol <- readxl::read_excel(
  "~/clearinghouse_analysis_data/table_data/tabn303.70.xls",
  skip = 102,
  n_max = 37
) %>%
  rename(
    year = "4-year insti-\n   tutions",
    enrollment = "...13"
  ) %>%
  arrange(as.numeric(year)) %>%
  mutate(
    level = "private 4 year inst",
    enrollment = as.numeric(enrollment),
    pct_change = (enrollment - lag(enrollment)) / lag(enrollment),
    year = parse_date(as.character(year), format = "%Y"),
    source = "IPEDS"
  ) %>%
  select(year, enrollment, pct_change, level, source) %>%
  filter(year >= "1990-01-01")

# Table 323.20.
# Master's degrees conferred by postsecondary institutions, by race/ethnicity and sex of student:
# Selected years, 1976-77 through 2017-18
# https://nces.ed.gov/programs/digest/d19/tables/dt19_323.20.asp?current=yes

grad_dgr_enrol <- readxl::read_excel(
  "~/clearinghouse_analysis_data/table_data/tabn323.20.xls",
  skip = 4,
  n_max = 23
) %>%
  rename(enrollment = "...2") %>%
  mutate(year = parse_date(str_extract(Total, "...."), format = "%Y")) %>%
  filter(!is.na(year)) %>%
  arrange(year) %>%
  mutate(
    level = "graduate degree",
    pct_change = (enrollment - lag(enrollment)) / lag(enrollment),
    source = "IPEDS"
  ) %>%
  select(year, enrollment, pct_change, level, source) %>%
  filter(year >= "2004-01-01")

years <- 2018:2012

dist_enrollment_data <- list(
  "~/clearinghouse_analysis_data/ef2018a_dist.csv",
  "~/clearinghouse_analysis_data/ef2017a_dist.csv",
  "~/clearinghouse_analysis_data/ef2016a_dist.csv",
  "~/clearinghouse_analysis_data/ef2015a_dist.csv",
  "~/clearinghouse_analysis_data/ef2014a_dist.csv",
  "~/clearinghouse_analysis_data/ef2013a_dist.csv",
  "~/clearinghouse_analysis_data/ef2012a_dist.csv"
) %>%
  map(read_csv) %>%
  map2_df(years, ~ mutate(.x, year = .y)) %>%
  clean_names() %>%
  filter(efdelev %in% c(2, 12)) %>%
  group_by(year, efdelev) %>%
  summarize(enrollment = sum(efdetot)) %>%
  group_by(efdelev) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    pct_change = (enrollment - lag(enrollment)) / lag(enrollment),
    year = parse_date(as.character(year), format = "%Y")
  )


# LOAD CLEARINGHOUSE DATA -------------------------------------------------

enrol_data_clearinghouse <- read_excel(
  "~/clearinghouse_analysis_data/clearinghouse_data.xlsx",
  sheet = "level"
) %>%
  mutate(source = "student clearinghouse")
enrol_data_clearinghouse_poi <- read_excel("~/clearinghouse_analysis_data/clearinghouse_data.xlsx", sheet = "poi") %>%
  mutate(
    source = "student clearinghouse"
  )

# LOAD ECON DATA ----------------------------------------------------------

# JHDUSRGDPBR
# Dates of U.S. recessions as inferred by GDP-based recession indicator
# https://fred.stlouisfed.org/series/JHDUSRGDPBR

us_recessions <- read_csv("~/clearinghouse_analysis_data/JHDUSRGDPBR.csv")

# Reformat Recession Dates as Periods

for (date in seq_along(us_recessions$DATE)) {
  if (date > 1) {
    if (!us_recessions$JHDUSRGDPBR[date] == us_recessions$JHDUSRGDPBR[date - 1]) {
      if (us_recessions$JHDUSRGDPBR[date] == 1) {
        recession_start <- us_recessions$DATE[date]
      } else {
        recession_end <- us_recessions$DATE[date]
        recession <- tibble(recession_start = c(recession_start), recession_end = c(recession_end))
        recession_span <- bind_rows(recession_span, recession)
        recession_start <- NA
      }
    }
  }
}

if (!is.na(recession_start)) {
  recession <- tibble(recession_start = c(recession_start), recession_end = c(today()))
  recession_span <- bind_rows(recession_span, recession)
}

recession_span <- recession_span %>%
  mutate(
    recession_start = as_datetime(recession_start),
    recession_end = as_datetime(recession_end)
  ) %>%
  filter(recession_end > "1990-01-01")

# LNS14027662
# Unemployment Rate - Bachelor's Degree and Higher, 25 Yrs. & Over
# https://fred.stlouisfed.org/series/LNS14027662

undergrad_unemployment <- read_csv("~/clearinghouse_analysis_data/LNS14027662.csv") %>%
  rename(undergrad_unemployment_rate = "LNS14027662")

# CGMD25O
# Unemployment Rate - College Graduates - Master's Degree, 25 years and over
# https://fred.stlouisfed.org/series/CGMD25O

grad_unemployment <- read_csv("~/clearinghouse_analysis_data/CGMD25O.csv") %>%
  rename(grad_unemployment_rate = "CGMD25O")

# LNU04027683
# Unemployment Rate - Associate Degree, 25 Yrs. & Over
# https://fred.stlouisfed.org/series/LNU04027683

associate_unemployment <- read_csv("~/clearinghouse_analysis_data/LNU04027683.csv") %>%
  rename(associate_unemployment_rate = "LNU04027683")

# Graphs ------------------------------------------------------------------

# Bind and Format Data

data <- bind_rows(
  grad_dgr_enrol,
  public_2yr_enrol,
  public_4yr_enrol,
  private_4yr_enrol,
  enrol_data_clearinghouse
)

data$level_f <- factor(
  data$level,
  levels = c(
    "public 2 year inst",
    "public 4 year inst",
    "private 4 year inst",
    "graduate degree"
  )
)


# Big Data Graph

big_data_graph <- ggplot() +
  geom_rect(
    data = recession_span,
    mapping =
      aes(
        xmin = recession_start,
        xmax = recession_end,
        ymin = -Inf,
        ymax = +Inf
      ),
    fill = "gray",
    alpha = 0.8
  )

big_data_graph +
  geom_col(
    data = data,
    aes(
      y = pct_change,
      x = year
    )
  ) +
  facet_grid(level_f ~ .,
    scales = "free_y",
  )

# 4-Year Institutions

undergrad_unemployment_formatted <- undergrad_unemployment %>%
  mutate(
    DATE = as_datetime(DATE),
    undergrad_unemployment_rate = undergrad_unemployment_rate / 100
  )

undergrad_graph <- data %>%
  filter(level == "private 4 year inst" |
    level == "public 4 year inst") %>%
  ggplot(aes(x = year, y = pct_change)) +
  geom_col() +
  facet_wrap(. ~ level, ncol = 1)

undergrad_graph + geom_line(
  data = undergrad_unemployment_formatted,
  mapping =
    aes(
      x = DATE,
      y = undergrad_unemployment_rate
    )
)

# 2-year Institutions

associate_unemployment_formatted <- associate_unemployment %>%
  mutate(
    DATE = as_datetime(DATE),
    associate_unemployment_rate = associate_unemployment_rate / 100
  )

graph <- data %>%
  filter(level == "public 2 year inst") %>%
  ggplot(aes(x = year, y = pct_change)) +
  geom_col()

graph + geom_line(
  data = associate_unemployment_formatted, mapping = aes(x = DATE, y = associate_unemployment_rate)
)

# Grad Degrees

grad_unemployment_formatted <- grad_unemployment %>%
  mutate(
    DATE = as_datetime(DATE),
    grad_unemployment_rate = grad_unemployment_rate / 100
  )

grad_graph <- data %>%
  filter(level == "graduate degree") %>%
  ggplot(aes(x = year, y = pct_change)) +
  geom_col()

grad_graph +
  geom_line(
    data = grad_unemployment_formatted,
    mapping =
      aes(
        x = DATE,
        y = grad_unemployment_rate
      )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  )

# Predominately Online Institutions

bind_rows(
  dist_enrollment_data,
  enrol_data_clearinghouse_poi
) %>%
  ggplot(
    mapping = aes(
      y = pct_change,
      x = year
    )
  ) +
  geom_col() +
  facet_grid(efdelev ~ .)
