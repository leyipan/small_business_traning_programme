# Load required packages
library(readr)
library(tidyverse)
library(skimr)
library(stringr)
library(here)
library(janitor)
library(lubridate)
library(purrr)
library(fixest)
library(kableExtra)
library(modelsummary)

# Set file paths ----------------------------------------------------------

## For input files in data
in_file <- function(filename) {
  here("data", filename)
}

## For processed files in data
proc_file <- function(filename) {
  here("data/proc", filename)
}

## For output files
out_file <- function(filename) {
  here("output", filename)
}


# Read data and print skim summaries -----------------------------------

## Read core data
firms <- read_csv(in_file("firm_information.csv"))
sales <- read_csv(in_file("aggregate_firm_sales.csv"))

## Skim core data
skim(firms)
skim(sales)

## Create a month column in sales
sales <- sales %>%
  mutate(month = floor_date(date, unit = "month"))

## Set path to monthly data
monthly_paths <- list.files(here("data/monthly_data"), pattern = "\\.csv$", full.names = TRUE)

read_monthly <- function(fp) {
  nm <- basename(fp)
  # Extract year and month from filename
  m <- str_match(nm, "(\\d{4})[-_](\\d{1,2})")
  month_from_file <- if (!any(is.na(m[1, 2:3]))) {
    make_date(as.integer(m[1, 2]), as.integer(m[1, 3]), 1)
  } else {
    as.Date(NA)
  }
  
  # Read the columns
  read_csv(
    fp,
    col_types = cols(
      date         = col_date(),
      firm_id      = col_character(),
      employment_t = col_double(),
      wage_bill_t  = col_double(),
      revenue_t    = col_double(),
      adopt_t      = col_double()
    ),
    show_col_types = FALSE
  ) %>%
    clean_names() %>% 
    mutate(
      # infer month from 'date'; if date is NA, fall back to filename month
      month = coalesce(floor_date(date, "month"), month_from_file)
    )
}

## Read monthly data into one data frame
monthly <- map_dfr(monthly_paths, read_monthly)

## Skim monthly data
skim(monthly)

## Print skim summaries for easier reference
dir.create("output/diagnostics", recursive = TRUE, showWarnings = FALSE)
diagno <- file.path("output/diagnostics",
                     paste0("skim_summaries_", Sys.Date(), ".txt"))

options(width = 120)
sink(diagno)
cat("Skim summaries\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n\n", sep = "")

print_section <- function(title, x) {
  bar <- paste(rep("=", nchar(title)), collapse = "")
  cat(bar, "\n", title, "\n", bar, "\n\n", sep = "")
  print(skim(x))
  cat("\n\n")
}

print_section("FIRM INFORMATION", firms)
print_section("AGGREGATE FIRM SALES", sales)
print_section("FIRM AUXILIARY DATA BY MONTH", monthly)

sink()
message("Saved to: ", diagno)


# 1. Data Cleaning --------------------------------------------------------
# 1.1 Extra IDs in Sales and Monthly --------------------------------------------------
# Supposedly firms, sales and monthly will have identical firm_ids.
# But from the skim summary sales and monthly have 988 unique firm_id values, while firms has 498.
# There are also firm ids of length 8 or 9 in sales and monthly, while in firms all ids have length 7.
# This section investigates why there is a difference.

## Focus on sales and firms first
## Take distinct firm ids out of firms and sales for inspection
ids_firms <- firms %>% distinct(firm_id)
ids_sales <- sales %>% distinct(firm_id)

## Single out abnormal firm ids in sales where number of characters is more than 7
abnorm_id <- ids_sales %>% filter(nchar(firm_id) > 7)
nrow(abnorm_id) # check if it equates to the difference of 490

## After inspection, seems like there are extra 0s appended to the extra firm_ids in sales
## Check how many ends in "00" -- if equates to 490 then applies to all extras
nrow(abnorm_id %>% filter(str_ends(firm_id, "00")))

## Trim 2 trailing 0s
trimmed_id <- abnorm_id %>%
  mutate(firm_id = if_else(is.na(firm_id), NA_character_,
                           str_replace(str_trim(firm_id), "00$", "")))
skim(trimmed_id) # ensure that all values are of length 7 now

## Check if trimmed_id is in ids_firms
anti_join(trimmed_id, ids_firms, by = "firm_id") # if 0 then yes


## Now trim the 2 railing 0s in the firm_id column in sales
sales$firm_id <- sub("0{2}$", "", trimws(sales$firm_id))

## Sanity check
length(unique(sales$firm_id))   # should be 498
identical(unique(sales$firm_id), unique(firms$firm_id)) # should be TRUE


## Do the same for monthly
monthly$firm_id <- sub("0{2}$", "", trimws(monthly$firm_id))

## Sanity check
length(unique(monthly$firm_id))  # should be 498
# check set since sequence is different
setequal(unique(monthly$firm_id), unique(firms$firm_id)) # should be TRUE



# 1.2 Resolve missing values -------------------------------------------------------

# In core files:
# Skim summary shows missing values for sales_t.

## Check for na, 0 and negative values (since sales can't go negative)
sales %>%
  summarise(
    n_rows = n(),
    n_na   = sum(is.na(sales_t)), # check for na
    n_zero = sum(sales_t == 0, na.rm = TRUE), # check for 0
    n_neg  = sum(sales_t < 0,  na.rm = TRUE) # check for negative
  )

## Check for gaps or odd months
range(sales$month)

## When do NAs occur?
na_by_month <- sales %>%
  group_by(month) %>%
  summarise(pct_na = 100 * mean(is.na(sales_t)), .groups = "drop") %>%
  arrange(month)

head(na_by_month); tail(na_by_month)

## Who has NAs?
na_by_firm <- sales %>%
  group_by(firm_id) %>%
  summarise(pct_na = 100 * mean(is.na(sales_t)), .groups = "drop") %>%
  arrange(desc(pct_na))

head(na_by_firm); tail(na_by_firm)

# Are NAs interior gaps or just at the edges? (streak type)

na_runs <- sales %>%
  arrange(firm_id, month) %>%                    
  group_by(firm_id) %>%                           
  mutate(
    is_na  = is.na(sales_t),                      # flag NA rows
    # create a run ID: increments whenever the NA/non-NA status flips.
    # Example per firm: F F F T T F  -> run_id: 0 0 0 1 1 2 (any monotone increasing is fine)
    run_id = cumsum(is_na != dplyr::lag(is_na, default = first(is_na))),
    first_m = first(month),                       # first observed month for this firm
    last_m  = last(month)                         # last observed month for this firm
  ) %>%
  group_by(firm_id, run_id) %>%                   # now each consecutive run is one group
  summarise(
    is_na  = first(is_na),                        # this run is NA (TRUE) or not (FALSE)
    start  = first(month),                        # run start month
    end    = last(month),                         # run end month
    len    = n(),                                 # run length in months
    first_m = first(first_m),                     # carry firm’s first month
    last_m  = first(last_m),                      # carry firm’s last month
    .groups = "drop"
  ) %>%
  filter(is_na) %>%                               # keep only the NA runs
  group_by(firm_id) %>%                           # summarise per firm
  summarise(
    longest_na_run = max(len),                    # longest consecutive NA stretch for the firm
    # interior NA? (i.e., an NA run that is strictly between the first and last months)
    has_interior_na = any(start > first_m & end < last_m),
    .groups = "drop"
  ) %>%
  arrange(desc(longest_na_run))                   # list firms with the longest NA streaks first, might be too sparse for baseline

head(na_runs); tail(na_runs)

# Are NAs concentrated pre/post 2013-01?
na_prepost <- sales %>%
  mutate(period = if_else(month < as.Date("2013-01-01"),
                          "pre2013", "post2013")) %>%   # tag each row as pre or post 2013-01
  group_by(period) %>%                                  # split data into the two periods
  summarise(pct_na = 100 * mean(is.na(sales_t)),
            .groups = "drop") %>%                       # within each period, percent of rows with NA sales_t
  arrange(period)                                       # print in period order

na_prepost


# In monthly:
# skim(monthly) shows missing values in employment_t, wage_bill_t, and revenue_t
# leave them as NA in baseline, check later



# 1.3 Check (firm_id, month) uniqueness ------------------------------------
# Since sales should give firm sales by month, there should be unique firm-month values.
# Standardise date to month to check for inconsistencies.

## Check that (firm_id, month) is unique in sales
nrow(sales %>% count(firm_id, month) %>% filter(n > 1)) # should be 0 if unique

## Check that (firm_id, month) is unique in monthly
nrow(monthly %>% count(firm_id, month) %>% filter(n>1)) # should be 0 if unique

## Check out (firm_id, month) duplicates in monthly
dup_keys <- monthly %>%
  count(firm_id, month, name = "n") %>%
  filter(n > 1)


# 1.4 Resolve monthly duplicates -------------------------------------------

# --- 1) Inspect whether duplicates are identical or conflicting ---
conflicts <- monthly %>%
  semi_join(dup_keys, by = c("firm_id","month")) %>%         # keep only the duplicated keys
  group_by(firm_id, month) %>%
  summarise(
    n_rows = n(),
    # how many distinct values (ignoring NA) per variable inside the key
    emp_n  = n_distinct(employment_t, na.rm = TRUE),
    wb_n   = n_distinct(wage_bill_t,  na.rm = TRUE),
    rev_n  = n_distinct(revenue_t,    na.rm = TRUE),
    adp_n  = n_distinct(adopt_t,      na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # keep only keys where any variable truly disagrees
  filter(emp_n > 1 | wb_n > 1 | rev_n > 1 | adp_n > 1)

nrow(conflicts)        # how many of those truly disagree (the rest are identical rows)
skim(conflicts)

# helper: last non-NA
last_non_na <- function(x) {
  x2 <- x[!is.na(x)]               # keep only non-missing values
  if (length(x2)) x2[length(x2)]   # if any remain, return the last one
  else NA_real_
}

# --- 2) Build a resolved monthly table with one row per (firm_id, month) ---
monthly_resolved <- monthly %>%
  arrange(firm_id, month, date) %>%                        # order inside month
  group_by(firm_id, month) %>%
  summarise(
    employment_t = last_non_na(employment_t),              # stock at period end
    wage_bill_t  = max(wage_bill_t),                       # take the max (bill can only increase)
    revenue_t    = max(revenue_t),         
    adopt_t      = max(adopt_t, na.rm = TRUE),             # status: once 1 then 1
    .groups = "drop"
  ) %>%
  # max() over all-NA returns -Inf; revert to NA in that rare case
  mutate(adopt_t = ifelse(is.infinite(adopt_t), NA_real_, adopt_t))

# --- 3) For the subset that had no conflicts, we could have just deduped; 
# From nrow(conflicts) above we see that there are no identical duplicates,
# thus we do not need to consider this subset.

# --- 4) Sanity checks: uniqueness and valid values ---
nrow(monthly_resolved %>% count(firm_id, month) %>% filter(n > 1)) == 0  # unique keys now
all(monthly_resolved$adopt_t %in% c(0, 1, NA))                           # adopt_t is 0/1/NA



# 1.5 Adoption validity checks ---------------------------------------------

# 1) Enforce monotonicity within firm (once 1, stays 1)
monthly_resolved <- monthly_resolved %>%
  arrange(firm_id, month) %>%
  group_by(firm_id) %>%
  mutate(adopt_cum = cummax(adopt_t)) %>%   # 0/1 running max; NAs treated as 0 for cummax
  ungroup()

# 2) First adoption month per firm (NA if never adopts)
first_adopt <- monthly_resolved %>%
  group_by(firm_id) %>%
  summarise(first_adopt = if (any(adopt_cum == 1)) min(month[adopt_cum == 1], na.rm = TRUE) else as.Date(NA),
            .groups = "drop")

# 3) Merge back and build event-time variables
monthly_resolved <- monthly_resolved %>%
  left_join(first_adopt, by = "firm_id") %>%
  mutate(
    ever_adopt = !is.na(first_adopt),
    rel_time   = as.integer((month - first_adopt) / 30),   # ~months relative to adoption; 0 at adoption month
    post       = as.integer(month >= first_adopt)          # 1 if on/after adoption (NA for never-adopters)
  )

# any reversals left?
monthly_resolved %>%
  arrange(firm_id, month) %>%
  group_by(firm_id) %>%
  mutate(lag_adopt = lag(adopt_t)) %>%
  filter(lag_adopt == 1 & adopt_t == 0) %>%
  nrow()   # should be 0



# 1.6 Build a clean analysis panel -----------------------------------------

# 1) Keep only what we need from each table
sales_keep   <- sales %>% select(firm_id, month, sales_t)
monthly_keep <- monthly_resolved %>%
  select(firm_id, month, employment_t, wage_bill_t, revenue_t, adopt_t, ever_adopt, first_adopt, rel_time, post)
firms_keep   <- firms %>% select(firm_id, firm_name, firm_sector)

# 2) Join: sales (outcome) ⟂ monthly (treatment/controls) ⟂ firms (labels)
analysis_df <- sales_keep %>%
  left_join(monthly_keep, by = c("firm_id","month")) %>%   # keep the outcome universe
  left_join(firms_keep,   by = "firm_id")

# Checks
## Post-join row count should equal sales rows (one per firm-month)
nrow(analysis_df) == nrow(sales_keep)
skim(analysis_df)

# Since sales_t has a higher complete_rate than revenue_t,
# we'll use sales_t as baseline outcome measure.

# 3) Keep only firm–months with sales observed (outcome available)
analysis_df <- analysis_df %>%
  filter(!is.na(sales_t))

# 4) Treatment timing overview
## Count treated vs never-treated firms
treat_overview <- analysis_df %>%
  group_by(firm_id) %>%
  summarise(ever_adopt = any(adopt_t == 1, na.rm = TRUE),
            first_adopt = suppressWarnings(min(first_adopt, na.rm = TRUE)),
            .groups="drop")

treat_overview %>%
  summarise(
    n_firms = n(),
    n_treated = sum(ever_adopt),
    n_never   = sum(!ever_adopt),
    share_treated = mean(ever_adopt)
  )

## Adoption calendar (how adoption spreads over time)
adopt_calendar <- treat_overview %>%
  filter(ever_adopt) %>%
  count(first_adopt, name = "n_firms") %>%
  arrange(first_adopt)

adopt_calendar



# ============================================================================
# DATA VISUALIZATION FOR PAPER
# ============================================================================

# Create figures directory
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# --------------------------------------------------------------------------
# Figure 1: Adoption Timing
# --------------------------------------------------------------------------

adoption_counts <- analysis_df %>%
  filter(ever_adopt, !is.na(first_adopt)) %>%
  distinct(firm_id, first_adopt) %>%
  count(first_adopt) %>%
  arrange(first_adopt)

# Create date labels separately to avoid namespace issues
adoption_counts$date_label <- format.Date(adoption_counts$first_adopt, "%b %Y")

png("output/figures/adoption_timing.png", width = 800, height = 500, res = 120)
par(mar = c(5, 5, 3, 2))
barplot(
  height = adoption_counts$n,
  names.arg = adoption_counts$date_label,
  las = 2,
  col = "#4575b4",
  border = NA,
  main = "Distribution of Adoption Dates",
  ylab = "Number of Firms",
  xlab = "",
  cex.lab = 1.2,
  cex.axis = 1,
  cex.main = 1.3
)
# Add value labels on top of bars
text(
  x = seq_along(adoption_counts$n) * 1.2 - 0.5,
  y = adoption_counts$n + 5,
  labels = adoption_counts$n,
  cex = 1.1,
  font = 2
)
dev.off()

cat("Created: output/figures/adoption_timing.png\n")

# --------------------------------------------------------------------------
# Figure 2: Missingness Pattern Over Time
# --------------------------------------------------------------------------

missingness_data <- sales %>%
  group_by(month) %>%
  summarise(
    pct_missing = 100 * mean(is.na(sales_t)),
    .groups = "drop"
  )

# Add vertical line date for program start
program_start <- as.Date("2013-01-01")

png("output/figures/missingness_pattern.png", width = 900, height = 500, res = 120)
par(mar = c(5, 5, 3, 2))
plot(
  missingness_data$month,
  missingness_data$pct_missing,
  type = "l",
  lwd = 2,
  col = "#d73027",
  main = "Missing Sales Data Over Time",
  xlab = "Month",
  ylab = "Percent Missing (%)",
  ylim = c(0, max(missingness_data$pct_missing) * 1.2),
  cex.lab = 1.2,
  cex.axis = 1,
  cex.main = 1.3
)
# Add horizontal line at mean
abline(h = mean(missingness_data$pct_missing), lty = 2, col = "gray50", lwd = 1.5)
# Add vertical line at program start
abline(v = program_start, lty = 2, col = "#4575b4", lwd = 2)
# Add legend
legend(
  "topright",
  legend = c("Missing %", "Mean (3.7%)", "Program Start"),
  col = c("#d73027", "gray50", "#4575b4"),
  lty = c(1, 2, 2),
  lwd = c(2, 1.5, 2),
  bty = "n",
  cex = 1
)
dev.off()

cat("Created: output/figures/missingness_pattern.png\n")

# --------------------------------------------------------------------------
# Figure 3: Raw Sales Trends (Treated vs Control)
# --------------------------------------------------------------------------

# Calculate mean sales by month and treatment group
trends_data <- analysis_df %>%
  group_by(month, ever_adopt) %>%
  summarise(
    mean_sales = mean(sales_t, na.rm = TRUE),
    se_sales = sd(sales_t, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    group_label = ifelse(ever_adopt, "Ever Treated", "Never Treated")
  )

png("output/figures/raw_trends.png", width = 900, height = 600, res = 120)
par(mar = c(5, 5, 3, 2))

# Plot never-treated first (solid, gray)
never_data <- trends_data %>% filter(!ever_adopt)
plot(
  never_data$month,
  never_data$mean_sales,
  type = "l",
  lwd = 2.5,
  lty = 1,  # Changed from 2 to 1 (solid)
  col = "#999999",
  main = "Raw Sales Trends: Treated vs. Control Firms",
  xlab = "Month",
  ylab = "Mean Sales",
  ylim = range(trends_data$mean_sales, na.rm = TRUE),
  cex.lab = 1.2,
  cex.axis = 1,
  cex.main = 1.3
)

# Add ever-treated (solid, blue)
ever_data <- trends_data %>% filter(ever_adopt)
lines(
  ever_data$month,
  ever_data$mean_sales,
  lwd = 2.5,
  col = "#4575b4"
)

# Add vertical line at program start
abline(v = program_start, lty = 2, col = "black", lwd = 1.5)  # Changed to dashed

# Add legend
legend(
  "topleft",
  legend = c("Ever Treated", "Never Treated", "Program Start (Jan 2013)"),
  col = c("#4575b4", "#999999", "black"),
  lty = c(1, 1, 2),  # Changed never-treated to 1 (solid), program line to 2 (dashed)
  lwd = c(2.5, 2.5, 1.5),
  bty = "n",
  cex = 1.1
)

dev.off()

cat("Created: output/figures/raw_trends.png\n")

# --------------------------------------------------------------------------
# Additional Data Checks for Text
# --------------------------------------------------------------------------

# Sector distribution
sector_dist <- analysis_df %>%
  distinct(firm_id, firm_sector, ever_adopt) %>%
  group_by(firm_sector, ever_adopt) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = ever_adopt, values_from = n, values_fill = 0) %>%
  mutate(
    total = `FALSE` + `TRUE`,
    pct_treated = 100 * `TRUE` / total
  )

cat("\n=== Sector Distribution ===\n")
print(sector_dist)

# Employment eligibility check
employment_check <- analysis_df %>%
  filter(ever_adopt & month < first_adopt) %>%
  group_by(firm_id) %>%
  summarise(
    mean_emp_pre = mean(employment_t, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  summarise(
    n_firms = n(),
    mean_employment = mean(mean_emp_pre, na.rm = TRUE),
    sd_employment = sd(mean_emp_pre, na.rm = TRUE),
    n_above_100 = sum(mean_emp_pre > 100, na.rm = TRUE),
    pct_above_100 = 100 * n_above_100 / n_firms
  )

cat("\n=== Pre-Treatment Employment (Treated Firms) ===\n")
print(employment_check)

# Never-treated employment
never_employment <- analysis_df %>%
  filter(!ever_adopt & month < as.Date("2013-01-01")) %>%
  group_by(firm_id) %>%
  summarise(
    mean_emp_pre = mean(employment_t, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  summarise(
    n_firms = n(),
    mean_employment = mean(mean_emp_pre, na.rm = TRUE),
    sd_employment = sd(mean_emp_pre, na.rm = TRUE)
  )

cat("\n=== Pre-2013 Employment (Never-Treated Firms) ===\n")
print(never_employment)

# Save statistics for reference
write.csv(sector_dist, "output/tables/sector_distribution.csv", row.names = FALSE)
write.csv(employment_check, "output/tables/employment_eligibility.csv", row.names = FALSE)

cat("\n=== All data visualization complete ===\n")

# Data output -------------------------------------------------------------

## Prepare panel
panel <- analysis_df %>%
  mutate(
    ym = as.integer((year(month) - 2010) * 12 + month(month)),
    first_treat = ifelse(is.na(first_adopt), Inf,  # Use Inf for never-treated
                         as.integer((year(first_adopt) - 2010) * 12 + month(first_adopt)))
  )

## Write panel to proc_file
write.csv(panel, proc_file("panel.csv"), row.names = FALSE)

## Write analysis_df to proc_file
write.csv(analysis_df, proc_file("analysis_df.csv"), row.names = FALSE)

# Define pre-period
first_any_adopt <- min(panel$ym[panel$adopt_t == 1], na.rm = TRUE)

# Create groups
pre_only <- panel %>%
  filter(ym < first_any_adopt) %>%
  mutate(group = ifelse(first_treat == Inf, "Never-treated (pre)", "Ever-treated (pre)"))

overall <- panel %>%
  mutate(group = "Overall")

# Combine data
t1_data <- bind_rows(
  overall %>% select(group, sales_t, employment_t, wage_bill_t, revenue_t),
  pre_only %>% select(group, sales_t, employment_t, wage_bill_t, revenue_t)
)

# Calculate statistics
stats_df <- t1_data %>%
  group_by(group) %>%
  summarise(
    sales_mean = mean(sales_t, na.rm = TRUE),
    sales_sd = sd(sales_t, na.rm = TRUE),
    emp_mean = mean(employment_t, na.rm = TRUE),
    emp_sd = sd(employment_t, na.rm = TRUE),
    wage_mean = mean(wage_bill_t, na.rm = TRUE),
    wage_sd = sd(wage_bill_t, na.rm = TRUE),
    rev_mean = mean(revenue_t, na.rm = TRUE),
    rev_sd = sd(revenue_t, na.rm = TRUE),
    n_obs = n()
  ) %>%
  mutate(across(where(is.numeric), ~round(., 1)))

# Transpose for cleaner table
stats_transposed <- data.frame(
  Variable = c("Sales", "", "Employment", "", "Wage Bill", "", "Revenue", "", "Observations"),
  Statistic = c("Mean", "SD", "Mean", "SD", "Mean", "SD", "Mean", "SD", "N")
)

# Add columns for each group
for (grp in unique(t1_data$group)) {
  grp_data <- stats_df %>% filter(group == grp)
  stats_transposed[[grp]] <- c(
    grp_data$sales_mean,
    grp_data$sales_sd,
    grp_data$emp_mean,
    grp_data$emp_sd,
    grp_data$wage_mean,
    grp_data$wage_sd,
    grp_data$rev_mean,
    grp_data$rev_sd,
    grp_data$n_obs
  )
}

# Create LaTeX table
kbl(stats_transposed,
    format = "latex",
    booktabs = TRUE,
    caption = "Descriptive Statistics",
    label = "tab:descriptives",
    col.names = c("Variable", "Statistic", names(stats_transposed)[3:ncol(stats_transposed)])) %>%
  pack_rows("Sales", 1, 2) %>%
  pack_rows("Employment", 3, 4) %>%
  pack_rows("Wage Bill", 5, 6) %>%
  pack_rows("Revenue", 7, 8) %>%
  footnote(general = "Standard deviations shown below means.",
           threeparttable = TRUE) %>%
  save_kable("output/tables/table1_descriptives.tex")

cat("Table saved to: output/tables/table1_descriptives.tex\n")
