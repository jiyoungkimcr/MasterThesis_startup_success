# Load the datasets
library(readr)
library(dplyr)
library(tidyverse)
library(dplyr)
library(gtsummary)
library(naniar)
library(scales)
library(mice)
library(ggmice)
library(dlookr)
library(gridExtra)
library(DataExplorer)
library(ggplot2)
library(plotly)
library(tidyr)
library(maditr)
library(lubridate)
library(forcats)
setwd("~/Master thesis")

# Read csv
acquisitions <- read_csv("~/Desktop/Thesis/Thesis code/raw_data/acquisitions.csv")
#category_groups <- read_csv("~/Desktop/Thesis/Thesis code/raw_data/category_groups.csv")
funding_rounds <- read_csv("~/Desktop/Thesis/Thesis code/raw_data/funding_rounds.csv")
#funds <- read_csv("~/Desktop/Thesis/Thesis code/raw_data/funds.csv")
#investment_partners <- read_csv("~/Desktop/Thesis/Thesis code/raw_data/investment_partners.csv")
investments <- read_csv("~/Desktop/Thesis/Thesis code/raw_data/investments.csv")
investors <- read_csv("~/Desktop/Thesis/Thesis code/raw_data/investors.csv")
ipos <- read_csv("~/Desktop/Thesis/Thesis code/raw_data/ipos.csv")
#jobs <- read_csv("~/Desktop/Thesis/Thesis code/raw_data/jobs.csv")
#org_parents <- read_csv("~/Desktop/Thesis/Thesis code/raw_data/org_parents.csv")
#organization_descriptions <- read_csv("~/Desktop/Thesis/Thesis code/raw_data/organization_descriptions.csv")
organizations <- read_csv("~/Desktop/Thesis/Thesis code/raw_data/organizations.csv")
#people_descriptions <- read_csv("~/Desktop/Thesis/Thesis code/raw_data/people_descriptions.csv")
#people <- read_csv("~/Desktop/Thesis/Thesis code/raw_data/people.csv")
#degrees <- read_csv("~/Desktop/Thesis/Thesis code/raw_data/degrees.csv")
#checksum <- read_csv("~/Desktop/Thesis/Thesis code/raw_data/checksum.csv")
#events <- read_csv("~/Desktop/Thesis/Thesis code/raw_data/events.csv")
#event_appearances <- read_csv("~/Desktop/Thesis/Thesis code/raw_data/event_appearances.csv")



# Data preparation
undesired <- c('cb_url', 'created_at', 'legal_name', 'domain', 'address', 'postal_code',
               'short_description', 'email', 'phone', 'facebook_url', 'linkedin_url', 
               'twitter_url', 'logo_url', 'alias1', 'alias2', 'alias3', 'total_funding', 'total_funding_currency_code',
               'region', 'city')

organizations_cleaned <- organizations %>%
  select(-one_of(undesired))

# write.csv(organizations_cleaned2,"~/Desktop/Thesis/Thesis code/cleaned_data/organizations_cleaned_NAfunding.csv", row.names = FALSE)
organizations_cleaned <- read_csv("/Users/jiyoung/Desktop/Thesis/Thesis code/cleaned_data/organizations_cleaned2.csv")

table(organizations$total_funding_currency_code)
# to check status summary of organizations
table(organizations_cleaned$status)
# to check the roles of each organizations
table(organizations_cleaned$roles)

# to check primary_role
organizations_company <- organizations_cleaned %>%
  filter(primary_role == "company")

organizations_investor <- organizations_cleaned %>%
  filter(primary_role == "investor")

organizations_school <- organizations_cleaned %>%
  filter(primary_role == "school")

######################################
## 1. Outlier check & Data Cleaning ##
######################################
# EDA 
# General check on dataset
introduce(organizations_cleaned)

# check any duplicates
sum(duplicated(organizations_cleaned)) # no duplicates

# 1-1. Missing data 
# Missing Values check in overall (BEFORE pre-processing/dealing with it)
plot_missing(organizations_cleaned)
# Which variables are affected?
organizations_cleaned %>% is.na() %>% colSums()
# Get number of missings per variable (n and %)
miss_var_summary(organizations_cleaned) %>% print(n=Inf)

# Which variables contain the most missing variables?
gg_miss_var(organizations_cleaned)
gg_miss_var(organizations_cleaned, show_pct = TRUE)

# Where are missings located?
vis_miss(organizations_cleaned, warn_large_data = FALSE) + theme(axis.text.x = element_text(angle=80))

# Which combinations of variables occur to be missing together? (data pattern plot)
gg_miss_upset(organizations_cleaned)

# 1-2. Missing data mechanism check

# Since Little's MCAR test is only taking the continuous variables, check if discrete numeric variables' levels before applying the test.
# If discrete variables have a large number of levels, it’s advisable to treat them like continuous variables.
table(organizations_cleaned$num_exits)
table(organizations_cleaned$num_funding_rounds)
# Above code shows these discrete numerical variables have too many levels. 
# So we will regard them as continuous variables and include in the test.
# one of my variables is much bigger than the others. (In my case, the trading volume was much bigger than the returns in two different moments.) 
# The problem is because of limitations in floating-point computations and precision, not anything inherently mathematical or statistical.
# Therefore I've divided the biggest variable by 1000000.

organizations_continuous <- organizations_cleaned %>% 
  select(where(is.numeric)) %>%
  mutate(total_funding_usd = total_funding_usd / 1000000) # change the metrics into million USD

#The p-value for the test is siginificant, indicating that the missings does not seem to be completely at random.
#Since its p-value<0.05, given high statistic value and low p-value, our data is not missing completely at random
mcar_test(data=organizations_continuous) 

organizations_missing_top <- organizations_cleaned %>%
  select(closed_on, num_exits, total_funding_usd,
         last_funding_on, num_funding_rounds, founded_on, country_code)

md.pattern(organizations_missing_top, plot = TRUE, rotate.names = TRUE)

plot_pattern(organizations_missing_top)

organizations_missing_funding <- organizations_cleaned %>%
  select(total_funding_currency_code, total_funding_usd,total_funding)

# Since there was value '0'
organizations_missing_funding[organizations_missing_funding == 0] <- NA

plot_pattern(organizations_missing_funding)

# 1-3. Missing data & general cleaning

# there are 29 cases which have earlier closed_on date than founded_on, so I also filtered them out too
organizations_to_remove <- organizations_cleaned %>%
  dplyr::filter((founded_on > closed_on & !is.na(closed_on)))

# there are 10898 cases which is NA in founded_on date
founded_on_na <- organizations_cleaned %>%
  drop_na(total_funding_usd) %>%
  dplyr::filter(is.na(founded_on))

# Final way to filter dataset (clean dataset) after checking Missing values as above
# Also filter out the ones which don't have category_list/category_groups_list 
# If roles is NA but still have primary_roles then input primary_role value in roles column
organizations_filtered <- organizations_cleaned %>%
  drop_na(category_list) %>%
  drop_na(category_groups_list) %>%
  # remove NA rows in total_funding_usd since those obs lack of valuable info(funding amount) for the research
  drop_na(total_funding_usd) %>%
  # company's founded earlier than 1990s and NA date is filtered
  dplyr::filter(!(total_funding_usd == 0 | founded_on < '1990-01-01')) %>% 
  dplyr::filter(!(founded_on > closed_on & !is.na(closed_on))) %>%
  dplyr::mutate(roles = ifelse(is.na(roles), primary_role, roles))


# Missing Values check (AFTER)
plot_missing(organizations_filtered)

#########################
## 2. Outlier Analysis ##
#########################
organizations_continuous <- organizations_filtered %>% 
  select(where(is.numeric)) %>% 
  mutate(total_funding_usd_log = log10(total_funding_usd))

# Histogram of Continuous Variables
plot_histogram(organizations_continuous) 
# Density plot of Continuous Variables
plot_density(organizations_continuous)

plot_histo_funding <- 
  ggplot(data.frame(organizations_continuous), aes(organizations_continuous$total_funding_usd)) + #Histogram with log10 axis
  geom_histogram(bins = 100) +
  scale_x_log10()

ggplotly(plot_histo_funding)

# Using Interquartile Rule to find outliers (BOX PLOT)
# https://www.thoughtco.com/what-is-the-interquartile-range-rule-3126244
# https://www.statisticshowto.com/upper-and-lower-fences/ 
# Logic when to drop and not drop : https://medium.com/@agarwal.vishal819/outlier-detection-with-boxplots-1b6757fafa21

plot_ly(
  data = organizations_continuous,
  y = ~total_funding_usd,
  type = "box"
)

# Since the data has huge range of numbers, it's hard to read in box plot with original values.
# So, we applied logarithms on our funding amount values (convert data  to log scale)
# reference to justify what to apply first(log): https://www.stata.com/support/faqs/graphics/box-plots-and-logarithmic-scales/
plot_ly(
  data = organizations_continuous,
  y = ~total_funding_usd_log,
  type = "box"
)

# upper fence: 9.162371 (total_funding_usd = 1453352805)
# lower fence: 3.489255 (total_funding_usd = 3085)

# Remove Outliers from total_funding_usd
org_after_outlier <- organizations_filtered %>%
  filter(!(total_funding_usd <= 3085 | total_funding_usd >= 1453352805))

# Checking num_funding_rounds outliers, but decided not to remove
# (due to reason that those obs with many funding_rounds s also valuable obs for analysis, we need them)
# (and it's possible one company can have so many rounds of funding)

# plot_ly(
#   data = organizations_continuous,
#   y = ~num_funding_rounds,
#   type = "box"
# )

plot_ly(
  data = org_after_outlier,
  y = ~num_funding_rounds,
  type = "box"
)

# Correlation check plot
plot_correlation(org_after_outlier, type = 'continuous','Review.Date')

plot_missing(org_after_outlier)

# write.csv(org_after_outlier,"~/Master thesis/data_cleaned/org_after_outlier.csv", row.names = FALSE)


##################################
## 3. Creation of New Variables ##
##################################

# 3-1. From funding_rounds table
# In the later section, there is one more pre-processing for 
# additionally adding announce_date for each funding_rounds
funding_rounds2 <- funding_rounds %>%
  select(org_uuid, org_name, investment_type, announced_on, raised_amount_usd, investor_count) %>%
  group_by(org_uuid) %>% #org_uuid
  mutate(first_funding_on = min(announced_on),
         avg_investors_per_round = round(mean(investor_count, na.rm=TRUE))) %>%
  ungroup() %>%
  # company's first funding date earlier than 1990s and 0 raised_amount_usd is filtered 
  # (however, we kept NA raised_amount_usd cause that means it's just unknown)
  dplyr::filter(!(raised_amount_usd == 0 | first_funding_on < '1990-01-01')) %>%
  select(-announced_on, -investor_count)

# sum all the funding amounts per each round 
# (if company has 3 series C, the total raised_amount_usd is sum of those 3)
funding_rounds3 <- funding_rounds2 %>%
  group_by(org_uuid, org_name, investment_type) %>%
  mutate(raised_amount_usd = sum(raised_amount_usd)) %>%
  distinct() %>%
  as.data.frame()

# Transpose the data(rows to columns) to create raised_amount columns for each funding rounds
funding_col_to_add <- funding_rounds3 %>%
  pivot_wider(id_cols = c(org_uuid, org_name, first_funding_on, avg_investors_per_round), 
              names_from = investment_type,
              values_from = raised_amount_usd)


# Check duplicates
# there are 3062 companies who have same name (duplicate)
# but anyway we will keep them bcz they have different & unique original uuid
# duplicated <- org_funding_columns2[duplicated(org_funding_columns2$org_name),]

# Check Sparsity(missing ratio) of the data table to decide which columns to combine
plot_missing(funding_col_to_add)


# Combining some funding rounds to decrease missing cases (remove columns with too much missing values)
col_to_sum <- colnames(funding_col_to_add[,c(13, 15, 22, 23, 29, 32)])

funding_col_to_add <- funding_col_to_add %>%
  mutate(series_e_j = select(., col_to_sum) %>% rowSums(na.rm = TRUE)) %>%
  mutate(series_e_j = case_when(series_e_j == 0 ~ NA_real_, 
                                TRUE ~ series_e_j)) %>%
  select(-col_to_sum)

# Add dummy columns with information of company have each funding round or not
funding_col_to_add$has_angel <- ifelse(!is.na(funding_col_to_add$angel), 1, 0)
funding_col_to_add$has_pre_seed <- ifelse(!is.na(funding_col_to_add$pre_seed), 1, 0)
funding_col_to_add$has_seed <- ifelse(!is.na(funding_col_to_add$seed), 1, 0)
funding_col_to_add$has_seriesUnknown <- ifelse(!is.na(funding_col_to_add$series_unknown), 1, 0)
funding_col_to_add$has_seriesA <- ifelse(!is.na(funding_col_to_add$series_a), 1, 0)
funding_col_to_add$has_seriesB <- ifelse(!is.na(funding_col_to_add$series_b), 1, 0)
funding_col_to_add$has_seriesC <- ifelse(!is.na(funding_col_to_add$series_c), 1, 0)
funding_col_to_add$has_seriesD <- ifelse(!is.na(funding_col_to_add$series_d), 1, 0)
funding_col_to_add$has_seriesE_J <- ifelse(!is.na(funding_col_to_add$series_e_j), 1, 0)
funding_col_to_add$has_equity_crowdfunding <- ifelse(!is.na(funding_col_to_add$equity_crowdfunding ), 1, 0)
funding_col_to_add$has_product_crowdfunding <- ifelse(!is.na(funding_col_to_add$product_crowdfunding ), 1, 0)
funding_col_to_add$has_private_equity <- ifelse(!is.na(funding_col_to_add$private_equity ), 1, 0)
funding_col_to_add$has_convertible_note <- ifelse(!is.na(funding_col_to_add$convertible_note ), 1, 0)
funding_col_to_add$has_debt_financing <- ifelse(!is.na(funding_col_to_add$debt_financing), 1, 0)
funding_col_to_add$has_secondary_market <- ifelse(!is.na(funding_col_to_add$secondary_market), 1, 0)
funding_col_to_add$has_grant <- ifelse(!is.na(funding_col_to_add$grant), 1, 0)
funding_col_to_add$has_corporate_round <- ifelse(!is.na(funding_col_to_add$corporate_round), 1, 0)
funding_col_to_add$has_initial_coin_offering <- ifelse(!is.na(funding_col_to_add$initial_coin_offering), 1, 0)
funding_col_to_add$has_post_ipo_equity <- ifelse(!is.na(funding_col_to_add$post_ipo_equity), 1, 0)
funding_col_to_add$has_post_ipo_debt <- ifelse(!is.na(funding_col_to_add$post_ipo_debt), 1, 0)
funding_col_to_add$has_post_ipo_secondary <- ifelse(!is.na(funding_col_to_add$post_ipo_secondary), 1, 0)
funding_col_to_add$has_non_equity_assistance <- ifelse(!is.na(funding_col_to_add$non_equity_assistance), 1, 0)
funding_col_to_add$has_undisclosed <- ifelse(!is.na(funding_col_to_add$undisclosed), 1, 0)

# change column names to make it easy to distinguish
col_to_change <- colnames(funding_col_to_add)[5:27]
colnames(funding_col_to_add)[5:27] <- paste0(col_to_change, "_raised_amount")

#write.csv(funding_col_to_add,"~/Master thesis/data_cleaned/funding_col_to_add.csv", row.names = FALSE)

#Another approach to remove every columns about raised_funding_amount due to too much missing values
col_to_remove <- colnames(funding_col_to_add)[5:27]

funding_col_final <- funding_col_to_add %>%
  select(-col_to_remove, -org_name)


#write.csv(funding_col_final,"~/Master thesis/data_cleaned/funding_col_final.csv", row.names = FALSE)

# Join created columns (from funding_rounds table) to our main table (the cleaned organizations table = org_after_outlier)
joined_org_funding <- left_join(org_after_outlier, funding_col_final, 
                           by = c("uuid" = "org_uuid"))


### 3-2. From acquisitions table ###
acquisitions_selected <- acquisitions %>%
  select(acquiree_uuid, acquirer_country_code, acquirer_city, acquisition_type, acquired_on, price_usd)

# Some companies went through multiple acquisitions 
#n_occur <- data.frame(table(acquisitions_selected$acquiree_uuid))
#sum(n_occur$Freq > 1) # there are 7719 companies for this case
# n_distinct(acquisitions_selected$acquiree_uuid) # only 126478 companies have one acquisitions


# Will only keep earliest acqusitions information row per acquiree_company (earliest acquired_on date)
acquisitions_cleaned <- acquisitions_selected %>% 
  group_by(acquiree_uuid) %>%
  filter(acquired_on == min(acquired_on)) %>%
  ungroup()

# there are still some companies who have multiple acquisition information because
# they have several acquirer and all acquisitions happened on the same date
#n_occur2 <- data.frame(table(acquisitions_cleaned$acquiree_uuid)) 
#sum(n_occur2$Freq > 1) # there are 188 companies for this case

# randomly only filter out one row per one companies for those 188 cases
acquisitions_cleaned <- acquisitions_cleaned[!duplicated(acquisitions_cleaned$acquiree_uuid),]

joined_org_acq <- left_join(joined_org_funding, acquisitions_cleaned, 
                                by = c("uuid" = "acquiree_uuid"))

joined_org_acq <- joined_org_acq %>%
  rename(acquired_price_usd = price_usd)

plot_missing(joined_org_acq)


# 3-3. From ipos table
# now we have all ipo_date information column in our main organizations table

n_distinct(ipos$org_uuid) # there are 46378 distinct IPO companies but we have 47690 rows
# Some companies may have multiple IPO rows because they registered in several different stock market 
# (different countries stock market registration in different date).
# Some companies registered in different countries stock market after few years or just few days later
n_occur3 <- data.frame(table(ipos$org_uuid))
sum(n_occur3$Freq > 1) # there are 1120 companies for this case

# So I will only keep earliest IPO date information row per each company (earliest went_public_on date)
ipos_cleaned <- ipos %>% 
  group_by(org_uuid) %>%
  filter(went_public_on == min(went_public_on)) %>%
  ungroup()

# there are still 115 companies who have multiple rows for IPO information because
# they have multiple IPOs in different stock markets that happened on the same date
#n_occur4 <- data.frame(table(ipos_cleaned$org_uuid))
#sum(n_occur4$Freq > 1)

# So, randomly only filter out one row per one companies for those 115 cases
# because anyway we only need IPO date columns and it's on the same date for several rows for these 115 companies
ipos_cleaned <- ipos_cleaned[!duplicated(ipos_cleaned$org_uuid),]

n_distinct(ipos_cleaned$org_uuid) # now we correctly have distinct 46378 companies with one row per one company

joined_org_ipo <- left_join(joined_org_acq, ipos_cleaned %>% select(org_uuid, went_public_on), 
                            by = c("uuid" = "org_uuid")) %>%
  rename(IPO_on = went_public_on)

plot_missing(joined_org_ipo)


# 3-4. From investments, investors table
# there are one more final pre-processing in the later section
investments_selected <- investments %>%
  select(-type, -permalink, -cb_url, -rank, -created_at, -updated_at)

investors_selected <- investors %>%
  select(uuid, investor_types, rank, country_code, city) %>% # to add which type of investors are(angel/VC), their rank
  rename(investor_rank = rank,
         investor_country_code = country_code, 
         investor_city = city)

joined_inv_final <- left_join(investments_selected, investors_selected, 
                                by = c("investor_uuid" = "uuid"))


funding_rounds_selected <- funding_rounds %>%
  select(uuid, org_uuid, org_name, investment_type) %>%
  rename(funded_org_uuid = org_uuid,
         funded_org_name = org_name)

joined_inv_funding <- left_join(joined_inv_final, funding_rounds_selected, 
                        by = c("funding_round_uuid" = "uuid"))


################
# Testing part #
################
# sum_invest_types <- as.data.frame(table(joined_inv_final$investor_types))
# write.csv(sum_invest_types,"~/Master thesis/data_cleaned/sum_invest_types.csv", row.names = FALSE)
# sum_invest_types_add <- sum_invest_types %>%
#   mutate(has_VC = case_when(
#     # When any rows containing 'venture capital' related string is found, fill new column has_VC with 1, otherwise 0
#     grepl(pattern = "corporate_venture_capital", x = Var1) ~ 1,
#     grepl(pattern = "venture_capital", x = Var1) ~ 1,
#     grepl(pattern = "micro_vc", x = Var1) ~ 1,
#     TRUE ~ 0),
#     has_angel = case_when(
#       # When any rows containing 'angel' related string is found, fill new column has_angel with 1, otherwise 0
#       grepl(pattern = "angel", x = Var1) ~ 1,
#       grepl(pattern = "angel_group", x = Var1) ~ 1,
#       TRUE ~ 0))


joined_inv_funding <- joined_inv_funding %>%
  mutate(has_VC_investor = case_when(
    # When any rows containing 'venture capital' related string is found, fill new column has_VC_investor with 1, otherwise 0
    grepl(pattern = "corporate_venture_capital", x = investor_types) ~ 1,
    grepl(pattern = "venture_capital", x = investor_types) ~ 1,
    grepl(pattern = "micro_vc", x = investor_types) ~ 1,
    TRUE ~ 0),
    has_angel_investor = case_when(
      # When any rows containing 'angel' related string is found, fill new column has_angel_investor with 1, otherwise 0
      grepl(pattern = "angel", x = investor_types) ~ 1,
      grepl(pattern = "angel_group", x = investor_types) ~ 1,
      TRUE ~ 0))

# filter out the most famous(influential) investor/invest company for each company 
# to check if investor's size and influence level affects success of startup
# picks highest ranked investor by each company (considering whole funding rounds)
joined_inv_funding2 <- joined_inv_funding %>%
  select(funded_org_uuid, has_VC_investor, has_angel_investor, investor_uuid, investor_name, investor_rank,
         investor_country_code, investor_city) %>%
  group_by(funded_org_uuid) %>%
  filter(investor_rank == min(investor_rank)) %>%
  ungroup() %>%
  distinct() %>%
  as.data.frame()

all_joined_org <- left_join(joined_org_ipo, joined_inv_funding2,
                            by = c("uuid" = "funded_org_uuid"))

# 3-5. Additional funding date columns from funding rounds table
# additionally adding announce_date for each funding_rounds
funding_date_to_add <- funding_rounds %>%
  select(org_uuid, investment_type, announced_on) %>%
  group_by(org_uuid, investment_type) %>%
  filter(announced_on == min(announced_on)) %>%
  filter(investment_type %in% c("series_a", "series_b", "series_c", "series_d")) %>%
  ungroup() %>%
  distinct() %>%
  as.data.frame()

# Transpose the data(rows to columns) to create columns showing funding_announced_date per each funding round
funding_date_to_add <- dcast(funding_date_to_add, org_uuid ~ investment_type)

funding_date_to_add <- funding_date_to_add %>%
  rename(date_series_a = series_a, date_series_b = series_b, 
         date_series_c = series_c, date_series_d = series_d)

joined_org_final <- left_join(all_joined_org, funding_date_to_add,
                            by = c("uuid" = "org_uuid"))


# 3-6. Create columns to calculate 'Age' with date difference between several columns

joined_org_final <- joined_org_final%>%
  mutate(age_first_funding = round(time_length(difftime(first_funding_on, founded_on), "years"), digits = 1)) %>%
  mutate(age_seriesA = round(time_length(difftime(date_series_a, founded_on), "years"), digits = 1)) %>%
  mutate(age_seriesB = round(time_length(difftime(date_series_b, founded_on), "years"), digits = 1)) %>%
  mutate(age_seriesC = round(time_length(difftime(date_series_c, founded_on), "years"), digits = 1)) %>%
  mutate(age_seriesD = round(time_length(difftime(date_series_d, founded_on), "years"), digits = 1)) %>%
  mutate(age_fundings = round(time_length(difftime(last_funding_on, first_funding_on), "years"), digits = 1)) %>%
  mutate(age_IPO = round(time_length(difftime(IPO_on, founded_on), "years"), digits = 1)) %>%
  mutate(age_Acquired = round(time_length(difftime(acquired_on, founded_on), "years"), digits = 1)) %>%
  # age of company till 2016 (which is our train dataset's last)
  mutate(age_years = round(time_length(difftime(as.Date("2016-12-31"), founded_on), "years"), digits = 1)) %>%
  select(-date_series_a, -date_series_b, -date_series_c, -date_series_d)

write.csv(joined_org_final,"~/Master thesis/data_cleaned/test.csv", row.names = FALSE)

# 3-7. Create Target Variable column (Success/Fail)
joined_org_final <- joined_org_final %>%
  mutate(success = case_when(
    # when status = acquired / ipo 
    status %in% c("acquired", "ipo") ~ 1,
    # when status = operating and has_seriesC = 1
    (status == "operating" & has_seriesC == 1) ~ 1,
    TRUE ~ 0))

# 3-8. Create Column with age_success (years took until success from founded date)
joined_org_final <- joined_org_final %>%
  mutate(age_success = case_when(status == "acquired" ~ age_Acquired,
                                 status == "ipo" ~ age_IPO,
                                 (status == "operating" & has_seriesC == 1) ~ age_seriesC))

# Filter out only 'Company' in our organization table as primary role
joined_org_final <- joined_org_final %>%
  filter(primary_role == "company") %>%
  filter(!roles %in% c("investor", "investor,school", "school", "school,investor"))

# 3-9. age_closed (years took until close the company)
joined_org_final <- joined_org_final %>%
  mutate(age_closed = round(time_length(difftime(closed_on, founded_on), "years"), digits = 1))


# 3-10. Cleaning dataset for the last time (drop columns not in-use anymore) after EDA
joined_org_final <- joined_org_final %>%
  mutate_at(c(5, 14, 21, 23:46, 54:55, 70), as.integer)

# There were 17 companies which didn't have information about any funding rounds.
NA_seriesA <- joined_org_final[is.na(joined_org_final$has_seriesA),]

# Remove them
joined_org_final <- joined_org_final %>% 
  filter(!is.na(joined_org_final$has_seriesA))

# Homepage URL 
joined_org_final$homepage_active <- ifelse(!is.na(joined_org_final$homepage_url), 1, 0)
joined_org_final <- joined_org_final %>%
  mutate_at(c(73), as.integer)

# 4. Feature Engineering
# 4-1. Binning Categorical features: Columns value binning/Factorize (to categorize, minimize distinct values)
joined_org_final$employee_count <- as.factor(joined_org_final$employee_count)
table(joined_org_final$employee_count)
(table(joined_org_final$employee_count)/nrow(joined_org_final))*100
sum(is.na(joined_org_final$employee_count)) # there is no NA but has 6419 unknown

ggplot(joined_org_final, aes(x=reorder(employee_count, employee_count, function(x)-length(x)))) +
  geom_bar(fill='red') +  labs(x='employee_count')

# combine(collapse) factor levels into manually defined 4 groups in large using forcats library
# check number of each level
joined_org_final$employee_count <- factor(joined_org_final$employee_count, 
                                          levels=c("1-10", "11-50", "51-100",
                                                   "101-250", "251-500", "501-1000",
                                                   "1001-5000", "5001-10000", "10000+",
                                                   "unknown"))
fct_count(joined_org_final$employee_count)

# https://www.researchgate.net/post/Dummy_variables_creation_for_a_regression_model
# https://stats.stackexchange.com/questions/77796/coding-for-an-ordered-covariate
# https://machinelearningmastery.com/one-hot-encoding-for-categorical-data/
# 0rdinal encoding for ordinal categorical variable
joined_org_final$employee_count <- fct_collapse(joined_org_final$employee_count, 
                                                "0" = c("unknown"), #unknown 
                                                "1" = c("1-10", "11-50", "51-100"), # small ~100
                                                "2" = c("101-250", "251-500", "501-1000"), # medium 100~999
                                                "3" = c("1001-5000", "5001-10000", "10000+")) # large

joined_org_final$employee_count <- factor(joined_org_final$employee_count, 
                                          levels=c("0", "1", "2", "3"))

fct_count(joined_org_final$employee_count)

# check distribution again
ggplot(joined_org_final, aes(x=reorder(employee_count, employee_count, function(x)-length(x)))) +
  geom_bar(fill='red') +  labs(x='employee_count')


# # only leave(keep) the columns we need FOR EDA
# joined_org_final <- joined_org_final %>%
#   select(-homepage_url, -funded_org_name, -type, -permalink, -updated_at, -roles, -closed_on, -primary_role)

#write.csv(joined_org_final,"~/Master thesis/data_cleaned/joined_org_final.csv", row.names = FALSE)

plot_missing(joined_org_final)


# 5. Clustering - CATEGORY REDUCING - LDA
data_clustering <- joined_org_final %>%
  select(uuid, name, category_groups_list)

#write.csv(data_clustering,"~/Master thesis/data_cleaned/data_clustering.csv", row.names = FALSE)



joined_org_final <- read_csv("~/Master thesis/data_cleaned/joined_org_final.csv")


joined_org_final <- joined_org_final %>%
  mutate_at(c('rank', 'num_funding_rounds', 'num_exits', 'avg_investors_per_round', 'investor_rank'), as.integer)%>%
  as.data.frame()




# Final DATA-CLEANING for 
# age_variables(remove obs with negative values) and 
# any date variables (remove ones <1990-01-01) and
# age_success = 0 ->THE ONES WITHOUT INFO OF IPO, ACQUIRED BUT STATUS IS ANYWAY IPO / ACQUIRED -> so SUCCESS
df_final <- read_csv("/Users/jiyoung/Master thesis/data_cleaned/df_final2.csv")
df_final <- as.data.frame(df_final)
df_final2 <- df_final %>%
  filter(!((!is.na(acquired_on) & acquired_on < '1990-01-01') | (!is.na(IPO_on) & IPO_on < '1990-01-01')))


df_final3 <- df_final2 %>% 
  filter(!((!is.na(age_first_funding) & age_first_funding < 0) | 
             (!is.na(age_seriesA) & age_seriesA < 0) |
             (!is.na(age_seriesB) & age_seriesB < 0) | 
             (!is.na(age_seriesC) & age_seriesC < 0) |
             (!is.na(age_seriesD) & age_seriesD < 0) |
             (!is.na(age_fundings) & age_fundings < 0) |
             (!is.na(age_IPO) & age_IPO < 0) |
             (!is.na(age_Acquired) & age_Acquired < 0) |
             # (!is.na(age_years) & age_years < 0) | 
             # I will proceed this later because we need to split the extra test set that contains 
             # companies founded on 2017.01.01~2022.04.22 (or 2020.12.31 or 2019.12.31)
             (!is.na(age_success) & age_success <= 0) |
             (!is.na(age_closed) & age_closed < 0)))

df_final4 <- df_final3 %>%
  select(-state_code)

df_final4 <- as.data.frame(df_final4)
str(df_final4)

vis <- plot_missing(df_final4)
vis

#write.csv(df_final4,"~/Master thesis/data_cleaned/real_final_data_v2.csv", row.names = FALSE)



# SOME VARIABLES LIKE has_VC_investor or has_angel_investor still have missing values
# Even though it's regarded as 'OK', I would rather imputate these missing values since they are missing just bcz
# the information is not available and additional info can be found in other columns like 'has_angel' in this case(THIS IS NOT TRUE...angel investment can also happen without angel investor)
# but I think imputating NA with '0' makes more sense in the sense it's unknown and even if company
# has_angel round = 1, still possible the investor was not angel_investor based on the definition of angel round funding, 
# so we will not include/regard has_angel == 1 as has_angel_investor == 1)
# (Investors in an angel round include individual angel investors, angel investor groups, friends, and family)
# (https://www.google.com/search?q=angel+round+meaning&ei=aQPrYsfPNeqjrgTY37-YCg&ved=0ahUKEwiHqNmm6Kv5AhXqkYsKHdjvD6MQ4dUDCA4&uact=5&oq=angel+round+meaning&gs_lcp=Cgdnd3Mtd2l6EAMyBQgAEIAEMgYIABAeEBYyBggAEB4QFjIGCAAQHhAWMgYIABAeEBYyBggAEB4QFjIGCAAQHhAWMgYIABAeEBYyBggAEB4QFjIGCAAQHhAWOgcIABBHELADOgUIABCRAjoECC4QQzoLCC4QgAQQxwEQ0QM6BQguEIAEOg4ILhCABBDHARDRAxDUAjoECAAQQzoHCC4Q1AIQQzoKCC4QxwEQ0QMQQzoLCC4QgAQQxwEQrwE6CAguEIAEENQCOgcILhCABBAKSgQIQRgASgQIRhgAULwFWK0kYPQmaAJwAXgAgAGHA4gBxBOSAQc3LjcuMi4xmAEAoAEByAEIwAEB&sclient=gws-wiz)

# not his one anymore
# final_data <- read_csv("/Users/jiyoung/Master thesis/data_cleaned/final_data.csv")

# changed into this dataset
final_data <- read_csv("~/Master thesis/data_cleaned/real_final_data_v2.csv")


# joined_inv_funding <- this dataset is the one with every single possible has_angel investor or either has_VC
inv_has_VC_investor_yes <- joined_inv_funding %>% 
  filter(has_VC_investor == 1) %>%
  select(funded_org_uuid, has_VC_investor) %>%
  rename(has_VC_investor_imputate = has_VC_investor) %>%
  distinct()

inv_has_angel_investor_yes <- joined_inv_funding %>%
  filter(has_angel_investor == 1) %>%
  select(funded_org_uuid, has_angel_investor) %>%
  rename(has_angel_investor_imputate = has_angel_investor) %>%
  distinct()


df_VC_imputated <- left_join(final_data, inv_has_VC_investor_yes, 
                                by = c("uuid" = "funded_org_uuid"))

df_VC_angel_imputated <- left_join(df_VC_imputated, inv_has_angel_investor_yes, 
                                by = c("uuid" = "funded_org_uuid"))


# write.csv(df_VC_angel_imputated,"~/Master thesis/data_cleaned/df_VC_angel_imputated_before_v2.csv", row.names = FALSE)

df_VC_angel_imputated$has_VC_investor_imputate[is.na(df_VC_angel_imputated$has_VC_investor_imputate)] <- 0
df_VC_angel_imputated$has_angel_investor_imputate[is.na(df_VC_angel_imputated$has_angel_investor_imputate)] <- 0

df_VC_angel_imputated <- df_VC_angel_imputated %>%
  select(-has_VC_investor, -has_angel_investor) %>%
  rename(has_VC_investor = has_VC_investor_imputate, 
         has_angel_investor = has_angel_investor_imputate)
  

# write.csv(df_VC_angel_imputated,"~/Master thesis/data_cleaned/df_VC_angel_imputated_after_v2.csv", row.names = FALSE)


######## Prepare/Subset out the Additional Test sets (for checking prediction robustness, accuracy) ########
######## Companies founded between 2017/01/01 ~ 2020/12/31 ##########


df_extra_testset <-df_VC_angel_imputated %>% 
  filter(founded_on > '2016-12-31' & founded_on < '2021-01-01')

# write.csv(df_extra_testset,"~/Master thesis/data_cleaned/df_extra_testset.csv", row.names = FALSE)

df_wholeset <-df_VC_angel_imputated %>% 
  filter(founded_on < '2017-01-01')

# write.csv(df_wholeset,"~/Master thesis/data_cleaned/df_wholeset.csv", row.names = FALSE)



#####################################################
## Add: EDA of success ratio by category(industry) ##
#####################################################

#THIS IS FOR PREVIOUS VERSION
#df_final <- read_csv("/Users/jiyoung/Master thesis/data_cleaned/df_final2.csv")

#NEW VERSION OF DATASET
#df_final <- read_csv("/Users/jiyoung/Master thesis/data_cleaned/df_wholeset.csv")
df_final <- as.data.frame(df_final)


#summary table for success ratio by categories 
# success_ratio_2 <- df_final %>%
#   select(uuid, name, category, success) %>%
#   group_by (category, success) %>%
#   summarise (n=n()) %>% 
#   mutate(rel.freq = (n/sum(n))*100)


count_pct <- function(df) {
  return(
    df %>%
      tally %>% 
      mutate(n_pct = 100*n/sum(n))
  )
}

success_ratio <- df_final %>%
  select(uuid, name, category, success) %>%
  group_by (category, success) %>%
  count_pct

success_ratio_only <- success_ratio %>%
  filter(success==1)

failed_ratio_only <- success_ratio %>%
  filter(success==0)


# write.csv(success_ratio_only,"~/Master thesis/data_cleaned/success_ratio_only_v2.csv", row.names = FALSE)
# write.csv(failed_ratio_only,"~/Master thesis/data_cleaned/failed_ratio_only_v2.csv", row.names = FALSE)

# success/fail ratio by country
success_ratio_country <- df_final %>%
  select(uuid, name, country_code, success) %>%
  group_by (country_code, success) %>%
  count_pct

success_ratio_country_only <- success_ratio_country %>%
  filter(success==1)

failed_ratio_country_only <- success_ratio_country %>%
  filter(success==0)


# write.csv(success_ratio_country_only,"~/Master thesis/data_cleaned/success_ratio_country_only.csv", row.names = FALSE)
# write.csv(failed_ratio_country_only,"~/Master thesis/data_cleaned/failed_ratio_country_only.csv", row.names = FALSE)


##### Final Data is ready for Modeling #####
df_final_before_split_ver0 <- read_csv("/Users/jiyoung/Master thesis/data_cleaned/data for modeling ready/df_final_before_split_ver0.csv")
df_final_before_split <- read_csv("/Users/jiyoung/Master thesis/data_cleaned/df_final_before_split.csv")
X_train_after_dummy <- read_csv("/Users/jiyoung/Master thesis/data_cleaned/data for modeling ready/X_train_after_dummy.csv")

df_final_before_split_ver0 <- df_final_before_split_ver0 %>%
  select(-...1) %>%
  as.data.frame()

df_final_before_split <- df_final_before_split %>%
  select(-...1) %>%
  as.data.frame()

X_train_after_dummy <- X_train_after_dummy %>%
  select(-...1) %>%
  as.data.frame()

missing_before_vis <- plot_missing(df_final_before_split_ver0)
missing_before_vis

missing_final_vis <- plot_missing(df_final_before_split)
missing_final_vis

missing_after_dummy_vis <- plot_missing(X_train_after_dummy)
missing_after_dummy_vis




# Based on the missing plot result, 
# top 3 missing variables num_exits, acquired_price_usd, age_closed, age_success were dropped
# in case of acquired_price_usd, the variable total_funding_usd already partially represents(contains) information of acquired price,
# and in case of age_closed and age_success, we can say that the variable age_years 
# (the total age of company calculated from founded_on date to 2016-12-31) kind of represents/contains the information

####### Final checking on missing data #######




# #######################################
# ## Add: Country comparative analysis ##
# #######################################
# 
# org_after_outlier %>%
#   filter(country_code %in% c("SGP", "KOR")) %>%
#   group_by(country_code) %>%
#   summarise(startup_cnt = n()) %>%
#   ggplot(aes(x = (country_code), y = startup_cnt)) +
#   geom_bar(stat = "identity") +
#   theme(axis.text.x=element_text(angle = -90, hjust = 0))
# 
# 
# 
# org_SGP <-  org_after_outlier %>%
#   filter(country_code %in% c("SGP"))
# 
# org_KOR <- org_after_outlier %>%
#   filter(country_code %in% c("KOR"))
# 
# org_after_outlier %>%
#   filter(country_code %in% c("SGP", "KOR")) %>%
#   group_by(country_code) %>%
#   summarise(total_funding_usd) %>%
#   ggplot(aes(x = (country_code), y = total_funding_usd)) +
#   geom_bar(stat = "identity") +
#   theme(axis.text.x=element_text(angle = -90, hjust = 0))
# 
# 
# 
