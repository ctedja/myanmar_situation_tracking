# Myanmar situation tracking dashboard backend data preparation. 



# Packages
library(readxl)
library(writexl)
library(tidyverse)
library(acled.api)
library(sf)
library(lubridate)
library(xlsx)



# Load ----
# Draw from the ACLED package API: https://CRAN.R-project.org/package=acled.api
#   Currently a faster refresh than Dots, by two weeks, and allow manual manipulation for preparation
conflict <- acled.api(email.address = "clinton.tedja@wfp.org",
                      access.key = "____TOKEN_____",
                      country = "Myanmar", 
                      start.date = "2021-01-01", 
                      end.date = Sys.Date())

# Load the corporate shapefile
geography <- read_sf("C:\\Users\\clinton.tedja\\OneDrive - World Food Programme\\Documents (OneDrive)\\Data\\rbb_adm2_may_2021\\adm2.shp") %>% 
  filter(iso3 == "MMR") %>%
  select(-c(source, source_id, source_dat, lst_update, rb))

# Alternative API workflow
#   conflict <- GET("https://api.acleddata.com/acled/read/?key=nrSMy2xwZwCbaaMfXW2W&email=clinton.tedja@wfp.org&terms=accept&year=2021&country=Myanmar&limit=10000")

#   conflict <- bind_rows(lapply(content(conflict)$data, as.data.frame))



rm(appended)
rm(beneficiaries)
rm(budgeting)
rm(displaced)
rm(donors)
rm(final_joined)
rm(joined)
rm(opportunities_donors)
rm(opportunities_forecast)
rm(pipeline)
rm(pipeline_absolute_latest)
rm(pipeline_latest)
rm(updates)
rm(opportunities)
rm(nmax)
rm(og_colnames)


# Three manual databases
updates <- read_excel('C:\\Users\\clinton.tedja\\OneDrive - World Food Programme\\Documents (OneDrive)\\Data\\Myanmar_Situation\\myanmar_manual_entry.xlsx', sheet = 'updates')

beneficiaries <- read_excel('C:\\Users\\clinton.tedja\\OneDrive - World Food Programme\\Documents (OneDrive)\\Data\\Myanmar_Situation\\myanmar_manual_entry.xlsx', sheet = 'beneficiaries')

displaced <- read_excel('C:\\Users\\clinton.tedja\\OneDrive - World Food Programme\\Documents (OneDrive)\\Data\\Myanmar_Situation\\myanmar_manual_entry.xlsx', sheet = 'displaced')



# Tidy ----
# _ a) Conflict ----
# Adjust the admin names to match our WFP spatial file
conflict <- conflict %>% 
  mutate(admin2 = case_when(admin2 == "Yangon-East" ~ "Yangon (East)",
                            admin2 == "Yangon-West" ~ "Yangon (West)",
                            admin2 == "Yangon-North" ~ "Yangon (North)",
                            admin2 == "Yangon-South" ~ "Yangon (South)",
                            TRUE ~ admin2)) %>%
  mutate(admin1 = case_when(admin1 == "Shan-South" ~ "Shan (South)",
                            admin1 == "Shan-North" ~ "Shan (North)",
                            admin1 == "Shan-East" ~ "Shan (East)",
                            admin1 == "Shan-West" ~ "Shan (West)",
                            admin1 == "Bago-West" ~ "Bago (West)",
                            admin1 == "Bago-East" ~ "Bago (East)",
                            TRUE ~ admin1)) %>%
  mutate(event_date = as.Date(event_date)) %>%
  # create unique IDs
  rowid_to_column("ID") %>%
  # create a string month variable to join with beneficiary data and allow for a cumulative value
  mutate(month_str = months(event_date))

conflict <- rbind(conflict,
                  (conflict %>%
                     mutate(month_str = "Cumulative")))




# _ b) Beneficiaries ----
# Note: the only adm_2 district not appearing in the conflict dataset is Matman, which was formerly part of Hopang. It is unclear whether or not our ACLED dataset has just merged anything from Matman in with Hopang, or if there were simply no events at that location, though I assume the former.

# Split out any comma separated values within admin_1 level cells
beneficiaries <- beneficiaries %>% 
  mutate(ben_planned = case_when(ben_planned == 0 ~ NA_real_,
                                 TRUE ~ as.numeric(ben_planned)),
         ben_actual = case_when(ben_actual == 0 ~ NA_real_,
                                 TRUE ~ as.numeric(ben_actual))) %>%
  filter(!is.na(ben_location_label),
         !is.na(ben_actual) | !is.na(ben_planned)) %>%
  mutate(ben_actual = case_when(is.na(ben_actual) ~ 0,
                                TRUE ~ ben_actual))


nmax <- max(str_count(beneficiaries$ben_admin_1, "\\,"))

beneficiaries <- separate(data = beneficiaries, 
                 col = ben_admin_1, 
                 into = paste0("c", seq_len(
                   nmax)),
                 sep = "\\,") %>%
  pivot_longer(cols = paste0("c", seq_len(nmax)),
               names_to = "ben_admin_1_n",
               values_to = "ben_admin_1",
               values_drop_na = TRUE) %>%
  select(-c(ben_admin_1_n)) %>%
  mutate(ben_admin_1 = str_trim(ben_admin_1))

#  Append adm2 names into our dataset for those which we only specify the broader adm1 level names.
beneficiaries <- map_df(seq(nrow(beneficiaries)), function(x){
  # If we didn't specify the adm2 district details in our manual entry and only the state (i.e. Kachin)
  if(is.na(beneficiaries$ben_admin_2)[x]) 
    tibble(beneficiaries[x, ], 
           #  then the function returns all the districts (i.e. Myitkyina, Puta-O, etc.)
           new_admin_2 = filter(geography, adm1_name == beneficiaries$ben_admin_1[x])[["adm2_name"]])
  else tibble(beneficiaries[x, ], new_admin_2 = beneficiaries$ben_admin_2[x])}) %>%
  select(-ben_admin_2) %>%
  rename(ben_admin_2 = new_admin_2) %>%
  mutate(ben_month = as.Date(ben_month)) %>%
  mutate(ben_planned = as.double(ben_planned))


# _ c) Updates ----
# Do the same for our updates dataset, but first pivot out the comma separated states/regions in our manual entry forms
nmax <- max(str_count(updates$update_adm_1, "\\,"))

updates <- separate(data = updates, col = update_adm_1, 
                     into = paste0("c", seq_len(nmax)),
                     sep = "\\,") %>%
  pivot_longer(cols = paste0("c", seq_len(nmax)),
               names_to = "update_adm_1_n",
               values_to = "update_adm_1",
               values_drop_na = TRUE) %>%
  select(-c(update_adm_1_n))


updates <- map_df(seq(nrow(updates)), function(x){
  # If we didn't specify the adm2 details in our manual entry and only the state or region (i.e. Kachin)
  if(is.na(updates$update_adm_2)[x]) 
    tibble(updates[x, ], 
           #  then the function returns the districts (i.e. Myitkyina, Puta-O, etc.)
           new_adm_2 = filter(geography, adm1_name == updates$update_adm_1[x])[["adm2_name"]])
  else tibble(updates[x, ], new_adm_2 = updates$update_adm_2[x])}) %>%
  select(-update_adm_2) %>%
  rename(update_adm_2 = new_adm_2) %>%
  mutate(update_date = as.Date(update_date))

# _ d) Displacement ----
# And again for our displacement dataset
displaced <- map_df(seq(nrow(displaced)), function(x){
  # Since we didn't specify the adm2 details in our manual entry and only the state (i.e. Kachin)
  tibble(displaced[x, ], displaced_adm2 = filter(geography, adm1_name == displaced$displaced_adm1[x])[["adm2_name"]])
})




# Join & Bind pt.1 ----
joined <- conflict %>%
  full_join(beneficiaries, by = c("admin2" = "ben_admin_2", 
                                  "month_str" = "ben_month_str")) %>%
  full_join(updates, by = c("admin2" = "update_adm_2",
                            "event_date" = "update_date")) %>%
  full_join(displaced, by = c("admin2" = "displaced_adm2")) %>%
  select(-c(admin1, region, country, year, displaced_adm1, update_adm_1, ben_admin_1, timestamp))


# This cheap hacky workaround is just in order to ensure that we have no missing admin2 names for some months in the dashboard
appended <- bind_rows(
  data.frame(event_date = as.Date("2021-01-01"), 
             admin2 = unique(geography$adm2_name),
             month_str = "January"),
  data.frame(event_date = as.Date("2021-02-01"), 
             admin2 = unique(geography$adm2_name),
             month_str = "February"),
  data.frame(event_date = as.Date("2021-03-01"), 
             admin2 = unique(geography$adm2_name),
             month_str = "March"),
  data.frame(event_date = as.Date("2021-04-01"), 
             admin2 = unique(geography$adm2_name),
             month_str = "April"),
  data.frame(event_date = as.Date("2021-05-01"), 
             admin2 = unique(geography$adm2_name),
             month_str = "May"),
  data.frame(event_date = as.Date("2021-06-01"), 
             admin2 = unique(geography$adm2_name),
             month_str = "June"),
  data.frame(event_date = NA, 
             admin2 = unique(geography$adm2_name),
             month_str = "Cumulative")) %>% 
  arrange(event_date)


joined <- bind_rows(joined, appended)




# ______________________________________________________________________________



# _ e) Resourcing ----
# Added in a second phase of design; tidy and join below

# Tableau Server file for now
pipeline <- read.csv('C:\\Users\\clinton.tedja\\OneDrive - World Food Programme\\Documents (OneDrive)\\Data\\Myanmar_Situation\\Pipeline.csv')

# Dots for now
opportunities <- read.csv('C:\\Users\\clinton.tedja\\OneDrive - World Food Programme\\Documents (OneDrive)\\Data\\Myanmar_Situation\\Opportunities_210617.csv')


opportunities_forecast <- opportunities %>%
  mutate(TDD = as.Date(TDD),
         TDD_month = floor_date(TDD, "months")) %>%
  filter(Country_Code == "MM",
         Contribution_Year == "2021",
         Probability == "High" | Probability == "Medium",
         Forecast_Or_Contribution == "Forecast") %>%
  summarise(forecast = sum(Total_Value_USD))


# Top donors
opportunities_donors <- opportunities %>%
  filter(Country_Code == "MM",
         Stage_Of_Negotiation == "Closed Won",
         Contribution_Year == "2021") %>%
  arrange(desc(Total_Value_USD))


opportunities_donors <- opportunities_donors %>%
  group_by(Donor_Name) %>%
  summarise(donor_usd = sum(Total_Value_USD)) %>%
  ungroup() %>%
  arrange(desc(donor_usd))




# WHILE WE CHECK THE DONORS SOURCE, USE THIS
opportunities_donors <- data.frame(Donor_Name = c("USA","Japan", "Australia",	"Switzerland", "European Union", "All Other Donors"), donor_usd = c(18000000, 7000000, 6604506, 3220272, 2409201, 5249684))



# Funding figures
# Check dates of extracts
unique(arrange(pipeline %>% mutate(data_extracted = as.Date(data_extracted, format = "%d/%m/%Y")), data_extracted)$data_extracted)


# Generate the absolute latest
pipeline_absolute_latest <- pipeline %>%
  mutate(period = as.Date(period, format = "%d/%m/%Y")) %>%
  filter(country == "Myanmar",
         latest_data == "x",
         period >= "2021-06-01" & period <= "2021-11-30")

# Generate the specific version of interest
pipeline_latest <- pipeline %>%
  mutate(period = as.Date(period, format = "%d/%m/%Y"),
         data_extracted = as.Date(data_extracted, format = "%d/%m/%Y")) %>%
  filter(country == "Myanmar",
         #  Manually enter the date of interest
         data_extracted == "2021-05-16",
         period >= "2021-06-01" & period <= "2021-11-30")
colnames(pipeline)


pipeline_latest <- pipeline_latest %>%
  group_by(commodity, transfer_modality, period) %>%
  summarise(nbp_total_usd = sum(requirements_project_plan_resourcing_fcr),
            nbp_shortfall_usd = sum(shortfalls_project_plan_resourcing_fcr),
            nbp_total_mt = sum(requirements_project_plan_resourcing),
            nbp_shortfall_mt = sum(shortfalls_project_plan_resourcing),
            # Add implementation plan
            ip_total_usd = sum(requirements_implementation_plan_resourcing_fcr),
            ip_shortfall_usd = sum(shortfalls_implementation_plan_resourcing_fcr),
            ip_total_mt = sum(requirements_implementation_plan_resourcing),
            ip_shortfall_mt = sum(shortfalls_implementation_plan_resourcing)) %>%
  # Create contrasting figure for nbp
  mutate(nbp_funded_usd = nbp_total_usd - nbp_shortfall_usd,
         nbp_funded_mt = nbp_total_mt - nbp_shortfall_mt,
         # and implementation plan
         ip_funded_usd = ip_total_usd - ip_shortfall_usd,
         ip_funded_mt = ip_total_mt - ip_shortfall_mt)


pipeline_latest %>% filter(period == "2021-07-01",
                           transfer_modality == "Food") %>% 
  select(c(transfer_modality, commodity, ip_total_usd, ip_total_mt, ip_shortfall_usd, ip_shortfall_mt, prepo, resourcing_non_fcr)) %>%
  glimpse()



# Join & Bind pt.2 ----
# Bind all our budgeting figures
budgeting <- bind_rows(pipeline_latest, opportunities_donors, opportunities_forecast)

# Bind all into one data frame
final_joined <- bind_rows(joined, budgeting) 

glimpse(final_joined)





# Write ----
write_xlsx(as.data.frame(final_joined),
           'C:\\Users\\clinton.tedja\\OneDrive - World Food Programme\\Documents (OneDrive)\\Data\\Myanmar_Situation\\myanmar_situation_prepared_data.xlsx')

max(conflict$event_date)

glimpse(displaced)




# [IGNORE] Formula testing area for myself
#append_event_date <- seq(as.Date("2021-01-01"), as.Date("2021-05-01"), by = "month")
#append_admin2 <- rep(unique(geography$adm2_name), times = length(append_event_date))
#append <- data.frame(append_event_date, append_admin2) %>% 
#  arrange(append_event_date)


# An aggregate, to rbind
#pipeline_latest_agg <- pipeline_latest %>%
#  summarise(nbp_total = sum(requirements_project_plan_resourcing_fcr),
#            nbp_shortfall = sum(shortfalls_project_plan_resourcing_fcr)) %>%
#  mutate(nbp_funded = nbp_total - nbp_shortfall,
#         nbp_percent = nbp_funded/nbp_total)

