# Myanmar situation tracking dashboard backend data preparation. 



# Packages
library(readxl)
library(writexl)
library(tidyverse)
library(sf)
library(lubridate)
library(xlsx)
library(acled.api)





# Load ----
# Draw from the ACLED package API: https://CRAN.R-project.org/package=acled.api
#   Currently a faster refresh than Dots, by two weeks, and allow manual manipulation for preparation
conflict <- acled.api(email.address = "clinton.tedja@wfp.org",
                      access.key = "___TOKEN____",
                      country = "Myanmar", 
                      start.date = "2021-01-01", 
                      end.date = Sys.Date())


# Load the corporate shapefile
geography <- read_sf("C:\\Users\\clinton.tedja\\OneDrive - World Food Programme\\Documents (OneDrive)\\Data\\rbb_adm2_may_2021\\adm2.shp") %>% 
  filter(iso3 == "MMR") %>%
  select(-c(source, source_id, source_dat, lst_update, rb))

# Alternative API workflow
#   library(acled.api)
#   conflict <- GET("https://api.acleddata.com/acled/read/?key=nrSMy2xwZwCbaaMfXW2W&email=clinton.tedja@wfp.org&terms=accept&year=2021&country=Myanmar&limit=10000")

#   conflict <- bind_rows(lapply(content(conflict)$data, as.data.frame))


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
#pipeline <- read.csv('C:\\Users\\clinton.tedja\\OneDrive - World Food Programme\\Documents (OneDrive)\\Data\\Myanmar_Situation\\Pipeline.csv')

pipeline <- read.csv('C:\\Users\\clinton.tedja\\OneDrive - World Food Programme\\Documents (OneDrive)\\Data\\Myanmar_Situation\\Pipeline_210622.csv')

# Dots for now
forecast <- read.csv('C:\\Users\\clinton.tedja\\OneDrive - World Food Programme\\Documents (OneDrive)\\Data\\Myanmar_Situation\\Opportunities_210617.csv')

# Contributions by donor, Dots for now
donors <- read.csv('C:\\Users\\clinton.tedja\\OneDrive - World Food Programme\\Documents (OneDrive)\\Data\\Myanmar_Situation\\Confirmed_Contributions.csv')



forecast <- forecast %>%
  filter(Country_Code == "MM",
         Contribution_Year == "2021",
         Probability == "High" | Probability == "Medium",
         Forecast_Or_Contribution == "Forecast")%>%
  summarise(forecast = sum(Total_Value_USD))


# Manual overwrite for June
forecast <- data.frame(forecast = c("13700000"))



# Top donors
donors <- donors %>% 
  filter(Recipient_Country_Code == "MM",
         Contribution_Year == 2021) %>%
  select(c(Confirmed_Contribution_USD, Top_Level_Donor_Name)) %>%
  rename(Donor_Name = Top_Level_Donor_Name,
         donor_usd = Confirmed_Contribution_USD) %>%
  group_by(Donor_Name) %>%
  summarise(donor_usd = sum(donor_usd)) %>%
  ungroup() %>%
  arrange(desc(donor_usd))


#
#
#

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
         data_extracted == "2021-06-18",
         period >= "2021-07-01" & period <= "2021-12-31",
         #  Remove plumpy doz
         commodity != "Plumpy Doz")





glimpse(pipeline_latest) %>%
  filter(wbs_element != "MM01.08.041.CPA2",
         transfer_modality == "Food",
         period == "2021-09-01") %>%
  group_by(commodity_group) %>%
  summarise(total = sum(requirements_implementation_plan_resourcing))

unique(pipeline_latest$transfer_modality)
pipeline_latest %>%
  filter(transfer_modality == "Service Delivery") %>%
  select(c(wbs_element, transfer_modality))


pipeline_latest <- pipeline_latest %>%
  filter(wbs_element != "MM01.08.041.CPA2") %>%
  group_by(commodity, transfer_modality, period) %>%
  summarise(nbp_total_fcr = sum(requirements_project_plan_resourcing_fcr),
            nbp_shortfall_fcr = sum(shortfalls_project_plan_resourcing_fcr),
            nbp_total = sum(requirements_project_plan_resourcing),
            nbp_shortfall = sum(shortfalls_project_plan_resourcing),
            # Add implementation plan
            ip_total_fcr = sum(requirements_implementation_plan_resourcing_fcr),
            ip_shortfall_fcr = sum(shortfalls_implementation_plan_resourcing_fcr),
            ip_total = sum(requirements_implementation_plan_resourcing),
            ip_shortfall = sum(shortfalls_implementation_plan_resourcing)) %>%
  # Create contrasting figure for nbp
  mutate(nbp_funded_fcr = nbp_total_fcr - nbp_shortfall_fcr,
         nbp_funded = nbp_total - nbp_shortfall,
         # and implementation plan
         ip_funded_fcr = ip_total_fcr - ip_shortfall_fcr,
         ip_funded = ip_total - ip_shortfall) 


# __ e.2) Alternative Pipeline ----

pipeline_rb_extract <- read_excel('C:\\Users\\clinton.tedja\\OneDrive - World Food Programme\\Documents (OneDrive)\\Data\\Myanmar_Situation\\Pipeline_Extract.xlsx', sheet = 'Sheet1')


pipeline_rb_extract <- pipeline_rb_extract %>% 
  mutate(period_date_format = as.Date(period_date_format, origin = "1900-01-01")) %>%
  filter(recipient_key == "MM",
         period_date_format >= "2021-07-01" & period_date_format <= "2021-12-31",
         wbs_element != "MM01.08.041.CPA2") %>%
  group_by(transfer_item, transfer_modality, period_date_format) %>%
  summarise(nbp_total_fcr = sum(requirements_nb_plan_resourcing_fcr),
            nbp_shortfall_fcr = sum(shortfalls_nb_plan_resourcing_fcr),
            nbp_total = sum(requirements_nb_plan_resourcing),
            nbp_shortfall = sum(shortfalls_nb_plan_resourcing),
            # Add implementation plan
            ip_total_fcr = sum(requirements_impl_plan_resourcing_fcr),
            ip_shortfall_fcr = sum(shortfalls_impl_plan_resourcing_fcr),
            ip_total = sum(requirements_impl_plan_resourcing),
            ip_shortfall = sum(shortfalls_impl_plan_resourcing)) %>%
  # Create contrasting figure for nbp
  mutate(nbp_funded_fcr = nbp_total_fcr - nbp_shortfall_fcr,
         nbp_funded = nbp_total - nbp_shortfall,
         # and implementation plan
         ip_funded_fcr = ip_total_fcr - ip_shortfall_fcr,
         ip_funded = ip_total - ip_shortfall) %>%
  rename(commodity = transfer_item,
         period = period_date_format) %>%
  mutate(transfer_modality = case_when(transfer_modality == "CBT and Commodity Voucher" ~ "C&V",
                                       TRUE ~ transfer_modality),
         period = floor_date(period, unit = "months"))


pipeline_rb_extract %>% group_by(transfer_modality, period) %>%
  summarise(total = sum(ip_total_fcr))

# pipeline_latest <- pipeline_rb_extract
rm(pipeline_rb_extract)

20*30*3



# _ g) Supply Chain -----

sc <- read_excel('C:\\Users\\clinton.tedja\\OneDrive - World Food Programme\\Documents (OneDrive)\\Data\\Myanmar_Situation\\myanmar_manual_entry.xlsx', sheet = 'sc')

sc <- sc %>% left_join(geography, by = c("sc_origin" = "adm2_name")) %>%
  select(c(1:9, "geometry")) %>%
  left_join(geography, by = c("sc_destination" = "adm2_name")) %>%
  select(c(1:10, "geometry.y")) %>%
  mutate(as.vector(as.data.frame(st_coordinates(st_centroid(geometry.x)))["X"]),
         as.vector(as.data.frame(st_coordinates(st_centroid(geometry.x)))["Y"])) %>%
  rename(sc_origin_lon = X,
         sc_origin_lat = Y) %>%
  mutate(as.vector(as.data.frame(st_coordinates(st_centroid(geometry.y)))["X"]),
         as.vector(as.data.frame(st_coordinates(st_centroid(geometry.y)))["Y"])) %>%
  rename(sc_destination_lon = X,
         sc_destination_lat = Y) %>%
  select(-c(geometry.y, geometry.x)) %>%
  pivot_longer(cols = c(sc_destination, sc_origin), 
               names_to = "sc_cat_location",
               values_to = "admin2") %>%
  mutate(sc_cat_location = case_when(sc_cat_location == "sc_destination" ~ "destination",
                                     sc_cat_location == "sc_origin" ~ "origin",
                                     TRUE ~ sc_cat_location)) %>%
  mutate(sc_label = case_when(sc_cat_location == "origin" ~ sc_origin_label,
                              sc_cat_location == "destination" ~ sc_destination_label)) %>%
  select(-c(sc_origin_label, sc_destination_label)) %>%
  mutate(sc_partners = case_when(sc_cat_location == "destination" ~ sc_partners),
         sc_quantity_mt = case_when(sc_cat_location == "destination" ~ sc_quantity_mt),
         sc_status = case_when(sc_cat_location == "destination" ~ sc_status),
         sc_population = case_when(sc_cat_location == "destination" ~ sc_population))





# Join & Bind pt.2 ----
# Bind all our budgeting figures
resourcing <- bind_rows(pipeline_latest, donors, forecast)


# Bind all into one data frame
final_joined <- bind_rows(joined, resourcing, sc)

glimpse(final_joined)
max(conflict$event_date)


# Write ----
write_xlsx(as.data.frame(final_joined),
           'C:\\Users\\clinton.tedja\\OneDrive - World Food Programme\\Documents (OneDrive)\\Data\\Myanmar_Situation\\myanmar_situation_prepared_data.xlsx')
Sys.time()




################################################################



# Quick EDA charts and calculations to map out conflict
ggplot(filter(conflict, 
              event_type %in% c("Battles", "Protests", "Explosions/Remote violence")) %>%
         mutate(event_type = factor(event_type, levels = c("Protests", "Battles", "Explosions/Remote violence"))), 
       aes(x = event_date, color = event_type, fill = event_type)) + 
  geom_density(aes(y = ..count..), alpha = 0.6) + 
  scale_fill_manual(values = (c("#33A977", "#923750", "#EE7449"))) +
  scale_color_manual(values = (c("#33A977", "#923750", "#EE7449"))) +
  theme_minimal()


conflict %>% mutate(month_str = factor(month_str, levels = c("January", "February", "March", "April", "May", "June", "July", "Cumulative"))) %>%
  group_by(month_str) %>%
  summarise(fatalities = sum(fatalities))

conflict %>% mutate(month_str = factor(month_str, levels = c("January", "February", "March", "April", "May", "June", "July", "Cumulative"))) %>% 
  group_by(month_str, event_type) %>%
  summarise(n_events = length(unique(ID))) %>% View()

conflict %>% mutate(month_str = factor(month_str, levels = c("January", "February", "March", "April", "May", "June", "July", "Cumulative"))) %>% 
  group_by(month_str) %>%
  summarise(n_events = length(unique(ID))) 
  

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

