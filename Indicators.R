library(data.table)
library(sf)
#library(viridis)
#library(ggmap)
#library(leaflet)
library(gstat)
library(httr)
library(jsonlite)
source('StatsCanadaAPI.R')
source('CKAN_API.R')

CKAN_url <- 'http://data.socialprogress.org/'
api_key <- Sys.getenv('CKAN_API') # Set your API key in .Renviron

# Stats Canada ----

fsa.data <- getStatsCanada('FSA', 48)
cmaca.data <- getStatsCanada('CMACA', 48)
ct.data <- getStatsCanada('CT', 48)

sc.housing <- ct.data[TOPIC_THEME == 'Housing'][
  HIER_ID %in% c('9.1.1',    # Total - Private households by tenure - 25% sample data
                 '9.1.1.1',  # Owner
                 '9.1.6',    # Total - Private households by number of persons per room
                 '9.1.6.2',  # More than 1 person per room
                 '9.1.7',    # Total - Private households by housing suitability
                 '9.1.7.1',  # Suitable
                 '9.1.13',   # Total - Owner households in non-farm, non-reserve private
                 '9.1.13.2', # % of owner households spending 30% or more of its income 
                 '9.1.14',   # Total - Tenant households in non-farm, non-reserve private
                 '9.1.14.2') # % of tenant households spending 30% or more of its incom
][
  , .(GEO_ID, HIER_ID, T_DATA_DONNEE, INDENT_ID)
][
  , subtopic := sub('\\.[0-9]*$', '', sub('9\\.1\\.', '', HIER_ID))
][
  , dcast(.SD, GEO_ID ~ subtopic + INDENT_ID, value.var='T_DATA_DONNEE')
][
  , setnames(.SD, names(.SD), make.names(names(.SD)))
][
  , s_homeownership := X1_1 / X1_0
][
  , s_overcrowding := X6_1 / X6_0
][
  , s_suitable := X7_1 / X7_0
][
  , s_hburdenowner := X13_1 / X13_0
][
  , s_hburdenrenter := X14_1 / X14_0
][
  , .(GEO_ID, s_homeownership, s_overcrowding,
      s_suitable, s_hburdenowner, s_hburdenrenter)
]

sc.education <- ct.data[TOPIC_THEME == 'Education'][
  HIER_ID %in% c('10.1.2',       # Total - Highest certificate, diploma or degree for the popula
                 '10.1.2.1',     # No certificate, diploma or degree
                 '10.1.2.3.1',   # Apprenticeship or trades certificate or diploma
                 '10.1.2.3.4.1', # Bachelor's
                 '10.1.2.3.4.2', # University certificate or diploma above bachelor level
                 '10.1.2.3.4.4', # Master's degree
                 '10.1.2.3.4.5') # Earned doctorate
][
  , .(GEO_ID, HIER_ID, T_DATA_DONNEE)
][
  , subtopic := sub('10\\.1\\.', '', HIER_ID)
][
  , dcast(.SD, GEO_ID ~ subtopic, value.var='T_DATA_DONNEE')
][
  , setnames(.SD, names(.SD), make.names(names(.SD)))
][
  , aae_bachelors := X2.3.4.1 / X2
][
  , aae_graduate := (X2.3.4.2 + X2.3.4.4 + X2.3.4.5) / X2
][
  , aae_apprentice := X2.3.1 / X2
][
  , abk_nodegree := X2.1 / X2
][
  , .(GEO_ID, aae_bachelors, aae_graduate, aae_apprentice,
    abk_nodegree)
]

cmaca.pop <- cmaca.data[HIER_ID == '1.1.1' | HIER_ID == '7.1.1.1',
                        .(GEO_ID, HIER_ID, T_DATA_DONNEE)
][
  , dcast(.SD, GEO_ID ~ HIER_ID, value.var='T_DATA_DONNEE')
][
  , setnames(.SD, names(.SD), c('CMACA', 'total.pop', 'minority.pop'))
]

sc.pop <- ct.data[HIER_ID == '1.1.1' | HIER_ID == '7.1.1.1',
                  .(GEO_ID, HIER_ID, T_DATA_DONNEE)][
  , dcast(.SD, GEO_ID ~ HIER_ID, value.var='T_DATA_DONNEE')
][
  , setnames(.SD, names(.SD), c('GEO_ID', 'total.pop', 'minority.pop'))
][
  , CMACA := substr(GEO_ID, 1, 3)
][
  , merge(.SD, cmaca.pop, by='CMACA', all.x=T)
][
  , setnames(.SD, c('total.pop.x', 'total.pop.y',
                    'minority.pop.x', 'minority.pop.y'),
             c('t_i', 'T',
               'x_i', 'X'))
][
  , (c('T', 'X')) := lapply(.SD, as.numeric), .SDcols = c('T', 'X')
][
  , p_i := x_i / t_i
][
  , P := X / T
][
  , i_dissimilarity := sum(t_i * abs(p_i - P), na.rm=T) / (2*T*P*(1-P))
  , by=CMACA
][
  , i_isolation := sum((x_i / X) * (x_i / t_i), na.rm=T)
  , by=CMACA
]

# Enviromental ----

# CMACA shapefile
cmaca <- st_read(
  CKAN_dlresource(CKAN_url, 'cmaca-geography', api_key),
  quiet=T
) %>% st_transform(4269)

# Alberta province shapefile
AB <- st_read(
  CKAN_dlresource(CKAN_url, 'alberta-geography', api_key),
  quiet=T
) %>% st_transform(2163)

# Sample grid for IDW predictions
grid <- st_as_sf(as.data.table(st_sample(AB, 40000))) %>% st_transform(4269)

pm25peak <- CKAN_getdata(CKAN_url, 'peak-pm2-5', api_key)[
  , .(Latitude, Longitude, pm25peak = as.numeric(sub('-', '', Concentration)))
][
  !is.na(pm25peak)
][
  , as.data.table(
    st_intersection(
      st_transform(cmaca, 2163),
      st_transform(
        idw(pm25peak~1,
            st_as_sf(.SD, coords=c('Longitude', 'Latitude'),
                     crs=4269),
            grid, idp=16.0), 2163)
    )
  )
][
  , .(pm25peak = mean(var1.pred, na.rm=T))
  , by = CMAPUID
]

ozonepeak <- CKAN_getdata(CKAN_url, 'peak-ozone', api_key)[
  , .(Latitude, Longitude, ozone.peak = Concentration)
][
  !is.na(ozone.peak)
][
  , as.data.table( 
    st_intersection( 
      st_transform(cmaca, 2163), 
      st_transform(
        idw(ozone.peak~1, 
            st_as_sf(.SD, coords=c('Longitude', 'Latitude'),
                     crs=4269),
            grid, idp=16.0), 2163)
    )
  )
][
  , .(ozone.peak = mean(var1.pred, na.rm=T))
  , by = CMAPUID
]





# Child Care -----

ccfs <- CKAN_getdata(CKAN_url, 'childcare-information', api_key)[
  , setnames(.SD, names(.SD), make.names(names(.SD)))
][
  Accreditation.Status == 'Y' & Type.of.program == 'DAY CARE PROGRAM',
  .(Postal.Code, Capacity)
][
  , FSA := substr(Postal.Code, 1, 3)
][
  , .(Capacity = sum(Capacity, na.rm=T))
  , by=FSA
][
  , merge(.SD, fsa.data[TEXT_NAME_NOM=='    0 to 4 years',
                        .(GEO_ID, T_DATA_DONNEE)][
                          , setnames(.SD, names(.SD), c('FSA', 'pop.under5'))
                        ],
          by='FSA',
          all.y=T)
][
  is.na(Capacity), Capacity := 0
][
  , ratio := as.numeric(pop.under5) / Capacity
][
  , cc.desert := ratio >= 3 | is.na(Capacity)
][
  , ratio := Capacity / as.numeric(pop.under5)
]

# Population ----

local_pop <- CKAN_getdata(CKAN_url, 'local-population', api_key)[
  , setnames(.SD, names(.SD), make.names(names(.SD)))
][
  Year == 2018
]

local_pop_children <- local_pop[
  Age %in% c("00", "01to04", "05to09", "10to14", "15to19")
][
  , .(Population = sum(Population)), by=Geography
][
  , Subzone := mapply((function (x) substr(x, 1, 4)), Geography)
][
  , .(Fractional_Pop = Population / (sum(Population, na.rm=T)), Geography = Geography), by=Subzone
]

aggr_pop <- CKAN_getdata(CKAN_url, 'aggregate-population', api_key)[
  , setnames(.SD, names(.SD), make.names(names(.SD)))
][
  Year == 2018
]

aggr_pop_no_age <- aggr_pop[
  , .(Population = sum(Population)), by=Geography
][
  , Subzone := mapply((function (x) substr(x, 1, 4)), Geography)
][
  , .(Fractional_Pop = Population / (sum(Population, na.rm=T)), Geography = Geography), by=Subzone
]

setkey(aggr_pop_no_age, Geography)

zonal_pop_no_age = aggr_pop[
  , Geography := mapply((function (x) substr(x, 1, 4)), Geography)
][
  , .(Population = sum(Population)), by=Geography
]

setkey(zonal_pop_no_age, Geography)

zone_map <- data.table(ZONE_NUMBER=c("Z1", "Z2", "Z3", "Z4", "Z5"), 
                       ZONE_NAME=c("SOUTH ZONE", "CALGARY ZONE", "CENTRAL ZONE", "EDMONTON ZONE", "NORTH ZONE"))

# Nutrition ----

#num_deaths <- CKAN_getdata(CKAN_url, 'number-of-deaths', api_key)[
#, setnames(.SD, names(.SD), make.names(names(.SD)))
#][
#  grep('Z[0-9].[0-9]$', Geography),
#][
#  Year == "2016past",
#][
#  Sex == "BOTH",
#][
#  , Year := mapply((function (x) strtoi(substr(x, 1, 4))), Year)
#]

inf_mort <- CKAN_getdata(CKAN_url, 'infant-mortality-detailed', api_key)[
  , setnames(.SD, names(.SD), make.names(names(.SD)))
][
  Year == "2009to2018"
][
  grep('Z[0-9].[0-9]$', Geography)
]

low_birth_weight <- CKAN_getdata(CKAN_url, 'low-birth-weight-detailed', api_key)[
  , setnames(.SD, names(.SD), make.names(names(.SD)))
][
  year == "2018"
][
  grep('V[0-9].[0-9]$', geography)
][
  WEIGHT == 'ALL <2500g'
][
  , -c(1:3, 7:10, 12:14)
]

low_birth_weight_pivot = dcast(low_birth_weight, geography + year ~ WEIGHT, value.var = "LBW_RATE")

life_expectancy <- CKAN_getdata(CKAN_url, 'life-expectancy-at-birth-10-year-combined', api_key)[
  , setnames(.SD, names(.SD), make.names(names(.SD)))
][
  Year == "2009to2018"
][
  grep('Z[0-9](.[0-9](.[A-Z])?)?', Geography)
][
  , -c(6:8)
][
  Sex == "BOTH"
]

life_expectancy <- life_expectancy[aggr_pop_no_age, on='Geography'][
  , .(Life.Expectancy.at.Birth = sum(Life.Expectancy.at.Birth * Fractional_Pop)), by = Subzone
][
  , .(Geography=Subzone, Life_Expectancy = Life.Expectancy.at.Birth)
]

#[
#  , Geography := mapply((function (x) substr(x, 1, 4)), Geography)
#][
#  
#][, mean(Life.Expectancy.at.Birth), by=(Geography)]

sepsis <- CKAN_getdata(CKAN_url, 'in-hospital-sepsis', api_key)[
  , setnames(.SD, names(.SD), make.names(names(.SD)))
][
  Comparator %notin% c("Alberta", "Canada")
][
  , Geography := mapply(function(x) if (nchar(x) > 13) {toupper(substr(x, 1, nchar(x) - 8))} else {toupper(x)}, Comparator)
][
  , .(ZONE_NAME = Geography, Result = Indicator.Results.2017.2018)
][
  zone_map, on="ZONE_NAME"
][
  , .(Geography = ZONE_NUMBER, Sepsis_rate = Result)
]

# Personal Safety ----

non_violent_deaths <- CKAN_getdata(CKAN_url, 'mortality-rates-by-subzone-various', api_key)[
  , setnames(.SD, names(.SD), make.names(names(.SD)))
][
  YEAR == "2009to2018"
][
  geography != "AB"
][
  , -c(1, 2, 4, 8:14)
]

non_violent_deaths_pivot = dcast(non_violent_deaths, geography + YEAR ~ CAUSE_OF_DEATH, value.var = "MORT_RATE_ADJUSTED")

emergency_visits_injury <- CKAN_getdata(CKAN_url, 'emergency-visit-rate-suicide-violence', api_key)[
  , setnames(.SD, names(.SD), make.names(names(.SD)))
][
  Year == 2018
][
  grep('Z[0-9].[0-9]$', Geography)
][
  , -c(1, 4, 7:11)
]

emergency_visits_injury_pivot = dcast(emergency_visits_injury, Geography + Year ~ Injury.Type, value.var = "Visit.Rate")

# Health and Wellness ----

diabetes <- CKAN_getdata(CKAN_url, 'diabetes-details', api_key)[
  , setnames(.SD, names(.SD), make.names(names(.SD)))
][
  Year == 2017
][
  grep('Z[0-9].[0-9]$', Geography)
][
  , Subzone := mapply((function (x) substr(x, 1, 4)), Geography)
][
  , .(Cases = sum(Prevalent.Cases..Across.all.ages.), Population = sum(Population..Across.all.ages.)), by = Subzone
][
  , .(Geography = Subzone, Diabetes_Prevalence = Cases / Population)
]

emergency_visits_drugs <- CKAN_getdata(CKAN_url, 'emergency-visit-rate-alcohol-drug-opioids', api_key)[
  , setnames(.SD, names(.SD), make.names(names(.SD)))
][
  Year == 2018
][
  grep('Z[0-9].[0-9]$', Geography)
][
  , -c(1, 4, 7:11)
]

emergency_visits_drugs_pivot = dcast(emergency_visits_drugs, Geography + Year ~ Description, value.var = "Age.Standardized.Visit.Rate")

sti_rates <- CKAN_getdata(CKAN_url, 'sti-subzone-detailed', api_key)[
  , setnames(.SD, names(.SD), make.names(names(.SD)))
][
  Year %in% c('2018Q1', '2018Q2', '2018Q3', '2018Q4')
][
  grep('Z[0-9].[0-9]$', Geography)
][
  Sex == "BOTH"
][
  , .(Case.Counts = sum(Case.Counts)), by = "Geography"
][zonal_pop_no_age, on="Geography"][
  , .(STI_Rate = (Case.Counts / Population), Geography = Geography)
]

child_immunization <- CKAN_getdata(CKAN_url, 'child-immunization', api_key)[
  , setnames(.SD, names(.SD), make.names(names(.SD)))
][
  Year == 2018
][
  grep('Z[0-9].[0-9]', Geography)
][
  , .(COVERAGE_RATE = mean(Coverage.Rate)), by=Geography
][
  local_pop_children, on='Geography'
][
  , .(COVERAGE_RATE = sum(COVERAGE_RATE * Fractional_Pop)), by = Subzone
][
  , .(Geography=Subzone, COVERAGE_RATE = COVERAGE_RATE)
]

mental_health <- CKAN_getdata(CKAN_url, 'mental-health-details', api_key)[
  , setnames(.SD, names(.SD), make.names(names(.SD)))
][
  Year == 2017
][
  Geography != "ALBERTA"
][
  Category == "Excellent or Very Good"
][
  , .(ZONE_NAME = Geography, Percent = Percent)
][
  zone_map, on="ZONE_NAME"
][
  , .(Geography = ZONE_NUMBER, Good_Mental_Health_Rate = Percent)
]

activity <- CKAN_getdata(CKAN_url, 'activity-details', api_key)[
  , setnames(.SD, names(.SD), make.names(names(.SD)))
][
  Year == 2017
][
  Geography != "ALBERTA"
][
  Activity.Category == "Active - moderately active"
][
  , .(ZONE_NAME = Geography, Percent = Percent)
][
  zone_map, on="ZONE_NAME"
][
  , .(Geography = ZONE_NUMBER, Good_Mental_Health_Rate = Percent)
]

# Personal Freedom and Choice ----

oral_contraceptives <- CKAN_getdata(CKAN_url, 'oral-contraceptives-details', api_key)[
  , setnames(.SD, names(.SD), make.names(names(.SD)))
][
  YEAR == 2018
][
  grep('Z[0-9].[0-9]$', GEOGRAPHY)
][
  AGE == "ALL"
][
  , .(DISPENSATION_RATE = sum(DISPENSATION_RATE)), by=GEOGRAPHY
]

# Maps ----

# 
# fsa = st_read('~/Dropbox (Social Progress)/Data/Canada/Alberta/forward sortation area/lfsa000a16a_e.shp', stringsAsFactors=F)
# fsa = fsa[which(fsa$PRUID == '48'), ]
# 
# fsa = merge(fsa, ccfs, by.x='CFSAUID', by.y='FSA')
# 
# fsa = st_transform(fsa,
#                    crs='+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
# 
# st_bbox(fsa)
# ab.map.tiles = get_stamenmap(c(left=-1540664.4, bottom=1091468.0,
#                                right=-764067.4, top=2452417.5),
#                              maptype='toner-lite', zoom=10)
# 
# ggplot() +
#   geom_sf(data = fsa, aes(fill=log(ratio)),
#           size=0.25, alpha=0.75, color='#f5f5f2',
#           inherit.aes = FALSE) +
#   coord_sf(datum=NA) +
#   theme_void() +
#   scale_fill_viridis_c(name='Child\nCare\nCapacity')
# 
# fsa= st_simplify(fsa, dTolerance=0.005)
# fsa = st_transform(fsa, crs='+proj=longlat +datum=WGS84')
# 
# pal = colorNumeric('viridis', domain = fsa$lratio)
# 
# m <- leaflet(fsa) %>%
#   setView(-113.87, 52.27, 5) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(
#     fillColor = ~pal(lratio),
#     weight = 2,
#     color = 'white',
#     opacity = 1,
#     fillOpacity = 0.8,
#     highlight = highlightOptions(
#       weight = 3,
#       color = "#666",
#       fillOpacity = 0.7,
#       bringToFront = TRUE),
#   ) %>%
#   addLegend(pal = pal, values = ~lratio, opacity = 0.7,
#             title = 'Child<br>Care<br>Ratio',
#             position = "bottomright")
