library(data.table)
library(sf)
#library(viridis)
#library(ggmap)
#library(leaflet)
library(gstat)
library(httr)
library(jsonlite)
source('StatsCanadaAPI.R')

CKAN_url <- 'http://data.socialprogress.org/'
CKAN_url <- 'http://data.socialprogress.org/api/3/action/datastore_search'
api_key <- Sys.getenv('CKAN_API') # Set your API key in .Renviron

CKAN_dl <- function(url, key) {
  jsonlite::fromJSON(
    httr::content(
      httr::GET(url,
          add_headers(Authorization = key)),
      'text'
    )
  )
}

CKAN_getdata <- function(url, id, key) {
  call <- paste0(url, 'api/3/action/package_show?id=', id)
  res <- CKAN_dl(call, key)
  res.id <- res$result$resources$id
  data.table::fread(
    rawToChar(
      httr::content(
        httr::GET(paste0(url, 'datastore/dump/', res.id),
            add_headers(Authorization=key)),
        'raw'
      )
    )
  )
}


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

dir <- '~/Dropbox (Social Progress)/Data/Canada/Alberta/Environmental Indicators/'

pm25peak <- fread(paste0(dir, 'Peak PM 2_5 2016.csv'))[
  , .(Latitude, Longitude, pm25peak = as.numeric(sub('-', '', Concentration)))
]

grid = st_as_sf(as.data.table(st_sample(cmaca, 20000)))

g8math.idw = idw(Average.Scale.Score ~ 1, g8math, grid, idp=8.0, weights=g8math$wgt)

g8math.idw$GEOID = apply(st_intersects(jxn.trcts, g8math.idw, sparse = FALSE), 2,
                         function(col) {
                           jxn.trcts[which(col), ]$GEOID
                         })

ozonepeak <- fread(paste0(dir, 'Peak ozone concentrations at monitoring stations.csv'))[
  , .(Latitude, Longitude, ozone.peak = as.numeric(sub('-', '', Concentration)))
]



# Child Care -----

ccfs <- fread('~/Dropbox (Social Progress)/Data/Canada/Alberta/child-care-information-201612.csv')[
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




# Maps ----

# cmaca = st_read('~/Dropbox (Social Progress)/Data/Canada/Alberta/CMACA/lcma000b16a_e.shp')
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
