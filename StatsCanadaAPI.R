
getDGUIDs = function(geos, cpt=NULL,
                     endpoint='https://www12.statcan.gc.ca/rest/census-recensement/CR2016Geo.json') {

  if (!is.null(cpt)) {
    params = paste0(endpoint, '?', 'lang=E&', 'geos=', geos, '&', 'cpt=', cpt)
  }
  else {
    params = paste0(endpoint, '?', 'lang=E&', 'geos=', geos)
  }
  
  call = utils::URLencode(params)
  print(call)
  get_call = httr::RETRY('GET', call)
  
  content = httr::content(get_call, 'text')
  content = substr(content, 3, nchar(content)) # Remove leading \\
  content = jsonlite::fromJSON(content, flatten=T)
  
  data = data.table::as.data.table(content['DATA'])
  data.table::setnames(data, names(data), unlist(content['COLUMNS']))
  
  dguids = data[, GEO_UID]
  
  return(dguids)
}

getStatsCanada = function(geos, cpt=NULL, topic=0,
                          endpoint='https://www12.statcan.gc.ca/rest/census-recensement/CPR2016.json') {
  
  dguids = getDGUIDs(geos, cpt)
  
  data = rbindlist(lapply(dguids, function(dguid) {
    params = paste0(endpoint, '?', 'lang=E&', 'dguid=', dguid, '&topic=', topic)
    
    call = utils::URLencode(params)
    get_call = httr::RETRY('GET', call)
    
    content = httr::content(get_call, 'text')
    content = substr(content, 3, nchar(content)) # Remove leading \\
    content = jsonlite::fromJSON(content, flatten=T)
    
    data = data.table::as.data.table(content['DATA'])
    data.table::setnames(data, names(data), unlist(content['COLUMNS']))
    
    data
  }))
  
  return(data)
}





