CKAN_dl <- function(url, key) {
    jsonlite::fromJSON(
        httr::content(
            httr::GET(url,
                      add_headers(Authorization = key)),
            'text'
        )
    )
}

CKAN_dlresource <- function(url, id, key) {
    call <- paste0(url, 'api/3/action/package_show?id=', id)
    res <- CKAN_dl(call, key)
    res.id <- res$result$resources$id
    pak.id <- res$result$id
    name <- res$resources$name
    rawToChar(
        httr::content(
            httr::GET(paste0(url, 'dataset/', pak.id,
                             '/resource/', res.id, '/download/',
                             name)),
            'raw'
                  )
        )
}

CKAN_dump <- function(url, id, key, json=F) {
    if(json) {
        httr::content(
            httr::GET(paste0(url, 'datastore/dump/', id, '?format=json'),
                      add_headers(Authorization=key)),
            'text'
        )
    }
    else {
        data.table::fread(
            rawToChar(
                httr::content(
                    httr::GET(paste0(url, 'datastore/dump/', id),
                              add_headers(Authorization=key)),
                    'raw'
                )
            )
        )
    }
}

CKAN_getdata <- function(url, id, key, json=F) {
    call <- paste0(url, 'api/3/action/package_show?id=', id)
    res <- CKAN_dl(call, key)
    res.id <- res$result$resources$id
    CKAN_dump(url, res.id, key, json)
}