getLidar2 <- function(bufferedPoly,
                      whichProd="LIDAR Tiles DTM",
                      whichYears,
                      minSurvey = 2,
                      userDataDirRoot='tmp',
                      overwrite=T) {
  
  if (is.null(bufferedPoly$tile50k_name_char)) {
    bufferedPoly$tile50k_name_char <- bufferedPoly$layer
  }
  
  # define chrome options
  eCaps <- list(chromeOptions = list(
    args = c(
      '--disable-gpu'
      ,'--headless',
      '--window-size=1280,800'
    )
  ))
  # rD <- rsDriver(browser = "chrome",
  #                chromever = "81.0.4044.138",
  #                extraCapabilities = eCaps,
  #                port = 
  #                  as.integer(base::sample(seq(32768,65535, by=1),1)))
  rD <- RSelenium::rsDriver(
    browser = "firefox",
    extraCapabilities = list(
      "moz:firefoxOptions" = list(
        args = list('--headless')
      )
    ),
    port = 
      as.integer(base::sample(seq(32768,65535, by=1),1))
  )
  
  remDr <- rD[["client"]]
  
  browseQuery <- function(remDr,bufferedPoly) {
    bufferedPoly <- st_union(bufferedPoly) %>% st_sf
    st_write(bufferedPoly, dsn=paste0('data/temp.shp'),
             delete_dsn=T)
    simplePoly <- paste0(getwd(),'/data/temp.shp')
    simplePolyZip <- paste0(getwd(),'/data/temp.zip')
    simplePolyFiles <- paste0(getwd(),'/data/temp*')
    # zip shapefiles into archive
    system(paste0('zip ',simplePolyZip,' ',simplePolyFiles))
    
    # navigate to the EA lidar portal
    remDr$navigate("https://environment.data.gov.uk/DefraDataDownload/?Mode=survey")
    # upload zipped shape file
    suppressMessages({
      try(remDr$findElement("id", "fileid"),TRUE)
    })
    while (remDr$status == 7) {
      Sys.sleep(2)
      print('waiting for portal to load...')
      suppressMessages({
        try(remDr$findElement("id", "fileid"),silent=TRUE)
      })
    }
    webElem <- remDr$findElement("id", "fileid")
    webElem$sendKeysToElement(list(simplePolyZip))
    print('uploading shape file...')
    # wait for upload to complete (2 seconds)
    Sys.sleep(5)
    # find 'Get Tiles' button
    getTiles <- remDr$findElement(using = 'css selector', ".grid-item-container")
    # click 'Get Tiles' button
    getTiles$clickElement()
    # sys.sleep ?
    suppressMessages({
      try(remDr$findElement(using = 'css selector', '.data-ready-container'),TRUE)
    })
    
    i <- 0
    while (remDr$status == 7) {
      Sys.sleep(5)
      print(paste0('waiting for tiles to be returned...'))
      suppressMessages({
        try(remDr$findElement(using = 'css selector', '.data-ready-container'),TRUE)
      })
      i <- i + 1
      if (i > 12) {
        print('error with shape file...')
        return('shapefile error')
      }
    }
    print('tiles returned!')
  } # / browseQuery
  browseResult <- browseQuery(remDr,bufferedPoly)
  if (browseResult == 'shapefile error') {
    print(browseResult)
    return(browseResult)
  }
  l <- leaflet() %>% addTiles() %>% 
    addPolygons(data=st_transform(bufferedPoly,4326))
  print(l)
  print('searching available tiles...')
  # select products DTMs
  desiredProds <- whichProd
  # desiredProds <- "LIDAR Tiles DTM"
  # desiredProds <- "LIDAR Point Cloud"
  prodElem <- remDr$findElement(using = 'css selector', '#productSelect')
  prodList <- unique(prodElem$selectTag()$text)
  prodsIndex <- which(prodList %in% desiredProds)
  
  xP <- paste0('//*[@id="productSelect"]/option[',prodsIndex,']')
  webElem <- remDr$findElement(using = 'xpath', 
                               value = xP)
  webElem$clickElement()
  webElem$getElementText()
  # check which year available
  yrElem <- remDr$findElement(using = 'css selector', '#yearSelect')
  yrList <- unique(yrElem$selectTag()$text)
  
  if (desiredProds == "LIDAR Tiles DTM") { 
    # cycle through years, selecting 1m res and recording tiles names
    # x <- 1
    tileList <- lapply(1:length(yrList), function(x) {
      yr <- yrList[x]
      xP <- paste0('//*[@id="yearSelect"]/option[',x,']')
      webElem <- remDr$findElement(using = 'xpath', 
                                   value = xP)
      webElem$clickElement()
      # now cycle through res
      resElem <- remDr$findElement(using = 'css selector', '#resolutionSelect')
      resVec <- unique(resElem$selectTag()$text)
      # pick only 1m
      if (length(which(resVec == 'DTM 1M')) == 0) {
        return(NULL) } else { r <- which(resVec == 'DTM 1M') }
      
      resElem$clickElement() # open drop down
      xP <- paste0('//*[@id="resolutionSelect"]/option[',r,']')
      webElem <- remDr$findElement(using = 'xpath', 
                                   value = xP)
      webElem$clickElement() # select 1m res
      tileLinks <- remDr$findElement(using = 'css selector', '.data-ready-container')
      tileLinks.a <- tileLinks$findChildElements('tag', 'a')
      tiles <- unlist(lapply(tileLinks.a, function(x) x$getElementAttribute('href')))
      
      return(tiles)
    })
    
    # name list by years
    names(tileList) <- yrList
    # remove nulls (years with no 1m res)
    tileList[unlist(lapply(tileList,is.null))] <- NULL
    
  }
  
  if (desiredProds == "LIDAR Point Cloud") {
    # yr <- "2011"
    tileList <- lapply(whichYears, function(yr) {
      x <- which(yrList==yr)
      if (length(x) == 0) {
        print(paste0('year ',yr,' not available as LAZ'))
        return(NULL)
      } 
      xP <- paste0('//*[@id="yearSelect"]/option[',x,']')
      webElem <- remDr$findElement(using = 'xpath', 
                                   value = xP)
      webElem$clickElement()
      
      tileLinks <- remDr$findElement(using = 'css selector', '.data-ready-container')
      tileLinks.a <- tileLinks$findChildElements('tag', 'a')
      tiles <- unlist(lapply(tileLinks.a, function(x) x$getElementAttribute('href')))
      
      return(tiles)
    })
    
    # name list by years
    names(tileList) <- whichYears
    tileList[unlist(lapply(tileList, is.null))] <- NULL
  }
  
  # extract tile names from download URLs
  # x <- names(tileList)[1]
  tileNames <- lapply(names(tileList), function(x) {
    unlist(lapply(str_split(tileList[[x]], 
                            paste0(x,'-')),function(y) substr(y[2],1,6)))
  })
  names(tileNames) <- names(tileList)
  # convert data to tile names with lists of years
  tilesYears <- lapply(unique(unlist(tileNames)), function(tile) {
    allYears <- lapply(names(tileNames), function(year) {
      if (tile %in% tileNames[[year]]) year
    })
    allYears[unlist(lapply(allYears,is.null))] <- NULL
    return(unlist(allYears))
  })
  names(tilesYears) <- unique(unlist(tileNames))
  
  # minimun number of years survey 3, remove the rest
  if (minSurvey > 0) {
    tilesYears[unlist(lapply(tilesYears, function(x) length(x) < minSurvey))] <- NULL
    if (length(tilesYears) == 0) {
      er <- 'no tiles with sequential surveys found...'
      print(er)
      return(er)
    }
  }
  
  allLinks <- as.character(unlist(tileList))
  dlLinks <- allLinks[str_detect(allLinks,paste(names(tilesYears),collapse = '|'))]
  
  # output URLs as list for Wget
  fileName <- paste0(unique(bufferedPoly$tile50k_name_char,'_list.txt'))
  
  write.table(dlLinks,
              file=paste0('wget/',fileName),
              quote = F,row.names=F,col.names = F)
  print(paste0('written download list to ... wget/',fileName))
  
  # close selenium
  remDr$close()
  rD$server$stop()
  gc()
  
  # create folder structure
  folderPath <- paste0(userDataDir,'/',userDataDirRoot,'/',unique(bufferedPoly$tile50k_name_char))
  if (!dir.exists(folderPath)) {
    dir.create(folderPath)
    lapply(unique(unlist(tilesYears)),function(x) dir.create(paste0(folderPath,'/',x)))
  }
  
  # overwrite=T
  if (overwrite) {
    system(paste0('rm ',folderPath,' -R'))
    dir.create(folderPath)
    lapply(unique(unlist(tilesYears)),function(x) dir.create(paste0(folderPath,'/',x)))
  }
  
  # download and uncompress EA lidar with magic!
  # x <- 1
  system(paste0('cat ',getwd(),'/',paste0('wget/',fileName),' | parallel --gnu ',
                shQuote(paste0('wget {} -P ',folderPath))))
  
  # extract to yearly folders 
  yrs <- unique(unlist(tilesYears))
  # x <- yrs[2]
  lapply(yrs, function(x) {
    zips <- paste0(folderPath,'/',
                   list.files(folderPath)[grep(paste0("*(",x,").*zip$"),list.files(folderPath))])
    if (length(zips) > 0) { 
      lapply(zips, function(y) {
        system(paste0("unzip -n ",y,
                      ' -d ',folderPath,'/',x,'/'))
      })
    }
  })
  
  return(folderPath)
}
