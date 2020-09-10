require(sf)
require(raster)
require(dplyr)
require(purrr)
require(osmdata)
require(Rsagacmd)
require(fasterize)

maskOSM <- function(dem) {
  # create grid from DEM
  osmGrid <- dem %>% st_bbox %>% st_as_sfc %>% 
    st_make_grid(cellsize=10000) %>% 
    st_transform(4326)
    
  # execute sequential OSM queries
  print(paste0('DEM extent broken into following number of grid squares...',length(osmGrid)))
  # x <- 1
  osmData <- lapply(1:length(osmGrid), function(x) {
    print(paste0('requesting grid square number...',x))
    # construct OSM query, excludes footpaths and bridleways
    qRoad <- st_bbox(osmGrid[[x]]) %>% 
      opq(timeout=100,memsize=100000000) %>% 
      add_osm_feature("highway")
    osmRoad <- osmdata_sf(qRoad) # pull data from OSM
    
    if (!is.null(osmRoad$osm_lines)) {
      roadsRet <- osmRoad$osm_lines %>% 
        dplyr::select(geometry,
                      highway)
    } else roadsRet <- NULL
    
    qBuilding <- st_bbox(osmGrid[[x]]) %>% 
      opq(timeout=100,memsize=100000000) %>% 
      add_osm_feature("building")
    osmBuilding <- osmdata_sf(qBuilding) # pull data from OSM
    
    if (!is.null(osmBuilding$osm_polygons)) {
      buildRet <- osmBuilding$osm_polygons %>% 
        dplyr::select(geometry,
                      building)
    } else buildRet <- NULL
    
    return(list(road = roadsRet,
                building = buildRet))
  })
  # extract osm datas
  
  osmRoads <- osmData %>% purrr::map('road')
  # remove NULL queries
  osmRoads[sapply(osmRoads, FUN=is.null)] <- NULL
  # bind all to single Sf data frame, transform and clip
  # by buffered earthwork polygon
  allRoads <- do.call(rbind,osmRoads) %>% 
    dplyr::filter(highway %!in% c('footway',
                                  'bridleway',
                                  'track')) %>% 
    dplyr::select(!highway)
  # extract buildings 
  osmBuilding <- osmData %>% purrr::map('building')
  # remove NULL queries
  osmBuilding[sapply(osmBuilding, FUN=is.null)] <- NULL
  allBuilding <- do.call(rbind,osmBuilding) %>% 
    st_cast('LINESTRING') %>% 
    dplyr::select(!building)
  
  allOSM <- rbind(allRoads,allBuilding) %>% 
    st_transform(27700) %>% 
    st_buffer(10) %>% 
    st_intersection(primaryDat$vec) %>% 
    st_cast('MULTIPOLYGON') %>% 
    st_cast('POLYGON')
  
  # preview
  plot(dem)
  plot(allOSM$geometry,add=T)
  
  # rasterize osm data
  print('rasterizing osm roads and buildings data...')
  osmRas <- raster(dem)
  osmRas <- fasterize(allOSM,osmRas)
  # cookie cutter dem
  print('clipping from original dem...')
  demCut <- mask(dem, osmRas, inverse=T)
  
  # interpolate holes with rsaga
  print('filling holes with interpolated values...')
  saga <- saga_gis(opt_lib = "grid_tools")
  demInt <- demCut %>% saga$grid_tools$close_gaps_with_spline()
  
  # resample
  print('downsampling raster...')
  demIntAg <- aggregate(demInt, fact=2, fun=mean)
  
  return(demInt)
}

