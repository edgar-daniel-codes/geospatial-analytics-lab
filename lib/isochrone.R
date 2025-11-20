# isochrone.R 

## Geospatial Analytics Lab
## Isochrone Calculator
## By Edgar Daniel

# This scripts allows you to create an shapefile corresponding to an n minutes 
# isochrone at a given velocity (transport media) at a given point inside a given
# Streets (LINESTRINGS) shapefile. 


# Set the seed to ensure reproducibility.
set.seed(999)

### ----------------------------------------------------------------------------
### Required Libraries  --------------------------------------------------------

library(geosphere)
library(sf)
library(rvest)
library(rjson)
library(maps)
library(leaflet)
library(lwgeom)
library(leaflet.extras)


### Functions  -----------------------------------------------------------------

calculate_isochrone <- function(lat,lon,T_segs,shp_streets,corners_sf
                             , VEL_MPS
                             , TWT_IND = TRUE, TRAFFIC_WT = 80,TWT_PROB=0.1  
                             , POINT_TOLERANCE_M=150, ADJ_TOLERANCE_M=3, ADJE_TOLERANCE_M=3
                             , DISPLAY = TRUE
                             , ECHO = FALSE
                             , CRS_ = 32614
) {
  
  
  ###
  # Function to construct isochrones from a location, a street file, and a corner file. 
  # Uses iterative DFS exhaustive search. 
  # Inputs:
  #   - lat: Latitude coordinate of the starting point 
  #   - lon: Longitude coordinate of the starting point 
  #   - shp_streets: SF object with streets projected in meters 
  #   - corners_sf: SF object with corners projected in meters 
  #   - T_segs: Time  
  #   - VEL_MPS: 1.4
  #   - TWT_IND: Indicates whether there are waiting times 
  #   - TRAFFIC_WT: Waiting time to cross in seconds
  #   - TWT_PROB: Probability of waiting to cross (0 assumes no spaces)
  #   - POINT_TOLERANCE_M: Tolerance radius for finding the indicated corner
  #   - ADJ_TOLERANCE_M: Tolerance radius for vertex adjacency with edge 
  #   - ADJE_TOLERANCE_M: Tolerance radius for edge adjacency with vertex
  #   - DISPLAY: Indicates whether or not to display the map 
  #   - ECHO: Indicates whether or not to print progress messages. 
  #   - CRS: Projection in meters to be used according to UTM. 
  # Outputs:
  # raw_isochrone: Explored routes reconstructed in SF object. 
  # hull_isochrone : Concave envelope of explored routes in SF object. 
  # paths_isochrone : Routes covered by the envelope in SF object. 
  # map : Leaflet object with the hull_isochrone and paths_isochrone layers
  
  iteracion_punto <- function(punto_sf, parent, parent_edge
                              ,edges_sf, nodes_sf,edges_w
                              ,T_current){
    
    # We obtain the adjacency matrix
    ids_adj_edge <- tryCatch(
      {
        st_intersection(
          edges_sf
          , nodes_sf |> filter(id_node ==punto_sf$id_node) |> 
            st_buffer(ADJ_TOLERANCE_M,nQuadSegs = 30) |> st_geometry()
        ) |>
          st_drop_geometry() |>
          pull(id_edge)
      },
      error = function(err) NULL
    )
    
    # We obtain the adjacenci nodes and weigts 
    ids_adj_node <- c()
    ids_adj_node_w <- c()
    
    # Validate the existence of edges 
    if(!is.null(ids_adj_edge)){
      # If any edge, we look for the related nodes 
      for(e in ids_adj_edge){
        ne <- tryCatch(
          {
            st_intersection(
              nodes_sf,
              edges_sf |> filter(id_edge == e) |>
                st_buffer(ADJE_TOLERANCE_M, nQuadSegs = 30) |>
                st_geometry()
            ) |>
              st_drop_geometry() |>
              filter(id_node != punto_sf$id_node) |>
              head(1) |>
              pull(id_node)
          },
          error = function(err) NULL
        )
        # We obtain the next nodes to explore  
        # We update the T budgets for each node 
        if(!is.null(ne)){
          ids_adj_node <- c(ids_adj_node, ne)
          ids_adj_node_w <- c(ids_adj_node_w,
                              edges_w |> filter(id_edge == e) |> pull(w))
        }
      }
    }
    
    # Update resources  (check for nulls)
    if(!is.null(ids_adj_node)){
      T_news <- T_current - ids_adj_node_w
      # Construct the nodes path v-v-...
      parents <- lapply(ids_adj_node, function(x) paste0(parent, "-", x))
      # Construct the edges path -e-e
      parents_edges <- lapply(ids_adj_edge, function(x) paste0(parent_edge, "-", x))
    }
    else{
      T_news <- c() 
      parents <- c() 
      parents_edges <- c()
    }
    
    
    # Update the network 
    #edges_sf <- edges_sf |> filter(!id_edge %in% ids_adj_edge)
    #nodes_sf <- nodes_sf |> filter(id_node!= punto_sf$id_node )
    #edges_w <- edges_w |> filter(!id_edge %in% ids_adj_edge)
    
    list(ids_adj_edge=ids_adj_edge
         ,ids_adj_node=ids_adj_node
         ,ids_adj_node_w=ids_adj_node_w
         ,edges_sf=edges_sf
         ,nodes_sf=nodes_sf
         ,edges_w=edges_w
         ,T_news=T_news
         ,parents=unlist(parents, recursive = FALSE)
         ,parents_edges=unlist(parents_edges, recursive = FALSE)
    )
  }
  
  
  
  tryCatch({
    
    # Transform the point to an SF object 
    point <- data.frame(lat = lat, lon = lon) |>
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    ### Initials SF objects 
    
    # Generate the geometries and asign the index 
    
    edges_sf <- shp_streets |>
      select(geometry) |>
      mutate(id_edge = row_number())
    
    nodes_sf <- corners_sf |>
      select(geometry) |>
      mutate(id_node = row_number())
    
    # Originals
    original_edges <- edges_sf 
    original_nodes <- nodes_sf
    
    # We filter the exploration cluster using the bounding box r = V_MPS*T + 500
    point_mts <- point |>
      st_transform(crs = CRS_) |> 
      st_geometry() |> st_coordinates()
    
    xmin <- point_mts[1] - (T_segs*VEL_MPS + 500)
    ymin <- point_mts[2] - (T_segs*VEL_MPS + 500)
    xmax <- point_mts[1] + (T_segs*VEL_MPS + 500)
    ymax <- point_mts[2] + (T_segs*VEL_MPS + 500)
    
    bbox <- st_as_sfc(st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
                              crs = CRS_))
    
    edges_sf <- edges_sf[st_intersects(edges_sf, bbox, sparse = FALSE), ]
    nodes_sf <- nodes_sf[st_intersects(nodes_sf, bbox, sparse = FALSE), ] 
    
    # We obtain the segment distances 
    edges_len <- edges_sf |>
      mutate(length = as.numeric(st_length(geometry))) |>
      st_drop_geometry()
    
    # Transform to lat/long
    edges_sf <- edges_sf |>
      st_transform(crs = 4326)
    nodes_sf <- nodes_sf |>
      st_transform(crs = 4326)
    
    # We obtain the convex hull from the given area 
    hull <- edges_sf |>
      st_geometry() |>
      st_union() |>
      st_convex_hull() |>
      st_as_sf() |>
      st_transform(crs = 4326)
    
    # We validate if the point in withing the streets range
    if((lengths(st_within(point, hull)) > 0)==FALSE){
      stop("The point is outside the region covered by the street file..")
    }
    
    ## We obtain the starting corner 
    
    # Nearby corners 
    close_corners <- st_intersection(
      nodes_sf
      , st_buffer(point,POINT_TOLERANCE_M,nQuadSegs = 90) |> st_geometry()
    ) |>
      st_drop_geometry() |>
      pull(id_node)
    
    if(is.null(close_corners) | length(close_corners) ==0){
      stop("No corner was found near the point. Please check again.")
    }
    
    # We select the closest corner
    min_dis_i <- 0
    min_dis_val <- Inf
    for(i in 1:length(close_corners)){
      d <- as.numeric(st_distance(point, nodes_sf |> 
                                    filter(id_node == close_corners[i])))
      
      if(d < min_dis_val){
        min_dis_val <- d
        min_dis_i <- close_corners[i]
      }
      
    }  
    
    ### MAIN: ALGORITH LOOP
    
    
    # We adjust the weights  
    # Weights = cost in seconds 
    edges_w <- edges_len |> 
      mutate(w = length/VEL_MPS) |>
      select(id_edge, w)
    
    # We create a full replica of the edges. 
    full_edges <- edges_sf |>
      left_join(edges_w, by='id_edge') |>
      left_join(edges_len, by='id_edge')
    full_nodes <- nodes_sf
    
    # We start the execution lists 
    parent_review <- c(as.character(min_dis_i))
    points_review <- c(min_dis_i)
    times_review <- c(T_segs)
    parent_edge_review <- c()
    
    path_completed_e <- c() # Follow the path - edges
    path_completed <- c() # Follow the path - nodes
    path_times <- c() # Remaining budget in each  path
    visited_nodes <- c() 
    visited_edges <-  c()
    
    ### EXECUTION LOOP
    # Counter init
    counter <- 0
    while(length(points_review)>0){
      
      # Get values from the current node 
      punto_idx <- points_review[[1]]
      parent <- parent_review[[1]]
      parent_edge <- parent_edge_review[[1]]
      T_current <- times_review[[1]]
      
      # We get the initial point 
      punto_sf <- nodes_sf |> filter(id_node == punto_idx)
      
      # We remove the data from the lists to be reviewed (pop)
      points_review <- points_review[-1]
      parent_review <- parent_review[-1]
      parent_edge_review <- parent_edge_review[-1]
      times_review <- times_review[-1]
      
      # We confirm that we have not evaluated that node
      if(!punto_idx %in% visited_nodes){
        
        # Execute iteration -Step
        res_it <- iteracion_punto(
          punto_sf, parent, parent_edge 
          ,edges_sf, nodes_sf,edges_w,T_current)
        
        
        # Unpack the results 
        ids_adj_edge <- res_it$ids_adj_edge
        ids_adj_node <- res_it$ids_adj_node
        ids_adj_node_w <- res_it$ids_adj_node_w
        edges_sf <- res_it$edges_sf
        nodes_sf <- res_it$nodes_sf
        edges_w <- res_it$edges_w
        T_news <- res_it$T_news
        parents <- res_it$parents
        parents_edges <- res_it$parents_edges
        
        # Step timeout process - if applicable
        if(TWT_IND){
          if(runif(1)<TWT_PROB){
            T_news[T_news>=TRAFFIC_WT] <- T_news[T_news>=TRAFFIC_WT] - TRAFFIC_WT
          }
        }
        
        # Update the lists - Consider whether it is already a terminal path or
        # Intermediate  
        visited_nodes <- c(visited_nodes,punto_idx)
        
        if(length(ids_adj_node[T_news>0])>0){
          parent_review <- c(parent_review,parents[T_news>0])
          parent_edge_review <- c(parent_edge_review,parents_edges[T_news>0])
          points_review <- c(points_review,ids_adj_node[T_news>0])
          times_review <- c(times_review,T_news[T_news>0])
          
          path_completed <- c(path_completed,parents[T_news<=0]) 
          path_completed_e <- c(path_completed_e,parents_edges[T_news<=0]) 
          path_times <- c(path_times,T_news[T_news<=0])
        }
        else{
          path_completed <- c(path_completed,parents) 
          path_completed_e <- c(path_completed_e,parents_edges) 
          path_times <- c(path_times,T_news)
        }
        
        # Update the counter 
        counter <- counter+1
        
        if(ECHO){
          cat("Completed iteration :",counter," Visited node : ",punto_idx,"\n")  
        }
        
        
        
      }
      else{
        if(ECHO){
          cat("Ignored point. Already visited: ", punto_idx, "\n")
        }
      }
    }
    
    ### Re-built the routes 
    
    # Complete routes 
    completas <- c()
    incompleta <- c()
    for(p in path_completed_e){
      aux_list <- p |> substring(2) |> strsplit("-") |> unlist()
      completas <- c(completas,aux_list[-length(aux_list)])
      incompleta <- c(incompleta,aux_list[[length(aux_list)]])  
    }
    
    completas <- completas |> unique()
    comp_paths_sf <- full_edges |>
      filter(id_edge %in% completas)
    
    
    # Incomplete routes 
    incompleta <- incompleta |> unique()
    
    # Final weights and segment measurements are used to 
    # determine the advance per segment. 
    
    incomplete_avance <- full_edges |>
      filter(id_edge %in% incompleta) |>
      pull(w) 
    
    # Validate from negative times 
    incomplete_avance <- incomplete_avance + path_times
    incomplete_avance <- unlist(lapply(incomplete_avance,function(x) max(x,0)))
    
    # We build the intermediate street segments. 
    fragmentos_sf <- st_sf(geometry = st_sfc(), crs = 4326)
    for(i in 1:length(path_completed)){
      
      # Path selection
      p <- path_completed[i]
      
      if(!is.na(p)){
        # We deconstruct the path v-v-v---v or -e-e-e-e
        # We extract the endpoints of the street, 
        # We maintain the direction of the path
        
        aux_list <- p |> strsplit("-") |> unlist() |> tail(2) 
        p1 <- corners_sf  |> 
          filter(id_node == aux_list[1]) |> 
          st_coordinates()
        
        p2 <- corners_sf  |> 
          filter(id_node == aux_list[2]) |> 
          st_coordinates()
        
        # Meters we want to advance 
        x <- incomplete_avance[i]
        
        # If advance 
        if(x>0){
          
          # We calculate the unit vector
          v <- p2 - p1
          d <- sqrt(sum(v^2))
          u <- v / d
          
          # Target point, origin point plus direction scaled by advance
          p_target <- p1 + u * x
          
          # We construct the object linestring, coordinates 
          line <- st_linestring(rbind(p1, p_target)) |>
            st_sfc(crs = st_crs(corners_sf)) |>
            st_transform(crs = 4326)
          
          # We added to the list 
          fragmentos_sf <- rbind(
            fragmentos_sf,
            st_sf(geometry = line)
          )
        }  
      }
      
    }
    
    
    # We create a final SF for the trajectories 
    raw_isochrone <- c(
      st_geometry(comp_paths_sf),
      st_geometry(fragmentos_sf)
    ) |> 
      st_combine()
    
    # We create an envelope (concave to be conservative).
    hull_isochrone <-  raw_isochrone |>
      st_transform(CRS_) |>          
      st_geometry() |>
      st_union() |>
      st_concave_hull(ratio = 0.5, allow_holes = FALSE) |>  
      st_as_sf() |>
      st_transform(4326)
    
    # We complete all paths inside the Hull
    # Even those that are unexplored but 'reachable' 
    paths_isochrone <- full_edges  |> 
      st_intersection(hull_isochrone)   |>
      select( geometry) |>
      filter(st_geometry_type(geometry) == "LINESTRING")
    
    map <- leaflet()  |>
      addProviderTiles(providers$CartoDB.Positron)  |> 
      addPolylines(
        data = original_edges |> st_transform(crs = 4326),
        color = "lightgray", 
        fillOpacity = 0.5, 
        weight = 3, 
        group="Zone of Interest"
        
      ) |> 
      addPolygons(
        data =  hull_isochrone ,
        color = 'cornflowerblue', 
        fillOpacity = 0.4, 
        opacity = 0.4,
        group= "Isochrone Area"
        
      ) |> 
      addPolylines(
        data =  paths_isochrone  ,
        color = 'black', 
        fillOpacity = 0.2,
        opacity = 0.4,
        weight = 2,
        group= "Paths within Isochrone"
      ) |> 
      addCircleMarkers(
        data =  full_nodes |>
          filter(id_node == min_dis_i) , 
        radius = 3, 
        color = 'darkgreen',
        opacity = 0.5,
        group= "Starting corner"
      ) |>
      addControl("<b>Isochrones in the area of interest.</b>", position = "topright") |>
      addLayersControl(
        overlayGroups = c(
          "Zone of Interest",
          "Isochrone Area", 
          "Paths within Isochrone",
          "Starting corner"
        ),
        options = layersControlOptions(collapsed = FALSE)
      ) 
    
    if(DISPLAY){
      print(map)
    }
    
    list(
      raw_isochrone=raw_isochrone,
      hull_isochrone=hull_isochrone,
      paths_isochrone=paths_isochrone,
      map=map
    )
    
    
    
  }, error = function(e) {
    message("Function Error: ", e$message)
    return(NULL)  
  })
  
}




