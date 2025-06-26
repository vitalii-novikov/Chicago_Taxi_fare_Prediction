

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    start_point_sf = NULL, 
    end_point_sf = NULL, 
    path_data = NULL, 
    map_view_set = FALSE,
    path_length_val = NULL, 
    trip_duration_sec = NULL,
    predicted_fare = NULL
  )
  
  output$map <- leaflet::renderLeaflet({
    req(leaflet::leaflet, leaflet::addTiles, leaflet::addProviderTiles, leaflet::setView, 
        leaflet::addLayersControl, leaflet::layersControlOptions, leaflet::addMeasure)
    leaflet::leaflet() %>%
      leaflet::addTiles(group = "OSM (Default)") %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "CartoDB Positron") %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
      leaflet::setView(lng = -87.6232, lat = 41.8818, zoom = 13) %>%
      leaflet::addLayersControl(
        baseGroups = c("OSM (Default)", "CartoDB Positron", "Esri World Imagery"),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) %>%
      leaflet::addMeasure(
        position = "topright", primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D", completedColor = "#7D4479"
      )
  })
  
  observe({
    if (is.numeric(input$start_lon_input) && is.numeric(input$start_lat_input) &&
        !is.na(input$start_lon_input) && !is.na(input$start_lat_input)) {
      rv$start_point_sf <- sf::st_sfc(sf::st_point(c(input$start_lon_input, input$start_lat_input)), crs = 4326)
    } else { rv$start_point_sf <- NULL }
    if (is.numeric(input$end_lon_input) && is.numeric(input$end_lat_input) &&
        !is.na(input$end_lon_input) && !is.na(input$end_lat_input)) {
      rv$end_point_sf <- sf::st_sfc(sf::st_point(c(input$end_lon_input, input$end_lat_input)), crs = 4326)
    } else { rv$end_point_sf <- NULL }
    if (!rv$map_view_set && !is.null(rv$start_point_sf) && !is.null(rv$end_point_sf)) { 
      start_coords_val <- sf::st_coordinates(rv$start_point_sf); end_coords_val <- sf::st_coordinates(rv$end_point_sf)
      avg_lon <- (start_coords_val[1,1] + end_coords_val[1,1]) / 2
      avg_lat <- (start_coords_val[1,2] + end_coords_val[1,2]) / 2
      if(is.finite(avg_lon) && is.finite(avg_lat)){
        leaflet::leafletProxy("map") %>% leaflet::setView(lng = avg_lon, lat = avg_lat, zoom = 13)
        rv$map_view_set <- TRUE
      }
    }
  })
  
  output$start_coords_display <- renderText({ req(rv$start_point_sf); coords <- sf::st_coordinates(rv$start_point_sf); paste("Lon:", round(coords[1,1], 5), "Lat:", round(coords[1,2], 5)) })
  output$end_coords_display <- renderText({ req(rv$end_point_sf); coords <- sf::st_coordinates(rv$end_point_sf); paste("Lon:", round(coords[1,1], 5), "Lat:", round(coords[1,2], 5)) })
  
  # --- NEW: Render function for path length display ---
  output$path_length_display <- renderText({
    if (!is.null(rv$path_length_val)) {
      paste(rv$path_length_val, "mi")
    } else {
      "N/A" # Or "Path not calculated" or similar
    }
  })
  
  output$trip_duration_display <- renderText({
    if (!is.null(rv$trip_duration_sec)) {
      paste(rv$trip_duration_sec, "s")
    } else {
      "N/A" # Or "Path not calculated" or similar
    }
  })
  
  output$predicted_fare_display <- renderText({
    if (!is.null(rv$predicted_fare) && !is.na(rv$predicted_fare)) {
      sprintf("$ %.2f", rv$predicted_fare)
    } else {
      "N/A"
    }
  })
  # --- END NEW ---
  
  process_incident_edges <- function(node_label, node_idx_int, network_obj, total_node_count, total_edge_count) {
    print(paste("--- Debugging incident edges for", node_label, "node, index:", node_idx_int, "---"))
    incident_edges_sf_final <- sf::st_sf(geometry = sf::st_sfc()) 
    if (!(node_idx_int > 0 && node_idx_int <= total_node_count)) {
      print(paste("  ERROR (process_incident_edges): node_idx_int", node_idx_int, "is out of bounds for graph with", total_node_count, "nodes."))
      return(incident_edges_sf_final)
    }
    print("  Step 1.1: Attempting to slice the node from the network object...")
    node_slice_graph <- NULL; error_in_slice <- NULL
    tryCatch({
      node_slice_graph <- network_obj %>% tidygraph::activate(nodes) %>% dplyr::slice(node_idx_int)
      print("    Slice for 'node_slice_graph' successful. Class:"); print(class(node_slice_graph))
    }, error = function(e_slice) { error_in_slice <<- e_slice$message })
    if (!is.null(error_in_slice)) { print(paste("    ERROR during node slicing:", error_in_slice)); return(incident_edges_sf_final) }
    if (is.null(node_slice_graph)) { print("    ERROR: 'node_slice_graph' is NULL after slicing."); return(incident_edges_sf_final) }
    print("  Step 1.2: Extracting node data as tibble from 'node_slice_graph' and checking geometry...")
    node_data_tibble <- NULL; error_in_tibble_extract <- NULL; 
    tryCatch({
      if (inherits(node_slice_graph, "tbl_graph")) {
        node_data_tibble <- node_slice_graph %>% tidygraph::activate(nodes) %>% tibble::as_tibble()
        print("      Node data extracted to tibble. Names:"); print(names(node_data_tibble))
        if(nrow(node_data_tibble) > 0) {
          print("      Structure of node_data_tibble (first row):"); print(utils::str(node_data_tibble[1, , drop = FALSE]))
          sfc_columns_node <- sapply(node_data_tibble, function(col) inherits(col, "sfc"))
          if (any(sfc_columns_node)) {
            print(paste("      Geometry column(s) (sfc) FOUND in node_data_tibble:", paste(names(node_data_tibble)[sfc_columns_node], collapse=", ")))
          } else { print("      CRITICAL WARNING: No sfc geometry column found in node_data_tibble.") }
        } else { print("      node_data_tibble is empty (0 rows).") }
      } else { error_in_tibble_extract <<- "node_slice_graph was not a tbl_graph." }
    }, error = function(e_tibble) { error_in_tibble_extract <<- e_tibble$message })
    if (!is.null(error_in_tibble_extract)) { print(paste("    ERROR extracting/inspecting node data:", error_in_tibble_extract)) }
    print(paste("  Step 2: Attempting igraph::incident_edges for node", node_idx_int))
    incident_edges_list_result <- tryCatch({
      igraph::incident_edges(graph = network_obj, v = node_idx_int, mode = "all")
    }, error = function(e) { e })
    if (inherits(incident_edges_list_result, "error")) {
      print(paste("    ERROR from igraph::incident_edges for", node_label, "node:", incident_edges_list_result$message))
      return(incident_edges_sf_final)
    }
    if (is.null(incident_edges_list_result) || length(incident_edges_list_result) == 0 || length(incident_edges_list_result[[1]]) == 0) {
      print(paste("    igraph::incident_edges returned no incident edges for", node_label, "node."))
      return(incident_edges_sf_final)
    }
    edge_indices <- as.integer(incident_edges_list_result[[1]])
    print(paste("    igraph::incident_edges for", node_label, "found indices:", paste(edge_indices, collapse = ", ")))
    if(any(is.na(edge_indices))) { edge_indices <- edge_indices[!is.na(edge_indices)] }
    if (length(edge_indices) == 0) { print("    No valid edge indices after NA removal."); return(incident_edges_sf_final) }
    if (!all(edge_indices > 0 & edge_indices <= total_edge_count)) { 
      print(paste("    ERROR: Some edge indices are out of bounds for graph with", total_edge_count, "edges. Indices:", paste(edge_indices, collapse=",")))
      return(incident_edges_sf_final)
    }
    print("    Step 3.1: Edge indices appear valid. Slicing edges from network_obj...")
    incident_edges_sf_final <- tryCatch({ 
      edges_graph_slice <- network_obj %>% tidygraph::activate(edges) %>% dplyr::slice(edge_indices)
      print("      Edge slice successful. Class of 'edges_graph_slice':"); print(class(edges_graph_slice))
      activated_edges_tibble <- edges_graph_slice %>% tidygraph::activate(edges) %>% tibble::as_tibble()
      print("      Class of 'activated_edges_tibble':"); print(class(activated_edges_tibble))
      print("      Names of 'activated_edges_tibble':"); print(names(activated_edges_tibble))
      print("      Is 'activated_edges_tibble' an sf object itself?"); print(inherits(activated_edges_tibble, "sf"))
      if (inherits(activated_edges_tibble, "sf")) {
        print("      'activated_edges_tibble' is already sf. Using directly.")
        activated_edges_tibble 
      } else {
        print("      WARNING: 'activated_edges_tibble' is not directly an 'sf' object. Checking for geometry column to attempt st_as_sf().")
        sfc_cols_in_edges_tibble <- sapply(activated_edges_tibble, function(col) inherits(col, "sfc"))
        if (any(sfc_cols_in_edges_tibble)) {
          geom_col_name <- names(activated_edges_tibble)[sfc_cols_in_edges_tibble][1]
          print(paste("      Geometry column '", geom_col_name, "' found in 'activated_edges_tibble'. Attempting st_as_sf()."))
          sf::st_as_sf(activated_edges_tibble, sf_column_name = geom_col_name) 
        } else {
          print("      ERROR: No sfc geometry column in 'activated_edges_tibble'. Cannot convert to sf.")
          sf::st_sf(geometry = sf::st_sfc()) 
        }
      }
    }, error = function(e_slice_conv) {
      print(paste("    ERROR during edge slicing/conversion for", node_label, "node:", e_slice_conv$message))
      sf::st_sf(geometry = sf::st_sfc()) 
    })
    if(nrow(incident_edges_sf_final) > 0 && "highway" %in% names(incident_edges_sf_final)) { 
      print(paste("  Highway types of successfully processed incident edges for",node_label,"Node:", paste(unique(incident_edges_sf_final$highway), collapse = ", ")))
    } else { 
      print(paste("  No final incident edges with 'highway' attribute processed for", node_label, "Node."))
    }
    return(incident_edges_sf_final) 
  } # End of process_incident_edges function
  
  observeEvent(input$calculate_path, {
    req(rv$start_point_sf, rv$end_point_sf) 
    
    # --- Test empty sf creation at start of observeEvent (using working method) ---
    print("Testing empty sf creation at start of observeEvent...")
    test_empty_sf_ok <- FALSE; rv$path_length_val <- NULL # Reset path length here
    tryCatch({
      test_empty_sf_obj <- sf::st_sf(geometry = sf::st_sfc()) 
      print("  sf::st_sf(geometry = sf::st_sfc()) test OK. Class of result:"); print(class(test_empty_sf_obj))
      test_empty_sf_ok <- TRUE
    }, error = function(e_test_sf) {
      print(paste("  CRITICAL ERROR in sf::st_sf(geometry = sf::st_sfc()) test:", e_test_sf$message))
      showNotification(paste("Critical sf creation test failed:", e_test_sf$message), type="error", duration = 10)
    })
    if (!test_empty_sf_ok) { print("Aborting calculate_path due to critical sf creation test failure."); return() }
    # --- END TEST ---
    
    start_sfg <- rv$start_point_sf[[1]]; end_sfg <- rv$end_point_sf[[1]]
    start_coords_for_marker <- sf::st_coordinates(start_sfg); end_coords_for_marker <- sf::st_coordinates(end_sfg)
    if(is.null(start_coords_for_marker) || nrow(start_coords_for_marker) == 0 || is.null(end_coords_for_marker) || nrow(end_coords_for_marker) == 0) {
      showNotification("Start or end coordinates are invalid for placing markers.", type = "error", duration = 5); return()
    }
    leaflet::leafletProxy("map") %>% leaflet::clearMarkers() %>% leaflet::clearShapes() %>%  
      leaflet::addAwesomeMarkers(lng = start_coords_for_marker[1,1], lat = start_coords_for_marker[1,2], label = "Start", icon = leaflet::awesomeIcons(icon = "play", library = "fa", markerColor = "green")) %>%
      leaflet::addAwesomeMarkers(lng = end_coords_for_marker[1,1], lat = end_coords_for_marker[1,2], label = "End", icon = leaflet::awesomeIcons(icon = "stop", library = "fa", markerColor = "red"))
    
    id_notify <- showNotification("Fetching OSM data (car streets) and calculating shortest path...", duration = NULL, type = "message", closeButton = FALSE) 
    on.exit(removeNotification(id_notify), add = TRUE) 
    
    print("--- Calculating Path: New Request (Car Streets Only, 'street_network_obj' used) ---")
    
    tryCatch({ # OUTERMOST TRY-CATCH
      start_coords_val <- sf::st_coordinates(rv$start_point_sf); end_coords_val <- sf::st_coordinates(rv$end_point_sf)
      print(paste("Start Coords (Lon, Lat):", round(start_coords_val[1,1],5), ",", round(start_coords_val[1,2],5)))
      print(paste("End Coords (Lon, Lat):", round(end_coords_val[1,1],5), ",", round(end_coords_val[1,2],5)))
      dx <- start_coords_val[1,1] - end_coords_val[1,1]; dy <- start_coords_val[1,2] - end_coords_val[1,2] 
      dist_deg_approx <- sqrt(dx^2 + dy^2)
      print(paste("Approx. distance between points (degrees):", round(dist_deg_approx, 5)))
      buffer_dist_deg <- 0.04 + (dist_deg_approx * 0.3); MAX_BUFFER_DEG <- 0.5 
      if (buffer_dist_deg > MAX_BUFFER_DEG) { buffer_dist_deg <- MAX_BUFFER_DEG; showNotification(paste("Points are very far apart; query area capped. Buffer set to:", round(MAX_BUFFER_DEG,3), "degrees."), type = "warning", duration = 7)}
      print(paste("Calculated buffer distance (degrees):", round(buffer_dist_deg, 5)))
      combined_points_geom <- sf::st_union(rv$start_point_sf, rv$end_point_sf); bbox_from_points <- sf::st_bbox(combined_points_geom) 
      manual_xmin <- unname(bbox_from_points["xmin"]) - buffer_dist_deg; manual_ymin <- unname(bbox_from_points["ymin"]) - buffer_dist_deg
      manual_xmax <- unname(bbox_from_points["xmax"]) + buffer_dist_deg; manual_ymax <- unname(bbox_from_points["ymax"]) + buffer_dist_deg
      actual_query_bbox_vector <- c(manual_xmin, manual_ymin, manual_xmax, manual_ymax)
      print(paste("MANUALLY Calculated Query Bounding Box (xmin, ymin, xmax, ymax):", round(actual_query_bbox_vector[1],4), ",", round(actual_query_bbox_vector[2],4), ",", round(actual_query_bbox_vector[3],4), ",", round(actual_query_bbox_vector[4],4)))
      if (actual_query_bbox_vector[1] >= actual_query_bbox_vector[3] || actual_query_bbox_vector[2] >= actual_query_bbox_vector[4]) { 
        showNotification("Invalid MANUALLY calculated bounding box for OSM query.", type = "error", duration = 10); print("ERROR: Invalid MANUALLY calculated bounding box for OSM query."); return()
      }
      highway_types_for_cars <- c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "service", "motorway_link", "trunk_link", "primary_link", "secondary_link", "tertiary_link", "road")
      print(paste("Fetching these CAR-SPECIFIC highway types:", paste(highway_types_for_cars, collapse=", ")))
      print("Fetching OSM data using MANUALLY calculated bbox...")
      osm_query <- osmdata::opq(bbox = actual_query_bbox_vector, timeout = 90) %>% osmdata::add_osm_feature(key = "highway", value = highway_types_for_cars ) %>% osmdata::osmdata_sf()
      osm_lines <- osm_query$osm_lines
      if (is.null(osm_lines) || nrow(osm_lines) == 0) {
        showNotification("No CAR street data found...", type = "warning", duration = 7); print("WARNING: No CAR street data (osm_lines) found..."); rv$path_data <- NULL; rv$path_length_val <- NULL; return()
      }
      print(paste("Number of CAR OSM lines fetched:", nrow(osm_lines)))
      
      print("Building sfnetwork (from car streets) and cleaning topology...")
      print("Debug A1: Before as_sfnetwork"); net_tmp1 <- sfnetworks::as_sfnetwork(osm_lines, directed = FALSE)
      print("Debug A2: Before st_set_crs"); net_tmp2 <- sf::st_set_crs(net_tmp1, 4326)
      print("Debug A3: Before convert(to_spatial_subdivision)"); net_tmp3 <- tidygraph::convert(net_tmp2, sfnetworks::to_spatial_subdivision) 
      print("Debug A4: Before activate(edges)"); net_tmp4 <- tidygraph::activate(net_tmp3, "edges") 
      print("Debug A5: Before mutate(weight)"); net_tmp5 <- dplyr::mutate(net_tmp4, weight = sfnetworks::edge_length())
      
      street_network_obj <- net_tmp5 
      print("Debug A6: Network 'street_network_obj' fully constructed.")
      print(paste("Debug A6_class of 'street_network_obj' variable:", paste(class(street_network_obj), collapse=", ")))
      
      graph_node_count <- NULL; graph_edge_count <- NULL
      print("Debug B0: About to attempt graph_order/size on street_network_obj")
      print(paste("Is 'street_network_obj' a tbl_graph?", inherits(street_network_obj, "tbl_graph")))
      print(paste("Is 'street_network_obj' an igraph?", inherits(street_network_obj, "igraph")))
      
      graph_node_count <- tryCatch({
        print("Attempting: count <- igraph::gorder(street_network_obj)"); count <- igraph::gorder(street_network_obj) 
        print(paste("Debug B1: igraph::gorder(street_network_obj) successful. Node count:", count)); count
      }, error = function(e_ig) { print(paste("ERROR in igraph::gorder(street_network_obj):", e_ig$message)); NULL }) 
      
      if (!is.null(graph_node_count)) {
        print("Debug B2: About to attempt graph_size on street_network_obj")
        graph_edge_count <- tryCatch({
          print("Attempting: count_e <- igraph::gsize(street_network_obj)"); count_e <- igraph::gsize(street_network_obj)
          print(paste("Debug B3: igraph::gsize(street_network_obj) successful. Edge count:", count_e)); count_e
        }, error = function(e_ig_size) { print(paste("ERROR in igraph::gsize(street_network_obj):", e_ig_size$message)); NULL })
      } else { print("Skipping graph_size because graph_order failed or returned NULL.") }
      
      if (!is.null(graph_node_count) && !is.null(graph_edge_count)) { print(paste("Network (car streets) created. Nodes:", graph_node_count, "Edges:", graph_edge_count))
      } else { print("Network node/edge counts could not be determined due to earlier errors.") }
      if(is.null(graph_node_count) || is.null(graph_edge_count)) { 
        showNotification("Failed to determine network structure (node/edge count).", type="error", duration=7); print("ERROR: graph_node_count or graph_edge_count is NULL."); rv$path_length_val <- NULL; return() 
      }
      
      net_nodes_table <- street_network_obj %>% tidygraph::activate(nodes) %>% tibble::as_tibble() 
      net_nodes_with_comp <- street_network_obj %>% tidygraph::activate(nodes) %>% dplyr::mutate(component = tidygraph::group_components()) 
      num_components <- length(unique(net_nodes_with_comp %>% dplyr::pull(component)))
      print(paste("Number of connected components in the CAR network:", num_components))
      
      activated_net_nodes <- street_network_obj %>% tidygraph::activate(nodes) 
      start_node_idx <- sf::st_nearest_feature(rv$start_point_sf, activated_net_nodes)
      end_node_idx <- sf::st_nearest_feature(rv$end_point_sf, activated_net_nodes)
      print(paste("Snapped Start Node Index (to CAR network):", start_node_idx)); print(paste("Snapped End Node Index (to CAR network):", end_node_idx))
      if (is.na(start_node_idx) || is.na(end_node_idx)) {
        showNotification("Could not snap start or end point to the CAR street network.", type = "error", duration = 7); print("ERROR: Could not snap start or end point to CAR network."); rv$path_length_val <- NULL; return()
      }
      
      incident_edges_start <- process_incident_edges("START", as.integer(start_node_idx), street_network_obj, graph_node_count, graph_edge_count)
      incident_edges_end   <- process_incident_edges("END",   as.integer(end_node_idx),   street_network_obj, graph_node_count, graph_edge_count)
      print(paste("Rows in incident_edges_start:", nrow(incident_edges_start)))
      print(paste("Rows in incident_edges_end:", nrow(incident_edges_end)))
      
      if (num_components > 1) {
        start_node_comp_df <- net_nodes_with_comp %>% tidygraph::activate(nodes) %>% dplyr::slice(as.integer(start_node_idx)); start_node_comp <- start_node_comp_df %>% dplyr::pull(component)
        end_node_comp_df <- net_nodes_with_comp %>% tidygraph::activate(nodes) %>% dplyr::slice(as.integer(end_node_idx)); end_node_comp <- end_node_comp_df %>% dplyr::pull(component)
        print(paste("Start Node Component ID (CAR network):", ifelse(length(start_node_comp) > 0, start_node_comp, "NA")))
        print(paste("End Node Component ID (CAR network):", ifelse(length(end_node_comp) > 0, end_node_comp, "NA")))
        if (length(start_node_comp) > 0 && length(end_node_comp) > 0 && start_node_comp != end_node_comp) { print("CRITICAL: Start and End nodes are in DIFFERENT components on CAR network. Pathfinding will fail.")
        } else if (length(start_node_comp) > 0 && length(end_node_comp) > 0 && start_node_comp == end_node_comp) { print("INFO: Start and End nodes are in the SAME component on CAR network.")
        } else { print("WARNING: Could not determine component for one or both nodes on CAR network.") }
      } else if (num_components == 1) { print("INFO: CAR Network has only 1 component. Nodes should be connectable if they exist.") }
      
      if (start_node_idx == end_node_idx) {
        showNotification("Start and end points are snapped to the same CAR network node.", type = "warning", duration = 7); print("WARNING: Start and end points snapped to the same CAR network node.")
        snapped_point_geom <- street_network_obj %>% tidygraph::activate(nodes) %>% dplyr::slice(as.integer(start_node_idx)) %>% sf::st_geometry() 
        leaflet::leafletProxy("map") %>% leaflet::addCircles(data = snapped_point_geom, radius = 20, color = "blue", label = "Snapped Point (Start/End are same)", layerId="snapped_point"); rv$path_length_val <- 0; return() # Path length is 0
      }
      
      print("Calculating shortest path with st_network_paths (on CAR network)...")
      paths_output <- sfnetworks::st_network_paths(street_network_obj, from = as.integer(start_node_idx), to = as.integer(end_node_idx), weights = "weight")
      print("--- Debugging paths_output (CAR network, after st_network_paths) ---")
      if(nrow(paths_output) == 0 || (is.list(paths_output$node_paths) && length(paths_output$node_paths[[1]]) == 0) ) { print("INFO: st_network_paths returned no path or an empty path (CAR network).") }
      
      path_found_and_processed <- FALSE; rv$path_data <- NULL 
      if (inherits(paths_output, "sf") && nrow(paths_output) > 0) {
        current_path_geom <- sf::st_geometry(paths_output[1, ]); if (!is.null(current_path_geom) && !sf::st_is_empty(current_path_geom)) {
          rv$path_data <- current_path_geom; path_length_m <- sf::st_length(rv$path_data)
          path_length_mi_value <- units::set_units(path_length_m, "mi")
          rv$path_length_val <- round(units::drop_units(path_length_mi_value), 2)
          average_speed_mph <- 35
          rv$trip_duration_sec <- round((rv$path_length_val / average_speed_mph) * 3600)
          load_and_predict_fare()
          leaflet::leafletProxy("map") %>% leaflet::addPolylines(data = rv$path_data, color = "blue", weight = 5, opacity = 0.8, layerId = "calculated_path", label = paste("Shortest Path:", rv$path_length_val, "km")) %>%
            leaflet::fitBounds(lng1 = sf::st_bbox(rv$path_data)[1], lat1 = sf::st_bbox(rv$path_data)[2], lng2 = sf::st_bbox(rv$path_data)[3], lat2 = sf::st_bbox(rv$path_data)[4])
          removeNotification(id_notify); showNotification(paste("Shortest path found:", rv$path_length_val, "km"), type = "message", duration = 8); path_found_and_processed <- TRUE
        }
      } else if (is.data.frame(paths_output) && "edge_paths" %in% names(paths_output) && nrow(paths_output) > 0) {
        if (is.list(paths_output$edge_paths) && length(paths_output$edge_paths) > 0 && !is.null(paths_output$edge_paths[[1]]) && length(paths_output$edge_paths[[1]]) > 0) { 
          id_recon <- showNotification("Path is tibble, reconstructing from edges...", duration = NULL, type = "message", closeButton = FALSE); on.exit(removeNotification(id_recon), add = TRUE) 
          path_edge_indices <- paths_output$edge_paths[[1]]
          path_edges <- street_network_obj %>% tidygraph::activate(edges) %>% dplyr::slice(as.integer(path_edge_indices)) %>% sf::st_as_sf()
          if (nrow(path_edges) > 0) {
            combined_geom <- sf::st_combine(path_edges$geometry); current_path_geom <- sf::st_sfc(combined_geom, crs = sf::st_crs(street_network_obj)) 
            if (!sf::st_is_empty(current_path_geom)) {
              rv$path_data <- current_path_geom; path_length_m <- sum(sf::st_length(path_edges)) 
              path_length_mi_value  <- units::set_units(path_length_m, "mi")
              rv$path_length_val    <- round(units::drop_units(path_length_mi_value), 2)
              
              average_speed_mph     <- 35
              rv$trip_duration_sec  <- round((rv$path_length_val / average_speed_mph) * 3600) # STORE LENGTH
              load_and_predict_fare()
              leaflet::leafletProxy("map") %>% leaflet::addPolylines(data = rv$path_data, color = "purple", weight = 5, opacity = 0.8, layerId = "calculated_path", label = paste("Shortest Path:", rv$path_length_val, "km")) %>%
                leaflet::fitBounds(lng1 = sf::st_bbox(rv$path_data)[1], lat1 = sf::st_bbox(rv$path_data)[2], lng2 = sf::st_bbox(rv$path_data)[3], lat2 = sf::st_bbox(rv$path_data)[4])
              removeNotification(id_recon); removeNotification(id_notify); showNotification(paste("Shortest path found:", rv$path_length_val, "km"), type = "message", duration = 8); path_found_and_processed <- TRUE
            }
          }
          if (!path_found_and_processed) { print("INFO: Path reconstruction from tibble failed or path_edges was empty (CAR network)."); if(exists("id_recon")) removeNotification(id_recon); rv$path_length_val <- NULL; }
        } else { print("INFO: paths_output is a tibble, but no actual edge_paths found (likely no path, CAR network)."); rv$path_length_val <- NULL; }
      }
      if (!path_found_and_processed) {
        showNotification("Could not find or process a valid path (CAR network). Network may be fragmented or points too far apart for CAR routing.", type = "warning", duration = 8)
        print("FINAL_DEBUG: Path not found or processed successfully (CAR network)."); leaflet::leafletProxy("map") %>% leaflet::clearShapes(); rv$path_length_val <- NULL;
      }
    }, error = function(e) { # OUTERMOST TRY-CATCH HANDLER
      showNotification(paste("Error calculating path (CAR network):", e$message), type = "error", duration = 10)
      print(paste("ERROR in tryCatch (CAR network) - Outer Handler:", e$message)); 
      rv$path_data <- NULL; rv$path_length_val <- NULL; leaflet::leafletProxy("map") %>% leaflet::clearShapes() 
    })
    print("--- Path Calculation Attempt Finished (CAR Streets Only) ---") 
  })
  
  get_datetime_features <- function(time_stamp = Sys.time()) {
    lt <- as.POSIXlt(time_stamp)
    
    year   <- lt$year + 1900
    month  <- lt$mon  + 1
    day    <- lt$mday
    # POSIXlt$wday: 0 = Sunday … 6 = Saturday.  Shift to 1–7:
    weekday <- (lt$wday %% 7) + 1
    
    time_decimal <- lt$hour + lt$min / 60 + lt$sec / 3600
    
    list(
      year          = year,
      month         = month,
      day           = day,
      weekday       = weekday,
      time_decimal  = time_decimal
    )
  }
  
  
  load_and_predict_fare <- function() {
    ## 0.  Preconditions ------------------------------------------------
    req(rv$trip_duration_sec,         # set after routing
        rv$path_length_val,
        rv$start_point_sf,
        rv$end_point_sf)
    
    ## 1.  Load model (and scaler if available) -------------------------
    model_path <- "model/fare_xgb.model"
    if (!file.exists(model_path)) {
      warning("XGBoost model file not found: ", model_path)
      rv$predicted_fare <- NA_real_
      return(NA_real_)
    }
    xgb_model <- xgboost::xgb.load(model_path)
    
    # optional scaling object
    scaler_path <- "scale_params.rds"
    scaler <- if (file.exists(scaler_path)) readRDS(scaler_path) else NULL
    
    ## 2.  Assemble feature row ----------------------------------------
    dt  <- get_datetime_features()                # year, month, day, weekday, time_decimal
    
    start_xy <- sf::st_coordinates(rv$start_point_sf)
    end_xy   <- sf::st_coordinates(rv$end_point_sf)
    
    new_row <- data.frame(
      trip_seconds                = rv$trip_duration_sec,
      trip_miles                  = rv$path_length_val,
      year                        = dt$year,
      month                       = dt$month,
      weekday                     = dt$weekday,
      day                         = dt$day,
      time_decimal                = dt$time_decimal,
      pickup_centroid_latitude    = start_xy[1, 2],
      pickup_centroid_longitude   = start_xy[1, 1],
      dropoff_centroid_latitude   = end_xy[1, 2],
      dropoff_centroid_longitude  = end_xy[1, 1]
    )
    
    ## 3.  Apply same preprocessing as training ------------------------
    if (!is.null(scaler)) {
      new_row <- predict(scaler, new_row)
    }
    
    ## 4.  Predict ------------------------------------------------------
    dmat <- xgboost::xgb.DMatrix(as.matrix(new_row))
    pred <- as.numeric(predict(xgb_model, dmat))
    
    ## 5.  Store & return ----------------------------------------------
    rv$predicted_fare <- pred
    return(pred)
  }
}