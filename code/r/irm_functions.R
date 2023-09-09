# temporary function to load raster data

# where: data_file_prefix is the file prefix showing the theme
#        raster_name is the name to be given to the raster layer or brick
#        raster_or_brick has two possible values, either raster for a single layer or brick for multiple layers


#load_raster_data <-
#  function(data_file_prefix,
#           raster_name,
#           rast_or_brick) {
#    raster_filename <-
#      as.character(paste("spatial_data/input/",
#                         data_file_prefix,
#                         ".tif",
#                         sep = ""))
#    raster_name <-  rast_or_brick(here(raster_filename))
#    return(raster_name)
#  }

# in terra there is no brick, just the same rast method
load_raster_data <-
  function(data_file_prefix,
           raster_name) {
    raster_filename <-
      as.character(paste("spatial_data/input/",
                         data_file_prefix,
                         ".tif",
                         sep = ""))
    raster_name <-  rast(here(raster_filename))
    return(raster_name)
  }

# temporary function to load vector data

# where: data_file_prefix is the file prefix showing the theme
#        raster_name is the name to be given to the raster layer or brick
#        raster_or_brick has two possible values, either raster for a single layer or brick for multiple layers

load_vector_data <-
  function(data_file_prefix) {
    vector_filename <-
      as.character(paste(
        "spatial_data/input/",
        data_file_prefix,
        ".geojson",
        sep = ""
      ))
    vector_name <-  vect(here(vector_filename))
    return(vector_name)
  }
        
# temporary function to load vector data and re-project from WGS84 (default for geojson) to chosen coordinate reference system

# where: data_file_prefix is the file prefix showing the theme
#        raster_name is the name to be given to the raster layer or brick
#        raster_or_brick has two possible values, either raster for a single layer or brick for multiple layers

load_crs_vector_data <-
  function(data_file_prefix, crs) {
    vector_filename <-
      as.character(paste(
        "spatial_data/input/",
        data_file_prefix,
        ".geojson",
        sep = ""
      ))
    vector_name <-  st_transform(st_read(here(vector_filename)), crs)
    
    return(vector_name)
  }


# temporary function for extracting coordinates and fields from sf to tibble / data frame
# from jm london @ https://github.com/r-spatial/sf/issues/231

sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}



# Alternative for SpatRasters using terra - 
# this is far quicker than the raster method and in principle only requires two inputs:
# the SpatRaster to be projected (x) and a template SpatRaster 
# the resolution and crs from the template are used

# optional arguments are the method (y) bilinear or ngb, mask

raster_project <- function(x, y) {
  raster_proj <- terra::project(
    x,
    rast_mask_proj,
    y)
  return(raster_proj)
  gc()
}

# temporary function for aggregating and resampling

# where: w is the original raster data
#        x is the calculated aggregation value
#        y is the method used in the aggregate function
#        z is the method used in the resample function

agg_resample <- function(w, x, y, z) {
  mask_x <- aggregate(w,
                      fact = x,
                      fun = y,
                      expand = TRUE) %>%
    terra::resample(rast_mask_proj, method = z) %>%
    terra::mask(rast_mask_proj)
  return(mask_x)
  gc()
}

# temporary function for determining growth periods which do not fit in a calendar year
# based on the .growth_period function in the irm package

# where: 

.growth_period_long <- function (day_begin, day_end, num_years) 
{
  days <- 1:365
  day_months <- days %>% as.character %>% as.Date("%j") %>% 
    format("%m") %>% as.integer 
  months <- rep.int(day_months, num_years)
  days <- round(day_begin):round(day_end)
  tabulate(bin = months[days], nbins = 12L)* num_years/tabulate(bin = months, 
                                                                nbins = 12L)
}



# https://github.com/tidyverse/lubridate/issues/617

#####################################################################################
#' Format date as yearly or monthly dekad (10-day period)
#'
#' @param x date to convert to dekad
#' @param type dekad of month (1:3) or year (1:36)
#' @inheritDotParams base::as.Date
#' @return integer dekad
#' @importFrom lubridate day
#' @examples
#' dekad(Sys.Date())
#' @export
#' 

dekad <- function(x, type = c("month", "year"), ...) {
  type <- match.arg(type)
  x <- as.Date(x, ...)
  res <- ifelse(day(x) > 20,  3, ifelse(day(x) > 10, 2, 1))
  if(type == "year") res <- month(x)*3 + res - 3
  return(res)
}



# temporary function for determining growth periods which do not fit in a calendar year
# based on the .growth_period function in the irm package

# where: 

.growth_period_long_dekad <- function (day_begin, day_end, num_years) 
{
  days <- 1:365
  day_dekads <- days %>% as.character %>% as.Date("%j") %>% 
    dekad(type = "year") %>% as.integer 
  dekads <- rep.int(day_dekads, num_years)
  days <- round(day_begin):round(day_end)
  tabulate(bin = dekads[days], nbins = 36L)* num_years/tabulate(bin = dekads, 
                                                                nbins = 36L)
}

# developed in 'sowing_date_test_4_CRASA.rmd'
# takes a data frame with fields for day_begin and day_end
# num_years is predefined
# produces a matrix of proportion of each month in growing period for each cell
# afterwards needs to be transposed and binded to original data frame, 
# converted to sf point object and rasterized

.growth_period_long_tbl <- function(x, day_begin, day_end, num_years) {
  
  days <- 1:365
  day_months <- days %>% as.character %>% as.Date("%j") %>% 
    format("%m") %>% as.integer
  months <- rep.int(day_months, num_years)
  days <- round(x[day_begin]):round(x[day_end])
  tabulate(bin = months[days], nbins = 12L)* num_years/tabulate(bin = months, 
                                                     nbins = 12L)
  
}



# developed in 'sowing_date_test_4_CRASA.rmd'
# takes a data frame with fields for day_begin and day_end
# num_years is predefined
# produces a matrix of proportion of each dekad in growing period for each cell
# afterwards needs to be transposed and binded to original data frame, 
# converted to sf point object and rasterized


.growth_period_long_dekad_tbl <- function (x, day_begin, day_end, num_years) {
  days <- 1:365
  day_dekads <- days %>% as.character %>% as.Date("%j") %>% 
    dekad(type = "year") %>% as.integer 
  dekads <- rep.int(day_dekads, num_years)
  days <- round(x[day_begin]):round(x[day_end])
  tabulate(bin = dekads[days], nbins = 36L)* num_years/tabulate(bin = dekads, 
                                                                nbins = 36L)
}

  
  
  
# temporary function for evaluating the lowest level biophysical rule bases

# where: theme_rbs_tbl is the tibble that contains the criteria to be evaluated
#        n_theme is the number of criteria per theme

evaluate_lowest_rb <- function(theme_rbs_tbl, n_theme) {
  
  eval_low_list <- vector(mode = "list", length = 0)
  fp_list <- vector(mode = "list", length = 0)
  rb_list <- vector(mode = "list", length = 0)
  cond_data_list <- vector(mode = "list", length = 0)
  
  for (i in 1:n_theme) {
    if (!is.na(theme_rbs_tbl[i, 18])) {
      # for those criteria with three proposition values
      # get  the 3 proposition levels
      prop_levels <-
        c(
          as.character(theme_rbs_tbl[i, 16]),
          as.character(theme_rbs_tbl[i, 17]),
          as.character(theme_rbs_tbl[i, 18])
        )
      
      # get  the 3 conclusion levels
      conc_levels <-
        c(
          as.character(theme_rbs_tbl[i, 19]),
          as.character(theme_rbs_tbl[i, 20]),
          as.character(theme_rbs_tbl[i, 21])
        )
      # get  the 2 crossover points
      cross_points <-
        c(theme_rbs_tbl[i, 6],
          theme_rbs_tbl[i, 7], recursive = TRUE)
      
      # get  the 2 transition widths
      trans_width <-
        c(theme_rbs_tbl[i, 8],
          theme_rbs_tbl[i, 9], recursive = TRUE)
      
      # get  the 2 xlims for the plot
      plot_xlim <-
        c((theme_rbs_tbl[i, 6] - theme_rbs_tbl[i, 8]),
          (theme_rbs_tbl[i, 7] + theme_rbs_tbl[i, 9]),
          recursive = TRUE
        )
      
      
    } else {
      # for those criteria with two proposition values
      # get  the 2 proposition levels
      prop_levels <-
        c(as.character(theme_rbs_tbl[i, 16]),
          as.character(theme_rbs_tbl[i, 17]))
      
      # get  the 2 conclusion levels
      conc_levels <-
        c(as.character(theme_rbs_tbl[i, 19]),
          as.character(theme_rbs_tbl[i, 20]))
      
      # get  the 1 crossover point
      cross_points <-
        c(theme_rbs_tbl[i, 6], recursive = TRUE)
      
      # get  the 1 transition width
      trans_width <-
        c(theme_rbs_tbl[i, 8], recursive = TRUE)
      
      # get  the 1 xlim for the plot
      plot_xlim <-
        c((theme_rbs_tbl[i, 6] - theme_rbs_tbl[i, 8]),
          (theme_rbs_tbl[i, 6] + theme_rbs_tbl[i, 8]),
          recursive = TRUE
        )
      
    }
    assign(
      paste0("fp_", theme_rbs_tbl[i, 3]),
      LinearFuzzyPartition(
        level =  prop_levels,
        crossoverPoint  = cross_points,
        transitionWidth = trans_width
      )
    )
    
    fp_list <- rlist::list.append(fp_list, get(paste0("fp_", theme_rbs_tbl[i, 3])))
    
                          
                          plot_fp <-
      plot(
        get(paste0("fp_", theme_rbs_tbl[i, 3])),
        xlim = plot_xlim,
        xlab = as.character(theme_rbs_tbl[i, 4]),
        title = "fuzzy partition"
      )
    print(plot_fp)
    
    # construct the rule bases
    
    
    # set the proposition name
    Prop_name <-
      paste0(as.character(theme_rbs_tbl[i, 3]))
    # set the conclusion name
    Conc_name <-
      paste0(as.character(theme_rbs_tbl[i, 3]),
             "_o")
    
    # thanks to https://stackoverflow.com/questions/45741498/add-column-in-tibble-with-variable-column-name for the following dynamic assignment of variable names in a tibble
    
    # set the proposition values
    p_tib <- tibble(!!Prop_name := c(prop_levels))
    # set the conclusion values
    c_tib <- tibble(!!Conc_name := c(conc_levels))
    
    # new proposition and conclusion class objects are made here
    new_prop <- new("Proposition", table = p_tib)
    new_conc <- new("Conclusion", table = c_tib)
    
    assign(paste0("rb_",
                  theme_rbs_tbl[i, 3]),
           
           RuleBase(new_prop, new_conc))
    
    rb_list <- rlist::list.append(rb_list, get(paste0("rb_",
                                                    theme_rbs_tbl[i, 3])))
    
    print(get(paste0("rb_",
                     theme_rbs_tbl[i, 3])))
    
    # apply the fuzzy partitions to the data
    
    
    # first construct the predict function with all arguments as text
    
    x <- paste0(
      "predict(rb_",
      theme_rbs_tbl[i, 3],
      ", newdata = cond_data,",
      theme_rbs_tbl[i, 3],
      "=",
      "fp_",
      theme_rbs_tbl[i, 3],
      ")"
    )
    
    # evaluate the rule base and create the fpm
    
    assign(paste0("fpm_",
                  theme_rbs_tbl[i, 3]), eval(parse(text = x)))
    
    # get the membership values from the fpm
    
    o_col <- getMembership(get(paste0("fpm_",
                                      theme_rbs_tbl[i, 3]))$optimal)
    s_col <- getMembership(get(paste0("fpm_",
                                      theme_rbs_tbl[i, 3]))$suboptimal)
    cond_data <-
      mutate(cond_data,!!as.character(paste0(theme_rbs_tbl[i, 3], "_o")) := o_col)
    cond_data <-
      mutate(cond_data,!!as.character(paste0(theme_rbs_tbl[i, 3], "_s")) := s_col)
    
    cond_data_list <- rlist::list.append(cond_data_list, cond_data)
    
    n = 2
    fpm_conc_var <-
      c(as.character(paste0(theme_rbs_tbl[i, 3], "_o")), as.character(paste0(theme_rbs_tbl[i, 3], "_s")), "x" , "y")
    fpm_conc_name <-
      c(as.character(paste0(theme_rbs_tbl[i, 4], " optimal")), as.character(paste0(theme_rbs_tbl[i, 4], " suboptimal")))
    fpm_plot_title <-
      paste0(theme_rbs_tbl[i, 4], "\noptimality\nmembership")
    
    
    
    plot_fpm <-
      rasterize_plot_fpm_new(n, fpm_conc_var, fpm_conc_name, cond_data, fpm_plot_title)
    print(plot_fpm)
  
  }
  #return(cond_data)
  eval_low_list <- rlist::list.append(eval_low_list, fp_list, rb_list, cond_data_list)
  return(eval_low_list)
}  

# temporary function for evaluating the lowest level biophysical rule bases

# where: theme_rbs_tbl is the tibble that contains the criteria to be evaluated
#        n_theme is the number of criteria per theme

evaluate_lowest_rb_i2 <- function(theme_rbs_tbl, n_theme) {
  eval_low_list <- vector(mode = "list", length = 0)
  fp_list <- vector(mode = "list", length = 0)
  rb_list <- vector(mode = "list", length = 0)
  cond_data_i2_list <- vector(mode = "list", length = 0)
  
  for (i in 1:n_theme) {
    if (!is.na(theme_rbs_tbl[i, 18])) {
      # for those criteria with three proposition values
      # get  the 3 proposition levels
      prop_levels <-
        c(
          as.character(theme_rbs_tbl[i, 16]),
          as.character(theme_rbs_tbl[i, 17]),
          as.character(theme_rbs_tbl[i, 18])
        )
      
      # get  the 3 conclusion levels
      conc_levels <-
        c(
          as.character(theme_rbs_tbl[i, 19]),
          as.character(theme_rbs_tbl[i, 20]),
          as.character(theme_rbs_tbl[i, 21])
        )
      # get  the 2 crossover points
      cross_points <-
        c(theme_rbs_tbl[i, 6],
          theme_rbs_tbl[i, 7], recursive = TRUE)
      
      # get  the 2 transition widths
      trans_width <-
        c(theme_rbs_tbl[i, 8],
          theme_rbs_tbl[i, 9], recursive = TRUE)
      
      # get  the 2 xlims for the plot
      plot_xlim <-
        c((theme_rbs_tbl[i, 6] - theme_rbs_tbl[i, 8]),
          (theme_rbs_tbl[i, 7] + theme_rbs_tbl[i, 9]),
          recursive = TRUE
        )
      
      
    } else {
      # for those criteria with two proposition values
      # get  the 2 proposition levels
      prop_levels <-
        c(as.character(theme_rbs_tbl[i, 16]),
          as.character(theme_rbs_tbl[i, 17]))
      
      # get  the 2 conclusion levels
      conc_levels <-
        c(as.character(theme_rbs_tbl[i, 19]),
          as.character(theme_rbs_tbl[i, 20]))
      
      # get  the 1 crossover point
      cross_points <-
        c(theme_rbs_tbl[i, 6], recursive = TRUE)
      
      # get  the 1 transition width
      trans_width <-
        c(theme_rbs_tbl[i, 8], recursive = TRUE)
      
      # get  the 1 xlim for the plot
      plot_xlim <-
        c((theme_rbs_tbl[i, 6] - theme_rbs_tbl[i, 8]),
          (theme_rbs_tbl[i, 6] + theme_rbs_tbl[i, 8]),
          recursive = TRUE
        )
      
    }
    assign(
      paste0("fp_", theme_rbs_tbl[i, 3]),
      LinearFuzzyPartition(
        level =  prop_levels,
        crossoverPoint  = cross_points,
        transitionWidth = trans_width
      )
    )
    
    fp_list <-
      rlist::list.append(fp_list, get(paste0("fp_", theme_rbs_tbl[i, 3])))
    
    
    plot_fp <-
      plot(
        get(paste0("fp_", theme_rbs_tbl[i, 3])),
        xlim = plot_xlim,
        xlab = as.character(theme_rbs_tbl[i, 4]),
        title = "fuzzy partition"
      )
    print(plot_fp)
    
    # construct the rule bases
    
    
    # set the proposition name
    Prop_name <-
      paste0(as.character(theme_rbs_tbl[i, 3]))
    # set the conclusion name
    Conc_name <-
      paste0(as.character(theme_rbs_tbl[i, 3]),
             "_o")
    
    # thanks to https://stackoverflow.com/questions/45741498/add-column-in-tibble-with-variable-column-name for the following dynamic assignment of variable names in a tibble
    
    # set the proposition values
    p_tib <- tibble(!!Prop_name := c(prop_levels))
    # set the conclusion values
    c_tib <- tibble(!!Conc_name := c(conc_levels))
    
    # new proposition and conclusion class objects are made here
    new_prop <- new("Proposition", table = p_tib)
    new_conc <- new("Conclusion", table = c_tib)
    
    assign(paste0("rb_",
                  theme_rbs_tbl[i, 3]),
           
           RuleBase(new_prop, new_conc))
    
    rb_list <- rlist::list.append(rb_list, get(paste0("rb_",
                                                      theme_rbs_tbl[i, 3])))
    
    print(get(paste0("rb_",
                     theme_rbs_tbl[i, 3])))
    
    # apply the fuzzy partitions to the data
    
    
    # first construct the predict function with all arguments as text
    
    x <- paste0(
      "predict(rb_",
      theme_rbs_tbl[i, 3],
      ", newdata = cond_data_i2,",
      theme_rbs_tbl[i, 3],
      "=",
      "fp_",
      theme_rbs_tbl[i, 3],
      ")"
    )
    
    # evaluate the rule base and create the fpm
    
    assign(paste0("fpm_",
                  theme_rbs_tbl[i, 3]), eval(parse(text = x)))
    
    # get the membership values from the fpm
    
    o_col <- getMembership(get(paste0("fpm_",
                                      theme_rbs_tbl[i, 3]))$optimal)
    s_col <- getMembership(get(paste0("fpm_",
                                      theme_rbs_tbl[i, 3]))$suboptimal)
    cond_data_i2 <-
      mutate(cond_data_i2, !!as.character(paste0(theme_rbs_tbl[i, 3], "_o")) := o_col)
    cond_data_i2 <-
      mutate(cond_data_i2, !!as.character(paste0(theme_rbs_tbl[i, 3], "_s")) := s_col)
    
    cond_data_i2_list <- rlist::list.append(cond_data_i2_list, cond_data_i2)
    
    n = 2
    fpm_conc_var <-
      c(as.character(paste0(theme_rbs_tbl[i, 3], "_o")), as.character(paste0(theme_rbs_tbl[i, 3], "_s")), "x" , "y")
    fpm_conc_name <-
      c(as.character(paste0(theme_rbs_tbl[i, 4], " optimal")), as.character(paste0(theme_rbs_tbl[i, 4], " suboptimal")))
    fpm_plot_title <-
      paste0(theme_rbs_tbl[i, 4], "\noptimality\nmembership")
    
    
    
    plot_fpm <-
      rasterize_plot_fpm_new(n, fpm_conc_var, fpm_conc_name, cond_data_i2, fpm_plot_title)
    print(plot_fpm)
    
  }
  #return(cond_data_i2)
  eval_low_list <-
    rlist::list.append(eval_low_list, fp_list, rb_list, cond_data_i2_list)
  return(eval_low_list)
}

  
  
# temporary function for rasterizing data frames

# where: n is the number of classes
#        df_class_var is a list of the variables assigned to each conclusion
#        df_class_name is a list of the names used for the layers of the raster brick and which will be seen in the title of each plot
#        df_data is the name of the data frame or tibble used to store the data



rasterize_data_frame <- function(n, df_class_var, df_class_name, df_data) {
  d0 <- df_data %>%
    dplyr::select(unlist(as.character(noquote(df_class_var)))) %>%
    na.omit %>%
    st_as_sf(coords = c("x", "y"))
  
  # initialise the dB rasterbrick using the first layer
  
  dB <- rasterize(d0, ra_mask, df_class_var[[1]]) %>% brick
  
  # in a loop add subsequent layers to the dB rasterbrick starting from the second layer (actually the addlayer changes dB to a rasterstack)
  
  for (i in 2:n) {
    dB <-
      addLayer(dB, rasterize(d0, ra_mask, field = df_class_var[[i]]))
  }
  
  # change dB rasterstack back to a rasterbrick
  dB <- brick(dB)
  
  # give the dB layers sensible names
  
  names(dB) <- df_class_name
  
  # plot dB rasterbrick using title
  
  return(dB) 
}


# temporary function for rasterizing and plotting fuzzy partition matrices

# where: n is the number of conclusions from the fuzzy partition matrix
#        fpm_conc_var is a list of the variables assigned to each conclusion
#        fpm_conc_name is a list of the names used for the layers of the raster brick and which will be seen in the title of each plot
#        fpm_plot_title is the title of the plot
#        df_data is the name of the data frame or tibble used to store the data



rasterize_plot_fpm <- function(n, fpm_conc_var, fpm_conc_name, df_data) {
  d0 <- df_data %>%
    dplyr::select(unlist(as.character(noquote(fpm_conc_var)))) %>%
    na.omit %>%
    st_as_sf(coords = c("x", "y"))
  
  # initialise the dB rasterbrick using the first layer
  
  dB <- rasterize(d0, rast_mask_proj, fpm_conc_var[[1]]) #%>% brick   # SpatRasters don't need to be bricks
  
  # in a loop add subsequent layers to the dB SpatRaster starting from the second layer 
  
  for (i in 2:n) {
    dB <-
      rast(list(dB, rasterize(d0, rast_mask_proj, field = fpm_conc_var[[i]])))
  }
  
  # give the dB layers sensible names
  
  names(dB) <- fpm_conc_name
  
  # plot dB using title
  
  dB %>% plot(main = fpm_plot_title)
}

# temporary function to write output to geojson

# where: x is the projected sf data
#        data_file_prefix is the prefix used to define the name of the output geojson

output_sf <- function(results_sf, data_file_prefix) {
  vector_filename <-
    as.character(paste("spatial_data/output/",
                       data_file_prefix,
                       ".geojson",
                       sep = ""))
  
  st_write(
    st_transform(results_sf,
                 crs = 4326),
    dsn = (here(vector_filename)),
    delete_layer = TRUE,
    delete_dsn = TRUE
  )
  
}

# temporary function to write output to geotiff

# where: results_raster is the projected raster data
#        data_file_prefix is the prefix used to define the name of the output tif

output_geotiff <- function(results_raster, data_file_prefix) {
  geotiff_filename <-
    as.character(paste("spatial_data/output/",
                       data_file_prefix,
                       ".tif",
                       sep = ""))
  
  writeRaster(
    results_raster,
    here(geotiff_filename),
    overwrite = TRUE,
    filetype = "GTiff"
  )
  
}

# temporary function to write stars output to geotiff

# where: results_raster is the projected raster data
#        data_file_prefix is the prefix used to define the name of the output tif

output_stars_geotiff <- function(results_raster, data_file_prefix) {
  geotiff_filename <-
    as.character(paste("spatial_data/output/",
                       data_file_prefix,
                       ".tif",
                       sep = ""))
  
  write_stars(
    results_raster,
    dsn = (here(geotiff_filename)),
    layer = 1,
    update = FALSE,
    driver = "GTiff"
  )
  
}
# temporary function to write output to csv

# where: results_sf is the projected sf data
#        data_file_prefix is the prefix used to define the name of the output csv


output_csv <- function(results_sf, data_file_prefix) {
  csv_filename <-
    as.character(paste("tab_data/output/",
                       data_file_prefix,
                       ".csv",
                       sep = ""))
  
  write_sf(
    results_sf,
    dsn = (here(csv_filename)),
    delete_layer = TRUE,
    delete_dsn = TRUE
  )
  
}

# temporary function to write output to txt

# where: text_object is the object to be written to text
#        data_file_prefix is the prefix used to define the name of the output txt


output_txt <- function(text_object, data_file_prefix) {
  txt_filename <-
    as.character(paste("tab_data/output/",
                       data_file_prefix,
                       ".txt",
                       sep = ""))
  
  write(text_object,
        file = (here(txt_filename)),
        append = FALSE)
  
}

# temporary function to create kebele level maps

# where: n is the number of maps to produce (the number of values for the suitability)
#        fill_var_list is a vector of the possible values for suitability
#        pal_col is a vector of the names of the colour palette


plot_subdiv_maps <- function(n, fill_var_list, pal_col) {
  subdiv_map_list <- list()
  
  for (i in 1:n)   {
    subdiv_map_list[[i]] <-
      ggplot() +
      geom_sf(data = v_subdiv, (aes_string(
        group = "id", fill = (fill_var_list[i])
      ))) +
      coord_sf(datum = st_crs(crs_irm)) +
      scale_fill_distiller(
        type = "seq",
        palette = pal_col[i],
        direction = 1,
        guide = "legend"
      ) +
      scale_x_continuous(
        name = "",
        labels = function(x) {
          1.0e-3 * x
        }
      ) +
      scale_y_continuous(
        name = "",
        labels = function(x) {
          1.0e-3 * x
        }
      )  +
      geom_point(
        data = d_v_ba_tri,
        mapping = aes(
          x = x,
          y = y,
          colour = ifelse(baclass == "optimal" , "green", "red")
        ),
        size = 2
      ) +
      geom_text(data = d_v_ba_tri,
                mapping = aes(
                  x = x + 800,
                  y = y + 800,
                  label = ifelse(type == "b", "b", "b/s")
                )) +
      scale_colour_identity()
  }
  return(subdiv_map_list)
}  

# old version  
plot_kebele_maps_old <- function(n, fill_var, pal_col) {
  kmap_list <- list()
  
  for (i in 1:n)   {
    kmap_list[[i]] <-
      ggplot(d_topo.df) +
      aes_string("long", "lat", group = "group", fill = fill_var_list[i]) +
      geom_polygon() +
      geom_path(color = "white") +
      coord_equal() +
      scale_fill_distiller(
        type = "seq",
        palette = pal_col[[i]],
        direction = 1,
        guide = "legend"
      ) +
      scale_x_continuous(
        name = "",
        labels = function(x) {
          1.0e-3 * x
        }
      ) +
      scale_y_continuous(
        name = "",
        labels = function(x) {
          1.0e-3 * x
        }
      ) +
      geom_point(
        data = d_vpoints,
        mapping = aes(
          x = x,
          y = y,
          colour = ifelse(suitclass == "high" , "green", "red")
        ),
        inherit.aes = FALSE,
        size = 2
      ) +
      geom_text(
        data = d_vpoints,
        mapping = aes(
          x = x + 800,
          y = y + 800,
          label = ifelse(type == "b", "b", "b/s")
        ),
        inherit.aes = FALSE
      )   +
      scale_colour_identity()
  }
  return(kmap_list)
}

# temporary function to add sub-division boundaries and labels to existing maps

# where: map1 is the existing ggplot map
#        subdiv_var is the sub-divisions name variable

# thanks to https://www.edureka.co/community/52697/change-the-order-of-multiple-legends-in-ggplot for the legend order


add_subdiv_plot <- function(map1) {
  addsubdiv_map <-       map1 +
    geom_spatvector(data = vect_subdiv,
                    colour = "dark grey",
                    fill = NA) +
    geom_spatvector(
      data = vect_subdiv_pt,
      aes(fill = subdiv_label),
      colour = "black",
      shape = 21,
      size = 2
    ) +
    geom_spatvector_text(
      data = vect_subdiv_pt,
      aes(label = id),
      colour = "black",
      size = 2,
      nudge_x = nudge_xval,
      nudge_y = nudge_yval
    ) +
    
    #scale_shape_manual(values = 1:nlevels(vect_subdiv_pt$subdiv_label)) +
    scale_fill_manual(values = 1:nlevels(vect_subdiv_pt$subdiv_label)) +
    #labs(shape = "Sub-division name") +
    labs(fill = "Sub-division name") +
    theme(legend.direction = "vertical", 
          legend.box = "horizontal",
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    guides(#alpha = guide_legend(order = 2),
      #shape = guide_legend(order = 3),
      #colour = guide_legend(order = 1)
      fill = guide_legend(order = 1)) 
  
  return(addsubdiv_map)
}

add_subdiv_simple_plot <- function(map1) {
  addsubdivsimple_map <-       map1 +
    geom_spatvector(data = vect_subdiv,
                    colour = "dark grey",
                    fill = NA) +
    theme(legend.direction = "vertical", 
          legend.box = "horizontal",
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  return(addsubdivsimple_map)
}

add_triangulation_plot <- function(map1) {
  addtriangulation_map <-       map1 +
    geom_spatvector(
      data = vect_triangulation,
      aes(fill = tri_label),
      colour = "black",
      shape = 21,
      size = 2
    ) +  
    geom_spatvector_text(
      data = vect_triangulation,
      aes(label = id),
      colour = "black",
      size = 2,
      nudge_x = nudge_xvaltri,
      nudge_y = nudge_yvaltri
    ) +
    
    scale_fill_manual(values = 1:nlevels(vect_triangulation$tri_label)) +
    labs(fill = "Comment") +
    theme(legend.direction = "vertical", 
          legend.box = "horizontal",
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    guides(#alpha = guide_legend(order = 2),
      #shape = guide_legend(order = 3),
      #colour = guide_legend(order = 1)
      fill = guide_legend(order = 1))
  
  return(addtriangulation_map)
         
}

base_raster_plot <- function(raster_data, fillvar, low_col, high_col, plot_title) {
  baseraster_map <-       
  ggplot() +
  geom_spatraster(data = rast_mask_proj, aes(fill = !!sym(fillvar))) +
  scale_fill_gradient(low = low_col, high = high_col) +
  labs(title = plot_title)
  
  #return(paste(fillvar))  
  return(baseraster_map)

  
}
