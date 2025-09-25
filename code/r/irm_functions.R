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
        

select_location <-
  function(division_data,
           division_fieldname,
           location_name) {
    
    expr <- paste0(division_fieldname , " == '", location_name, "'")
    location <- tidyterra::filter(division_data, eval(parse(text=(expr))))
    return(location)
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

# optional arguments are the method (y) bilinear or near, mask

raster_project <- function(x, y, rast_mask_proj) {
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

agg_resample <- function(w, x, y, z, rast_mask_proj) {
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
  if(type == "year") res <- lubridate::month(x)*3 + res - 3
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
# day_months removed because causing function to be very slow
# produces a matrix of proportion of each month in growing period for each cell
# afterwards needs to be transposed and binded to original data frame, 


.growth_period_long_tbl_short <- function(x, day_begin, day_end, num_years) {
  
  days <- round(x[day_begin]):round(x[day_end])
  tabulate(bin = months[days], nbins = 12L)* num_years/tabulate(bin = months, 
                                                                nbins = 12L)
  
}

# developed in 'sowing_date_test_4_CRASA.rmd'
# takes a data frame with fields for day_begin and day_end
# num_years is predefined
# day_dekads removed because causing function to be very slow
# produces a matrix of proportion of each dekad in growing period for each cell
# afterwards needs to be transposed and binded to original data frame, 



.growth_period_long_dekad_tbl <- function (x, day_begin, day_end, num_years) {
  days <- 1:365
  day_dekads <- days %>% as.character %>% as.Date("%j") %>% 
    dekad(type = "year") %>% as.integer 
  dekads <- rep.int(day_dekads, num_years)
  days <- round(x[day_begin]):round(x[day_end])
  tabulate(bin = dekads[days], nbins = 36L)* num_years/tabulate(bin = dekads, 
                                                                nbins = 36L)
}

# developed in 'sowing_date_test_4_CRASA.rmd'
# takes a data frame with fields for day_begin and day_end
# num_years is predefined
# produces a matrix of proportion of each dekad in growing period for each cell
# afterwards needs to be transposed and binded to original data frame, 
# converted to sf point object and rasterized


.growth_period_long_dekad_tbl_short <- function (x, day_begin, day_end, num_years) {
  
  days <- round(x[day_begin]):round(x[day_end])
  tabulate(bin = dekads[days], nbins = 36L)* num_years/tabulate(bin = dekads, 
                                                                nbins = 36L)
}


# temporary function to check on which days are used to define the phenological stages

daystest <- function(x, start_day, end_day) {
  
  c(round(x[start_day]),round(x[end_day]))
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



rasterize_plot_fpm <- function(n, fpm_conc_var, fpm_conc_name, df_data, rast_mask_proj) {
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
  
  dB %>% plot(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), axes = F)

  
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

# temporary function to write vect output to geojson

# where: x is the projected vect data
#        data_file_prefix is the prefix used to define the name of the output geojson

output_vect <- function(results_vect, data_file_prefix) {
  

  vector_filename <-
    as.character(paste("spatial_data/output/",
                       data_file_prefix,
                       ".geojson",
                       sep = ""))
  
  writeVector(
    project(results_vect,
            "epsg:4326"),
    filename = (here(vector_filename)),
    filetype="geojson",
    overwrite = TRUE)
  
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

# where: results_vect is the projected vect data
#        data_file_prefix is the prefix used to define the name of the output csv


output_csv <- function(results_vect, data_file_prefix) {
  csv_filename <-
    as.character(paste("tab_data/output/",
                       data_file_prefix,
                       ".csv",
                       sep = ""))
  write_csv(
  #writeVector(
    results_vect,
    (here(csv_filename))#,
    #filetype = "csv",
    #overwrite=TRUE
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

# temporary function to create kebele level maps with sf

# where: n is the number of maps to produce (the number of values for the suitability)
#        fill_var_list is a vector of the possible values for suitability
#        pal_col is a vector of the names of the colour palette

plot_subdiv_maps <- function(n, fill_var_list, pal_col) {
  subdiv_map_list <- list()
  
  for (i in 1:n)   {
    fillvar <- fill_var_list[[i]]
    subdiv_map_list[[i]] <-
      ggplot() +
      geom_spatvector(data = vect_subdiv, aes(group = "id", fill = !!sym(fillvar))) +
      scale_fill_distiller(
        type = "seq",
        palette = pal_col[i],
        direction = 1,
        guide = "legend"
      ) +
      #   scale_x_continuous(
      #      name = "",
      #      labels = function(x) {
      #        1.0e-3 * x
      #      }
      #    ) +
      #    scale_y_continuous(
      #      name = "",
      #      labels = function(x) {
      #        1.0e-3 * x
      #      }
    #   )  +
      geom_spatvector(data = vect_ba_tri, aes(colour = ifelse(baclass == "optimal" , "green", "red")),
                      shape = 21,
                      size = 2) +
      #geom_spatvector_text(data = vect_ba_tri, aes(label = ifelse(type == "b", "b", "b/s"))) +
      scale_colour_identity()
  }
  return(subdiv_map_list)
}  

# temporary function to create subdivision level maps of % with terra

# where: n is the number of maps to produce (the number of values for the suitability)
#        fill_var_list is a vector of the possible values for suitability
#        pal_col is a vector of the names of the colour palette

plot_subdiv_maps_pc <- function(n, fill_var_list, pal_col) {
  subdiv_map_pc_list <- list()
  
  for (i in 1:n)   {
    fillvar <- fill_var_list[[i]]
    subdiv_map_pc_list[[i]] <-
      ggplot() +
      geom_spatvector(data = vect_subdiv, aes(group = "id", fill = !!sym(fillvar))) +
      scale_fill_distiller(
        type = "seq",
        palette = pal_col[i],
        direction = 1,
        limits = c(0, 100),
        guide = "legend"
        ) +
       #   scale_x_continuous(
      #      name = "",
      #      labels = function(x) {
      #        1.0e-3 * x
      #      }
      #    ) +
      #    scale_y_continuous(
      #      name = "",
      #      labels = function(x) {
      #        1.0e-3 * x
      #      }
      #   )  +
          geom_spatvector(data = vect_ba_tri, aes(colour = ifelse(baclass == "optimal" , "green", "red")),
                          size = 1) +
          #geom_spatvector_text(data = vect_ba_tri, aes(label = ifelse(type == "b", "b", "b/s"))) +
          scale_colour_identity()
  }
  return(subdiv_map_pc_list)
} 

# temporary function to create kebele level maps with sf

# where: n is the number of maps to produce (the number of values for the suitability)
#        fill_var_list is a vector of the possible values for suitability
#        pal_col is a vector of the names of the colour palette


plot_subdiv_maps_sf <- function(n, fill_var_list, pal_col) {
  subdiv_map_list <- list()
  
  for (i in 1:n)   {
    subdiv_map_list[[i]] <-
      ggplot() +
      geom_sf(data = vect_subdiv, (aes_string(
        group = "id", fill = (fill_var_list[i])
      ))) +
      coord_sf(datum = st_crs(crs_irm)) +
      scale_fill_distiller(
        type = "seq",
        palette = pal_col[i],
        direction = 1,
        guide = "legend"
      ) +
      #   scale_x_continuous(
      #      name = "",
      #      labels = function(x) {
      #        1.0e-3 * x
      #      }
      #    ) +
      #    scale_y_continuous(
      #      name = "",
      #      labels = function(x) {
      #        1.0e-3 * x
      #      }
    #   )  +
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


add_subdiv_plot <- function(map1, vect_subdiv, vect_subdiv_pt, nudge_xval, nudge_yval) {
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

add_subdiv_simple_plot <- function(map1, vect_subdiv) {
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

add_subdiv_proj_simple_plot <- function(map1, vect_subdiv) {
  addsubdivprojsimple_map <-       map1 +
    geom_spatvector(data = project(vect_subdiv, wkt_lam),
                    colour = "dark grey",
                    fill = NA) +
    theme(legend.direction = "vertical", 
          legend.box = "horizontal",
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  return(addsubdivprojsimple_map)
}

add_triangulation_plot_ad <- function(map1, vect_triangulation, nudge_xvaltri, nudge_yvaltri) {
  addtriangulation_map_ad <-       map1 +
    geom_spatvector(
      data = vect_triangulation,
      aes(fill = tri_label_ad),
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
    scale_fill_manual(values = 1:nlevels(vect_triangulation$tri_label_ad)) +
    labs(fill = "Comment") +
    theme(
      legend.direction = "vertical",
      legend.box = "horizontal",
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    guides(fill = guide_legend(order = 1))
  
  return(addtriangulation_map_ad)
         
}

add_triangulation_plot_no_labels_ad <- function(map1, vect_triangulation) {
  addtriangulation_map_nl_ad <-       map1 +
    geom_spatvector(
      data = vect_triangulation,
      aes(fill = tri_label_ad),
      colour = "black",
      shape = 21,
      size = 2,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = 1:nlevels(vect_triangulation$tri_label_ad)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
  return(addtriangulation_map_nl_ad)
  
}

add_triangulation_plot_ba <- function(map1, vect_triangulation, nudge_xvaltri, nudge_yvaltri) {
  addtriangulation_map_ba <-       map1 +
    geom_spatvector(
      data = vect_triangulation,
      aes(fill = tri_label_ba),
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
    scale_fill_manual(values = 1:nlevels(vect_triangulation$tri_label_ba)) +
    labs(fill = "Comment") +
    theme(
      legend.direction = "vertical",
      legend.box = "horizontal",
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    guides(fill = guide_legend(order = 1))
  
  return(addtriangulation_map_ba)
  
}

add_triangulation_plot_no_labels_ba <- function(map1, vect_triangulation) {
  addtriangulation_map_nl_ba <-       map1 +
    geom_spatvector(
      data = vect_triangulation,
      aes(fill = tri_label_ba),
      colour = "black",
      shape = 21,
      size = 2,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = 1:nlevels(vect_triangulation$tri_label_ba)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
  return(addtriangulation_map_nl_ba)
  
}

add_triangulation_plot_se <- function(map1, vect_triangulation, nudge_xvaltri, nudge_yvaltri) {
  addtriangulation_map_se <-       map1 +
    geom_spatvector(
      data = vect_triangulation,
      aes(fill = tri_label_se),
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
    scale_fill_manual(values = 1:nlevels(vect_triangulation$tri_label_se)) +
    labs(fill = "Comment") +
    theme(
      legend.direction = "vertical",
      legend.box = "horizontal",
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    guides(fill = guide_legend(order = 1))
  
  return(addtriangulation_map_se)
  
}

add_triangulation_plot_no_labels_se <- function(map1, vect_triangulation) {
  addtriangulation_map_nl_se <-       map1 +
    geom_spatvector(
      data = vect_triangulation,
      aes(fill = tri_label_se),
      colour = "black",
      shape = 21,
      size = 2,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = 1:nlevels(vect_triangulation$tri_label_se)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
  return(addtriangulation_map_nl_se)
  
}

base_raster_plot <- function(raster_data, fillvar, low_col, high_col, plot_title) {
  baseraster_map <-       
  ggplot() +
  geom_spatraster(data = raster_data, aes(fill = !!sym(fillvar))) +
  scale_fill_gradient(low = low_col, high = high_col,  na.value = "transparent") +
  labs(title = plot_title)
  
  #return(paste(fillvar))  
  return(baseraster_map)

  
}


# # Functions for hierarchies.rmd
# 
# Each row in each higher-level df is a rule base stack
# 
# For each row in each higher-level df starting with lowest level (e.g. 6)
# 
# Join to lower-level criteria to give joined df
# 
# 1) Get the lower-level rule bases (1 entry per row in joined df)
# 2) Construct the list of lower-level rule bases (1 entry per row in joined df)
# 3) Construct the list of lower-level fuzzy partitions
# 4) Get the conclusion values of the lower-level rule bases (multiple per row in joined df)
# 5) Use the weights, number of lower-level rule bases and conclusion values of the lower-level rule bases to determine the number of rules (1 per joined df)
# 6) Use the weights (if any) to define the higher-level rule base conclusions (multiple per joined df)
# 7) Make a new proposition object (1 per joined df)
# 8) Make a new conclusion object 1 per joined df)
# 9) Create a new rule base (1 per joined df)
# 10) Add to the list of previous rule bases (1 list per joined df)
# 11) Evaluate the new rule base (and stack) (per joined df)
# 12) Add results to df_irm (per joined df)
# 13) Plot results (per joined df)
# 
# 
# ### Function to construct the new rule base - ALL
# 
# Logic:
#   
#   1) How many lower rule bases
# 2) Get conclusions from lower rule bases
# 3) multiply conclusions by weights (if required)
# 



fn_create_rb <- function(lower_conc_values, weights, rb_conc_name) {
  
  # Create conc_options default
  conc_options <- c("suboptimal", "suboptimal", "optimal")
  c_o <- "ba"
  
  # Check if the text string is present in lower_conc_values
  if (any(grepl("good",lower_conc_values))) {
    conc_options <- c("poor", "moderate", "good")
    c_o <- "sef"
  }
  
  #cat(paste("\nlower_conc_values =", lower_conc_values, "\n"))
  n <- length(lower_conc_values)
  weights[is.na(weights)] <- (1/n) # replace NA values in weights with values that sum to 1
  
  rb <- reduce(lower_conc_values, crossing) %>% unique() # added 09/03/2024
  n_rb <- nrow(rb)
  #cat(paste("str(rb) =", str(rb), "\n"))
  #cat(paste("Number of rules =", n_rb, "\n"))
  
  #rb[,] <- apply(rb, function(x) type.convert(as.character(x), as.is = TRUE))
  
  rb_integer <- rb %>%
    mutate(across(
      where(is.character),
      ~ case_when(. == "poor" ~ -1,
                  . == "suboptimal" ~ -1,
                  . == "moderate" ~ 0,
                  . == "good"  ~ 1,
                  . == "optimal" ~ 1,
                  TRUE ~ NA_integer_),
      .names = "{col}_integer"
    ))  %>% select(-where(is.character)) %>% drop()
  
  # Display the modified data frame
  
  #cat(paste("str(rb_integer) =", str(rb_integer), "\n"))
  #cat(paste("Weights =", weights, "\n"))
  #cat(paste("Possible Conclusions =", conc_options, "\n"))
  
  rb_integer_weighted <- sweep(rb_integer, 2, weights, `*`)
  rb_integer_weighted$conc <- rowSums(rb_integer_weighted, na.rm = TRUE)
  rb_integer_weighted <- cbind(rb, rb_integer_weighted)
  
  if (c_o == "sef") {
    rb_integer_weighted$conclusion <- 
      ifelse(
        rb_integer_weighted$conc <= (-1 / 3),
        conc_options[1],
        ifelse(
          rb_integer_weighted$conc <= (1 / 3),
          conc_options[2],
          conc_options[3]
        )
      )
  } else {
    rb_integer_weighted$conclusion <-
      ifelse(rb_integer_weighted$conc < 1,
             conc_options[2],
             conc_options[3])
  }
  
  
  #cat(paste("\nstr(rb_integer_weighted) =", str(rb_integer_weighted), "\n"))
  names(rb_integer_weighted)[names(rb_integer_weighted)=="conclusion"] <- rb_conc_name
  #cat(paste("\nstr(rb_integer_weighted) =", str(rb_integer_weighted), "\n"))
  
  # new proposition object is made here
  prop <- new("Proposition", table = rb %>% select(where(is.character)))
  # new conclusion object is made here
  conc <- new("Conclusion", table = select(rb_integer_weighted, {{ rb_conc_name }}))
  # create a rule base
  rulebase <- RuleBase(prop, conc) #%>% print()
  
  return_list <- list(rulebase, conc)
  
  return(return_list)
}


### Function to get all previous fuzzy partition names

# Logic:
#   
#   1) Does the row of the one-to-many df have data
# If yes:
#   A2)  get fp name using rulebase number
# A3)  append the fp name to the fp_list for the one-to-many df
# If no:
#   B2) this row has an existing fp_list - get fp_list
# B3) append this list to the fp_list for the one-to-many df
# 
# return the fp_list for the one-to-many df



fn_fill_fp_list <- function(df_one_many) {
  
  fp_list <- list() #initialise list
  df_one_many_data <-
    dplyr::filter(df_one_many, !is.na(data_file_prefix)) # get rows that have a fuzzy partition
  df_one_many_nodata <-
    dplyr::filter(df_one_many, is.na(data_file_prefix)) # get rows that have a fuzzy partition
  
  #cat(paste("\nnrow data = ", nrow(df_one_many_data), "\n"))
  
  if (nrow(df_one_many_data) > 0) {
    fp_prefix <- "fp_"
    df_one_many_data$fp <-
      paste(fp_prefix, df_one_many_data$rulebase_number.y, sep = "")
    #str(df_one_many_data)
    fp_list <-
      append(fp_list, list(
        paste(
          df_one_many_data$rulebase_number.y,
          " = ",
          df_one_many_data$fp
        )
      ))
    fp_list <- as.list(unlist(fp_list))
  }
  
  #cat(paste("\nnrow no data = ", nrow(df_one_many_nodata), "\n"))
  
  if (nrow(df_one_many_nodata) > 0) {
    for (i in 1:nrow(df_one_many_nodata)){
      fp_list <-
        append(fp_list, get(
          paste0("fp_",
                 df_one_many_nodata$rulebase_number.y[i],
                 "_list")
        ))
    }}
  
  return(fp_list)
}


### Function to get all previous rule base names

# Logic:
#   
#   1) Does the row of the one-to-many df have data
# If yes:
#   A2)  get rb name using rulebase number
# A3)  append the rb name to a rule base list for the one-to-many df
# If no:
#   B2) this row has an existing rule base stack - get the rule base stack list
# B3) append this rule base stack list to the rule base list for the one-to-many df
# 
# return the rule base list for the one-to-many df


fn_fill_rb_list <- function(df_one_many) {
  
  rb_list <- list() #initialise list
  df_one_many_data <-
    dplyr::filter(df_one_many, !is.na(data_file_prefix)) # get rows that have a fuzzy partition
  df_one_many_nodata <-
    dplyr::filter(df_one_many, is.na(data_file_prefix)) # get rows that have a fuzzy partition
  
  #cat(paste("\nnrow data = ", nrow(df_one_many_data),"\n"))
  
  if (nrow(df_one_many_data) > 0) {
    # rb_prefix <- "rb_"
    # df_one_many_data$rb <-
    #   paste(rb_prefix, df_one_many_data$rulebase_number.y, sep = "")
    # str(df_one_many_data)
    rb_list <-
      append(rb_list, list(
        paste(df_one_many_data$rb
        )
      ))
    rb_list <- as.list(unlist(rb_list))
  }
  
  #cat(paste("\nnrow no data = ", nrow(df_one_many_nodata),"\n"))
  
  if (nrow(df_one_many_nodata) > 0) {
    for (i in 1:nrow(df_one_many_nodata)) {
      # cat(paste0("rb_list_",
      #              df_one_many_nodata$rulebase_number.y[i],"\n"))
      rb_list <-
        append(rb_list, get(
          paste0("rb_list_",
                 df_one_many_nodata$rulebase_number.y[i])
        ))
    }
  }
  
  return(rb_list)
}



### Function to get the previous rule-base stack names

# Logic:
#   
#   1) Get the criteria that contribute to the higher-level rule base
# 2) Get the code for the suffix for use in mapping
# 3) If no code available use the number of the lower-level rule bases as a suffix 
# 
# return the codes/names of the contributing criteria for the one-to-many df
# 


fn_rbs_names <- function(df_one_many) {
  
  if ("mapping_code" %in% colnames(df_one_many)) {
    df_one_many <- select(df_one_many, rulebase_number.y, mapping_code)
  } else {
    df_one_many <- select(df_one_many, rulebase_number.y)
    df_one_many$mapping_code <- df_one_many$rulebase_number.y
  }
  
  rbs_names <- as.list(df_one_many)
  return(rbs_names)
}


## Get the data frames, conclusions (labels from a different list still using lapply), rule bases and fuzzy partitions for each rulebase stack - create the new rule base from the propositions and conclusions.

# This uses the code from the previous chunks.


# function to get a list of lower level conclusions values in this format

fn_get_rb_conc <- function(list_object) {
  #print(paste("rb list_object =" , list_object))
  rb_list_object <- get(paste(list_object))
  #print(rb_list_object)
  #str(rb_list_object)
  lower_conc_table <-
    rb_list_object@conclusion@table # gets the tibble of values
  return(lower_conc_table)
}

fn_get_rb_prop <- function(list_object) {
  #print(paste("rb list_object =" , list_object))
  rb_list_object <- get(paste(list_object))
  #print(rb_list_object)
  #str(rb_list_object)
  lower_prop_table <-
    rb_list_object@proposition@table # gets the tibble of values
  return(lower_prop_table)
}

rasterize_plot_fpm <-
  function(n,
           fpm_conc_var,
           fpm_conc_name,
           df_data,
           fpm_plot_title, rast_mask_proj) {
    df_plot <- df_data %>%
      dplyr::select(unlist(as.character(noquote(fpm_conc_var)))) %>%
      na.omit %>%
      st_as_sf(coords = c("x", "y"))
    
    # initialise the dB raster brick using the first layer
    rast_plot <-
      rasterize(df_plot, rast_mask_proj, fpm_conc_var[[1]]) #%>% brick   # SpatRasters don't need to be bricks
    
    # in a loop add subsequent layers to the dB SpatRaster starting from the second layer
    for (k in 2:n) {
      rast_plot <-
        rast(list(rast_plot, rasterize(df_plot, rast_mask_proj, field = fpm_conc_var[[k]])))
    }
    
    # give the dB layers sensible names
    names(rast_plot) <- fpm_conc_name
    
    # plot all the layers in facets
    ggplot() +
      geom_spatraster(data = rast_plot, na.rm = TRUE) +
      scale_fill_stepsn(
        na.value = "transparent",
        n.breaks = 10,
        colours = hcl.colors(palette = "Greens 3", 10, rev = T),
        guide = "legend"
      ) +
      facet_wrap( ~ lyr) +
      labs(title = fpm_plot_title) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_rect(fill=NA),
        panel.background = element_rect(fill=NA))
  }



# Classified maps
# cat(
#   "\n\n\n
# Classified maps are currently produced for Adoption, Biophysical Aptitude and for Socio-economic Feasibility.
# 
# Problem 1): Currently, the evaluation of the chunks that create these maps is determined by a parameter in the chunk options. That parameter is either TRUE or FALSE depending on the name of the rule base stack in the priorities table.
# 
# Problem 2): The number of classes for the biophysical aptitude limitations maps is also dependent on the number of types of limitations possible (e.g. soil fertility).
# 
# Logic: For the two top hierarchy level rule-bases produce classified maps with limitations based on lower-level inputs.\n\n\n")
# 

classify_maps_FAO <-
  function(n,
           fpm_conc_var,
           fpm_conc_name,
           df_data,
           fpm_plot_title,
           df_one_many,
           rast_mask_proj,
           list_rast_clas_FAO_cat, vect_subdiv, innovation) {
    df_plot <- df_data %>%
      dplyr::select(unlist(as.character(noquote(fpm_conc_var)))) %>%
      na.omit %>%
      st_as_sf(coords = c("x", "y"))
    
    #cat("str(df_one_many) = ", str(df_one_many),"\n")
    stack_code <<- unique(df_one_many[['stack']])
    cat("stack_code = ", stack_code,"\n")
    
    if ("optimal" %in% fpm_conc_name) { # biophysical aptitude criteria
      
      cat("OPTIMAL")           
      print(match("optimal",fpm_conc_name))
      opt_match <- match("optimal",fpm_conc_name)
      opt_match_glob <<- opt_match
      
      #create raster using the 'optimal' layer
      rast_plot <-
        rasterize(df_plot, rast_mask_proj, fpm_conc_var[[opt_match]])
      names(rast_plot) <- fpm_conc_name[[opt_match]]
      
    } else {
      # socio-economic feasibility criteria
      
      cat("GOOD")
      match_set <- c("good", "high")
      print(match(match_set, fpm_conc_name))
      good_match <- max(match(match_set, fpm_conc_name), na.rm = T)
      good_match_glob <<- good_match
      
      #create raster using the 'good/high' layer
      rast_plot <-
        rasterize(df_plot, rast_mask_proj, fpm_conc_var[[good_match]])
      names(rast_plot) <- fpm_conc_name[[good_match]]
      
      if (length(fpm_conc_name) == 3) {# only run when a moderate conclusion
        
        cat("MODERATE")  
        match_set <- c("moderate")
        print(match(match_set, fpm_conc_name))
        moderate_match <- max(match(match_set, fpm_conc_name), na.rm = T)
        moderate_match_glob <<- moderate_match
        
        #create raster using the 'good/high' layer
        rast_plot2 <- rasterize(df_plot, rast_mask_proj, fpm_conc_var[[moderate_match]])
        names(rast_plot2) <- fpm_conc_name[[moderate_match]]
        
        #*Optimal*
        # High feasibility has a value of 1.
        # Moderate feasibility has a value of 0.5.
        
        rast_plot <- sum(rast_plot, (rast_plot2 * 0.5))
      }
    }
    
    rast_plot_name <- names(rast_plot)
    
    # plot the 'optimal' layer
    ggplot() +
      geom_spatraster(data = rast_plot, na.rm = TRUE, aes(fill = rast_plot_name)) +
      scale_fill_stepsn(
        na.value = "transparent",
        n.breaks = 10,
        colours = hcl.colors(palette = "Greens 3", 10, rev = T),
        guide = "legend"
      ) +
      #facet_wrap(~ lyr) +
      labs(title = fpm_plot_title) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_rect(fill = NA),
        panel.background = element_rect(fill = NA)
      )
    
    # classify values and export
    ## from-to-becomes
    # classify the optimal values into five groups
    # all values >= 0 and <= 0.25 become N2, etc.
    v_clas_fao <- c(-Inf, 0.25, 5,
                    0.25, 0.4, 4,
                    0.4, 0.6, 3,
                    0.6, 0.85, 2,
                    0.85, Inf, 1)
    m_clas_fao <- matrix(v_clas_fao, ncol = 3, byrow = TRUE)
    rast_clas_fao_bin <-
      classify(rast_plot, m_clas_fao, include.lowest = TRUE)
    rast_clas_fao_cat <- rast_clas_fao_bin
    df_clas_fao <-
      data.frame(id = 1:5, FAO = c("S1", "S2", "S3", "N1", "N2"))
    levels(rast_clas_fao_cat) <- df_clas_fao
    rast_clas_fao_brick <-
      rast(list(rast_plot, rast_clas_fao_bin, rast_clas_fao_cat))
    names(rast_clas_fao_brick) <- c(rast_plot_name, 'bin', 'FAO')
    output_geotiff(rast_clas_fao_brick, paste0("FAO_", stack_code, "_", innovation))
    
    assign(paste0("rast_clas_fao_cat", fpm_plot_title),
           rast_clas_fao_cat,
           .GlobalEnv)
    
    list_rast_clas_FAO_cat <<-
      append(list_rast_clas_FAO_cat,
             paste0("rast_clas_fao_cat", fpm_plot_title))
    
    g <-  ggplot() +
      geom_spatraster(data = rast_clas_fao_cat, na.rm = TRUE, aes(fill = FAO)) +
      scale_fill_manual(
        name = paste0("FAO\n", fpm_plot_title, "\n", innovation),
        na.value = "transparent",
        values = c(
          S1 = rgb(51, 160, 44, maxColorValue = 255),
          S2 = rgb(178, 223, 138, maxColorValue = 255),
          S3 = rgb(255, 255, 153, maxColorValue = 255),
          N1 = rgb(255, 127, 0, maxColorValue = 255),
          N2 = rgb(227, 26, 28, maxColorValue = 255)
        )
      )
    add_subdiv_proj_simple_plot(g, vect_subdiv) %>% print()
    
    
    ## leaflet maps
    
    # export class and optimal value as polygon
    vect_clas_fao_cat <-
      as.polygons(rast_clas_fao_brick, dissolve = F)
    vect_filename <-
      as.character(paste0("FAO_", stack_code, "_", innovation))
    output_vect(vect_clas_fao_cat, vect_filename)
    
    # project to geo for display in leaflet
    vect_clas_fao_cat_geo <- project(vect_clas_fao_cat, "epsg:4326")
    
    # convert to sf until leaflet properly accepts spatvector
    sf_clas_fao_cat_geo <- st_as_sf(vect_clas_fao_cat_geo)
    
    FAO_palette <- (c(
      #S1 = 
      rgb(51, 160, 44, maxColorValue = 255),
      #S2 = 
      rgb(178, 223, 138, maxColorValue = 255),
      #S3 = 
      rgb(255, 255, 153, maxColorValue = 255),
      #N1 = 
      rgb(255, 127, 0, maxColorValue = 255),
      #N2 = 
      rgb(227, 26, 28, maxColorValue = 255)
      
    )) 
    
    
    pal <-
      colorFactor(FAO_palette, domain = c(1:5), na.color = "#808080")
    
    leaflet_widget <- leaflet(sf_clas_fao_cat_geo, width = "100%") %>%
      addProviderTiles(providers$OpenStreetMap.HOT, group = "OSM (HOT)") %>%
      addPolygons(
        label = ~ stringr::str_c('FAO =', FAO),
        labelOptions = labelOptions(direction = 'auto'),
        color = "#03F",
        weight = 1,
        opacity = 0.5,
        fill = layer,
        fillColor = ~ pal(bin),
        fillOpacity = 0.2,
        dashArray = NULL,
        smoothFactor = 1,
        noClip = FALSE,
        popup = paste(
          'FAO  =',
          sf_clas_fao_cat_geo$FAO
        ),
        popupOptions = NULL,
        highlightOptions = NULL,
      )
    
    return(leaflet_widget)
    
  } # end of classify_maps_FAO function
# ### TO BE DONE 17/02/2024 ### START
# cat(paste("\nDEBUG1 START\n\n"))

classify_maps_limits <-
  function(n,
           fpm_conc_var,
           fpm_conc_name,
           df_data,
           fpm_plot_title,
           df_one_many,
           weights,
           rast_mask_proj,
           vect_subdiv,
           list_rast_limits_max_FAO, innovation) {
    
  
  # what are the previous level inputs - the rows in the one-to-many table
  
  rbs_names <- fn_rbs_names(df_one_many)[[1]]
  
  lower_rb_var_1 <- lapply(rbs_names, function(x) paste0(x, "_s"))
  lower_rb_var_2 <- lapply(rbs_names, function(x) paste0(x, "_p"))
  lower_rb_var_3 <- lapply(rbs_names, function(x) paste0(x, "_l"))
  lower_rb_var_4 <- lapply(rbs_names, function(x) paste0(x, "_m"))
  lower_rb_var   <- c(lower_rb_var_1, lower_rb_var_2, lower_rb_var_3, lower_rb_var_4)
  
  mapping_code_suffixes <- (fn_rbs_names(df_one_many)[[2]]) 
  
  limits_var <- c(unlist(as.character(noquote(fpm_conc_var))), unlist(lower_rb_var))
  #cat("limits_var")
  #print(limits_var)
  
  # df_limits <- df_data %>%
  #     dplyr::select(any_of(limits_var)) %>%
  #     na.omit %>%
  #     st_as_sf(coords = c("x", "y"))
  
  df_limits <- df_data %>%
    dplyr::select(any_of(limits_var))
  
  #cat("df_limits")
  #str(df_limits)
  
  # get a combined score for each contributing rule base
  
  #cat("\n\ncombined score\n\n")
  # get a weighted score
  
  #cat("\n\nweighted score\n\n")
  
  w_score_list <- list() # initialise list
  
  for (l in 1:length(rbs_names)) {
    #cat("rbs_names[[l]] = ", rbs_names[[l]],"\n")
    #cat("weights[[l]] = ", weights[[l]],"\n")
    
    if (is.na(weights[[l]])) {weights[[l]] <- 1/length(rbs_names)}
    
    score_var <- paste0(rbs_names[[l]], "_score")
    w_score_var <- paste0(rbs_names[[l]], "_w_score")
    w_score_list <- append(w_score_list, w_score_var)
    
    expr_w_score <- parse(text = paste0(weights[[l]]," * ", score_var)) # weighted score
    
    
    if (paste0(rbs_names[[l]], "_m") %in% colnames(df_limits)) {
      #print("moderate")# there is a moderate conclusion
      expr_score <- parse(text = paste0(rbs_names[[l]], "_p + (", rbs_names[[l]], "_m * 0.5)" ))
      df_limits <- mutate(
        df_limits,
        !!score_var := eval(expr_score), !!w_score_var := eval(expr_w_score), .keep = c("all"))
      #str(df_limits)
      
    } else {
      if (paste0(rbs_names[[l]], "_p") %in% colnames(df_limits)) {
        #print("poor") # there is a poor conclusion
        expr_score <-
          parse(text = paste0(rbs_names[[l]], "_p"))
        df_limits <- mutate(df_limits,!!score_var := eval(expr_score), !!w_score_var := eval(expr_w_score),  .keep = c("all"))
        #str(df_limits)
      } else {
        if (paste0(rbs_names[[l]], "_l") %in% colnames(df_limits)) {
          #print("low") # there is a low conclusion
          expr_score <-
            parse(text = paste0(rbs_names[[l]], "_l"))
          df_limits <- mutate(df_limits,!!score_var := eval(expr_score), !!w_score_var := eval(expr_w_score),  .keep = c("all"))
          #str(df_limits)
        } else {
          #print("suboptimal") # there is a suboptimal conclusion
          expr_score <-
            parse(text = paste0(rbs_names[[l]], "_s"))
          df_limits <- mutate(df_limits,!!score_var := eval(expr_score), !!w_score_var := eval(expr_w_score),  .keep = c("all"))
          #str(df_limits)
        }
      }
    }
  }
  
  # get the max value
  cat("\n\nmax value\n\n")
  df_limits_w_score <<-
    select(df_limits, unlist(w_score_list)) %>%
    mutate(across(everything(), ~ replace_na(.x, 0)))
  
  #cat("DEBUG LIMITS df_limits_w_score")
  #print(df_limits_w_score)
  
  w_score_max_index <<-
    matrix(apply(df_limits_w_score, 1 , which.max))
  value = integer(0)
  
  map_code <<- mapping_code_suffixes[w_score_max_index]
  
  w_score_max_name <<-
    matrix(colnames(df_limits_w_score)[apply(df_limits_w_score, 1 , which.max)])
  
  df_map_code <<-
    data.frame(
      w_score_max_index = w_score_max_index[, 1],
      w_score_max_name = w_score_max_name[, 1],
      map_code = map_code
    )
  
  rast_filename <-
    as.character(paste0(
      "spatial_data/output/FAO_",
      stack_code,
      "_",
      innovation,
      ".tif"
    ))
  
  rast_clas_fao_brick <- rast(here(rast_filename)) # get the FAO raster
  #cat("rast_clas_fao_brick \n")
  #print(rast_clas_fao_brick)
  
  #str(df_map_code)
  df_limits_max <<- cbind(df_limits, df_map_code)
  # str(df_limits_max)
  
  v_limits_max <<- df_limits_max %>% na.omit %>% st_as_sf(coords = c("x", "y"))
  # str(v_limits_max)
  
  rast_limits_max <<- rasterize(v_limits_max, rast_mask_proj, field = "w_score_max_index")
  #cat("rast_limits_max \n")
  #print(rast_limits_max)
  
  #cat("df_map_code \n")
  #str(df_map_code)
  
  #cat("df_map_code_cat \n")
  df_map_code_cat <<- unique(df_map_code[c("w_score_max_index", "map_code")])
  #str(df_map_code_cat)
  
  levels(rast_limits_max) <<- df_map_code_cat
  
  #rast_limits_max_FAO_2 <<-  ifel(rast_clas_fao_brick$FAO == "S1", rast_clas_fao_brick$FAO, concats(rast_clas_fao_brick$FAO, rast_limits_max))
  rast_limits_max_FAO <<-  concats(rast_clas_fao_brick$FAO, rast_limits_max)
  
  
   # replace all S1 limits with just a S1 class
  # Extract the levels of the raster
  lvl <<- levels(rast_limits_max_FAO)[[1]]
  #cats1 <<- cats(rast_limits_max_FAO)
  lvl1 <<- lvl
  
  # Replace categories starting with "S1" with "X"
  lvl$FAO_map_code <<- ifelse(grepl("^S1", lvl$FAO_map_code), "S1", lvl$FAO_map_code)

  # Assign the new levels back to the raster
  levels(rast_limits_max_FAO) <<- lvl
  cats2 <<- cats(rast_limits_max_FAO)  
  
  lvlunique <<- lvl %>% distinct(FAO_map_code, .keep_all = TRUE)
  lvlunique$ID2 <<- seq.int(nrow(lvlunique))  
  
  rast_limits_max_FAO2 <<-  subst(rast_limits_max_FAO, lvlunique$ID, lvlunique$ID2, others=NULL, raw=TRUE)
  rast_limits_max_FAO3 <<- rast_limits_max_FAO2
  
  lvlunique$ID <<- lvlunique$ID2
  lvlunique <- lvlunique[,1:2]
  levels(rast_limits_max_FAO3) <<- lvlunique
  droplevels(rast_limits_max_FAO3, level=NULL, layer=1)
  
  names(rast_limits_max_FAO3) <<- c('FAO_limit')
  #cat("rast_limits_max_FAO \n")
  #print(rast_limits_max_FAO)
 
  
  # how many suitability levels?
  FAO_levels <<- unique(rast_clas_fao_brick$FAO)
  #cat(nrow(FAO_levels), " FAO_levels \n")
 # print(str(FAO_levels))
  
  # how many limits levels?
  FAO_limit_levels <<- unique(droplevels(pull(rast_limits_max_FAO3, FAO_limit)))
  #cat(length(FAO_limit_levels), " FAO_limit_levels \n")
  #print(str(FAO_limit_levels))
  
  # how many limits levels?
  df_FAO_limit_levels <<-
    unique(rast_limits_max_FAO3$FAO_limit)
  
  df_FAO_limit_levels_test <<-    df_FAO_limit_levels # this line just for debugging
  
  #cat(nrow(df_FAO_limit_levels), " df_FAO_limit_levels \n")
  #print(str(df_FAO_limit_levels))
  
  df_FAO_limit_levels <<- df_FAO_limit_levels %>%
    mutate(value = case_when(
      grepl("S1", FAO_limit) ~ 1,
      grepl("S2", FAO_limit) ~ 0.875,
      grepl("S3", FAO_limit) ~ 0.75,
      grepl("N1", FAO_limit) ~ 0.625,
      grepl("N2", FAO_limit) ~ 0.5,
      TRUE ~ 0.5  # Default value if none of the conditions are met
    ))
  
  df_FAO_limit_levels <<- df_FAO_limit_levels %>%
    mutate(
      hue = case_when(
        grepl("BA", FAO_limit) ~ 0.25,
        grepl("SE", FAO_limit) ~ 0.75,
        grepl("LU", FAO_limit) ~ 0,
        grepl("Cl", FAO_limit) ~ 0.2,
        grepl("Ls", FAO_limit) ~ 0.4,
        grepl("SF", FAO_limit) ~ 0.6,
        grepl("SP", FAO_limit) ~ 0.8,
        grepl("MA", FAO_limit) ~ 0.5,
        grepl("FP", FAO_limit) ~ 1,
        TRUE ~ 0.9  # Default value if none of the conditions are met
      )
    )
  
  df_FAO_limit_levels <<-
    df_FAO_limit_levels %>%
    mutate(
      sat = case_when(
        grepl("BA", FAO_limit) ~ 0.8,
        grepl("SE", FAO_limit) ~ 0.8,
        grepl("LU", FAO_limit) ~ 0.5,
        grepl("Cl", FAO_limit) ~ 0.5,
        grepl("Ls", FAO_limit) ~ 0.5,
        grepl("SF", FAO_limit) ~ 0.5,
        grepl("SP", FAO_limit) ~ 0.5,
        grepl("MA", FAO_limit) ~ 0.5,
        grepl("FP", FAO_limit) ~ 0.5,
        TRUE ~ 0.4  # Default value if none of the conditions are met
      )
    )
  
  df_FAO_limit_levels$hsv <<-
    paste(
      "(",
      df_FAO_limit_levels$hue, ",",
      df_FAO_limit_levels$sat, ",",
      df_FAO_limit_levels$value,
      ")")
  
  # Create a new column using the apply function
  df_FAO_limit_levels$hsv_eval <<-
    apply(df_FAO_limit_levels[, c("hue", "sat", "value")], 1, function(row) {
      hsv(row[1], row[2], row[3])
    })
  
  df_FAO_limit_levels_order <- df_FAO_limit_levels[order(df_FAO_limit_levels$FAO_limit),]
  #cat(nrow(df_FAO_limit_levels), " df_FAO_limit_levels \n")
  #print(str(df_FAO_limit_levels))
  
  test_FAO_limits <<- pull(df_FAO_limit_levels_order, FAO_limit)
  test_FAO_colours <<- pull(df_FAO_limit_levels_order, hsv_eval)
  
  # save raster
  assign(paste0("rast_limits_max_FAO_", fpm_plot_title), rast_limits_max_FAO3,
         .GlobalEnv)
  
  # add raster to list
  list_rast_limits_max_FAO <<- append(list_rast_limits_max_FAO, paste0("rast_limits_max_FAO_", fpm_plot_title))
  
  output_geotiff(rast_limits_max_FAO3, paste0("FAO_limits_", stack_code, "_", innovation))
  
  
  g2 <- ggplot() +
    geom_spatraster(data = rast_limits_max_FAO3, aes(fill = FAO_limit), na.rm = TRUE) +
    scale_fill_manual(
      name = paste0("FAO limitations\n", fpm_plot_title, "\n", innovation),
      na.value = "transparent",
      breaks = test_FAO_limits,
      values = test_FAO_colours
    )
  
  add_subdiv_proj_simple_plot(g2, vect_subdiv) %>% print()
  
  # convert the spatraster (showing the FAO suitability limitations classes)
  
  vect_limits_max_FAO <- as.polygons(rast_limits_max_FAO3, dissolve=FALSE)
  
  # project to geo for display in leaflet
  vect_limits_max_FAO_geo <- project(vect_limits_max_FAO, "epsg:4326")
  
  # convert to sf until leaflet properly accepts spatvector
  sf_limits_max_FAO_geo <- st_as_sf(vect_limits_max_FAO_geo)
  
  
  
  pal <-
    colorFactor(test_FAO_colours, domain = test_FAO_limits, na.color = "#808080")
  
  leaflet_widget <- leaflet(sf_limits_max_FAO_geo, width = "100%") %>%
    addProviderTiles(providers$OpenStreetMap.HOT, group = "OSM (HOT)") %>%
    addPolygons(
      label = ~ stringr::str_c('Limitations =', FAO_limit),
      labelOptions = labelOptions(direction = 'auto'),
      color = "#03F",
      weight = 1,
      opacity = 0.5,
      fill = T,
      fillColor = ~ pal(FAO_limit),
      fillOpacity = 0.2,
      dashArray = NULL,
      smoothFactor = 1,
      noClip = FALSE,
      popup = paste(
        'FAO_limits  =',
        sf_limits_max_FAO_geo$FAO_limit),
      popupOptions = NULL,
      highlightOptions = NULL,
    )
  
  return(leaflet_widget)  
  
}# end of classify_maps_limits function


classify_maps_CONC <-
  function(n,
           fpm_conc_var,
           fpm_conc_name,
           df_data,
           fpm_plot_title,
           df_one_many,
           rast_mask_proj,
           list_rast_clas_FAO_cat,
           vect_subdiv,
           innovation) {
    
    df_plot <- df_data %>%
      dplyr::select(unlist(as.character(noquote(fpm_conc_var)))) %>%
      na.omit %>%
      st_as_sf(coords = c("x", "y"))
    
    #cat("str(df_one_many) = ", str(df_one_many),"\n")
    stack_code <<- unique(df_one_many[['stack']])
    cat("stack_code = ", stack_code, "\n")
    
    if ("optimal" %in% fpm_conc_name) {
      # biophysical aptitude criteria
      
      cat("OPTIMAL")
      print(match("optimal", fpm_conc_name))
      opt_match <- match("optimal", fpm_conc_name)
      opt_match_glob <<- opt_match
      
      #create raster using the 'optimal' layer
      rast_plot_opt <-
        rasterize(df_plot, rast_mask_proj, fpm_conc_var[[opt_match]])
      names(rast_plot_opt) <- fpm_conc_name[[opt_match]]
      
      cat("SUBOPTIMAL")
      print(match("suboptimal", fpm_conc_name))
      subopt_match <- match("suboptimal", fpm_conc_name)
      subopt_match_glob <<- subopt_match
      
      #create raster using the 'suboptimal' layer
      rast_plot_subopt <-
        rasterize(df_plot, rast_mask_proj, fpm_conc_var[[subopt_match]])
      names(rast_plot_subopt) <- fpm_conc_name[[subopt_match]]
      
      rast_plot_opt_subopt <- rast(list(rast_plot_opt, rast_plot_subopt))
   
      # get max value and export
      rast_plot_max <- which.max(rast_plot_opt_subopt)
      names(rast_plot_max) <- c("class")
      rast_opt_subopt_brick <- rast(list(rast_plot_opt, rast_plot_subopt, rast_plot_max))
      #names(rast_opt_subopt_brick) <- c('high', 'moderate', 'low', 'class')
      output_geotiff(rast_opt_subopt_brick, paste0("optsubopt", stack_code, "_", innovation))
      
      
      # plot the max value
      rast_plot_name <- names(rast_plot_max)
      
      # classify values and export
      ## from-to-becomes
      # classify the optimal values into five groups
      # all values >= 0 and <= 0.25 become N2, etc.
      
      df_clas_opt <-
        data.frame(id = 1:2, class = c("Optimal", "Suboptimal"))
      levels(rast_plot_max) <- df_clas_opt
      
      # rast_clas_fao_brick <-
      #   rast(list(rast_plot, rast_clas_fao_bin, rast_clas_fao_cat))
      # names(rast_clas_fao_brick) <- c(rast_plot_name, 'bin', 'FAO')
      # output_geotiff(rast_clas_fao_brick,
      #                paste0("FAO_", stack_code, "_", innovation))
      # 
      # assign(paste0("rast_clas_fao_cat", fpm_plot_title),
      #        rast_clas_fao_cat,
      #        .GlobalEnv)
      # 
      # list_rast_clas_FAO_cat <<-
      #   append(list_rast_clas_FAO_cat,
      #          paste0("rast_clas_fao_cat", fpm_plot_title))
      
      g <-  ggplot() +
        geom_spatraster(data = rast_plot_max, na.rm = TRUE, aes(fill = class)) +
        scale_fill_manual(
          name = paste0("Optimality\n", fpm_plot_title, "\n", innovation),
          na.value = "transparent",
          na.translate = F,
          values = c(
            Optimal = rgb(51, 160, 44, maxColorValue = 255),
            Suboptimal = rgb(227, 26, 28, maxColorValue = 255)
          )
        )
      add_subdiv_proj_simple_plot(g, vect_subdiv) %>% print()
      
      
      ## leaflet maps
      
      # export class and optimal value as polygon
      vect_clas_opt_cat <-
        as.polygons(rast_plot_max, dissolve = F)
      vect_filename <-
        as.character(paste0("Optimality_", stack_code, "_", innovation))
      output_vect(vect_clas_opt_cat, vect_filename)
      
      # project to geo for display in leaflet
      vect_clas_opt_cat_geo <- project(vect_clas_opt_cat, "epsg:4326")
      
      # convert to sf until leaflet properly accepts spatvector
      sf_clas_opt_cat_geo <- st_as_sf(vect_clas_opt_cat_geo)
      
      class_palette <- (c(
        #Optimal =
        rgb(178, 223, 138, maxColorValue = 255),
        #Suboptimal =
        rgb(227, 26, 28, maxColorValue = 255)
        
      ))
      
      
      pal <-
        colorFactor(class_palette,
                    domain = c(1:2),
                    na.color = "#808080")
      
      leaflet_widget <- leaflet(sf_clas_opt_cat_geo, width = "100%") %>%
        addProviderTiles(providers$OpenStreetMap.HOT, group = "OSM (HOT)") %>%
        addPolygons(
          label = ~ stringr::str_c('class =', class),
          labelOptions = labelOptions(direction = 'auto'),
          color = "#03F",
          weight = 1,
          opacity = 0.5,
          fill = layer,
          fillColor = ~ pal(class),
          fillOpacity = 0.2,
          dashArray = NULL,
          smoothFactor = 1,
          noClip = FALSE,
          popup = paste('Class  =', sf_clas_opt_cat_geo$class),
          popupOptions = NULL,
          highlightOptions = NULL,
        )
      
      return(leaflet_widget)
      
      
      
    } else {
      # socio-economic feasibility or adoption criteria
      
      cat("GOOD")
      match_set <- c("good", "high")
      print(match(match_set, fpm_conc_name))
      good_match <- max(match(match_set, fpm_conc_name), na.rm = T)
      good_match_glob <<- good_match
      
      #create raster using the 'good/high' layer
      rast_plot_good <-
        rasterize(df_plot, rast_mask_proj, fpm_conc_var[[good_match]])
      names(rast_plot_good) <- fpm_conc_name[[good_match]]
      
      cat("POOR")
      match_set <- c("poor", "bad", "low")
      print(match(match_set, fpm_conc_name))
      poor_match <- max(match(match_set, fpm_conc_name), na.rm = T)
      poor_match_glob <<- poor_match
      
      #create raster using the 'poor/low' layer
      rast_plot_poor <-
        rasterize(df_plot, rast_mask_proj, fpm_conc_var[[poor_match]])
      names(rast_plot_poor) <- fpm_conc_name[[poor_match]]
      
      
      if (length(fpm_conc_name) == 3) {
        # only run when a moderate conclusion
        
        cat("MODERATE")
        match_set <- c("moderate", "medium")
        print(match(match_set, fpm_conc_name))
        moderate_match <- max(match(match_set, fpm_conc_name), na.rm = T)
        moderate_match_glob <<- moderate_match
        
        #create raster using the 'good/high' layer
        rast_plot_moderate <- rasterize(df_plot, rast_mask_proj, fpm_conc_var[[moderate_match]])
        names(rast_plot_moderate) <- fpm_conc_name[[moderate_match]]
        
        rast_plot_gmp <- rast(list(rast_plot_good, rast_plot_moderate, rast_plot_poor))
        
        # get max value and export
        rast_plot_max <- which.max(rast_plot_gmp)
        names(rast_plot_max) <- c("class")
        rast_gmp_brick <- rast(list(rast_plot_good, rast_plot_moderate, rast_plot_poor, rast_plot_max))
        #names(rast_opt_subopt_brick) <- c('good', 'moderate', 'poor', 'class')
        output_geotiff(rast_gmp_brick, paste0("gmp", stack_code, innovation))
        
        # plot the max value
        rast_plot_name <- names(rast_plot_max)
        
        # classify values and export

        df_clas_gmp <-
          data.frame(id = 1:3, class = c("Good", "Moderate", "Poor"))
        levels(rast_plot_max) <- df_clas_gmp
        
        # rast_clas_fao_brick <-
        #   rast(list(rast_plot, rast_clas_fao_bin, rast_clas_fao_cat))
        # names(rast_clas_fao_brick) <- c(rast_plot_name, 'bin', 'FAO')
        # output_geotiff(rast_clas_fao_brick,
        #                paste0("FAO_", stack_code, "_", innovation))
        # 
        # assign(paste0("rast_clas_fao_cat", fpm_plot_title),
        #        rast_clas_fao_cat,
        #        .GlobalEnv)
        # 
        # list_rast_clas_FAO_cat <<-
        #   append(list_rast_clas_FAO_cat,
        #          paste0("rast_clas_fao_cat", fpm_plot_title))
        
        g <-  ggplot() +
          geom_spatraster(data = rast_plot_max, na.rm = TRUE, aes(fill = class)) +
          scale_fill_manual(
            name = paste0("Good-Moderate-Poor\n", fpm_plot_title, "\n", innovation),
            na.value = "transparent",
            na.translate = F,
            values = c(
              Good = rgb(51, 160, 44, maxColorValue = 255),
              Moderate = rgb(255, 255, 153, maxColorValue = 255),
              Poor = rgb(227, 26, 28, maxColorValue = 255)
            )
          )
        add_subdiv_proj_simple_plot(g, vect_subdiv) %>% print()
        
        
        ## leaflet maps
        
        # export class and optimal value as polygon
        vect_clas_gmp_cat <-
          as.polygons(rast_plot_max, dissolve = F)
        vect_filename <-
          as.character(paste0("Good-Moderate-Poor", stack_code, "_", innovation))
        output_vect(vect_clas_gmp_cat, vect_filename)
        
        # project to geo for display in leaflet
        vect_clas_gmp_cat_geo <- project(vect_clas_gmp_cat, "epsg:4326")
        
        # convert to sf until leaflet properly accepts spatvector
        sf_clas_gmp_cat_geo <- st_as_sf(vect_clas_gmp_cat_geo)
        
        class_palette <- (c(
          #Good =
          rgb(178, 223, 138, maxColorValue = 255),
          #Moderate = 
          rgb(255, 255, 153, maxColorValue = 255),
          #Poor =
          rgb(227, 26, 28, maxColorValue = 255)
          
        ))
        
        
        pal <-
          colorFactor(class_palette,
                      domain = c(1:3),
                      na.color = "#808080")
        
        leaflet_widget <- leaflet(sf_clas_gmp_cat_geo, width = "100%") %>%
          addProviderTiles(providers$OpenStreetMap.HOT, group = "OSM (HOT)") %>%
          addPolygons(
            label = ~ stringr::str_c('class =', class),
            labelOptions = labelOptions(direction = 'auto'),
            color = "#03F",
            weight = 1,
            opacity = 0.5,
            fill = layer,
            fillColor = ~ pal(class),
            fillOpacity = 0.2,
            dashArray = NULL,
            smoothFactor = 1,
            noClip = FALSE,
            popup = paste('Class  =', sf_clas_gmp_cat_geo$class),
            popupOptions = NULL,
            highlightOptions = NULL,
          )
        
        return(leaflet_widget)
        
      } else {
        
        rast_plot_gp <- rast(list(rast_plot_good, rast_plot_poor))
        
        # get max value and export
        rast_plot_max <- which.max(rast_plot_gp)
        names(rast_plot_max) <- c("class")
        rast_gp_brick <- rast(list(rast_plot_good, rast_plot_poor, rast_plot_max))
        #names(rast_opt_subopt_brick) <- c('high', 'moderate', 'low', 'class')
        output_geotiff(rast_gp_brick, paste0("gp", stack_code, innovation))
        
        # plot the max value
        rast_plot_name <- names(rast_plot_max)
        
        # classify values and export
        
        df_clas_gp <-
          data.frame(id = 1:2, class = c("Good", "Poor"))
        levels(rast_plot_max) <- df_clas_gp
        
        # rast_clas_fao_brick <-
        #   rast(list(rast_plot, rast_clas_fao_bin, rast_clas_fao_cat))
        # names(rast_clas_fao_brick) <- c(rast_plot_name, 'bin', 'FAO')
        # output_geotiff(rast_clas_fao_brick,
        #                paste0("FAO_", stack_code, "_", innovation))
        # 
        # assign(paste0("rast_clas_fao_cat", fpm_plot_title),
        #        rast_clas_fao_cat,
        #        .GlobalEnv)
        # 
        # list_rast_clas_FAO_cat <<-
        #   append(list_rast_clas_FAO_cat,
        #          paste0("rast_clas_fao_cat", fpm_plot_title))
        
        g <-  ggplot() +
          geom_spatraster(data = rast_plot_max, na.rm = TRUE, aes(fill = class)) +
          scale_fill_manual(
            name = paste0("Good-Poor\n", fpm_plot_title, "\n", innovation),
            na.value = "transparent",
            na.translate = F,
            values = c(
              Good = rgb(51, 160, 44, maxColorValue = 255),
              Poor = rgb(227, 26, 28, maxColorValue = 255)
            )
          )
        add_subdiv_proj_simple_plot(g, vect_subdiv) %>% print()
        
        
        ## leaflet maps
        
        # export class and optimal value as polygon
        vect_clas_gp_cat <-
          as.polygons(rast_plot_max, dissolve = F)
        vect_filename <-
          as.character(paste0("Good-Poor", stack_code, "_", innovation))
        output_vect(vect_clas_gp_cat, vect_filename)
        
        # project to geo for display in leaflet
        vect_clas_gp_cat_geo <- project(vect_clas_gp_cat, "epsg:4326")
        
        # convert to sf until leaflet properly accepts spatvector
        sf_clas_gp_cat_geo <- st_as_sf(vect_clas_gp_cat_geo)
        
        class_palette <- (c(
          #Good =
          rgb(178, 223, 138, maxColorValue = 255),
          #Poor =
          rgb(227, 26, 28, maxColorValue = 255)
          
        ))
        
        
        pal <-
          colorFactor(class_palette,
                      domain = c(1:2),
                      na.color = "#808080")
        
        leaflet_widget <- leaflet(sf_clas_gp_cat_geo, width = "100%") %>%
          addProviderTiles(providers$OpenStreetMap.HOT, group = "OSM (HOT)") %>%
          addPolygons(
            label = ~ stringr::str_c('class =', class),
            labelOptions = labelOptions(direction = 'auto'),
            color = "#03F",
            weight = 1,
            opacity = 0.5,
            fill = layer,
            fillColor = ~ pal(class),
            fillOpacity = 0.2,
            dashArray = NULL,
            smoothFactor = 1,
            noClip = FALSE,
            popup = paste('Class  =', sf_clas_gp_cat_geo$class),
            popupOptions = NULL,
            highlightOptions = NULL,
          )
        
        return(leaflet_widget)
      
      }
    }
  }
