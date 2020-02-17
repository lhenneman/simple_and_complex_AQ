## =========================================================== ##
## Calculate the evaluation metrics
## =========================================================== ##
eval.fn <- function( Yhat, Yact, mod.name = NULL){
  num.diff <- sum( Yhat - Yact, na.rm = T)
  abs.diff <- sum( abs( Yhat - Yact), na.rm = T)
  denom <- sum( Yact, na.rm = T)
  metrics <- data.table( mod.name = mod.name,
                         NMB = num.diff / denom,
                         NME = abs.diff / denom,
                         MB   = num.diff / length( Yhat),
                         RMSE = sqrt( sum( ( Yhat - Yact) ^ 2, na.rm = T) / length( Yhat)),
                         R.p = cor( Yhat, Yact, use = 'complete.obs') ^ 2,
                         R.s = cor( Yhat, Yact, use = 'complete.obs', method = 'spearman') ^ 2)
  return( metrics)
}

## =========================================================== ##
## Plot top units by impact
## =========================================================== ##

plot_ranked_facs <- function( ranks.dt,
                              state.abbrev = 'US',
                              rank.name = 'hyspdisp.rank',
                              size.var = 'hyspdisp.py.mean',
                              size.name = 'Pop-weighted HyADS',
                              toprank = 10,
                              geo.name = NULL,
                              xlims = NULL,
                              ylims = NULL,
                              latlonrange.year = 2005,
                              dist.scalebar = 400){
  
  # -- limit data table to units under the rank -- #
  ranks.dt.trim <- ranks.dt[get( rank.name) <= toprank & state %in% state.abbrev]
  
  # -- set name of variable size variable -- #
  setnames( ranks.dt.trim, size.var, 'size.var')
  
  # -- link with PP data if not already  -- #
  if( !( 'Longitude' %in% names( ranks.dt.trim) & 'Latitude' %in% names( ranks.dt.trim))){
    D05 <- fread("~/Dropbox/Harvard/ARP/inMAP/Merge_AMPD_NEI/final_merge_nei_ampd_all_units_2005.csv")[, year := 2005]
    D06 <- fread("~/Dropbox/Harvard/ARP/inMAP/Merge_AMPD_NEI/final_merge_nei_ampd_all_units_2006.csv")[, year := 2006]
    D11 <- fread("~/Dropbox/Harvard/ARP/inMAP/Merge_AMPD_NEI/final_merge_nei_ampd_all_units_2011.csv")[, year := 2011]
    D12 <- fread("~/Dropbox/Harvard/ARP/inMAP/Merge_AMPD_NEI/final_merge_nei_ampd_all_units_2012.csv")[, year := 2012]
    D <- rbind( D05, D06, D11, D12)
    D[, uID := gsub('_|-|\\*', '.', ID)]
    ranks.dt.units <- merge( ranks.dt.trim, D, by = c( 'uID', 'year'))
  } else
    ranks.dt.units <- ranks.dt.trim
  
  # -- find lat/lon range  -- #
  if( is.null( xlims) & is.null( ylims)){
    latlonrange <- data.table( xlim = c( min( ranks.dt.units$Longitude) - .1,
                                         max( ranks.dt.units$Longitude) + .1),
                               ylim = c( min( ranks.dt.units$Latitude - .5),
                                         max( ranks.dt.units$Latitude + .1)),
                               year = latlonrange.year)
  } else
    latlonrange <- data.table( xlim = xlims,
                               ylim = ylims,
                               year = latlonrange.year)
  
  
  # -- download states  -- #
  states <- data.table( map_data("state"))
  # if( state.abbrev == 'US')
  #   state.abbrev <- state.abb
  state <- tolower( state.name[ match( state.abbrev, state.abb)])
  state.poly <- states[region %in% state]
  
  # -- make the plot  -- #
  if( is.null( geo.name)){
    plot.title <- NULL
  } else
    plot.title <- paste("Top facilities by pop-weighted exposure on", geo.name)
  
  # -- define columns for scalebar  -- #
  ranks.dt.units[, `:=` ( long = Longitude,
                          lat = Latitude)]
  
  # -- make the plot  -- #
  gg_coal <- ggplot() + 
    theme_bw() + 
    labs(title = plot.title) +
    facet_wrap( . ~ year, ncol = 2) +
    geom_polygon(data = states, 
                 aes(x = long, y = lat, group = group),
                 fill = 'white', 
                 color = "grey70",
                 size = .25) + 
    geom_polygon(data = state.poly, 
                 aes(x = long, y = lat, group = group),
                 fill = 'thistle1', 
                 color = "black",
                 size = .25) + 
    coord_sf(
      xlim = latlonrange$xlim,
      ylim = latlonrange$ylim,
      datum = NA
    ) +
    geom_point(data = ranks.dt.units, 
               aes(x = Longitude, 
                   y = Latitude, 
                   color = size.var),
               stroke = 1,
               size = 3,
               shape = 21) +#,
    #      color = '#479ddd') + 
    geom_rect( data = latlonrange,
               aes(xmin = xlim[1] - 5, 
                   xmax = xlim[1] + (xlim[2] - xlim[1]) / 2, 
                   ymin = ylim[1] - 5, 
                   ymax = ylim[1] + .5), 
               fill = 'white', 
               color = NA) +
    scale_color_gradient( low = '#dd8747', 
                          high = '#4C061D', #'#6d213c',
                          guide = guide_colorbar(title.position = "left"),
                          name = bquote( atop( .( size.name), 'source impacts, µg'~m^{'-3'}))#,
                          # expression(paste('2012 total ', SO[2], ' emissions [tons]'))
                          # ylab( parse( text = 'list(PWSI["i,P"]^{m},~mu*"g"~m^{-3})')) +
                          # range = c(.5,5.0)
    ) +
    expand_limits(color = 0) +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5), #element_blank(), #
      axis.title = element_text(size = 24),
      axis.text = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.title = element_text(size = 16, vjust = 1, face = 'bold'),
      legend.title.align = 0,
      legend.position = "bottom", #c(.22, .15),
      legend.text = element_text(size = 14, angle = 30, vjust = .5),
      legend.background = element_rect(fill = 'transparent'),
      legend.key.size = unit(.04, 'npc'),
      legend.direction = 'horizontal',
      strip.text = element_text( size = 20, face = 'bold'),
      strip.background = element_blank( )
    )  +
    ggsn::scalebar( data = ranks.dt.units,
                    location = 'bottomleft',
                    anchor = c( x = latlonrange$xlim[1] - 1, y = latlonrange$ylim[1] + 2),
                    x.min = latlonrange$xlim[1],
                    y.min = latlonrange$ylim[1],
                    x.max = latlonrange$xlim[2],
                    y.max = latlonrange$ylim[2],
                    dist = dist.scalebar / 2, 
                    dist_unit = 'km',
                    height = 0.05, 
                    st.dist = 0.1, 
                    st.size = 4, 
                    box.fill = c( 'white', 'grey50'),
                    box.color = 'grey50',
                    transform = T,
                    model = 'WGS84',
                    facet.var = 'year',
                    facet.lev = as.character( latlonrange.year))
  
  print( gg_coal)
  return( list( dt = ranks.dt.units,
                plot = gg_coal,
                latlonrange = copy( latlonrange)[,year := NULL]))
  
}
