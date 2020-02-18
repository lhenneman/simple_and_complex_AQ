library( data.table)
library( ggplot2)
library( viridis)

# CHANGE ME
setwd( '/DIRECTORY/LOCATION')

# source R script containing plotting functions
source( 'functions.R')

## ====================================================== ##
#   read in the data, 
## ====================================================== ##
# annual data
data.in <- fread( 'dataset_annual.csv')
summary( data.in)

# monthly data
data.month.in <- fread( 'dataset_monthly.csv')
summary( data.month.in)

## ====================================================== ##
#   ANNUAL: summarize, melt for easier plotting 
## ====================================================== ##
# impacts by state -- total from all coal units
data.in_bystate <- data.in[, .( idwe_bystate = sum( idwe.pw),
                                hyads_bystate = sum( hyads.pw),
                                adj.initial_bystate = sum( initial)),
                           by = .( state, year)]

data.in.m <- melt( data.in, id.vars = c( 'uID', 'year', 'state', 'idwe.pw', 'hyads.pw',
                                         'Longitude', 'Latitude'),
                   measure.vars = c( 'initial', 'layers_2-5', 'stack_height', 
                                     'stack_height_plus1', 'stack_height_plus2'),
                   variable.name = 'adj_name', value.name = 'adj.pw')

# factorize state to keep order
states.use <- c( 'US', 'CA', 'CO', 'TX', 'WI', 'KY', 'GA', 'PA')
data.in.m[, state := factor(`state`, levels =  states.use)]

## ====================================================== ##
#  Figure 2
#  plot top 50 ranked facilities for adjoint
## ====================================================== ##
# Rank adjoint facilities by impact on each state
data.in.m[, adj_popwgt.rank := frank( adj.pw), by = .( year, adj_name, state)]

# create the plot using the function above
ranks_US50.adj <- plot_ranked_facs( ranks.dt = data.in.m,
                                    state.abbrev = 'US',
                                    rank.name = 'adj_popwgt.rank',
                                    size.var = 'adj.pw',
                                    size.name = 'Adjoint population-weighted',
                                    toprank = 50,
                                    geo.name = NULL,
                                    dist.scalebar = 1000,
                                    latlonrange.year = 2006,
                                    xlims = c(-123, -68), 
                                    ylims = c(25, 49) 
)
ranks_US50.adj$plot

## ====================================================== ##
#  Figure 3
#  Evaluation by year/state
## ====================================================== ##
## calculate the evaluation metrics for HyADS and IDWE
eval.hyads <- data.in.m[, eval.fn( hyads.pw, adj.pw, 'HyADS'), by = .( state, year, adj_name)]
eval.idwe  <- data.in.m[, eval.fn( idwe.pw,  adj.pw, 'IDWE'),  by = .( state, year, adj_name)]
eval.all <- rbind( eval.hyads, eval.idwe)

# melt
eval.all.m <- melt( eval.all, id.vars = c( 'state', 'year', 'adj_name', 'mod.name'),
                    variable.name = 'metric')

# redenane metric names
eval.all.m[metric == 'R.p', metric := 'Pearson~R']
eval.all.m[metric == 'R.s', metric := 'Spearman~R']

## define adjoint names, set up for plotting
adj.names <- data.table( adj_name = c( "initial", "layers_2-5", "stack_height", "stack_height_plus1", "stack_height_plus2"),
                         adj_name.adjust = c("Average", "Layers 2-5", "Stack Height", "Stack Height +1", "Stack Height +2"))
corrs.all <- merge( eval.all.m, adj.names, by = 'adj_name')
corrs.all[, state.factor := factor( state, levels = states.use)]

# don't plot very high NME for CA and CO
cors.all.use <- corrs.all[ metric %in% c( 'Pearson~R', 'NMB', 'RMSE')]
corrs.removed <- cors.all.use[ state.factor %in% c( 'CA', 'CO') & value > 2]
cors.all.use[ state.factor %in% c( 'CA', 'CO') & value > 2, value := NA]
cors.all.use[ metric == 'NMB', value := value * 100]
cors.all.use[ metric == 'NMB', metric := 'NMB~"%"']
cors.all.use[ metric == 'RMSE', metric := 'RMSE~mu*"g"~m^{-3}']

# create the plot
gguse <- ggplot( data = cors.all.use,
                 aes( x = state.factor,
                      y = value,
                      color = mod.name,
                      shape = adj_name.adjust
                 )) +
  geom_hline( yintercept = 0) +
  geom_point( size = 4,
              position = position_dodge( width = .25)) + 
  scale_shape_manual( values = c( 0:2, 5:6)) +
  scale_color_manual( values = c( '#479ddd', '#dd8747')) +
  geom_vline( xintercept = 1.5,
              lty = 2) +
  facet_grid( metric ~ year, scales = 'free_y', labeller = label_parsed) +
  expand_limits( y = 0) +
  scale_y_continuous( expand = expansion( .1)) +
  guides(
    color = guide_legend(order = 1,
                         keywidth = 1.5),
    shape = guide_legend(order = 0,
                         keywidth = 1.5)
  ) +
  theme_bw() +
  theme( axis.text = element_text( size = 12),
         axis.title = element_blank( ),
         legend.direction = 'horizontal',
         legend.position = 'bottom',
         legend.text = element_text( size = 12),
         legend.title = element_blank(),
         panel.grid.major.x = element_line( size = 10,
                                            color = 'grey90'),
         panel.grid.minor = element_blank(),
         strip.background = element_blank(),
         strip.text = element_text( size = 18))

gguse


## ====================================================== ##
#   MONTHLY: summarize, melt for easier plotting 
## ====================================================== ##
# apply evaluation function
data.month.eval <- data.month.in[, eval.fn( idwe.pw, hyads.pw), 
                                 by = .( state_abbr, month)]

# melt
data.month.eval.m <- melt( data.month.eval, id.vars = c( 'state_abbr', 'month'),
                             variable.name = 'metric')

# formatting for plots
states.use.mo <- c( 'US', 'PA', 'KY', 'GA', 'WI', 'TX', 'CO', 'CA')
data.month.eval.m[, `:=` ( year.n = year( month),
                             month.n =  factor( month.name[ month( month)], levels = month.name),
                             state.factor = factor( state_abbr, levels = states.use.mo))]


## ====================================================== ##
#  Figure 4
#  monthly evaluations
## ====================================================== ##
# define rectangles for legibility
rect_bot2 <- seq( -30, 30, 1)
rectangles2 <- data.frame(
  xmin = rect_bot2,
  xmax = rect_bot2 + .5,
  ymin = -Inf,
  ymax = +Inf,
  fill = 'grey50'
)

# plot normalized mean error/bias
ggNMB <- ggplot( data = data.month.eval.m[metric %in% c( 'NMB')],
                 aes( x = as.Date( month),
                      y =  value)) +
  coord_cartesian( ylim = c( -1,3.5)) +
  scale_x_date( date_labels = '%B', breaks = unique( as.Date( data.month.eval.m$month))) +
  scale_y_continuous( labels = scales::percent_format(accuracy = 1)) +
  geom_rect( data = rectangles2,
             aes( ymin = xmin,
                  ymax = xmax),
             fill = 'grey95',
             xmin = -Inf,
             xmax = +Inf,
             inherit.aes = FALSE) +
  geom_hline( yintercept = 1,
              size = 0.5,
              color = 'grey50') + 
  geom_hline( yintercept = 0,
              size = 0.5,
              color = 'grey50') + 
  geom_line( size = 1.5) +
  facet_grid( state.factor ~ year.n, scales = 'free') +
  ylab( "NMB") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        axis.text = element_text( size = 14),
        axis.text.x = element_text( angle = 45,
                                    vjust = 1,
                                    hjust = 1),
        axis.title = element_text( size = 20),
        axis.title.x = element_blank(),
        legend.position = 'bottom', #c( .185, .025),	
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.box.background = element_rect(),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.direction = 'horizontal',
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(), #rect(fill='white'),
        strip.text = element_text(size=24))
ggNMB
## ====================================================== ##
#  supplementary Figures
#  monthly evaluations
## ====================================================== ##
rect_bot <- seq( -10.75, 10.75, .5)
rect_bot3 <- seq( -30, 30, .04)
rect_bot4 <- seq( -30, 30, .02)
rectangles <- data.frame(
  xmin = rect_bot,
  xmax = rect_bot + .25,
  ymin = -Inf,
  ymax = +Inf,
  fill = 'grey50'
)
rectangles3 <- data.frame(
  xmin = rect_bot3,
  xmax = rect_bot3 + .02,
  ymin = -Inf,
  ymax = +Inf,
  fill = 'grey50'
)
rectangles4 <- data.frame(
  xmin = rect_bot4,
  xmax = rect_bot4 + .01,
  ymin = -Inf,
  ymax = +Inf,
  fill = 'grey50'
)

# plot correlations across the year
ggcors <- ggplot( data = data.month.eval.m[metric %in% c( 'R.p', 'R.s')],
                  aes( x = as.Date( month),
                       y =  value, #substring( value, 2)
                       color = metric)) +
  coord_cartesian( ylim = c( 0,1)) +
  scale_x_date( date_labels = '%B', breaks = unique( as.Date( data.month.eval.m$month))) +
  geom_rect( data = rectangles,
             aes( ymin = xmin,
                  ymax = xmax),
             fill = 'grey95',
             xmin = -Inf,
             xmax = +Inf,
             inherit.aes = FALSE) +
  geom_hline( yintercept = 1,
              size = 0.5,
              color = 'grey50') + 
  geom_hline( yintercept = 0,
              size = 0.5,
              color = 'grey50') + 
  geom_line( size = 1.5) +
  facet_grid( state.factor ~ year.n, scales = 'free_x') +
  scale_color_viridis( begin = 0.4, end = 0.8, discrete = T,
                       breaks = c( 'R.p', 'R.s'),
                       labels = c( 'Pearson', 'Spearman')) +
  ylab( "Correlations") +
  xlab( "Month") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        axis.text = element_text( size = 14),
        axis.text.x = element_text( angle = 45,
                                    vjust = 1,
                                    hjust = 1),
        axis.title = element_text( size = 20),
        axis.title.x = element_blank(),
        legend.position = 'bottom', #c( .185, .025),	
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.box.background = element_rect(),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.direction = 'horizontal',
        panel.grid = element_blank(),
        strip.background = element_blank(), #rect(fill='white'),
        strip.text = element_text(size=24))
ggcors

# plot normalized mean error/bias
ggNME <- ggplot( data = data.month.eval.m[metric %in% c( 'NME')],
                 aes( x = as.Date( month),
                      y =  value)) +
  coord_cartesian( ylim = c( 0,4)) +
  scale_x_date( date_labels = '%B', breaks = unique( as.Date( data.month.eval.m$month))) +
  scale_y_continuous( labels = scales::percent_format(accuracy = 1)) +
  geom_rect( data = rectangles,
             aes( ymin = xmin,
                  ymax = xmax),
             fill = 'grey95',
             xmin = -Inf,
             xmax = +Inf,
             inherit.aes = FALSE) +
  geom_hline( yintercept = 1,
              size = 0.5,
              color = 'grey50') + 
  geom_hline( yintercept = 0,
              size = 0.5,
              color = 'grey50') + 
  geom_line( size = 1.5) +
  facet_grid( state.factor ~ year.n, scales = 'free_x') +
  ylab( "NME") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        axis.text = element_text( size = 14),
        axis.text.x = element_text( angle = 45,
                                    vjust = 1,
                                    hjust = 1),
        axis.title = element_text( size = 20),
        axis.title.x = element_blank(),
        legend.position = 'bottom', #c( .185, .025),	
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.box.background = element_rect(),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.direction = 'horizontal',
        panel.grid = element_blank(),
        strip.background = element_blank(), #rect(fill='white'),
        strip.text = element_text(size=24))
ggNME

# plot mean error and MSE
ggME <- ggplot( data = data.month.eval.m[metric %in% c( 'ME', 'RMSE')],
                aes( x = as.Date( month),
                     y =  value#, #substring( value, 2)
                )) +
  coord_cartesian( ylim = c( 0,.099)) +
  scale_x_date( date_labels = '%B', breaks = unique( as.Date( data.month.eval.m$month))) +
  scale_y_continuous( breaks = seq( 0,.08,.04)) +
  geom_rect( data = rectangles3,
             aes( ymin = xmin,
                  ymax = xmax),
             fill = 'grey95',
             xmin = -Inf,
             xmax = +Inf,
             inherit.aes = FALSE) +
  geom_hline( yintercept = 0,
              size = 0.5,
              color = 'grey50') + 
  geom_line( aes( color = metric), size = 1.5) +
  facet_grid( state.factor ~ year.n, scales = 'free_x') +
  scale_color_viridis( begin = 0.4, end = 0.8, discrete = T) +
  ylab( parse( text = 'list(ME~and~RMSE,mu*"g"~m^{-3})')) +
  xlab( "Month") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        axis.text = element_text( size = 14),
        axis.text.x = element_text( angle = 45,
                                    vjust = 1,
                                    hjust = 1),
        axis.title = element_text( size = 20),
        axis.title.x = element_blank(),
        legend.position = 'bottom', #c( .185, .025),	
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.box.background = element_rect(),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.direction = 'horizontal',
        panel.grid = element_blank(),
        strip.background = element_blank(), #rect(fill='white'),
        strip.text = element_text(size=24))
ggME

# plot mean bias
ggMB <- ggplot( data = data.month.eval.m[metric %in% c( 'MB')],
                aes( x = as.Date( month),
                     y =  value)) +
  coord_cartesian( ylim = c( -.025,.03)) +
  scale_x_date( date_labels = '%B', breaks = unique( as.Date( data.month.eval.m$month))) +
  scale_y_continuous( breaks = seq( -.02, .02, .02)) +
  geom_rect( data = rectangles4,
             aes( ymin = xmin,
                  ymax = xmax),
             fill = 'grey95',
             xmin = -Inf,
             xmax = +Inf,
             inherit.aes = FALSE) +
  geom_hline( yintercept = 1,
              size = 0.5,
              color = 'grey50') + 
  geom_hline( yintercept = 0,
              size = 0.5,
              color = 'grey50') + 
  geom_line( size = 1.5) +
  facet_grid( state.factor ~ year.n, scales = 'free_x') +
  ylab( parse( text = 'list(MB,~mu*"g"~m^{-3})')) +
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        axis.text = element_text( size = 14),
        axis.text.x = element_text( angle = 45,
                                    vjust = 1,
                                    hjust = 1),
        axis.title = element_text( size = 20),
        axis.title.x = element_blank(),
        legend.position = 'bottom', #c( .185, .025),	
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.box.background = element_rect(),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.direction = 'horizontal',
        panel.grid = element_blank(),
        strip.background = element_blank(), #rect(fill='white'),
        strip.text = element_text(size=24))
ggMB



