# Unleash the beasts
library(ggplot2)

# Load up the Data 
rural_exp <- read.csv('PATH')
urb_exp <- read.csv('PATH')
ed_prov <- read.csv('PATH')


# Plot lack of education by province
p1 <- ggplot(data = ed_prov, aes(x = reorder(X...Province, -NS_Perc), y = NS_Perc)) + 
  geom_bar(stat = 'identity', fill = 'dark blue') +
  xlab('Province') + 
  ylab('% of People without Schooling') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot primary school by province 
p3 <- ggplot(data = ed_prov, aes(x = reorder(X...Province, -Pri_Perc), y = Pri_Perc)) + 
  geom_bar(stat = 'identity', fill = 'dark green') +
  xlab('Province') + 
  ylab('% of People in Primary School') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot secondary school by province 
p4 <- ggplot(data = ed_prov, aes(x = reorder(X...Province, -RS_Perc), y = RS_Perc)) + 
  geom_bar(stat = 'identity', fill = 'dark red') +
  xlab('Province') + 
  ylab('% of People in Secondary Schoo') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot undergrad degrees by province 
p2 <- ggplot(data = ed_prov, aes(x = reorder(X...Province, -UG_Perc), y = UG_Perc)) + 
  geom_bar(stat = 'identity', fill = 'orange') +
  xlab('Province') + 
  ylab('% People with University Schooling') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = 'No_School.png', plot = p1, height = 12, units = "cm")
ggsave(filename = 'Uni.png', plot = p2, height = 12, units = "cm")
ggsave(filename = 'Pri_Perc.png', plot = p3, height = 12, units = "cm")
ggsave(filename = 'Rs_Perc.png', plot = p4, height = 12, units = "cm")


# Multiplot Function 
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
