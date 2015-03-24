
# Define topology: who eats whom ?
trophic_topology <- list(list(from=c(5,6), to=c(1,2,3,4)),
                        list(from=c(7,8), to=c(5,6)))
trophic_topology <- gen_interaction_matrix(trophic_topology, 8)

dat <- data.frame(species = c('ag1','ag2','ag3','ag4',
                              'ic1', 'ic2', 'tp1', 'tp2'),
                  x       = c(1,2,3,4, 2,3, 2,3),
                  y       = c(1,1,1,1, 2,2, 3,3))

library(ggplot2)

dat.plot <- ggplot(dat) + 
              geom_point(aes(x,y), size=4) + 
              geom_path(aes(x,y), data=ggedge(dat,trophic_topology)) +
              ylab('Trophic level') + 
              xlab('') + 
              theme(axis.text.x=element_blank(), 
                    axis.ticks.x=element_blank())


export_plot(dat.plot, "trophic_topology.png", 6, 4)
