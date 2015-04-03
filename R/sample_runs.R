
library(tidyr)
library(ggplot2)


# Define one simulation 
one_simulation <- function(sys) { 
  sys %>% 
    alter_system(state=runif(get_size(sys),0,1)) %>% 
    alter_parms(q=runif(1,0,1)) %>% 
    run %>% 
    insert_parms(q) # insert some param values as columns
}

# Run all simulations
hundred_replicates <- . %>% with_fcache(mrun)(100, one_simulation, 
                                              .progress='time') %>% 
                            zero_below(1e-5) 

format <- . %>% select_ranges(initial=c(0, 300),
                              after_removal=c(2950, 3300)) %>%
                adjust_names %>%
                as.data.frame %>% 
                gather(sp, ab, sp1:sp8) 

# Create base systems
sys_withnti <- syspreset_rockyshore_nti(tmax=5000) %>% 
                 set_removal(species=5, at=3000) %>%
                 compile.system()
sys_trophic <- sys_withnti 

# Adjust data and plot results
sys_trophic %>% hundred_replicates %>% format -> dat
plot.obj <- ggplot(dat) + 
              geom_line(aes(time, ab, color=sp, group=id), alpha=.2) + 
              facet_grid(sp ~ range, scales='free_x') + 
              scale_y_sqrt()
print(plot.obj)
export_plot(plot.obj, "trophic_sample_output.png", 9, 7)

sys_withnti %>% hundred_replicates %>% format -> dat
plot.obj <- ggplot(dat) + 
              geom_line(aes(time, ab, color=sp, group=id), alpha=.2) + 
              facet_grid(sp ~ range, scales='free_x') + 
              scale_y_sqrt()
print(plot.obj)
export_plot(plot.obj, "ntrophic_sample_output.png", 9, 7)
