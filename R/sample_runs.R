
library(tidyr)
library(ggplot2)

# Create base systems
sys_withnti <- syspreset_rockyshore_nti(tmax=10e3, 
                                         remove_species=TRUE) %>%
                  compile.system()
sys_trophic <- sys_withnti %>% 
                  alter_parms(dK=matrix(0, nrow=8, ncol=8)) 

# Define one simulation 
runsim <- function(sys) { 
  sys %>% 
    alter_system(state=runif(get_size(sys),0,1)) %>% 
    alter_parms(q=runif(1,0,1)) %>% 
    run
}

# Run all simulations
hundred_replicates <- function(sys) { 
  sys %>%
    mrun(100, runsim, .progress='time') %>% 
    zero_below(1e-8) %>%
    adjust_names %>% 
    select_ranges(initial=c(0, 300),
                  after_removal=c(2950, 3300)) %>%
    as.data.frame -> dat
}

plot_and_format <- function(dat) { 
plot.dat <- gather(dat,sp,ab,sp1:sp8) 
plot.obj <- ggplot(plot.dat) + 
              geom_line(aes(time, ab, color=sp, group=id), alpha=.2) + 
              facet_grid(sp ~ range, scales='free_x') 
}

# Adjust data and plot trophic_only results
sys_withnti %>% hundred_replicates %>% plot_and_format -> plot.obj
print(plot.obj)
export_plot(plot.obj, "trophic_sample_output.png", 7, 6)

sys_trophic %>% hundred_replicates %>% plot_and_format -> plot.obj
print(plot.obj)
export_plot(plot.obj, "trophic_sample_output.png", 7, 6)
