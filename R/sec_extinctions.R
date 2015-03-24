
library(tidyr)
library(ggplot2)

# Create base systems
sys_withnti <- syspreset_rockyshore_nti(tmax=10e3, 
                                        remove_species=TRUE) %>%
                 compile.system()
sys_trophic <- sys_withnti %>% 
                 alter_parms(dK=matrix(0, nrow=8, ncol=8)) # all dKs to 0 -> no nti

# Define one simulation 
one_replicate <- . %>% 
  alter_system(state=runif(get_size(.),0,1)) %>% 
  alter_parms(q=runif(1,0,1)) %>% 
  run

# Run all simulations
run_replicates <- . %>%
  FC(mrun, ., 10e3, one_replicate, .progress='time', .parallel=parjob()) %>% 
  zero_below(1e-8) %>%
  adjust_names() %>% 
  select_ranges(before_removal = c(2699, 2999),
                after_removal  = c(10e3-300, 10e3)) %>%
  as.data.frame()

# Format data
format <- . %>% 
  gather(sp, ab, sp1:sp8) %>%  
  group_by(id, range, sp) %>% 
  summarise(abm = median(ab)) %>% 
  mutate(trophic_level = sp2tl(sp)) %>% 
  group_by(id, range, trophic_level) %>% 
  summarise(nextinct = sum(abm==0))

# Create a plot
create_plot <- . %>% 
  { ggplot(.) + 
    geom_line(aes(range, nextinct, group = id), alpha=.5) + 
    facet_wrap( ~ trophic_level ) }


# Adjust data and plot trophic_only results
sys_withnti %>% run_replicates %>% format %>% create_plot -> plot.obj
print(plot.obj)
export_plot(plot.obj, "trophic_sec_extinct.png", 7, 6)

sys_trophic %>% run_replicates %>% format %>% create_plot -> plot.obj
print(plot.obj)
export_plot(plot.obj, "ntrophic_sec_extinct.png", 7, 6)
