
library(tidyr)
library(ggplot2)
library(dplyr)

# Create base systems
sys_withnti <- syspreset_rockyshore_nti(tmax=10e3) %>%
                 set_removal(species=5, at=3000) %>% 
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
  with_fcache(mrun)(., 10, one_replicate, .progress='time', .parallel=parjob()) %>% 
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

# Adjust data and plot trophic_only results
sys_trophic %>% run_replicates %>% format -> dat
plot.obj <- ggplot(dat) + 
              geom_line(aes(range, nextinct, group = id), alpha=.5) + 
              facet_wrap( ~ trophic_level )
print(plot.obj)
export_plot(plot.obj, "trophic_sec_extinct.png", 7, 6)

sys_withnti %>% run_replicates %>% format -> dat
plot.obj <- ggplot(dat) + 
              geom_line(aes(range, nextinct, group = id), alpha=.5) + 
              facet_wrap( ~ trophic_level )
print(plot.obj)
export_plot(plot.obj, "ntrophic_sec_extinct.png", 7, 6)
