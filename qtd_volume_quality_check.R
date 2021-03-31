ibema_group_split <- ibema %>% 
  group_by(across(where(is.factor))) %>% 
  group_split()

ibema_group_split[[2]] %>% 
  select(-where(is.factor)) %>% 
  pivot_longer(-dat_movimento) %>% 
  filter(name == "qtd_volume") %>% 
  pad_by_time(.date_var = dat_movimento, .pad_value = 0) %>% 
  plot_time_series(.date_var = dat_movimento, value, .smooth = FALSE)

ibema_group_split[[2]] %>% 
  select(-where(is.factor)) %>% 
  pivot_longer(-dat_movimento) %>% 
  filter(name == "qtd_volume") %>% 
  pad_by_time(.date_var = dat_movimento, .pad_value = 0) %>% 
  rename(qtd_volume = value) %>% 
  select(-name) %>% 
  rio::export("00_output/qtd_volume.xlsx")

ibema_group_split[[2]] %>% 
  select(where(is.factor)) %>% 
  .[1, ]