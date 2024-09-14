# SAFMR payment Standards Oct, 2023
# *Need to update on Oct 1, 2024*

library(tidyverse)

payment_standards <- data.frame(
  zip_code = c(19120, 19124, 19126, 19132, 19133, 19134, 19136, 19139, 19140, 19141, 19142, 19143, 19151, # Group 1
               19101, 19104, 19105, 19109, 19110, 19111, 19112, 19114, 19115, 19116, 19119, 19121, 19122, # Group 2
               19131, 19135, 19137, 19138, 19144, 19145, 19149, 19150, 19152, # Group 3
               19118, 19127, 19146, 19147, # Group 4
               19102, 19103, 19106, 19107, 19123, 19130), # Group 5
  safmr = rep(c(1, 2, 3, 4, 5), times = c(13, 13, 9, 4, 6)),
  safmr_label = c(rep("Basic Rents", 13), rep("Traditional Rents", 13), rep("Mid Range Rents", 9), rep("Opportunity Rents", 4), rep("High Opportunity Rents", 6)),
  cost_sro = c(rep(828, 13), rep(990, 13), rep(1197, 9), rep(1449, 4), rep(1584, 6)),
  cost_0br = c(rep(1104, 13), rep(1320, 13), rep(1596, 9), rep(1932, 4), rep(2112, 6)),
  cost_1br = c(rep(1236, 13), rep(1476, 13), rep(1776, 9), rep(2160, 4), rep(2352, 6)),
  cost_2br = c(rep(1476, 13), rep(1764, 13), rep(2124, 9), rep(2580, 4), rep(2820, 6)),
  cost_3br = c(rep(1788, 13), rep(2136, 13), rep(2568, 9), rep(3120, 4), rep(3408, 6)),
  cost_4br = c(rep(2064, 13), rep(2460, 13), rep(2964, 9), rep(3600, 4), rep(3936, 6)),
  cost_5br = c(rep(2373, 13), rep(2829, 13), rep(3408, 9), rep(4140, 4), rep(4526, 6)),
  cost_6br = c(rep(2683, 13), rep(3198, 13), rep(3853, 9), rep(4680, 4), rep(5116, 6)),
  cost_7br = c(rep(2992, 13), rep(3567, 13), rep(4297, 9), rep(5220, 4), rep(5707, 6)),
  cost_8br = c(rep(3302, 13), rep(3936, 13), rep(4742, 9), rep(5760, 4), rep(6297, 6))
)

st_write(payment_standards, "safmr_groups_2023.csv", driver = "csv")
