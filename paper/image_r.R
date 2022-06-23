dbs = seq(-130, -50, by = 10)
s = mobloc::db2s(dbs, midpoint = -92.5, steepness = 0.2)

library(tidyverse)
library(cowplot)

df = expand.grid(S1 = dbs, S2 = dbs)
df$s1 = mobloc::db2s(df$S1, midpoint = -92.5, steepness = 0.2)
df$s2 = mobloc::db2s(df$S2, midpoint = -92.5, steepness = 0.2)
df$r = df$s1 / (df$s1 + df$s2)
df$perc = round(df$r * 100)

tidyr::pivot_wider(df %>% select(S1,S2,perc), names_from = S2, values_from = perc)

plot(df$S2, df$s2)


(g = ggplot(df, aes(y = as.factor(S1), x = as.factor(S2), fill = perc, label = perc)) +
    geom_tile(color = "black") +
    geom_text(size = 2) +
    scale_fill_distiller("Percentage of connection\nto the first cell", palette = "Spectral", direction = 1) +
    #scale_fill_distiller(expression(r(S[1],S[2])~"(in %)"), palette = "Spectral", direction = 1) +
    scale_x_discrete(expression(S[2]~("in"~dBm))) +
    scale_y_discrete(expression(S[1]~("in"~dBm))) +
    theme_minimal(base_size = 8) +
    coord_fixed(1))




ggsave("output/r.png", plot = g, width = 1600, height = 1000, units = "px")



#
# plot(dbs, s)
#
# exp = list()
#
# set.seed(1234)
#
# n = 1000
#
# ks = round(runif(n, min = 1.5, max = 5.499999))
#
# S = sample(dbs, size = sum(ks), replace = TRUE)
# n2 = sum(ks)
#
# ind = unlist(mapply(rep, seq_len(n), ks, SIMPLIFY = FALSE))
#
# S2 = split(S, ind)
#
#
# ps = lapply(S2, function(Si) {
#     s = mobloc::db2s(Si, midpoint = -92.5, steepness = 0.2)
#     s / sum(s)
# })
#
# sels = mapply(
#
# )





