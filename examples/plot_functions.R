########################
### distance_plot: plot signal strength over distance
########################

# Antenna of 5W, path loss exponent 2, which resembles free space
distance_plot(W = 5, ple = 2, range = 10000, show_classes = TRUE)

# Antenna of 5W, path loss exponent 4, which resembles urban area
distance_plot(W = 5, ple = 5, range = 1000, show_classes = TRUE)



########################
### signal_dominance: plot the relation between signal strength and the signal dominance,
### which is an indicator between 0 and 1 which represents the (modeled) value of connection
########################

# The default parameter values. The colors classes indicate the value of connection: from red
# (no/hardly connectivity, to yellow (fair connectivity) to blue/purple (excellent connectivity).
signal_dominance_plot(midpoint = -92.5, steepness = .2, show_classes = TRUE)

# explore the steepness parameter
gridExtra::grid.arrange(
    {signal_dominance_plot(-90, steepness = .1, show_classes = FALSE)},
    {signal_dominance_plot(-90, steepness = .25, show_classes = FALSE)},
    {signal_dominance_plot(-90, steepness = .5, show_classes = FALSE)},
    {signal_dominance_plot(-90, steepness = 1, show_classes = FALSE)},
    ncol = 1
)

########################
### radiation_plot: plot the relation between signal strength and the signal dominance,
### which is an indicator between 0 and 1 which represents the (modeled) value of connection
########################

# Azimuth plan pattern with a beam width of 64 degrees
# (so -32 and 32 degrees deviation means a signal loss of 3dB, the red dots)
radiation_plot(type = "a", beam_width = 64, db_back = -30)

# Elevation plan pattern with a beam width of 12 degrees
# (so -6 and 6 degrees deviation means a signal loss of 3dB, the red dots)
radiation_plot(type = "e", beam_width = 12, db_back = -30)
