library("RSimpactCyan")
library("ggplot2")


simpact.set.simulation("maxart")

# In a first simulation, we'll just initialize the number of men and women to 150,000 each.
# Relationships are disabled by setting population.eyecap.fraction to 0, to speed up initialization
# and to reduce memory requirements, and the simulation is told to stop after one event. These
# settings will already allow us to plot a histogram of the simulated person positions, which
# should then resemble the population density from the Hhohho region.
cfg <- list()
cfg["population.nummen"] <- 150000
cfg["population.numwomen"] <- 150000
cfg["population.eyecap.fraction"] <- 0
cfg["population.maxevents"] <- 1
cfg["facilities.randomization"] <- "${SIMPACT_DATA_DIR}/maxart-randomization-fake_1.csv"

# look at the full configuration
simpact.showconfig(cfg)


# runs simpact cyan
res = simpact.run(cfg, "Analyses/simpactCyan_tests/simptest")



# Bin size control + color palette
#ggplot(data, aes(x=x, y=y) ) +
#  geom_bin2d(bins = 70) +
#  scale_fill_continuous(type = "viridis") +
#  theme_bw()

persons <- read.csv(res$logpersons)

ggplot(persons, aes(x=XCoord, y=YCoord) ) +
  geom_bin2d(bins = 100) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

xhist(persons$XCoord, persons$YCoord, breaks = 100)

plt.figure(1, figsize=(8,8))
plt.hist2d(persons["XCoord"], persons["YCoord"], bins=100, norm=matplotlib.colors.LogNorm());
plt.gca().set_aspect("equal")
plt.gca().invert_yaxis()
