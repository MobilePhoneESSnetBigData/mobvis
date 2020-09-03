---
output:
  pdf_document: default
  html_document: default
---
mobstat: processing Mobile Phone metadata for statistical purposes
===

This repo contains code to process mobile phone network data with the purpose to produce statistical output, such as daytime population.

Data
---

Data is not contained in this repo. Open datasets, such as CBS Wijk-Buurtdata, are downloaded by running the scripts. Other required datasets can be accessed in a private environment.

Output
---

The first output we want to produce is *the number of devices per hour per 100 squared meter grid cells* and *per 1000 squared meter grid cells*, where values less than 15 are reported as missing. These datasets are send to CBS for further analysis and to produce estimations on daytime population.

Input
---

* Cellplan, which contains information how cell towers are placed. Each technology (2G, 3G, 4G) has its own cellplan. Basically, there are four options in which cellplan data can be delivered:

    + Lat/lon coordinates of the cell towers. With this information, a [Voronoi tesselation](https://en.wikipedia.org/wiki/Voronoi_diagram) can be generated.
    + The following variables per cell (notice that a cell tower may contain multiple cells, typically 3): lat/lon coordinates, angle, mechanical and electrical tilt, vertical beamwdith (-3dB point), horizontal beamwidth (-3dB point), frequency (900, 1800, 2100 Hz).
    + Best server maps. These maps specify for each 70x100 meter grid cell which cell has the best connection. These maps are produced with measurement data.
    + Signal strength maps. For each cell the signal strength is measured for all surrounding locations.

* Signalling data, which contains all events on the network that are registered for billing purposes (CDR) and network analysis.


Procedures / algorithms
---

**1. Constructing the cellplan** 

For each cell, a polygon is drawn with spline line though the center, and the -3dB points at angle -/+ horizontal beamwidth, where the radius of these points are caluculated using the tilt (with a maximum of 10 kilometer).

*Input*: cellplan, second option above

*Output*: polygon per cell

*R-Function*: process_cellplan()

**2. Determine probabilities of presence** 

This algorithm calculates the probability that a device is present in a grid cell $i$ when an event is registered in the signalling data at cell $j$. This probability depends on the distance between $i$ and $j$, and the number of overlapping cells.

Bayes' formula is used to calculate these probabilities:

$$P(i|j) \propto P(i)P(j|i)$$ 

where $i$ represents a grid cell and $j$ the cell polygon.

The prior, $P(i)$, is fixed to 1. The likelihood is $$\frac{1}{(\sum_k \frac{1}{d(k,i)^2})d(j, i)^4}$$ where $d(j,i)$ is the distance between the center of $i$ and the location of cell tower $j$. 
This method is implemented as follows.

1. Per polygon, the bounding box is calculated
2. The grid is recursively divided into quadrants, and per quadrant, the polygons with a intersection bounding box are assigned
3. For each quadrant (on deepest recursive level), the intersection of the grid points (i.e. center of grid cells) and the polygons are determined. For each intersection point, the 1/distance to the mast^2 is calculated. We refer to these values as $p$-values.
4. The combined result is a data.frame with polygon-id, grid-cell-id, and $p$-value
5. Per polygon, the $p$-values are normalized to 1.
6. Since grid cell may have many records, which is the case when polygons are overlapping. To reduce complexity, for each grid cell, the number of overlapping polygons is truncated to 5, i.e. the records with the top-5 $p$ values are selected.
7. The $p$-values are divided by the sum of $p$-values per grid cell. 
8. Per polygon, the $p$-values are again normalized to 1 (due to the previous two steps)

*Input*: cellplan (polygon per cell), grid

*Output*: data.frame with three columns: *cell_name*, *grid-id*, and *p*

*R-Function*: rasterize_cellplan()

**3. Determine administrative region per grid-cell**

Administrative regions can be municipalities, neighbourhoods, postal codes, etc.
The method is straightforward. Per grid cell, the region with the largest overlap is allocated. For computational reasons, an easier method is to find the intersecting admin polygon per grid cell center point.

*Input* grid, admin regions (polygons)

*Output* data.frame with two columns: *grid-id* and *region-id*



**4. Determine probabilities of presence per admin region**

The probabilities that are calculated in step 2 are aggregated to administrative regions.

*Input* data.frame of step 2, data.frame of step 3
*Output* data.frame with three columns: *cell_name*, *region-id*, and *p*


**5. Determine place of residence**

The devices (IMSI numbers) may not be joined with customer data due to privacy. Therefore, it is not possible to know to residential address. However, by following a device over time, the place of residence can be approximated. In literature there two pragmatic methods are proposed (references to be added):

1. During the observed period (say, one month) the place of residence is where the device is most frequently during weekday evenings/nights. A suitable time window can be between 20:00 and 07:00. This is suitable for students and 'nine-to-five' workers, but not for people who work at other times, and people who have a long holiday.
2. During the observed period (say, one month) the place of residence is where the device is most frequently. This method is more robust, since it is better suited for people who work at other times than 'nine-to-five'. However, the place of residense of people with long holidays will still be misclassified.

The procedure to implement this method can be the following. Per device:

1. Collect all events per month. We adopt a calendar month as time unit for residence.  (sliding window is another option, that may give better results.)

~~2. Calculate a weight per event, namely the time in seconds between the previous and the next event. (In essence, the same method is used for the statistics on road statistics to calibrating the road sensor data. The road corresponds to the time scale and the road sensors to the event times.)~~

2. Calculate a weight per event, namely 1 over the number of events per device per hour: simple, scales well (better than taking previous event into account) and no strange edge cases (e.g. previous event was two weeks ago). 

3. Aggregate the events by cell; per cell, sum the weights. This results in a data.frame with two columns: *cell_name*, and total weight *w*.
4. Join the output of algorithm 4 to this result. This results in a data.frame with four columns: *cell_name*, *region-id*, *p* and *w*.
5. Aggregate this data.frame by *region-id*, and sum *p* * *w*. Call the result *pw*.
6. Find the *region-id* with the maximum value of *pw*. This is the (approximation of) the place of residence. Currently this is based on the primary cell (not the combination of primary cells).

(alternative is to distribute place of residence over multiple region-id's)

*Input* signaling data, data.frame of step 4
*Output* data.frame with two columns: *IMSI* (device) and *region-id*.

This place of residence generates a region of residence.

A different approach is a location of residence:

1. collect all events
2. calculate the average location for this imsi during the period (i.e. month)
3. remove the lowest probabilities of the locations (still open how to do this.


