## Mobility

Mobility provides functions and methods to study location data. 
Please report bugs or suggestions at: https://github.com/nyu-mhealth/Mobility/issues

### Install

To install the latest version from [Github](https://github.com/nyu-mhealth/Mobility/tree/master): 
```
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("nyu-mhealth/Mobility")
```

### Functions

**Mobility** is created to provide easy and fast methods of dealing with spatial location data, especially continuous mobility data. Research methods are based on literature (see the following section). The package currently has the following functions (selected): 

* `radiusofgyration`calculates the radius of gyration of geospatial points during a certain time period. 
* `seqgroup` generates sequential unique identifiers based on changes of other variables. 
* `stayevent` detects stays based on geospatial locations and stay time. 

### Methods

#### Radius of Gyration

In physics, [Radius of gyration](https://en.wikipedia.org/wiki/Radius_of_gyration) measures the distribution of components in an object around an axis. It's been applied in urban science in recent years to study travel behaviors of human and animals. Here, radius of gyration (*rg*) was used to estimate the distance participants traveled in a certain time period[1].  *rg* is defined by the standard deviation between locations and their center of mass. 

#### Stayevents

With continuous mobility or trip data, it is important to identify stays to link to built-in environment and social behaviors. The algorithm for identifying stay events involves 2 steps. First, distance between two sequential points *P<sub>n</sub>* and *P<sub>n+1</sub>* is calculated and compared to a preset threshold. If the distance is smaller than the threshold, both points are put in a stay event candidate set. Then the distance between *P<sub>n+1</sub>* and *P<sub>n+2</sub>* is calculated to identify more points in the candidate set. Second, when the distance becomes larger than the threshold, the time difference between the first point in the candidate set, *P<sub>n</sub>*, and the last point, *P<sub>n+m</sub>*, is calculated. If the time difference is larger than a time threhold, the centroid of all the points in candidate set is taken as the stay event's location. During the process, if the distance and time exceeds thresholds, points in candidate set are reset. 


*More functions will be added as we expand our methods.*

======
**Reference:**

[1] González MC, Hidalgo CA, Barabási AL. 2008. Understanding individual human mobility patterns. *Nature* 453:779-782.
