Circular Descriptives
============

Circular descriptives allows you to obtian basic descriptives statistics for circular data like the mean direction or the circular variance. It further gives you several plotting options for circular data.

Default Options
-------
### Statistics:
- Central Tendency:
  - Mean direction: The mean direction of the data points on the defined period.
- Dispersion:
  - Std. deviation: Circular standard deviation of the data points obtained from the mean resultant length.

Default Output
-------
### Descriptive Statistics:
- Valid: Number of valid cases.
- Missing: Number of missing values.
- Mean direction: The mean direction of the data points on the defined period.
- Std. deviation: Circular standard deviation of the data points obtained from the mean resultant length.

Additional Options
------------------
### Plots:
- Distribution Plots: Plots the data points on a circle with the defined period.
  - Stack points: Points that highly overlap are stacked on top of each other to avoid loss of visual information.
  - Display mean vector: Displays the mean direction as a pointer in the plot. Its length corresponds to the mean resultant length.
  - Display histogram: Displays a histogram density estimate in the plot.

### Statistics:
- Central Tendency:
  - Mean resultant length: The length of the mean vector of the data points, normalized to the unit circle.
  - Median: Smallest median angle of the data points.

- Dispersion:
  - Variance: Circular variance of the data points obtained from the mean resultant length.
  - Range: Smallest angle that comprises all data points.

Additional Output
------------------
### Display Distribution Plots:
  - Plots of the data points on a circle with the defined period.

### Descriptive Statistics:
  - Mean resultant length: The length of the mean vector of the data points, normalized to the unit circle.
  - Median: Smallest median angle of the data points.
  - Variance: Circular variance of the data points obtained from the mean resultant length.
  - Range: Smallest angle that comprises all data points.

References
-------
- Fisher, N.I. (1993). *Statistical Analysis of Circular Data*. Cambridge University Press. (include chapter???)
