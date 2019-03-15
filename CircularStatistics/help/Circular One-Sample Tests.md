Circular Hypothesis-Testing: One-sample Tests
==========================

Circular one-sample tests allow you to test a hypothesis on a group of circular data. It provides tests for uniformity (i.e. the data is distributed uniformly around the circle) and for a von Mises assumption (i.e. the data in the group comes from a von Mises distribution).

Assumptions
-----------
- Circular variable with specified period
- Rao's spacing test: At least four data points in each sample
- Rayleigh test: The alternativ hypothesis assumes an unimodal distribution with unspecified location and unspecified circular variance.
- V test: The alternativ hypothesis assumes an unimodal distribution with the location as the specified test value, but unspecified circular variance.
- Von Mises assumption check: None

Default Options
-------
### Uniformity Tests:
- Rao's spacing with &#945; = 0.01: Runs a Rao spacing test with a signifance level of 1%

Default Output
-------

### Uniformity Tests:
- Rao's spacing:
  - &#945;: Significance level that was specified in the input
  - Statistic: Rao test statistics
  - Critical: critical value of the test statistic according to the specified significance level &#945;

Additional Options
-------

### Uniformity Tests:
- Rayleigh: Runs a rayleigh test
- V: Runs a V test, i.e. a modified Rayleigh test
  - Test value: The test direction of the V test

### Assumption Checks:
- Von Mises: Runs a Watson's goodness-of-fit test on the data with a von Mises distribution as evaluation and with the specified significance level &#945;


Additional Output
-------

### Uniformity Tests:
- Rayleigh:
  - p: p-value of the test
  - Statistic: Rayleigh test statistics (Rayleigh's R)
- V: 
  - p: p-value of the test
  - Statistic: V test statistics (Rayleigh's R weighted with cos(&#952; - &#956;<sub>0</sub>), where &#952; is the mean direction of the sample and &#956;<sub>0</sub> is the test direction. It therefore can be negative.)

### Von Mises Assumption Check:
- &#945;: Significance level that was specified in the input
- U<sup>2</sup>: The Watson test statistics
- Critical: Critical value of the test statistic according to the specified significance level &#945;
- Est. &#954;: The maximum likelihood estimate of the concentration parameter of the von Mises distribution given by the data points

References
-------
- Rao's spacing test:
  - Rao JS (1976). *Some Tests Based on Arc-Lengths for the Circle.* Sankhyä: The Indian Journal of Statistics, Series B (1960-2002), 38(4), 329–338.
  - Russell GS, Levitin DJ (1995). *An expanded table of probability values for rao's spacing test.* Communications in Statistics - Simulation and Computation, 24(4), 879–888.
- Rayleigh test:
  - Fisher NI (1993). *Statistical Analysis of Circular Data*. Chapter 4. Cambridge University Press.
- V test:
  - Zar JH (1998). *Biostatistical Analysis (4th Edition)*. Chapter 27. Prentice Hall.
- Watson's goodness-of-fit:
  - Lockhart RA, Stephens MA (1985). *Tests of fit for the von Mises distribution.* Biometrika, 72(3), 647–652.
