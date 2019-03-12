Circular Hypothesis-Testing: One-sample Tests
==========================

Circular one-sample tests allow you to test the null hypothesis on a group of circular data. It provides tests for uniformity (i.e. the data is distributed uniformly around the circle) and for a von Mises assumption (i.e. the data in the group comes from a von Mises distribution)

Assumptions
-----------
- Circular variable with specified period
- Rao's spacing test: At least four data points in each sample
- Rayleigh test: The alternativ hypothesis assumes an unimodal distribution with unspecified location and unspecified circular variance 
- V test: The alternativ hypothesis assumes an unimodal distribution with the location as the specified test value, but unspecified circular variance

- Von Mises assumption check: None

Default Options
-------
### Uniformity Tests:
- Rao's spacing with p = 0.01: Runs a Rao spacing test with a signifance level of 1%.

Default Output
-------

### Uniformity Tests:
- Rao's spacing:
  - p: significance level that was specified in the input
  - Statistic: Rao test statistics
  - Critical: critical value of the test statistic according to the specified significance p

Additional Options
-------

### Uniformity Tests:
- Rayleigh: Runs a rayleigh test.
- V: Runs a V test, i.e. a modified Rayleigh test.
  - Test value: The test direction of the V test.

### Assumption Checks:
- Von Mises: Runs a Watson's goodness-of-fit test on the data with a von Mises distribution as evaluation


Additional Output
-------

### Uniformity Tests:
- Rayleigh:
  - p: p-value of the test
  - Statistic: Rayleigh test statistics
- V: 
  - p: p-value of the test
  - Statistic: V test statistics

### Von Mises Assumption Check:
- p: significance level that was specified in the input
- U<sup>2</sup>: the Watson test statistic
- Critical: critical value of the test statistic according to the specified significance p
- Est. Kappa: the maximum likelihood estimate of the concentration parameter of the von Mises distribution given by the data points

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
