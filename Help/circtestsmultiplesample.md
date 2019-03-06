Circular Hypothesis-Testing: Multiple-sample Tests
============

Circular multple-sample tests allow you to perform one-way and two-way ANOVAs on circular data. The null hypothesis is equal mean direction across all groups

Assumptions
-----------
- Dependent circular variable with specified period
- Watson-Williams test: the data in each group comes from a von Mises distribution. All of these have the same concentration parameter which are sufficiently large (>1)
- Watson-Wheeler test: all groups contain at least 10 data points
- Harrison-Kanji test: the data of each cell comes from a von Mises distribution with concentration parameter between 0 and 2

Default Options
-------
### One-way ANOVA:
- Watson-Wheeler: Non parametric one-way ANOVA for circular data

Default Output
-------
### One-way ANOVA:
- Watson-Wheeler:
  - p: p-value of the test
  - W: the Watson-Wheeler test statistic
  - df: the degrees of freedom of the Chi^2 distribution to which W is compared

Additional Options
------------------
### One-way ANOVA:
- Watson-Williams: Parametric one-way ANOVA for circular data

### Two-way ANOVA:
- Harrison-Kanji: two-way ANOVA for circular data

Additional Output
------------------
### One-way ANOVA:
- Watson-Williams: Parametric one-way ANOVA for circular data
  - p: p-value of the test
  - F: the F statistic of the test
  - df1 and df2: the degrees of freedom of the F distribution to which F is compared

### Two-way ANOVA:
  - Harrison-Kanji: It has one of the following outputs, depending on the estimate of the concentration parameter kappa of the whole data set
    
  if kappa < 2
  - p: p-value of the test
  - Chi^2: the associated chi^2 component that is used as a test of difference
  - df: the degrees of freedom of the associated chi^2 distribution

  if kappa > 2
  - p: p-value of the test
  - F: the F ratio of the associated component
  - df: the degrees of freedom of the associated component
  - Sum of Square: the sum of the squares of the associated component
  - Mean of Square: the mean of the squares of the associated component

References
-------
- Watson-Williams: 
  - Zar JH (1998). *Biostatistical Analysis (4th Edition).* Chapter 27. Prentice Hall.
- Watson-Wheeler: 
  - Zar JH (1998). *Biostatistical Analysis (4th Edition).* Chapter 27. Prentice Hall.
- Harrison-Kanji:
  - Harrison D, Kanji GK, Gadsden RJ (1986). *Analysis of variance for circular data.* Journal of Applied Statistics, 13(2), 123–138.
  - Harrison D, Kanji GK (1988). *The development of analysis of variance for circular data.* Journal of Applied Statistics, 15(2), 197–223.