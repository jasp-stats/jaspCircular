Circular Hypothesis-Testing: Multiple-sample Tests
============

Circular multple-sample tests allow you to perform one-way and two-way ANOVAs on circular data. The null hypothesis is equal mean direction across all groups.

Assumptions
-----------
- Dependent circular variable with specified period
- Watson-Williams test: The data in each group comes from a von Mises distribution. All of these have the same concentration parameter which are sufficiently large (>1).
- Watson-Wheeler test: All groups contain at least 10 data points
- Harrison-Kanji test: The data in each group comes from a von Mises distribution. All of these have the same concentration parameter between 0 and 2.

Default Options
-------
### One-way ANOVA:
- Watson-Wheeler: Non parametric one-way ANOVA for circular data

Default Output
-------
### One-way ANOVA:
- Watson-Wheeler:
  - p: p-value of the test
  - W: The Watson-Wheeler test statistics
  - df: The degrees of freedom of the &#967;<sup>2</sup>-distribution to which W is compared

Additional Options
------------------
### One-way ANOVA:
- Watson-Williams: Parametric one-way ANOVA for circular data

### Two-way ANOVA:
- Harrison-Kanji: Two-way ANOVA for circular data (it always runs on the first two factors in the `Fixed Factors` list)

Additional Output
------------------
### One-way ANOVA:
- Watson-Williams: Parametric one-way ANOVA for circular data
  - p: p-value of the test
  - F: The F statistics of the test
  - df1 and df2: The degrees of freedom of the F distribution to which F is compared

### Two-way ANOVA:
  - Harrison-Kanji: It has one of the following outputs, depending on the estimate of the concentration parameter &#954; of the pooled data:
    
    - if &#954; < 2
      - p: p-value of the test
      - &#967;<sup>2</sup>: The associated &#967;<sup>2</sup> component that is used as a test of difference
      - df: The degrees of freedom of the associated &#967;<sup>2</sup> distribution

    - if &#954; > 2
      - p: p-value of the test
      - F: The F ratio of the associated component
      - df: The degrees of freedom of the associated component
      - Sum of Square: The sum of the squares of the associated component
      - Mean of Square: The mean of the squares of the associated component

References
-------
- Watson-Williams: 
  - Zar JH (1998). *Biostatistical Analysis (4th Edition).* Chapter 27. Prentice Hall.
- Watson-Wheeler: 
  - Zar JH (1998). *Biostatistical Analysis (4th Edition).* Chapter 27. Prentice Hall.
- Harrison-Kanji:
  - Harrison D, Kanji GK, Gadsden RJ (1986). *Analysis of variance for circular data.* Journal of Applied Statistics, 13(2), 123–138.
  - Harrison D, Kanji GK (1988). *The development of analysis of variance for circular data.* Journal of Applied Statistics, 15(2), 197–223.