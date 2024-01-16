# Phylogenetic Comparative Methods Workshop.
Liam Revell and Luke Harmon.

# Lecture 1.
Organism: Tailed frog

What is the message of the tree of life?
In some times and places species form at different rates, studying how speciation and extinction rate change across the tree of life is a phylogenetic comparative methods goal. This rate varies dramatically depending on time and clade.

The tree of life is a tool to look into the past. Macroevolution used to focused on things with a good fossil record, but comparative methods expands this.

How are there so many species? And why they are so different from one another?

CM theoretical basis and algorithms came from:
* population genetics (usually few loci)
* quantitative genetics (infinite loci of small effect)
* paleobiology (paleontology with math)
* phylogenetics

Phylogenies and the comparative method by Joe Felsenstein introduced the method of independent contrasts.

Phylogenetic trees are difficult to reconstruct because there are so many of them. But also because they are full of information. Building the tree is just the start for phylogenetic comparative methods.

Comparative methods work for trees that are ultrametric and non-ultrametric. To keep this in mind, non-ultrametric trees would have different assumptions.

Integration and modularity, and their relation to evolutionary innovation. Caetano section feeding fish.

## R Introduction

Common concepts of programming language, console, RStudio. 
See also Lecture1.R

Installation of required packages: 
```{r}
install.packages('ape')
install.packages('phytools')
install.packages('geiger')
```

Ape is a core phylogenetic R package, many packages depend on this package.

### Continue instructions on Lecture1.R

# Phylogenetic Independent Contrasts

Brownian Motion is a model for the evolution of continous characters. In brownian motion we can model how traits evolve over time.

Describes a random walk of evolution of continously variable characters.

Three facts describe BM.
1. There is no trend to get bigger or smaller.
2. Successive steps are independent.
3. Expected values follow normal distribution where variance depends on rate and time.

PGLS (phylogenetic generalized least squares) is related to this.

Parameters of BM.
* $X_0$, the starting value
* $\sigma^2$, the variance.

Multiple BM simulations. Some will go up, others down, on average they don't go out from zero. The variance is linearly related to time.

Why use BM for trait evolution?
BM can describe processes such as a the movement of a beach ball on a stadium. Any time that trait evolution can be described as a sum of independent actions over a unit of time.

Central Limit Theory: indenpendent movements with greater sample size results in normal distributions.

In the present of selection that changes randomly, genetic drift, random punctuated changed, or weak selection, they can also result in BM.

BM is a null hypothesis, to compare with other models such as OU.

BM on a phylogeny. Close relatives are similar to one another. And they inherit the trait value of the ancestor after speciation.

If we do this a bunch of times we end up with a cloud of points that seem to be correlated, they covary due to common ancestry.

What about phylogenetic signal?
A pattern where closely related species on a phylogenetics trat have trait values that are more similar than expected by chance.

1. We expect phylogenetic signal under a wide range of evolutionary models. BM, OU, early burst
2. It is a pattern, not a process.
3. Phylogenetic signal is NOT a constraint. Unconstrained models create lots of phylogenetic signal (BM), while constrained models (OU) have low phylogenetic singal.

Measuring phylogenetic signal. Pagel's $\lambda$ and Blomberg's $\kappa$. Phylogenetic signal is unitless.

Phylogenetic Independent Contrasts.
A simple way to understand the evolution of traits evolving under BM.

### Felsensteins' Worst Case Scenario.
Imagine there is a correlation between two traits. Two clades with two traits that are not correlated between x and y. The relationship is cause simply by the differences between two clades.

PICs are a way to analyze the data that comes from phylogenetic trees.

Test for evolutionary correlations between characters.

We can think of PICs as a statistical transformation that creates independent data points.

In a standard correlation. Can we predict Y from X? We might be able to do this for two reasons: species are related, or X and Y tend to evolve together.

Evolutionary correlation
X and Y evolve in a correlated fashion.
When X changes, Y tends to change in a predictable way.

Contrasts: basic idea.
Instead of looking at the value at the tips, we look at their difference between them, divided over the sum of the branch lengths that separate the species from their common ancestor. We compare this for every pair of species. There will be n-1 contrasts, than total species.
Each standardized contrasts will tell us something about the rate of evolution, the amount of change over time. If we square the contrasts and sum them, we get an estimate of $\sigma^2$.

Character correlations:
Calculate independent contrast, and carry out a regression analysis with no intercept (force regression through the origin). If p<0.05, then we reject the null hypothesis of evolutionary correlation.

Each contrast, the direction of substraction is arbitraty, so each contrast is a vector, that's why we forced it through the origin.

Alternative to PIC is PGLS. A framework for testing all sorts of hypotheses about correlated evolution. Flexible, allows for different types of data, not just continuous.

PGLS assumes that the residuals from a linear model show phylogenetic covariance.

When you do PGLS in R, you have to supply a covariance matrix. This matrix has two components rate and shared branch length. Each species also has a variance which the rate*total branch length. Variance-Covariance matrix

With PGLS we can multiple predictor variables, and predictors that are not continuous, for example including interactions (pANOVA, pANCOVA).
We are not limited to BM, we can used OU, EB, etc, looking at residuals, combine discrete and continuous models, and can obtain the intercept value.

## Continue Lecture2.R