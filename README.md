# Phylogenetic Comparative Methods Workshop.
Liam Revell and Luke Harmon.

## Lecture 1.
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