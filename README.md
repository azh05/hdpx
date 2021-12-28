# hdpx
R pkg for Hierarchical Dirichlet Process

[![Build Status](https://travis-ci.com/steverozen/hdpx.svg?branch=master)](https://travis-ci.com/steverozen/hdpx)

To install, first ensure `remotes` package is installed and the BioConductor repositories are available (run `setRepositories()`). 
It might take a few minutes to download any missing dependencies and build the vignettes. 
```R
remotes::install_github(repo = "steverozen/hdpx", ref = "master", build_vignettes = FALSE)
```

Model categorical count data with a hierarchical Dirichlet
    Process. Includes functions to initialize a HDP with a custom tree
    structure, perform Gibbs sampling of the posterior distribution,
    and analyse the output. The underlying mathematical theory is
    described by Teh et al. 
    "Hierarchical Dirichlet Processes", Journal of the American Statistical
    Association 2006;101(476):1566-1581
    (https://doi.org/10.1198/016214506000000302).

This R package is based on code forked from Nicola Roberts, 
    https://github.com/nicolaroberts/hdp. Roberts adapted the R code
    from Teh and colleagues' open source MATLAB code
    and incorporated Teh's C code, as does this package.
    THe MATLAB and C code are available at
    http://www.stats.ox.ac.uk/~teh/research/npbayes/npbayes-r21.tgz.
    Robert's thesis is at
    https://www.repository.cam.ac.uk/bitstream/handle/1810/275454/Roberts-2018-PhD.pdf
    
Subsequent changes by Rozen and Liu are confined to the R code.
    These include
    
1. Corrections to garbage collection in the interface
    to the C code
    
2. A new function for computing unsigned 
    Stirling numbers of the first kind. See functions xmake.s in xmake.s.R
    and function .onLoad in zzz.R.
    
3. A complete re-working of
    the process by which "raw clusters" sampled in posterior chains are 
    combined into "components" (sets of mutations generated by one 
    mutational process)
    
4. New functions for plotting to visualize 
    and evaluate components extracted by the new procedures
    
There are also
    revised suggestions for burnin procedures and for setting hyperparameters 
    for the concentration parameters; see https://github.com/steverozen/mSigHdp.

```

This program is free software: you can redistribute it and/or 
modify it under the terms of the GNU General Public License version 3 
as published by the Free Software Foundation. 

This program is distributed in the hope that it will be useful, 
but WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
General Public License for more details <http://www.gnu.org/licenses/>. 
```

Copyright statement on original MATLAB and C code written by Yee Whye Teh, downloaded from 
http://www.stats.ox.ac.uk/~teh/research/npbayes/npbayes-r21.tgz

```
(C) Copyright 2004, Yee Whye Teh (ywteh -at- eecs -dot- berkeley -dot- edu)
http://www.cs.berkeley.edu/~ywteh

Permission is granted for anyone to copy, use, or modify these
programs and accompanying documents for purposes of research or
education, provided this copyright notice is retained, and note is
made of any changes that have been made.
 
These programs and documents are distributed without any warranty,
express or implied.  As the programs were written for research
purposes only, they have not been tested to the degree that would be
advisable in any important application.  All use of these programs is
```
