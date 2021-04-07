# swgoh-analysis

Analysis of SWGOH data primarily for an ISU Undergraduate Research Symposium.

The analysis depends on data from the 
[swgoh R package](https://github.com/jarad/swgoh) which depends on the 
data in the 
[swgoh-data repository](https://github.com/jarad/swgoh-data).
We are focusing on data from 2019-12-03 until 2020-09-15 partly to avoid 
[double drops that started on 2020-11-25](https://github.com/jarad/swgoh-data/issues/4).

## Hierarchical Bayesian logistic regression

The primarily analysis is a hierarchical Bayesian logistic regression fit for 
characters and equipment (or components) separately. This is performed in the 
[swgoh_models.R](swgoh_analysis.R) script and produces the 
swgoh_models.RData file that is not included in the repository since it is 
a large file. 

Then these results are organized into useful tables/figures using the 
[swgoh_analysis.R](swgoh_analysis.R) script. 
