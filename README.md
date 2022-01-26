# Asian-Discrim-EuroEconRev
Publicly available data and code to replicate results in "Asian American Discrimination in Harvard Admissions" published in European Economic Review

## Software requirements

- Matlab R2019a or later
- R 4.0 or later

## Steps to replicate our analysis:
All figures will be saved in the `Figures/` folder.

1. To reproduce Figure 1 and Online Appendix Figure F3, execute `Analysis/R/graphCoefs.R`
2. To reproduce Online Appendix Figure F1, execute `Analysis/Matlab/graphOAFigF1.m`
3. To reproduce the simulations discussed in Online Appendix D and reported in Online Appendix Table F9, execute
    - `Analysis/Matlab/modelpredict2.m` (for normal distribution)
    - `Analysis/Matlab/modelpredict2v2.m` (for flexible distribution)
    - `Analysis/Matlab/modelpredictoptweight.m` (for flexible distribution)
4. All other results are taken directly from the trial documents
