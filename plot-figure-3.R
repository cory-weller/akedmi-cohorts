#!/usr/bin/env Rscript
library(ggplot2)
library(data.table)
library(ggthemes)
library(xlsx)

dat <- xlsx::read.xlsx('data-for-figure-3.xlsx', sheetIndex=1, header=TRUE)
setDT(dat)
setnames(dat, c('Cohort', 'Ancestry', 'Disease','Status', 'N'))
dat[Disease=='dementia ', Disease := 'Dementia']
dat[Disease=='demenia and PD', Disease := 'Dementia and PD']

# Remove trailing spaces
dat[, Disease := gsub(' $', '', Disease)]
dat[, Status := gsub(' $', '', Status)]

# Replace 'NA' ancestry with 'None Reported'
dat[is.na(Ancestry) | Ancestry == 'NA', Ancestry := 'None Reported']

#dat[is.na(N), N := 0]
# Add AD and Dementia individuals as AD as well
md1 <- dat[Disease == 'AD and Dementia']
md1[, Disease := 'AD']

# Add Dementia and PD individuals as PD as well
md2 <- dat[Disease == 'Dementia and PD']
md2[, Disease := 'PD']

dat <- rbindlist(list(dat[Disease != 'AD and Dementia'][Disease != 'Dementia and PD'], md1, md2))

# Calculate relative proportion per Cohort
dat[, Cohort_total := sum(N), by=list(Disease,Cohort)]
dat[, proportion := N/Cohort_total]
md1 <- dat[Disease == 'AD and Dementia']
md1[, Disease := 'AD']
md2 <- dat[Disease == 'AD and Dementia']
md2[, Disease := 'Dementia']
# Order control as first factor so it's the dark color, and case is semitransparent
dat[, Status := factor(Status, levels=c('control','case'))]

Cohort_levels <- c('Genomics England','UK Biobank')
dat[, Cohort := factor(Cohort, levels=Cohort_levels)]

# Plot with unscaled height
g1 <- ggplot(data=dat, aes(x=Disease, y=N, alpha=Status, fill=Ancestry, group=Ancestry)) +
    geom_bar(stat='identity') +
    facet_grid(.~Cohort, scales='free_x', space='free_x') +
    scale_alpha_discrete(range=c(0.3,1)) +
    theme_few() +
    labs(x='Disease', y='N', fill='Ancestry')

# Plot as proportion
g2 <- ggplot(data=dat, aes(x=Disease, y=proportion, alpha=Status, fill=Ancestry, group=Ancestry)) +
    geom_bar(stat='identity') +
    facet_grid(.~Cohort, scales='free_x', space='free_x') +
    scale_alpha_discrete(range=c(0.3,1)) +
    theme_few() +
    labs(x='Disease', y='Proportion', fill='Ancestry')


ggsave(g1, file='fig3-unscaled.png', width=20, height=20, units='cm', dpi=300)
ggsave(g1, file='fig3-unscaled.pdf', width=20, height=20, units='cm', dpi=300)
ggsave(g2, file='fig3-scaled.png', width=20, height=20, units='cm', dpi=300)
ggsave(g2, file='fig3-scaled.pdf', width=20, height=20, units='cm', dpi=300)

