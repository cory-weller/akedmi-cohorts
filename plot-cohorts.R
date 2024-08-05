#!/usr/bin/env Rscript
library(ggplot2)
library(data.table)
library(ggthemes)

dat <- fread('cohort-data.txt')
dat[is.na(N), N := 0]
md1 <- dat[disease == 'AD and Dementia']
md1[, disease := 'AD']
md2 <- dat[disease == 'AD and Dementia']
md2[, disease := 'Dementia']

dat <- rbindlist(list(dat[disease != 'AD and Dementia'], md1, md2))

# Calculate relative proportion per cohort
dat[, cohort_total := sum(N), by=list(disease,cohort)]
dat[, proportion := N/cohort_total]
md1 <- dat[disease == 'AD and Dementia']
md1[, diease := 'AD']
md2 <- dat[disease == 'AD and Dementia']
md2[, diease := 'Dementia']
# Order control as first factor so it's the dark color, and case is semitransparent
dat[, status := factor(status, levels=c('control','case'))]

cohort_levels <- c('ADSP','All of US','AMP PDRD','AMP PD','GP2 WGS','GP2 Genotyping Imputed')
dat[, cohort := factor(cohort, levels=cohort_levels)]

# Plot with unscaled height
g1 <- ggplot(data=dat, aes(x=disease, y=N, alpha=status, fill=ancestry, group=ancestry)) +
    geom_bar(stat='identity') +
    facet_grid(.~cohort, scales='free_x', space='free_x') +
    scale_alpha_discrete(range=c(0.3,1)) +
    theme_few() +
    labs(x='Disease', y='N', fill='Ancestry')

# Plot as proportion
g2 <- ggplot(data=dat, aes(x=disease, y=proportion, alpha=status, fill=ancestry, group=ancestry)) +
    geom_bar(stat='identity') +
    facet_grid(.~cohort, scales='free_x', space='free_x') +
    scale_alpha_discrete(range=c(0.3,1)) +
    theme_few() +
    labs(x='Disease', y='Proportion', fill='Ancestry')


ggsave(g1, file='unscaled.png', width=40, height=20, units='cm', dpi=300)
ggsave(g1, file='unscaled.pdf', width=40, height=20, units='cm', dpi=300)
ggsave(g2, file='scaled.png', width=40, height=20, units='cm', dpi=300)
ggsave(g2, file='scaled.pdf', width=40, height=20, units='cm', dpi=300)

