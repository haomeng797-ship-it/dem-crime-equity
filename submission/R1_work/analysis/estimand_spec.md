# R1 estimand specification

Status: provisional analysis specification for the local revision workspace.

## Conceptual distinction

- Relative disparity: Black/White imprisonment-rate ratio.
- Absolute disparity: Black minus White imprisonment-rate gap per 100,000.
- Group-specific burden: Black and White imprisonment rates per 100,000.

The joint model does not combine these quantities into a composite score. It estimates the two
race-specific rates and derives each estimand from the same posterior draws.

## Primary comparison

The primary application remains descriptive and between states. It compares otherwise typical
states one standard deviation apart in their long-run State Democracy Index score. Predictions are
standardized over 2016-2020, the same window used in the descriptive figure, with the within-state
democracy deviation set to zero. State random-intercept variation is integrated out.

For every posterior draw, calculate:

1. Percent change in the Black/White ratio for a one-SD increase in democracy.
2. Change in the Black-White rate gap per 100,000.
3. Percent change in the Black imprisonment rate.
4. Percent change in the White imprisonment rate.
5. The joint probability that relative disparity increases while absolute disparity does not.

The first two quantities are disparity estimands. The last two are components that explain their
movement and describe group-specific burden.

## Secondary comparison

Repeat the calculations for a one-SD within-state democracy change. This answers a different
descriptive question and is not treated as a causal estimate or as a superior version of the
between-state comparison.

## Model sequence

- Primary: joint Gaussian model of log rates with state intercepts, correlated outcomes, and AR(1)
  residuals.
- Sensitivity 1: joint Gaussian model without AR(1).
- Sensitivity 2: region-adjusted joint Gaussian model.
- Sensitivity 3: joint negative-binomial count model with population offsets.
- Sensitivity 4: AR(1) model after the Hispanic-recording screen.

## Reporting rule

Report posterior medians, 95% credible intervals, and directional probabilities. Do not describe an
interval crossing zero as evidence of no effect. Do not call the Black rate or White rate a measure
of racial disparity.

