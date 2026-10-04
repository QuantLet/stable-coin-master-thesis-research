# Statistical reporting audit

- Analysis family: the six non-overlapping events frozen in Section 4.4.
- Independent time-series observations: 31 daily returns per event window, 186 pooled event-window days.
- Portfolio inputs: leakage-free daily returns and weights generated in Section 6.3; no event-specific re-estimation.
- Primary endpoints: pooled annualised volatility and pooled 5% Expected Shortfall.
- Primary comparisons: GMV, LTEC and QTEC versus EW across two endpoints, giving six tests.
- Dependence handling: paired event-stratified moving-block bootstrap, 4,999 replications, seven-day blocks.
- Multiplicity: Holm adjustment across all six primary tests.
- Event-level endpoint: maximum drawdown from an initial wealth of one; descriptive because each event contains only 31 observations.
- Sensitivity checks: three- and fourteen-day block lengths; leave-one-event-out re-estimation with 1,999 replications.
- Main inference: GMV lowers both pooled endpoints; LTEC and QTEC raise both pooled endpoints after Holm correction.
- Boundary: signs are stable under event omission, but GMV precision weakens when the May 2021 or Iran–Hormuz window is removed. The result is not event-invariant.

