---
title: "Maki: Gaussian NLL + covariance loss family"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:40:52.792083+02:00"
---

Driving workload: from-scratch training of an uncertainty-predicting model (docs/model-cad.md Driving workload). Add checked loss words to maki/loss.f + maki/loss-tensor.f: Gaussian negative log likelihood with predicted variance (diagonal covariance first, log-variance parameterization for stability), Mahalanobis distance, Huber. Each loss ships with its analytic gradient AND a numeric gradcheck exactly like existing MSE/L1 entries; tensor-scale apply follows the loss-tensor.f pattern; VJP entries registered for autograd orchestration. Tests: gradcheck per loss, tensor-scale reduction, degenerate variance rejection (fail closed on non-positive variance). Files: maki/loss.f, maki/loss-tensor.f, maki/loss-test.f, maki/loss-tensor-test.f.
