---
title: Bind owned source in loader acceptance
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-30T11:38:02.101132+02:00\\\"\""
closed-at: "2026-09-30T12:09:04.591482+02:00"
close-reason: Positive loader fixture now installs owned callbacks through explicit SOURCE-POLICY before private compiler entry. Red child134/PC0 becomes real-load E2Erc0 with retained-byte equality, unknown input refusal and callback retirement. Bootstrap negative unchanged; Astra follow-up PASS.
---

After the ordinary/unit driver split, native-source-view-child calls private OPEN-AND-COMPILE without installing SOURCE-POLICY!, so SOURCE-BIND is null and the fresh child crashes134 atPC0. Fix that positive fixture's invocation by explicitly binding SOURCE-VIEW callbacks to the fresh target loader and installing the policy before logical reset. Do not add a production fallback or alter the independently passing bootstrap-negative fixture. The existing real-load E2E must prove retained-byte fresh-loader equality, unknown-source refusal and provider retirement; require red-before/green-after and independent Astra follow-up.
