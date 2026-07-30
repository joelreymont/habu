---
title: Own completion server limit roles
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T08:22:26.391803+02:00"
---

Why: connection, request-body, prompt, and token-output capacities all use one cell, so positional raw values can be swapped while typechecking. Result: package SERVE defines four distinct immutable nominal values, conn-cap, body-cap, prompt-cap, and output-cap, with checked CONN-CAP, BODY-CAP, PROMPT-CAP, and OUTPUT-CAP constructors from positive raw arguments. The serve argument parser calls the matching constructor while handling each named flag and stores only these typed values; SERVE:PLAN consumes them directly. Owner: completion server requested-limit roles and constructors only. Production red: swapping any two raw limits at PLAN still typechecks. Acceptance: exact positive values round-trip through their named projection; zero and overflow refuse; every cross-role PLAN argument rejects statically; mutation swapping parser constructors fails the real serve-argument and plan path. Forbidden: separate limit namespace, generic limit type, storage formula, derived model limit, default, parser grammar, allocation, adapter, version, compatibility, metric, or lint. Smallest owning check: the real tools/serve-args-test.f values passed into maki/serve/server-plan-test.f. Claim: unassigned.
