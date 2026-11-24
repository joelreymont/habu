---
title: Get an Azure managed-identity token in BEARER
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T20:40:44.361459+01:00"
---

Goal: design /tmp/jerry-p1/design-azure.md "IDENTITY-TOKEN": BEARER:IDENTITY-TOKEN ( str str ^VEC:vector<u8> -- ) appends a token for a resource from the IMDS origin given, kept per resource on BEARER:CURRENT's rows until expires_in minus LEEWAY-S; BEARER:IMDS$ answers http://169.254.169.254. Zen (server.f:219, worker.f:2154) and Maki (db.f:71) call it to sign in to Azure PostgreSQL. Acceptance: a hermetic test against a loopback IMDS gets a token appended after existing bytes, maps 404/410 to E-UNAVAILABLE and 400 to E-REFUSED, makes one request for tasks asking at once, fetches again after a short expires_in, and keeps its tokens across BEARER:CLIENT!. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-imds.
