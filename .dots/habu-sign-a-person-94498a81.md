---
title: "Sign a person in with a browser on Habu's HTTP server"
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T20:03:30.551906+01:00"
---

Problem: a Habu server can admit only a bearer token a client already holds (src/lib/net/bearer.f BEARER:ADMIT?) and its HTTP server reads and sets no cookies, so a browser user cannot sign in; Niwa needs the person's token to make On-Behalf-Of calls (request: otto, /tmp/otto-habu-signin.md). Acceptance: the OAuth 2.0 authorization code grant with PKCE as a confidential client, a verified ID token, a server-side session behind an HTTP-only Secure SameSite cookie, a guard that hands a route the person's current access token renewed when it expires, sign-out, and cookies on the HTTP server; a person opening a Niwa site is sent to Entra and comes back signed in. The package is WEB-SESSION (Joel 2026-11-26). Depends: a design Joel has seen. Claim: unassigned.
