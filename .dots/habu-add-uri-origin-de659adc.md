---
title: "Add URI:ORIGIN?"
status: active
priority: 2
issue-type: task
created-at: "2026-11-23T19:15:28.978839+01:00"
---

Problem: sapd (mark) needs whether a text is an http(s) origin and nothing more; without a Habu word it holds two copies (SAP src/config.f:563-587 and its SAP client), each catching E-URI-HTTP and E-URI-HOST around URI:REQUEST-TARGET$. Acceptance: URI:ORIGIN? ( str -- bool ) in src/lib/uri.f answers true for http(s)://host[:port] alone and false, with no throw, for anything else; the reader answers a code instead of throwing. Depends: none. Claim: agent=carl-origin-worker workspace=.jj-ws.noindex/carl-origin.
