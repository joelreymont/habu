---
title: Hide sets and properties in an OData model
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T17:23:21.693504+01:00"
---

Problem: sapd must serve SAP services with some entity sets and fields hidden (mark's request /tmp/mark-odata-hide.md; design /tmp/carl-odhide-design.md): nothing ODATA lists, finds or copies may reveal them, and a query option naming one must be caught before SAP. Acceptance: ODATA:HIDE-SET ( ^metadata str -- bool ), ODATA:HIDE-PROPERTY ( ^metadata str -- bool ) and ODATA:HIDDEN-NAME$ ( ^metadata str -- str bool ) (Joel's names, 2026-11-19); after a hide, ENTITY-SET, EACH-ENTITY-SET, EACH-PROPERTY, EACH-NAVIGATION and COPY-ENTITY behave as if the hidden parts were absent at every depth; a model nothing hid behaves as today; the odata suite shows each case. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-odhide.
