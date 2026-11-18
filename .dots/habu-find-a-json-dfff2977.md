---
title: Find a JSON Schema name in constant time
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T10:19:40.334726+01:00"
---

Problem: src/lib/json-schema.f NAMED?/PROPERTY-NAMED and LISTED? compare each new property or required name with every earlier one, so a schema with n properties or required names costs n²/2 compares at COMPILE; VALID?'s KEY-STEP (PROPERTY-NAMED, MARK-REQUIRED) pays the same per member key. Acceptance: a schema with 40000 properties and 40000 required names compiles, and checks an object with those 40000 members, in time linear in n (measured at 20000 and 40000); json-schema and mcp tests unchanged. Depends: none. Claim: unassigned.
