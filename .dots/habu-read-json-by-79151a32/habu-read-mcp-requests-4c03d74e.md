---
title: Read MCP requests by value
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T17:22:27.440196+01:00"
blocks:
  - habu-validate-a-json-9ec8e7c9
---

Problem: the MCP server reads tool arguments with a cursor and READ-REPEAT, and ADD-TOOL's body takes ^u8 n. Acceptance: ADD-TOOL ( str [ JSON:value -- ] -- ), the body gets the arguments object ({} when absent); UNREPEATED? and its READ-REPEAT use deleted. mcp.f's GIVEN$ (params as text) goes. Depends: JSON-RPC, compile and validate lanes. Claim: unassigned.
