---
title: "Design Zen's server-turn needs from HTTP, MCP and CURL"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T22:12:49.657002+02:00"
---

Problem: Zen's Z2 v3 turn design (zen phil-dots .out/z2-design-v3.md 7) runs a turn on a server task, not a request worker. phil's needs: (1) answer a TAKE-OVER connection from a non-worker task (http-response.f renders only into worker rows, :20-24, :125-131); (2) cancel an in-flight MCP:TOOLS/CALL from another task so CALL returns E-TRANSFER (curl.f:1697-1699 forbids cross-task handle use); (3) OAuth token acquisition (Entra OBO, client credentials) as a Habu library or not; (4) whether TASK:NEW tasks may use PG:CONNECT/MODEL:OPEN/CURL while the server's CURL loop runs. Fable Plan design running (read-only). Acceptance: design, complexity pass, public words to Joel in one interview question, lanes dotted; tell phil. Depends: none. Claim: agent=carl (design agent, read-only).
