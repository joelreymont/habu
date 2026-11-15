---
title: Let an MCP app page load its module through a tool
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T08:21:36.123564+02:00"
---

Problem: MCP-APP:PAGE+ (src/lib/browser/app.f) embeds the Wasm module and its scripts in the page, and ChatGPT keeps a plugin's copy of that page until a workspace admin presses Refresh, for which OpenAI has no API. So each Maki release that changes its viewer breaks every Maki view in ChatGPT until someone refreshes: on 2026-11-15 the copied page ran the previous release's viewer, which reads scene version 7, against a service writing version 8, and every view said invalid scene. Acceptance: MCP-APP offers a page whose bytes stay the same when the module or scripts change: at start it fetches them from the server through one named app-only tool and runs them under ChatGPT's view CSP; and a server-side word answers that tool with the module and scripts the server holds. Depends: none. Claim: unassigned.
