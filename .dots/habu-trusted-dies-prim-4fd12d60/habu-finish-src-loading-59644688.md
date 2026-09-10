---
title: Finish source loading and public Habu documentation
status: open
priority: 2
issue-type: task
created-at: "2026-09-10T18:03:13.389278+03:00"
---

Owner: Cedar; source loader, public package visibility, and docs. Explicit user asked excellent documentation for other agents and to fix observed limitations. Finish consistent source-root lookup for standalone tools/application callers; ordinary use must not depend on an accidental repository cwd or per-app loader shims. Stop pre-checker implementation globals leaking into consumer lookup (Maki reported compiler MM colliding with QTY:MM); preserve public names without app renames or undefining compiler cells. Validate examples through the real load path. Previously published documentation/style change11d4342e remains completed; retain the user rule that multiline definitions have two blank lines between them. Update only behavior that is implemented and tested; do not turn unfixed limitations into a declared product contract. Acceptance: source-root and namespace collision reducers pass, concise runnable onboarding/build/debug guidance, correct compiler/PRIM boundary description.
