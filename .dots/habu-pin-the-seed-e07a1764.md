---
title: "Pin the seed's digest without HABU_SEED_SHA256"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:34:19.539348+02:00"
---

Problem: tools/seed.f:94 reads the environment variable HABU_SEED_SHA256 to pin the trusted hb's digest (docs/bootstrap.md:36). An environment census classed it a switch replaceable by a flag or argument. Source, Joel 2026-10-29: remove env vars except where Habu really needs one and cannot do without it. The replacement is a new seed.f argument, a public interface nobody has settled. Acceptance: Joel's choice of how the digest is given to seed.f, recorded here; then HABU_SEED_SHA256 is read nowhere and docs/bootstrap.md says the new form. Files: tools/seed.f, docs/bootstrap.md. Verify: rg -n HABU_SEED_SHA256 prints nothing outside .dots. Depends: none. Ownership: unassigned. Claim: unassigned.
