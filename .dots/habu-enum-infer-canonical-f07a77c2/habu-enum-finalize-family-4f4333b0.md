---
title: "ENUM: finalize family kind"
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-23T14:53:49.128448+02:00\""
---

Why: full ENUM chooses TK-SUM before variants are known. Publication must
derive the final kind exactly once from the authoritative declaration event
stream, without a second persisted registry.

Outcome: this is an aggregate only. Its children guard declaration-event
publication and reset, add the core finalizer, bind the production ENUM
frontend, make finalization mandatory before publication, and prove the chosen
kind survives a real warm snapshot. The children own all implementation; this
parent owns no code.

Forbidden: a second family list or count, persisted finalization state, restore
hook, reader migration, public raw capability, forwarding alias, post-publish
mutation, constructor work, legacy definer edit, or changed publication
lifecycle.

Acceptance: every child is reviewed, landed, and closed in dependency order;
then payloadless ENUM publishes as TK-ENUM, any payload publishes as TK-SUM,
publication cannot bypass finalization, no same-savepoint path can finalize
twice or erase its evidence, and a warm image preserves the chosen kind without
new persisted authority.

Claim: unassigned.
