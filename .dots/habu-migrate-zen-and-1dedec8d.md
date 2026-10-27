---
title: "Migrate Zen and Tender onto JR:cursor before the install"
status: open
priority: 1
issue-type: task
created-at: "2026-10-27T21:33:01.048923+02:00"
---

Habu master c71a377a retires JR:STORAGE-BYTES and the caller-sized JR:INIT; Zen and Tender run the installed hb, so they break at the next install unless migrated first. Sites known: Tender src/answers.f:82,:104 (jerry), src/questions/saved.f, src/probing.f, tenderd/{classify,company,runner,drafts}.f; Zen src/sql-check.f (design-jr-cursor.md list; it missed answers.f, so rg each repo for JR:STORAGE-BYTES and E-JR-DEPTH). New form: TYPED-VARIABLE X JR:cursor; X ptr u X JR:INIT; JR:DISPOSE where a record outlives an abandoned reader. Prove each client against the staged tree /tmp/carl-jr-stage/bin/hb (master 62525503 sources plus the qualified engine, gen 1 equals it). Order: client changes green on the staged engine, then carl installs, then the client changes push at once. Owner: phil.
