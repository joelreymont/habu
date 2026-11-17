---
title: "Give the core's thrown codes their texts"
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T15:39:22.288442+01:00"
---

Problem: 76 thrown codes sit in files that cannot reach ERROR:CODE (src/core, src/habu, src/os/script-argv.f, lib files that never require src/lib/error.f, tools/unicode/class-parse.f; 11 of them in files src/lib/error.f itself loads; 5 compiled into machine code), so they have no text; src/core/include.f INCLUDE-RANGE-RC is -2110, the value of error.f E-FS-TIMEOUT, so its throw reads 'a descriptor transfer reached its deadline'; exit statuses 70, 76 and 77 are thrown under several names, and src/habu/debug.f:115 throws a bare 76; and ERROR:CODE takes one name per value. /tmp/carl-ec/census.md lists them. error.f keeps by-value constants with no text for core codes: E-COMP-BINDING (src/core/checker-owner-abi.f:60 BINDING-RC), E-PROF-RATE, E-EVAL-RESIDUE and E-INTERN-REWIND (src/habu prof-abi.f, stack-abi.f, intern-table.f). Acceptance: each of them has its text, declared by value at src/lib/error.f's end as the engine faults are (-9850), INCLUDE-RANGE-RC has a value of its own, and each exit status one text. A code a file below error.f mints keeps its constant there; error.f's last package section records its text under that name and defines no word, since a second global of the same name is E-USING-SHADOW-GLOBAL wherever a program uses the minting package; a library file loaded after error.f declares its codes with ERROR:CODE where it mints them, as src/lib/base85.f does. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ec1.
