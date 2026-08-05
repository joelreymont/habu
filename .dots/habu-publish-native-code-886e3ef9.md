---
title: Publish native code through one bulk window
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.602853+02:00"
---

CG-15 + CG-16 + CG-18, phase 3 of the hard cut. Today publish.f:348-354 pokes per instruction via patch32 (RW mprotect, store, RX mprotect, cache barriers each; a four-instruction routine performs 16 protection syscalls), REC-CELL! tears a 64-bit record cell across two 32-bit pokes, RETARGET writes START before LEN, BL instructions are written without updating CALLMAP (publish.f:346-354,372-386 — a snapshot restore then preserves a writer-run displacement and can misbranch), and LOG+ can throw E-NPUB-CAP after code and record mutation. Fix: the retained final publisher prevalidates the complete publication (every fallible condition before the first byte), performs one RW transition, bulk-copies the emission, one RX transition, one range flush; writes the call and classifies/sets/clears its exact relocation record atomically (relocation metadata belongs to the publisher, not a later scanner); commits code, relocation, and dictionary state atomically. patch32 stays only for isolated debugger edits, stated in the publisher contract. Invariant: no address-keyed native fact may outlive code reclamation — facts live and retire with the authoritative XREF/code record. One relocation authority: the publisher is the only writer of code addresses, the XREF record the only durable store.
