---
title: "Declare an attribute key's value domain in the schema"
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T18:10:48.351895+02:00"
---

src/compiler/ir/schema.f records which attribute KEYS an opcode requires, and src/compiler/ir/verify.f checks that an operation carries exactly one attribute per declared key and no undeclared ones. Neither records what values that attribute may hold. A64IR bounds its two move-wide operands in A64IR:IMM-ATTR and A64IR:SHIFT-ATTR, but those are only the polite door: a caller can intern a raw int attribute with IR-BUILD:INTERN-INT-ATTR under the key A64IR:KEY-IMM, and the module freezes and verifies with a 17-bit immediate in a 16-bit field. The only thing left to catch it is src/arch/arm64/asm.f's ?IMM16, which ends the process (see the sibling dot on catchable assembler refusals). Right fix: let a schema declare an attribute's value domain (at least an integer range) beside its key, and have the freeze verifier decide the attribute's VALUE against it the way it already decides its key. Then A64IR states its two bounds once, in the schema, and a hand-built module carrying an out-of-field move-wide operand is refused at freeze with E-IR-VERIFY-ATTR rather than at encode with a process exit. Found while building src/compiler/native/emit.f, which deliberately does not duplicate the assembler's bounds.
