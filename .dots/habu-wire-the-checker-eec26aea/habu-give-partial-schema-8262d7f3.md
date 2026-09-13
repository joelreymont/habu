---
title: Give partial schema constructors canonical persistent identity
status: open
priority: 1
issue-type: task
created-at: "2026-09-13T21:36:14.479617+03:00"
---

The eight-store registry serializes SCH-CON.A as a numeric checker constructor code. Codes at or above CC-MAX are allocated dynamically (for example DEFLINEAR pcs-linear), so an equal destination constructor count cannot prove that such a code names the same constructor.

Concrete current-source negative fixture: test/aot-payload-constructor-child.f declares a real DEFLINEAR pcs-linear and replays STRUCTURE pcs-holder 0 FIELD value pcs-linear ;STRUCTURE through the normal declaration owner. It asserts a real SCH-CON with code >= CC-MAX occurs in the delta, then CHECKER-REG-AOT-SAVE refuses 76 with tfam: captured schema constructor has process-local identity. The corresponding incoming schema-code check rejects before registry publication. Both export and import also inspect every carried prefix schema, because a partial payload now proves its complete registry base by content. This is an explicit unsupported partial-payload boundary, not completed arbitrary constructor restoration. The full persistent-owner route does not use this partial registry exporter.

Implement canonical constructor references in partial schema records, bind them to the registry/schema version and captured base, and resolve/validate name, class, physical width and signedness before publishing any store. Preserve primitive codes as their fixed ABI identities; either remap dynamic references to verified destination constructors or install their declared identities transactionally. Same-count foreign constructor occupants must refuse. Add independently constructed same-count A/B constructors, corrupted late references, source-state destruction, JIT/native checked consumers and wrong-type negatives. Keep partial graph acceptance open until this and complete schema-reference validation pass. Parent is habu-wire-the-checker-eec26aea. Claim: payload_resume.

