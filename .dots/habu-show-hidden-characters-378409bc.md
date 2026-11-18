---
title: "Show hidden characters in every rejection's quoted text"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T10:08:56.865354+01:00"
---

Problem: rejection writers outside the JSON and XML readers copy untrusted text into the text raw, so a Cc, Cf, Zl or Zp character hides or reorders the message (Unicode Fable review of ztzyrtms): src/lib/json-schema.f ADD-CUT, reached from QUOTE, PATH, FOUND, EXTRA and CHOICE-HEAD (a member name written "\u001b[2J\n" is decoded, so raw ESC and LF reach the text, which MCP forwards for client tool arguments); src/lib/regex.f PUT-TOKEN (a multi-byte token); src/lib/odata/key.f SAY-QUOTED (the key value); src/lib/odata/metadata.f QUOTE (names and values from a service's metadata); src/lib/net/model.f CONFIG-REJECT's found text. Acceptance: each writes its quoted untrusted text through UNICODE-VISIBLE:LEN and PUT, sized by LEN; one test per package pins U+202E in a rejection. Depends: none. Claim: unassigned.
