---
title: Seal the checking result behind its producer
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T18:41:16.744677+02:00"
---

src/compiler/native/cert.f publishes NCERT-RESULT:MAKE as an open constructor, so checked code can fabricate a result whose VERDICT field says certified while binding the honest digests of a real tape and source - NCERT:VERIFY compares only the two digests and cannot see that the verdict never came from the checker. Today the only consumers are tests, so nothing is broken, but the moment a production stage trusts a result's verdict (the migrated compiler entry, the comparison shadow), the verdict needs provenance. Same shape as habu-close-the-alloc-af5b68a2: a capability only the producer holds - NFEED END-UNIT mints results, nothing else can - the way HIR-WORD's interned token forces declarers to check first. Owners: NCERT, NFEED. Depends on habu-feed-the-src-f7ed8733.
