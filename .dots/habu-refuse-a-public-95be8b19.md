---
title: Refuse a public and a private word with one name in a package
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:35:39.672249+02:00"
---

Problem: Joel chose 'Refuse all twins' (2026-11-03): 'it makes no sense to have public and private words with the same name since they will clash within the same package'; chosen again after forwarders were explained with src/lib/task.f HALT (private :1645, public forwarder :2329). docs/forth.md:331-336 still calls the forwarder legal and dot habu-retire-e-shadowed-087422cf line 13 calls same-package forwarders deliberate: settle that dot by this ruling. Acceptance: a package that defines one name in both sections is refused at the second definition in every mode; the tree's twins are renamed; forth.md and the card say so. Files: the checker's definition path, docs/forth.md, docs/forth-card.md, the twins. Verify: a rejected program per mode; the tree loads. Depends: none. Ownership: unassigned. Claim: unassigned.
