---
title: A package public reachable only from open-owner scopes should be private
status: open
priority: 2
issue-type: task
created-at: "2026-08-21T14:47:32.045837+02:00"
---

Route 3's step-2 census was wrong for every name it mattered for because it
asked 'does this FILE reopen the package' instead of 'is the owning package
OPEN at this reference site'. src/core/type-family.f reopens package TFAM
fourteen times and still carries five sibling packages (TYPE-FIELD,
TYPE-FIELD-OWNER, CHECKER-DECL-FRAME, PREFIX-BOUND, TYPE-NAME) plus 21
references in global spans between ';package' and the next 'package TFAM', all
of which reach TFAM through 'using TFAM' - that is, through its PUBLIC
wordlist. The file-granular rule reported 71 free conversions; the scope-aware
rule reports 0, and applying the file-granular answer broke the prefix load at
src/core/type-family.f:448 with E-UNDEFINED: TF.NAME-OFF (dot
habu-route-3-the-64078d43 sections 9-11). The durable rule the tree lacks: a
package public every one of whose reference sites has its owner open is not
API and should be private, and a lint can decide that from the structural
token stream the tree already produces (tools/lint/source-lex.f) by tracking
package/;package/using/;using per token. Route 3 built this as a lane
instrument; making it a scheduled lint would keep the published surface honest
as files are added. Needs a named first consumer and a failing probe through a
real gate before any tool is minted - see the Simplify Relentlessly rule.
