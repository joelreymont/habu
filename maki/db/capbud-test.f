\ maki/db/capbud-test.f - the aggregate suite for the capability + budget subsystem
\ (dot habu-v2-capability-and-0970a96d). ONE maki/test.f entry runs the four focused
\ concern suites - the budget-dimension vocabulary, the capability GRANT token, the
\ monotonic budget LEDGER, and the authorized-commit gate - each of which is a real,
\ standalone `bin/hb --load`-able suite (its own package + T-RESET/T-REPORT). Aggregating
\ them keeps ONE concern per test file while fitting the shared maki suite-table budget
\ (lib/test/suite.f ITEM-MAX). A failing child suite `die`s, so the aggregate fails closed.

require maki/db/budget-dim-test.f
require maki/db/capability-test.f
require maki/db/budget-ledger-test.f
require maki/db/commit-store-auth-test.f
