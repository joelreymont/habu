\ generated-name-collide-bad.f - checker-only generated-name collision.
\
\ TRUST records a checker symbol but creates no xref namespace. The real PRODUCT
\ therefore passes the exact-child namespace fence, plans MAKE, then hard-exits
\ when TDPLAN-NAME+ sees the trusted UNMAKE symbol. Candidate validation owns the
\ process boundary until habu-throw-on-generated-38b50740 makes this catchable and
\ proves rollback plus pending-queue cleanup.

s" GDCLASH:UNMAKE" s" n -- n" TRUST
s" GENERATED-NAME-COLLIDE-ARMED" type cr
PRODUCT gdclash 0 FIELD x n ;PRODUCT
s" LEAKED-PAST-NAME-COLLIDE" type cr
