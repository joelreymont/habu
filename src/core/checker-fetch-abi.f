\ The appended frozen-certificate callback, also loadable by a retained prefix.
package CHECKER-FETCH-ABI
public

$230 constant CERTIFICATE-OFF

\ The record's TOTAL size, which every owner asks this file for. The owner table
\ appends its own rows after the certificate cell (checker-owner-abi.f, last
\ offset $250), so this is the end of the last appended field whichever table
\ declared it; checker.f checks the two agree before it commits the storage.
$258 constant BYTES

;package
