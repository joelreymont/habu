#!/bin/sh
# pg-fixture.sh - run a command against a throwaway PostgreSQL cluster.
#
#   test/db/pg-fixture.sh build/hb-pq --load lib/db/pq-test.f
#
# Starts an empty trust-authentication cluster on a free loopback port in a
# temporary directory, exports HABU_PG_CONNINFO, runs the command, then stops
# and removes the cluster. The command's exit status is the script's. This is
# a foreign-server harness, the same exception test/net/udp4.py takes: it
# carries no Habu logic of its own.
set -eu

if [ "$#" -eq 0 ]; then
	echo "pg-fixture: no command given" >&2
	exit 2
fi

root=${XDG_RUNTIME_DIR:-/tmp}
dir=$(mktemp -d "$root/habu-pg.XXXXXX")
data=$dir/data
log=$dir/postgres.log
started=0

cleanup() {
	if [ "$started" -eq 1 ]; then
		pg_ctl -D "$data" -m immediate -s stop >/dev/null 2>&1 || true
	fi
	rm -rf "$dir"
}
trap cleanup EXIT INT TERM

initdb -D "$data" -A trust -U habu --no-sync >"$dir/initdb.log" 2>&1 || {
	echo "pg-fixture: initdb failed" >&2
	cat "$dir/initdb.log" >&2
	exit 2
}

port=0
attempt=0
while [ "$attempt" -lt 16 ]; do
	candidate=$((20000 + ($$ + attempt * 101) % 20000))
	if pg_ctl -D "$data" -l "$log" -w -s \
		-o "-p $candidate -k $dir -c listen_addresses=127.0.0.1" \
		start >/dev/null 2>&1; then
		port=$candidate
		started=1
		break
	fi
	attempt=$((attempt + 1))
done

if [ "$started" -eq 0 ]; then
	echo "pg-fixture: no free loopback port in 16 attempts" >&2
	[ -f "$log" ] && cat "$log" >&2
	exit 2
fi

HABU_PG_CONNINFO="host=127.0.0.1 port=$port dbname=postgres user=habu connect_timeout=5"
export HABU_PG_CONNINFO

set +e
"$@"
status=$?
set -e
exit "$status"
