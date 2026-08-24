#!/usr/bin/env bash
#
# Export the composer graph as a single-file archive for publication.
#
# The dump is produced by hand, not by CI: it needs a populated Neo4j, and at a few
# hundred MB it has no business moving through GitHub Actions. Upload the result to the
# dedicated `corpus-v1` release so one stable URL serves every code release.
#
# NOTE ON SYNTAX: this stack runs neo4j 4.4.13, which uses `neo4j-admin dump` /
# `neo4j-admin load`. The `neo4j-admin database dump` form is Neo4j 5+ and will NOT work
# here. 4.4 also cannot dump a running database, so the container is stopped for the
# duration and restarted afterwards.
#
# Usage:  scripts/export_graph.sh [output-dir]     (default: out/)

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

OUT_DIR="${1:-out}"
IMAGE="neo4j:4.4.13"
DUMP_NAME="ycacl-graph.dump"

mkdir -p "$OUT_DIR"
OUT_ABS="$(cd "$OUT_DIR" && pwd)"

echo "==> Graph contents before dump"
if curl -fsS -u neo4j:password -H 'Content-Type: application/json' \
     -d '{"statements":[{"statement":"MATCH (n:Cadence) RETURN count(n)"},{"statement":"MATCH ()-[r:NEXT]->() RETURN count(r)"}]}' \
     http://localhost:7474/db/neo4j/tx/commit 2>/dev/null | grep -o '"row":\[[0-9]*\]'; then
  :
else
  echo "    (Neo4j not reachable — cannot report counts; continuing)"
fi

echo "==> Stopping neo4j (4.4 cannot dump a live database)"
docker compose stop neo4j

restart() { echo "==> Restarting neo4j"; docker compose start neo4j; }
trap restart EXIT

echo "==> Dumping to $OUT_ABS/$DUMP_NAME"
rm -f "$OUT_ABS/$DUMP_NAME"
docker run --rm \
  -v "$ROOT/neo4j/data:/data" \
  -v "$OUT_ABS:/backups" \
  "$IMAGE" \
  neo4j-admin dump --database=neo4j --to="/backups/$DUMP_NAME"

echo "==> Result"
ls -lh "$OUT_ABS/$DUMP_NAME"
( cd "$OUT_ABS" && shasum -a 256 "$DUMP_NAME" | tee "$DUMP_NAME.sha256" )

cat <<EOF

==> Next steps

1. Upload as a single stable asset, decoupled from code releases:

     gh release create corpus-v1 \\
       --title "Composer graph (YCACL)" \\
       --notes "Neo4j 4.4 dump of the harmonic transition graph." \\
       "$OUT_ABS/$DUMP_NAME" "$OUT_ABS/$DUMP_NAME.sha256"

   To replace it later WITHOUT breaking links, keep the tag and filename identical:

     gh release upload corpus-v1 "$OUT_ABS/$DUMP_NAME" --clobber

2. Users load it with (note: 4.4 syntax, and neo4j must be stopped):

     docker compose stop neo4j
     docker run --rm \\
       -v "\$PWD/neo4j/data:/data" \\
       -v "\$PWD:/backups" \\
       $IMAGE \\
       neo4j-admin load --database=neo4j --from=/backups/$DUMP_NAME --force
     docker compose start neo4j

3. Verify from a clean container before announcing it, then generate with seek "*".
EOF
