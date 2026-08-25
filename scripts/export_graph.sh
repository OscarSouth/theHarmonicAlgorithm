#!/usr/bin/env bash
#
# Export the composer graph as a single-file archive for publication.
#
# The dump is produced by hand, not by CI: it needs a populated Neo4j. Upload the
# result to the dedicated corpus release (currently `corpus-v2`) so one stable
# URL serves every code release. Since corpus-v2 the graph is sparse at source,
# so this plain "dump whatever the live graph holds" tool IS the artefact
# builder; the historical sparsify step is no longer part of the flow.
#
# NOTE ON SYNTAX: this stack runs Neo4j 5.26, which uses `neo4j-admin database dump`
# / `neo4j-admin database load`. The bare `neo4j-admin dump` form is 4.x and will NOT
# work here. The database must be offline for the dump, so the container is stopped
# for the duration and restarted afterwards.
#
# Usage:  scripts/export_graph.sh [output-dir]     (default: out/)

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

OUT_DIR="${1:-out}"
IMAGE="neo4j:5.26"
DUMP_NAME="ycacl-graph.dump"

mkdir -p "$OUT_DIR"
OUT_ABS="$(cd "$OUT_DIR" && pwd)"

echo "==> Graph contents before dump"
if curl -fsS -u neo4j:password -H 'Content-Type: application/json' \
     -d '{"statement":"MATCH (n:Cadence) WITH count(n) AS cad OPTIONAL MATCH (ch:Change) WITH cad, count(ch) AS chg MATCH (a)-[r:NEXT]->(b) WHERE labels(a) = labels(b) RETURN cad, chg, count(r)"}' \
     http://localhost:7474/db/neo4j/query/v2 2>/dev/null \
     | python3 -c 'import json,sys; v=json.load(sys.stdin)["data"]["values"][0]; print(f"    Cadence nodes: {v[0]}  Change nodes: {v[1]}  NEXT edges: {v[2]}")'; then
  :
else
  echo "    (Neo4j not reachable — cannot report counts; continuing)"
fi

echo "==> Stopping neo4j (the database must be offline for a dump)"
docker compose stop neo4j

# `docker compose start` reuses the old container, which has been observed to wedge
# in a silent entrypoint crash-loop after a stop; recreate on failure.
restart() {
  echo "==> Restarting neo4j"
  docker compose start neo4j
  for i in $(seq 1 60); do
    if curl -fsS -o /dev/null http://localhost:7474 2>/dev/null; then return 0; fi
    sleep 2
  done
  echo "==> Restart wedged — recreating the container"
  docker compose up -d --force-recreate neo4j
}
trap restart EXIT

echo "==> Dumping to $OUT_ABS/$DUMP_NAME"
rm -f "$OUT_ABS/$DUMP_NAME" "$OUT_ABS/neo4j.dump"
docker run --rm \
  -v "$ROOT/neo4j/data:/data" \
  -v "$OUT_ABS:/backups" \
  "$IMAGE" \
  neo4j-admin database dump neo4j --to-path=/backups
mv "$OUT_ABS/neo4j.dump" "$OUT_ABS/$DUMP_NAME"

echo "==> Result"
ls -lh "$OUT_ABS/$DUMP_NAME"
( cd "$OUT_ABS" && shasum -a 256 "$DUMP_NAME" | tee SHA256SUMS )

cat <<EOF

==> Next steps

1. Upload as a single stable asset, decoupled from code releases:

     gh release create corpus-v2 \\
       --title "Harmonic graphs (YCACL :Cadence + Bunks jazz :Change, corpus-v2)" \\
       --notes "Neo4j 5.26 dump of the harmonic transition graph (consistent-path counting)." \\
       "$OUT_ABS/$DUMP_NAME" "$OUT_ABS/SHA256SUMS"

   To replace it later WITHOUT breaking links, keep the tag and filename identical:

     gh release upload corpus-v2 "$OUT_ABS/$DUMP_NAME" --clobber

2. Users load it with (note: 5.x syntax, and neo4j must be stopped):

     docker compose stop neo4j
     docker run --rm -i \\
       -v "\$PWD/neo4j/data:/data" \\
       $IMAGE \\
       neo4j-admin database load neo4j --from-stdin --overwrite-destination < $DUMP_NAME
     docker compose start neo4j

   (--from-stdin keeps the published filename; a file-based load would require
   renaming it to neo4j.dump.)

3. Verify from a clean container before announcing it, then generate with seek "*".
EOF
