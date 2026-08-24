#!/usr/bin/env bash
#
# Build the sparse, modern-format corpus artefact from the live dense graph.
#
# HISTORICAL NOTE: this performed the one-time 4.4 -> 5.26 migration on 2026-08-24
# (GATE 1 and GATE 2 both passed with zero score drift). The dump-dense and scratch-a
# stages deliberately speak Neo4j 4.4 CLI syntax because they consume the pre-migration
# store; they are only useful again if migrating another 4.4-era graph.
#
# Why: 99% of the dense graph's 928MB string store is zero-valued composer
# entries (every edge carries every composer; avg 6 of 469 are non-zero).
# Dropping zeros is score-neutral by construction — resolveWeights reads a
# missing key as 0, and r.confidence (the wildcard aggregate) is untouched —
# but it must be PROVEN, not assumed. This script does the mechanics and the
# structural checks; the behavioural gates (resolved-candidate parity through
# the actual Haskell query path) are run from GHCi afterwards — the stage
# banners say exactly when.
#
# The live database is NEVER modified. Pipeline:
#
#   baseline    counts + samples captured from the live graph (read-only)
#   dump-dense  live container briefly stopped for a 4.4 dump (auto-restart)
#   scratch-a   dense dump loaded into a throwaway 4.4 container; zeros
#               dropped there with apoc.periodic.iterate; structural checks
#               (GATE 1 — same server, same driver — runs from GHCi after this)
#   rebuild     scratch A exported to CSV over HTTP, imported offline into a
#               fresh Neo4j 5.26 via neo4j-admin database import (a dump only
#               archives store files, so shrinking REQUIRES a rebuild; 4.4
#               Community has no neo4j-admin copy), constraint recreated,
#               counts verified, query/v2 probed, dumped to the artefact
#               (GATE 2 — old stack vs new stack — runs from GHCi after this)
#   status      show scratch containers and outputs
#   clean       remove scratch containers and their data dirs
#
# Usage: scripts/sparsify_graph.sh [baseline|dump-dense|scratch-a|rebuild|status|clean|all]
#
# Ports (chosen clear of live 7474/7687 and the unrelated 7476/7689 pair):
#   scratch A (neo4j:4.4.13): http 7475, bolt 7688
#   scratch B (neo4j:5.26):   http 7477, bolt 7690

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

WORK="$ROOT/out/sparsify"
IMPORT_DIR="$WORK/import"
IMAGE_44="neo4j:4.4.13"
IMAGE_526="neo4j:5.26"
A_NAME="ha-sparsify-a"
B_NAME="ha-sparsify-b"
A_HTTP=7475; A_BOLT=7688
B_HTTP=7477; B_BOLT=7690
DENSE_DUMP="$WORK/ycacl-graph-dense.dump"
ARTEFACT="$ROOT/out/ycacl-graph.dump"

mkdir -p "$WORK/baseline" "$IMPORT_DIR"

stage="${1:-all}"

http_query() {  # http_query <port> <cypher> [params-json]
  local port="$1" cypher="$2" params="${3:-}"
  if [ -z "$params" ]; then params='{}'; fi
  curl -fsS -u neo4j:password -H 'Content-Type: application/json' \
    -X POST "http://localhost:$port/db/neo4j/tx/commit" \
    -d "$(python3 -c 'import json,sys; print(json.dumps({"statements":[{"statement":sys.argv[1],"parameters":json.loads(sys.argv[2])}]}))' "$cypher" "$params")"
}

wait_http() {  # wait_http <port> <label>
  local port="$1" label="$2" i
  for i in $(seq 1 120); do
    if curl -fsS -o /dev/null "http://localhost:$port" 2>/dev/null; then
      # port up; now wait until cypher answers (auth + db ready)
      if http_query "$port" "RETURN 1" >/dev/null 2>&1; then
        echo "    $label ready"
        return 0
      fi
    fi
    sleep 2
  done
  echo "!!  $label did not come up on :$port" >&2
  return 1
}

counts_json() {  # counts_json <port>  -> prints "nodes edges confsum"
  http_query "$1" \
    "MATCH (c:Cadence) WITH count(c) AS n MATCH ()-[r:NEXT]->() RETURN n, count(r), sum(r.confidence)" \
    | python3 -c 'import json,sys; r=json.load(sys.stdin)["results"][0]["data"][0]["row"]; print(r[0], r[1], repr(r[2]))'
}

# ---------------------------------------------------------------- baseline --
stage_baseline() {
  echo "==> [baseline] live graph (read-only, :7474)"
  counts_json 7474 | tee "$WORK/baseline/counts.txt"

  echo "==> [baseline] sampling 25 source keys (hottest, coldest, spread)"
  http_query 7474 "
    MATCH (c:Cadence)-[r:NEXT]->() WITH c.show AS k, count(r) AS d ORDER BY d DESC
    WITH collect(k) AS ks
    RETURN ks[0..10] + ks[size(ks)-5..size(ks)] + [x IN range(0, size(ks)-1, size(ks)/10) | ks[x]][0..10]" \
    | python3 -c '
import json,sys
keys = json.load(sys.stdin)["results"][0]["data"][0]["row"][0]
seen, out = set(), []
for k in keys:
    if k not in seen:
        seen.add(k); out.append(k)
print("\n".join(out[:25]))' > "$WORK/baseline/keys.txt"
  wc -l "$WORK/baseline/keys.txt"

  echo "==> [baseline] per-edge dense reference for 200 random edges (id, nonzero count, confidence)"
  http_query 7474 "
    MATCH ()-[r:NEXT]->() WITH r, rand() AS x ORDER BY x LIMIT 200
    WITH r, apoc.convert.fromJsonMap(r.weights) AS w
    RETURN id(r), size([k IN keys(w) WHERE w[k] > 0]), r.confidence,
           reduce(s=0.0, k IN keys(w) | s + w[k])" \
    > "$WORK/baseline/edge_sample_dense.json"
  python3 - "$WORK/baseline/edge_sample_dense.json" <<'PY'
import json, sys
rows = json.load(open(sys.argv[1]))["results"][0]["data"]
bad = [r["row"] for r in rows if abs(r["row"][2] - r["row"][3]) > 1e-9]
print(f"    {len(rows)} edges sampled; confidence==sum(weights) violations: {len(bad)}")
assert not bad, bad[:3]
PY
}

# -------------------------------------------------------------- dump-dense --
stage_dump_dense() {
  echo "==> [dump-dense] stopping live neo4j (4.4 cannot dump a running DB)"
  docker compose stop neo4j
  # `docker compose start` reuses the old container, which has been observed to
  # wedge in a silent entrypoint crash-loop after a stop; recreating the
  # container (same volumes) recovers it.
  restart_live() {
    echo "==> [dump-dense] restarting live neo4j"
    docker compose start neo4j
    if ! wait_http 7474 "live neo4j"; then
      echo "==> [dump-dense] restart wedged — recreating the container"
      docker compose up -d --force-recreate neo4j
      wait_http 7474 "live neo4j (recreated)"
    fi
  }
  trap restart_live EXIT
  rm -f "$DENSE_DUMP"
  docker run --rm -v "$ROOT/neo4j/data:/data" -v "$WORK:/backups" "$IMAGE_44" \
    neo4j-admin dump --database=neo4j --to="/backups/$(basename "$DENSE_DUMP")"
  restart_live
  trap - EXIT
  ls -lh "$DENSE_DUMP"
  ( cd "$WORK" && shasum -a 256 "$(basename "$DENSE_DUMP")" | tee "$(basename "$DENSE_DUMP").sha256" )
}

# --------------------------------------------------------------- scratch-a --
stage_scratch_a() {
  echo "==> [scratch-a] loading dense dump into a fresh 4.4 store"
  docker rm -f "$A_NAME" >/dev/null 2>&1 || true
  rm -rf "$WORK/scratch_a"; mkdir -p "$WORK/scratch_a/data"
  docker run --rm -v "$WORK/scratch_a/data:/data" -v "$WORK:/backups" "$IMAGE_44" \
    neo4j-admin load --database=neo4j --from="/backups/$(basename "$DENSE_DUMP")" --force

  echo "==> [scratch-a] starting $A_NAME (http :$A_HTTP, bolt :$A_BOLT)"
  docker run -d --name "$A_NAME" \
    -p 127.0.0.1:$A_HTTP:7474 -p 127.0.0.1:$A_BOLT:7687 \
    -v "$WORK/scratch_a/data:/data" \
    -v "$ROOT/neo4j/plugins:/plugins:ro" \
    -v "$IMPORT_DIR:/import-out" \
    -e NEO4J_AUTH=neo4j/password \
    -e NEO4J_dbms_security_procedures_allowlist='apoc.*' \
    -e NEO4J_dbms_security_procedures_unrestricted='apoc.*' \
    -e NEO4J_dbms_memory_heap_max__size=1G \
    -e NEO4J_dbms_memory_pagecache_size=1G \
    "$IMAGE_44" >/dev/null
  wait_http $A_HTTP "scratch A"

  echo "==> [scratch-a] pre-check: counts match baseline"
  counts_json $A_HTTP | tee "$WORK/scratch_a_counts_before.txt"
  python3 - "$WORK/baseline/counts.txt" "$WORK/scratch_a_counts_before.txt" <<'PY'
import sys
a, b = open(sys.argv[1]).read().split(), open(sys.argv[2]).read().split()
assert a[0] == b[0] and a[1] == b[1], (a, b)
print(f"    nodes {a[0]}, edges {a[1]} — match")
PY

  echo "==> [scratch-a] dropping zero-valued composer entries (apoc.periodic.iterate)"
  http_query $A_HTTP "
    CALL apoc.periodic.iterate(
      \"MATCH ()-[r:NEXT]->() RETURN r\",
      \"WITH r, apoc.convert.fromJsonMap(r.weights) AS w
       SET r.weights = apoc.convert.toJson(
         apoc.map.fromPairs([k IN keys(w) WHERE w[k] > 0 | [k, w[k]]]))\",
      {batchSize: 1000, parallel: false})
    YIELD batches, total, failedBatches, failedOperations, errorMessages
    RETURN batches, total, failedBatches, failedOperations, errorMessages" \
    | python3 -c '
import json, sys
row = json.load(sys.stdin)["results"][0]["data"][0]["row"]
print(f"    batches={row[0]} total={row[1]} failedBatches={row[2]} failedOps={row[3]}")
assert row[2] == 0 and row[3] == 0, row[4]'

  echo "==> [scratch-a] structural checks vs baseline"
  counts_json $A_HTTP | tee "$WORK/scratch_a_counts_after.txt"
  python3 - "$WORK/baseline/counts.txt" "$WORK/scratch_a_counts_after.txt" <<'PY'
import sys
a, b = open(sys.argv[1]).read().split(), open(sys.argv[2]).read().split()
assert a[0] == b[0] and a[1] == b[1], (a, b)
ca, cb = float(eval(a[2])), float(eval(b[2]))
rel = abs(ca - cb) / ca
print(f"    counts match; sum(confidence) rel delta = {rel:.2e}")
assert rel < 1e-9, (ca, cb)
PY
  echo "==> [scratch-a] per-edge check: sparse keys == dense non-zero keys (the 200-edge sample)"
  python3 - "$WORK/baseline/edge_sample_dense.json" <<'PY'
import json, subprocess, sys
rows = json.load(open(sys.argv[1]))["results"][0]["data"]
ids = [r["row"][0] for r in rows]
q = json.dumps({"statements":[{"statement":
  "MATCH ()-[r:NEXT]->() WHERE id(r) IN $ids "
  "WITH r, apoc.convert.fromJsonMap(r.weights) AS w "
  "RETURN id(r), size(keys(w)), size([k IN keys(w) WHERE w[k] > 0]), r.confidence",
  "parameters": {"ids": ids}}]})
out = subprocess.run(
  ["curl","-fsS","-u","neo4j:password","-H","Content-Type: application/json",
   "-X","POST","http://localhost:7475/db/neo4j/tx/commit","-d",q],
  capture_output=True, check=True).stdout
sparse = {r["row"][0]: r["row"] for r in json.loads(out)["results"][0]["data"]}
bad = 0
for r in rows:
    rid, dense_nz, conf, _ = r["row"]
    srow = sparse[rid]
    ok = srow[1] == srow[2] == dense_nz and abs(srow[3] - conf) < 1e-12
    bad += 0 if ok else 1
print(f"    {len(rows)} edges: sparse key-count == dense non-zero count, all values > 0, confidence untouched — {'OK' if bad==0 else f'{bad} MISMATCHES'}")
assert bad == 0
PY

  cat <<'GATE'

==> [scratch-a] STRUCTURAL CHECKS PASSED.
    Now run GATE 1 from GHCi before proceeding to `rebuild`:
    resolved-candidate parity, live dense (:7687) vs scratch A sparse (:7688),
    through the real fetchTransitions/resolveWeights path, 25 keys x 4 blends.
GATE
}

# ----------------------------------------------------------------- rebuild --
stage_rebuild() {
  echo "==> [rebuild] exporting scratch A to CSV over HTTP"
  python3 - "$IMPORT_DIR" <<'PY'
import csv, json, subprocess, sys
out_dir = sys.argv[1]

def q(cypher):
    body = json.dumps({"statements":[{"statement": cypher}]})
    res = subprocess.run(
        ["curl","-fsS","-u","neo4j:password","-H","Content-Type: application/json",
         "-X","POST","http://localhost:7475/db/neo4j/tx/commit","-d",body],
        capture_output=True, check=True).stdout
    doc = json.loads(res)
    assert not doc["errors"], doc["errors"]
    return [r["row"] for r in doc["results"][0]["data"]]

nodes = q("MATCH (c:Cadence) RETURN c.show, c.chord, c.movement, c.dissonance")
with open(f"{out_dir}/nodes.csv", "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["show:ID", "chord", "movement", "dissonance:long", ":LABEL"])
    for show, chord, movement, dissonance in nodes:
        w.writerow([show, chord, movement, dissonance, "Cadence"])

rels = q("MATCH (a:Cadence)-[r:NEXT]->(b:Cadence) "
         "RETURN a.show, b.show, r.confidence, r.weights")
with open(f"{out_dir}/rels.csv", "w", newline="") as f:
    w = csv.writer(f)
    w.writerow([":START_ID", ":END_ID", ":TYPE", "confidence:double", "weights"])
    for a, b, conf, weights in rels:
        w.writerow([a, b, "NEXT", repr(conf), weights])

print(f"    nodes.csv: {len(nodes)} rows, rels.csv: {len(rels)} rows")
assert len(nodes) == 660 and len(rels) == 131031, (len(nodes), len(rels))
PY
  ls -lh "$IMPORT_DIR"/nodes.csv "$IMPORT_DIR"/rels.csv

  echo "==> [rebuild] offline import into a fresh Neo4j 5.26 store"
  docker rm -f "$B_NAME" >/dev/null 2>&1 || true
  rm -rf "$WORK/scratch_b"; mkdir -p "$WORK/scratch_b/data"
  docker run --rm -v "$WORK/scratch_b/data:/data" -v "$IMPORT_DIR:/import" "$IMAGE_526" \
    neo4j-admin database import full neo4j \
      --nodes=/import/nodes.csv --relationships=/import/rels.csv

  echo "==> [rebuild] starting $B_NAME (http :$B_HTTP, bolt :$B_BOLT)"
  docker run -d --name "$B_NAME" \
    -p 127.0.0.1:$B_HTTP:7474 -p 127.0.0.1:$B_BOLT:7687 \
    -v "$WORK/scratch_b/data:/data" \
    -e NEO4J_AUTH=neo4j/password \
    "$IMAGE_526" >/dev/null
  wait_http $B_HTTP "scratch B"

  echo "==> [rebuild] recreating the uniqueness constraint (5.x syntax)"
  http_query $B_HTTP "CREATE CONSTRAINT cadence_show IF NOT EXISTS FOR (c:Cadence) REQUIRE c.show IS UNIQUE" >/dev/null
  echo "==> [rebuild] verifying counts against baseline"
  counts_json $B_HTTP | tee "$WORK/scratch_b_counts.txt"
  python3 - "$WORK/baseline/counts.txt" "$WORK/scratch_b_counts.txt" <<'PY'
import sys
a, b = open(sys.argv[1]).read().split(), open(sys.argv[2]).read().split()
assert a[0] == b[0] and a[1] == b[1], (a, b)
rel = abs(float(eval(a[2])) - float(eval(b[2]))) / float(eval(a[2]))
print(f"    nodes {a[0]}, edges {a[1]}, sum(confidence) rel delta {rel:.2e}")
assert rel < 1e-9
PY

  echo "==> [rebuild] probing the modern HTTP endpoint (query/v2) on 5.26"
  curl -fsS -u neo4j:password -H 'Content-Type: application/json' \
    -X POST "http://localhost:$B_HTTP/db/neo4j/query/v2" \
    -d '{"statement":"MATCH (c:Cadence) RETURN count(c) AS n"}' \
    | python3 -c 'import json,sys; d=json.load(sys.stdin); print("    query/v2 OK:", d["data"]["values"][0][0], "nodes")'

  echo "==> [rebuild] dumping the artefact"
  docker stop "$B_NAME" >/dev/null
  rm -f "$ARTEFACT" "$WORK/neo4j.dump"
  docker run --rm -v "$WORK/scratch_b/data:/data" -v "$WORK:/backups" "$IMAGE_526" \
    neo4j-admin database dump neo4j --to-path=/backups
  mv "$WORK/neo4j.dump" "$ARTEFACT"
  docker start "$B_NAME" >/dev/null
  wait_http $B_HTTP "scratch B"

  SIZE_MB=$(( $(stat -f %z "$ARTEFACT") / 1048576 ))
  ls -lh "$ARTEFACT"
  if [ "$SIZE_MB" -gt 100 ]; then
    echo "!!  artefact is ${SIZE_MB}MB — expected 10-30MB. Investigate before publishing." >&2
    exit 1
  fi
  ( cd "$(dirname "$ARTEFACT")" && shasum -a 256 "$(basename "$ARTEFACT")" | tee SHA256SUMS )

  cat <<'GATE'

==> [rebuild] ARTEFACT BUILT.
    Now run GATE 2 from GHCi: resolved-candidate parity, scratch A (:7688,
    Bolt, 4.4) vs scratch B (:7690 Bolt is closed on 5.26 to hasbolt — use
    HTTP :7477 query/v2 via the Phase III transport, or curl-level parity),
    then the end-to-end generation smoke. Publish only after GATE 2 passes.
GATE
}

case "$stage" in
baseline)   stage_baseline ;;
dump-dense) stage_dump_dense ;;
scratch-a)  stage_scratch_a ;;
rebuild)    stage_rebuild ;;
all)
  stage_baseline
  stage_dump_dense
  stage_scratch_a
  stage_rebuild
  echo "==> all stages complete"
  ;;

# ------------------------------------------------------------------ status --
status)
  docker ps -a --filter "name=ha-sparsify" --format '{{.Names}}\t{{.Image}}\t{{.Status}}\t{{.Ports}}'
  ls -lh "$WORK" 2>/dev/null || true
  ls -lh "$ARTEFACT" 2>/dev/null || true
  ;;

# ------------------------------------------------------------------- clean --
clean)
  docker rm -f "$A_NAME" "$B_NAME" 2>/dev/null || true
  rm -rf "$WORK/scratch_a" "$WORK/scratch_b"
  echo "scratch containers and stores removed (baseline, dumps and CSVs kept)"
  ;;

*)
  echo "usage: $0 [baseline|dump-dense|scratch-a|rebuild|status|clean|all]" >&2
  exit 2 ;;
esac
