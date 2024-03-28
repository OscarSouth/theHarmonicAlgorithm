match (n:Cadence{show:"( pedal -> min )"})
with n
match (n)-[r]->(to)
return r, to
  order by r.confidence desc
  limit 10;


MATCH (n:Cadence{show:"( pedal -> min )"})
WITH n
MATCH (n)-[r]->(to)
RETURN to.chord, to.movement, r.confidence
  ORDER BY r.confidence DESC
  LIMIT 30;


match (n:Cadence{show:"( pedal -> min )"})
call {
with n
match (n)-[r]->(to)
return r, to
  order by (r.confidence*rand()) desc
  limit 3
union all
with n
match (n)-[r]->(to)
return r, to
  order by r.confidence desc
  limit 1
}
return to.functionality, to.movement, to.chord
  order by rand()
  limit 1;