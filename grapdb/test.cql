match (n:Cadence{show:"( asc 2 -> maj )"})
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