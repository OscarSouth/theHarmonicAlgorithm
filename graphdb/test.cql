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


match (n_0:Cadence{show:"( asc 2 -> maj )"})
call {
  with n_0
    call {
    with n_0
    match (n_0)-[r]->(n_1)
    return r, n_1
      order by (r.confidence*rand()) desc
      limit 3
    union all
    with n_0
    match (n_0)-[r]->(n_1)
    return r, n_1
      order by r.confidence desc
      limit 1
  }
  return n_1
    order by rand()
    limit 1
}
call {
  with n_1
    call {
    with n_1
    match (n_1)-[r]->(n_2)
    return r, n_2
      order by (r.confidence*rand()) desc
      limit 3
    union all
    with n_1
    match (n_1)-[r]->(n_2)
    return r, n_2
      order by r.confidence desc
      limit 1
  }
  return n_2
    order by rand()
    limit 1
}
call {
  with n_2
    call {
    with n_2
    match (n_2)-[r]->(n_3)-->(n0)
    return r, n_3
      order by (r.confidence*rand()) desc
      limit 3
    union all
    with n_2
    match (n_2)-[r]->(n_3)-->(n0)
    return r, n_3
      order by r.confidence desc
      limit 1
  }
  return n_3
    order by rand()
    limit 1
}
return n_0, n_1, n_2, n_3;

match (n:Cadence)
with apoc.coll.randomItem(COLLECT(n)) AS n_0
call {
with n_0
call {
with n_0
match (n_0)-[r]->(n_1)
return r, n_1
  order by (r.confidence*rand()) desc
  limit 3
union all
with n_0
match (n_0)-[r]->(n_1)
return r, n_1
  order by r.confidence desc
  limit 1
}
return n_1
  order by rand()
  limit 1
}
call {
with n_1
call {
with n_1
match (n_1)-[r]->(n_2)
return r, n_2
  order by (r.confidence*rand()) desc
  limit 3
union all
with n_1
match (n_1)-[r]->(n_2)
return r, n_2
  order by r.confidence desc
  limit 1
}
return n_2
  order by rand()
  limit 1
}
call {
with n_2
call {
with n_2
match (n_2)-[r]->(n_3)-->(n0)
return r, n_3
  order by (r.confidence*rand()) desc
  limit 3
union all
with n_2
match (n_2)-[r]->(n_3)-[r_last]->(n0)
return r, n_3
  order by r.confidence desc, r_last.confidence desc
  limit 1
}
return n_3
  order by rand()
  limit 1
}
return
  n_0.chord, n_0.movement,
  n_1.chord, n_1.movement,
  n_2.chord, n_2.movement,
  n_3.chord, n_3.movement;
