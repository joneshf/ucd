s --> abs, abs.
s --> acs, acs.
s --> bas, bas.
s --> bcs, bcs.
s --> cas, cas.
s --> cbs, cbs.

abs --> abs_ab.
abs --> abs_ba.
abs --> abs_c.
abs --> epsilon.

abs_ab --> [a], abs, [b].
abs_ab --> [a, b], abs.

abs_ba --> [b], abs, [a].
abs_ba --> [b, a], abs.

abs_c --> [c], abs.

acs --> acs_ac.
acs --> acs_ca.
acs --> acs_b.
acs --> epsilon.

acs_ac --> [a], acs, [c].
acs_ac --> [a, c], acs.

acs_ca --> [c], acs, [a].
acs_ca --> [c, a], acs.

acs_b --> [b], acs.

bas --> bas_ba.
bas --> bas_ab.
bas --> bas_c.
bas --> epsilon.

bas_ba --> [b], bas, [a].
bas_ba --> [b, a], bas.

bas_ab --> [a], bas, [b].
bas_ab --> [a, b], bas.

bas_c --> [c], bas.

bcs --> bcs_bc.
bcs --> bcs_cb.
bcs --> bcs_a.
bcs --> epsilon.

bcs_bc --> [b], bcs, [c].
bcs_bc --> [b, c], bcs.

bcs_cb --> [c], bcs, [b].
bcs_cb --> [c, b], bcs.

bcs_a --> [a], bcs.

cas --> cas_ca.
cas --> cas_ac.
cas --> cas_b.
cas --> epsilon.

cas_ca --> [c], cas, [a].
cas_ca --> [c, a], cas.

cas_ac --> [a], cas, [c].
cas_ac --> [a, c], cas.

cas_b --> [b], cas.

cbs --> cbs_cb.
cbs --> cbs_bc.
cbs --> cbs_a.
cbs --> epsilon.

cbs_cb --> [c], cbs, [b].
cbs_cb --> [c, b], cbs.

cbs_bc --> [b], cbs, [c].
cbs_bc --> [b, c], cbs.

cbs_a --> [a], cbs.

epsilon --> [].

r --> t, t.
r --> u, u.
r --> v, v.

t --> [a], t, [b].
t --> [a, b], t.
t --> [b], t, [a].
t --> [b, a], t.
t --> [c], t.
t --> epsilon.
t --> t, [a, b].
t --> t, [b, a].

u --> [a], u, [c].
u --> [a, c], u.
u --> [c], u, [a].
u --> [c, a], u.
u --> [b], u.
u --> epsilon.
u --> u, [a, c].
u --> u, [c, a].

v --> [b], v, [c].
v --> [b, c], v.
v --> [c], v, [b].
v --> [c, b], v.
v --> [a], v.
v --> epsilon.
v --> v, [b, c].
v --> v, [c, b].
