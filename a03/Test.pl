on(x,y).
on(y,z).

above(X,Y):- on(X,Y).
above(X,Y):- (on(X,Z),above(Z,Y)).

q:-writeln(qa),r.
q:-writeln(qb), !, r.
q:-writeln(qc), fail.

r:-writeln(ra),!, fail.
r:- writeln(rb),fail.