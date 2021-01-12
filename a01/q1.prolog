/* NAME: JEFF KWAN
 * COURSE: EECS 3401
 * DATE: OCT 21 2020
 * DESCRIPTION: Write  and  test  a  Prolog  programq1.prologthat  deals  with  family  relations. 
 * CONSTANTS: john, paul, henry, helen, june, mary, adam
 * PREDICATES: ancestor(X, Y), common_ancestor(X, Y, Z), ancestorList( X, Y, L), descendantTree(X, L)
 * L = [john, [paul, [henry, [helen]], [june]], [mary, [adam]]]    
 *    
 *                     john
 *                 /          \
 *               paul          mary
 *              /    \            \
 *            henry  june        adam
 *            /
 *          helen
 */
/*
john.
paul.
henry.
june.
mary.
adam.

parent(john, paul).
parent(john, mary).


parent(paul, henry).
parent(paul, june).

parent(henry, helen).
parent(mary, adam).

*/

ancestor(X,Y):- parent(X,Y). /*X is parent of Y then X is an ancestor of Y*/
ancestor(X,Y):- parent(X,Z), ancestor(Z,Y). /* recursive check if X is an ancestor of Y */

common_ancestor(X,Y,Z):- ancestor(X,Y), ancestor(X,Z). /*common ancestor is X is ancestor of both Y, and Z*/

closest_common_ancestor(X,Y,Z) :- common_ancestor(X,Y,Z), parent(X,I), \+common_ancestor(I,Y,Z). /*Child I is not a common ancestor of Y and Z*/

ancestorList(X, Y, []):- parent(X,Y).
ancestorList(X, Y, [Head|Tail]):- ancestor(X,Head), ancestor(Head,Y), ancestorList(Head,Y,Tail).


descendantTree(X,[X]) :- \+parent(X,_).
descendantTree(X,[X | L]) :- parent(X,_),
    setof(I, Y^(parent(X,Y), descendantTree(Y,I)), L).
