% EECS 3401 Fall 2020 Assignment 3 Starter Code for Question 2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 2 Part A
:- 
%%% Primitive action declarations

primitive_action(go(Loc1, Loc2)).
primitive_action(push(Box_num, Loc1, Loc2)).
primitive_action(climbUp(Box)).
primitive_action(climbDown(Box)).
primitive_action(turnOn(Switch)).
primitive_action(turnOff(Switch)).




%%% Preconditions for Primitive Actions

sameLocation(Loc1, Loc2):- in(Loc1, R1), in(Loc2, R1).

poss(go(Loc1, Loc2), S) :- onTop(floor, S), sameLocation(Loc1, Loc2), 
                           robotLoc(Loc1, S). 
poss(push(Box, Loc1, Loc2), S) :- box(Box), sameLocation(Loc1, Loc2), 
                           boxLoc(Box, Loc1, S), robotLoc(Loc1,S).
poss(climbUp(Box), S) :- box(Box), onTop(floor, S), boxLoc(Box, LocB, S), 
                                    robotLoc(LocB,S).
poss(climbDown(Box), S):- box(Box), onTop(Box, S), boxLoc(Box, LocB, S), 
                                    robotLoc(LocB,S).
poss(turnOn(Switch),S) :- switch(Switch), onTop(Box, S), not(up(Switch, S)),
                              boxLoc(Box, LocB, S), switchLoc(Switch, LocB).
poss(turnOff(Switch),S) :- switch(Switch), onTop(Box,S), up(Switch,S), 
                              boxLoc(Box, LocB, S), switchLoc(Switch,LocB).






%%% Successor State Axioms for Primitive Fluents

% Pattern:
% myfluent(Arg, do(A,S)) :- /* positive effects */ ;
%                           myfluent(Arg, S), not (/*negative effecs */).
robotLoc(Loc,do(A,S)):- A = go(_,Loc); A = push(_, _, Loc);
                  not(A = go(_, Loc)), not(A = push(_, _, Loc)), robotLoc(Loc,S).
boxLoc(Box,Loc,do(A,S)):- A = push(Box, _, Loc); 
                        not(A = push(Box, _, Loc)), boxLoc(Box, Loc, S).
onTop(B,do(A,S)):- A = climbUp(B); A = climbDown(B); not(A = climbUp(B)), not(A = climbDown(B)), onTop(B,S).
up(Sw,do(A,S)):- A = turnOn(Sw); A = turnOff(Sw); not(A = turnOn(Sw)), not(A = turnOff(Sw)), up(Sw,S).
onn(Light,do(A,S)):- controls(Sw, Light), A = turnOn(Sw); controls(Sw,Light), not(A = turnOn(Sw)), onn(Light,S).


%%% Defined Fluents
% Feel free to define your own fluents here as needed.




%%% Non-Fluent Predicates
% Describe static facts here, like the names and types of objects in the world,
% their locations, which location is in which room, etc.

box(box1).  % Et cetera.
box(box2).
box(box3).
box(box4).

switch(switch1).
switch(switch2).
switch(switch3).
switch(switch4).

room(room1).
room(room2).
room(room3).
room(room4).


location(initBox1Loc). % Et cetera.
location(initBox2Loc).
location(initBox3Loc).
location(initBox4Loc).

location(switch1Loc).
location(switch2Loc).
location(switch3Loc).
location(switch4Loc).

location(door1Loc).
location(door2Loc).
location(door3Loc).
location(door4Loc).

in(initBox1Loc,room1).
in(initBox2Loc,room1).
in(initBox3Loc,room1).
in(initBox4Loc,room1).

in(door1Loc, room1).
in(door2Loc, room2).
in(door3Loc, room3).
in(door4Loc, room4).

in(door1Loc, corridor).
in(door2Loc, corridor).
in(door3Loc, corridor).
in(door4Loc, corridor).

in(switch1Loc, room1).
in(switch2Loc, room2).
in(switch3Loc, room3).
in(switch4Loc, room4).

in(shakeyLocation, room3).

controls(switch1, light1).
controls(switch2, light2).
controls(switch3, light3).
controls(switch4, light4).

switchLoc(switch1, switch1Loc).
switchLoc(switch2, switch2Loc).
switchLoc(switch3, switch3Loc).
switchLoc(switch4, switch4Loc).

%%% Initial Situation
% Define which *fluents* are true in situation s0

robotLoc(shakeyLocation, s0).
boxLoc(box1,initBox1Loc,s0). % Etc.
onTop(floor,s0).
up(switch1,s0).
up(switch4,s0).
onn(light1,s0).
onn(light4,s0).
% ...


% Restore suppressed situation arguments.
% Needed by GOLOG for technical purposes.
% Update if you are introducing additional fluents.

restoreSitArg(robotLoc(L),S,robotLoc(L,S)).
restoreSitArg(boxLoc(B,L),S,boxLoc(B,L,S)).
restoreSitArg(onTop(B),S,onTop(B,S)).
restoreSitArg(up(Sw),S,up(Sw,S)).
restoreSitArg(onn(Light),S,onn(Light,S)).
% do the same for the remaining fluents


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Question 2 Part B

% goal of having box2 in room2
% A simple condition with respect to situation S
goalPartB(S) :- boxLoc(box2,Loc,S),in(Loc,room2).


% solPartB(S) binds S to a ground situation term that achieves goalPartB(S)
% solPartB(S):- S = (do(go(shakeyLocation, door3Loc)), 
%                      do(go(door3Loc,door1Loc)),
%                      do(go(door1Loc,initBox2Loc)),
%                      do(push(box2,initBox2Loc,door1Loc)),
%                      do(push(box2,door1Loc,door2Loc)),
%                      do(push(box2,door2Loc,switch2Loc))).
solPartB(S):- A0 = go(shakeyLocation, door3Loc),
               A1 = go(door3Loc, door1Loc),
               A2 = go(door1Loc, initBox2Loc),
               A3 = push(box2,initBox2Loc, door1Loc),
               A4 = push(box2, door1Loc, door2Loc),
               A5 = push(box2, door2Loc, switch2Loc),
               S = do(A5, do(A4, do(A3, do(A2, do(A1, do(A0, s0)))))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Question 2 Part C

proc(getBox, go(door1Loc, initBox2Loc), go(door3Loc, door1Loc), go(shakeyLocation, door3Loc)).
proc(pushBoxToLoc, push(box2, door2Loc, switch2Loc), push(box2, door1Loc, door2Loc), push(box2,initBox2Loc, door1Loc)).
proc(getBox2toRoom2,   % a Golog procedure that implements the plan in B
   /* Here goes a complex action
      representing the same plan
      as in previous question */
      pushBoxToLoc, getBox
).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Question 2 Part D

% To implement this part, you need to get somewhat comfortable
% with the syntax of GOLOG. Refer to the definitions in the interpreter
% and the last section of the Assignment 3 handout.
% To keep the program intelligible, feel free to define additional procedures
% such as
%    - goTurnOnLight(L)   % Given a light, go and turn it on
%    - getBoxToLoc(B)     % Given a box, get to its location
%    - pushBoxToLoc(B,L)  % Given a box and a location, move the box there
%    - goToLoc(L)         % Given a location, get there
% and so on. These procedures abstract away from the primitive actions
% and serve as building block for the even more abstract procedure "allLightsOn".


proc(allLightsOn,
   /* Your GOLOG code here */
). 
% getBoxToLoc(B) go to B Location
% pushBoxToLoc(B, L) push the B to L
   % - push every box to a switch
% goToLoc(L) go to L
   % - use to go to a switch
% goTurnOnLight(L) turn on L
   % - use to climbup a box to turn on the switch which turn on the light




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Question 2 Part E

acceptable(A,S) :- /* your code here */.

restoreSitArg(acceptable(A),S,acceptable(A,S)).

% For testing???
goal(S) :- goalPartB(S).
restoreSitArg(goal,S,goal(S)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% added by Yves Lesperance

% Pretty printing of situations
show_act_seq(s0).
show_act_seq(do(A,S)):- show_act_seq(S), write(A), nl.

% definition of executable (legal) situation
executable(s0).
executable(do(A,S)) :- poss(A,S), executable(S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
