:- op(100,xfy,on).
    %kid(Name, Last, Gender, Age)
find(Gang):- 
    gangMember(3, Gang), %number of members in a gang
    % Facts %
    kid('Angela', _, female, _) on Gang,
    kid('David', _, male, _)on Gang,
    kid('Mary', _, female, _)on Gang,
    kid(_, _, _, 5)on Gang,
    kid(_, _, _, 7)on Gang,
    kid(_, _, _, 8)on Gang,
    kid(_,'Diamond', _, _)on Gang,
    kid(_, 'Grant', _, Older) on Gang,
    kid(_, 'Leung', _, Younger) on Gang,
    Older is Younger + 3. % Grant is 3 years older than Leung in the gang

% Definitions %
gangMember(0,[]). %base case

gangMember(X,[kid(Name, Last, Gender, Age) | Gang]):-
    X > 0, Y is X - 1, gangMember(Y,Gang). 

X on Gang :- member(X, Gang). %backtracking