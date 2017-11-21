:- dynamic(numbers/1).
:- dynamic(goal/1).

app([],L,L).
app([X|Xs],Y,[X|Zs]) :- app(Xs,Y,Zs).

% Current Goal
goal([2,2,2,5]).

% Current possible entries
numbers([0,1,2,3,4,5,6,7]).

% Change possible entries range and reroll the them
% defRange(+)
defRange(Range) :-
	random(0,30,X),
	Z is X + Range - 1,
	numlist(X,Z,L),
	retractall(numbers(_)),
	assert(numbers(L)),
	writeln("New Range is: "),
	writeln(L),
	newGoal.
	
% print all allowed entries
printRange :-
	numbers(L),
	writeln("Range is: "),
	writeln(L).

% ceate new Goal with current possible entries
newGoal :-
	retractall(goal(_)),
	randomInRange(X1),
	randomInRange(X2),
	randomInRange(X3),
	randomInRange(X4), 
	Goal = [X1,X2,X3,X4],
	assert(goal(Goal)),
	writeln("New Goal in Range: "),
	numbers(L),
	writeln(L).
	
% randomInRange(-Number)
randomInRange(Z) :-
	numbers(X),
	last(X,UpperBound),
	Y is UpperBound + 1,
	X = [H|_],
	random(H,Y,Z).
	
% start game and restart on bad input
start :-
	readi(Bulls),
	restart(Bulls).
start :-
	numbers(L),
	format("~s ~w ~n",["Bad Input must be in Range of: ",L]),
	start.
	
% restart questioning until all are found
% restart(+Bulls)
restart(Bulls) :-
	Bulls < 4,
	writeln("Try again!"),
	start.
restart(4) :-
	writeln("You got it!").

% read List of 4 numbers and check correctness
% readi(+Bulls)
readi(Bulls) :-
	read([X1,X2,X3,X4]),
	Choice = [X1,X2,X3,X4],
	numbers(Y),
	member(X1,Y),
	member(X2,Y),
	member(X3,Y),
	member(X4,Y),
	goal(Goal),
	bulls(Choice,Goal,Bulls,ChoiceRemainder,GoalRemainder),
	cows(ChoiceRemainder,GoalRemainder,Cows),
	tellB(Bulls),
	tellC(Cows).

% look for correct numbers at correct places
% bulls(+Choice,+Goal,-Bulls,-ChoiceRemainder,-GoalRemainder)
bulls([],[],0,[],[]).
bulls(List1,Goal,CorrectN,CR,Remainder) :- 
	Goal = [G1|Gs],
	List1 = [L1|Ls],
	G1 = L1,
	bulls(Ls,Gs,Correct,CR,Remainder),
	CorrectN is Correct + 1.
bulls(List1,Goal,CorrectN,[CH|CT],[H|T]) :- 
	Goal = [G1|Gs],
	List1 = [L1|Ls],
	G1 \= L1,
	H = G1,
	CH = L1,
	bulls(Ls,Gs,CorrectN,CT,T).
	
% look for correct numbers at wrong places
% cows(+ChoiceRemainder,+GoalRemainder,-Cows)
cows([],_,0).
cows([H|T],Goal,CowsTotal) :-
	findCow(H,Goal,Cows,Remainder),
	cows(T,Remainder,CowsN),
	CowsTotal is CowsN + Cows.
	
findCow(Element,Goal,Cows,Remainder) :-
	member(Element,Goal),
	del(Element,Goal,Remainder),
	Cows is 1.
findCow(_,G,0,G).
	
% print correct bulls
% tellB(+Bulls)
tellB(Bulls) :-
	Bulls > 1,
	format('~w ~s ~n',[Bulls,"are at correct Position."]).
tellB(1) :-
	format('~w ~s ~n',[1,"is at correct Position."]).
tellB(Bulls) :-
	format('~w ~s ~n',[Bulls,"are at correct Position."]).

% print correct cows
% tellC(+Cows)
tellC(Cows) :-
	Cows > 1,
	format('~w ~s ~n',[Cows,"are at wrong Position."]).
tellC(1) :-
	format('~w ~s ~n',[1,"is at wrong Position."]).
tellC(Cows) :-
	format('~w ~s ~n',[Cows,"are at wrong Position."]).

del(A,[A|B],B).
del(A,[B,C|D],[B|E]) :-
	del(A,[C|D],E).





