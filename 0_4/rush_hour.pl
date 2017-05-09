/*
File: rush_hour.pl
Authors:
  Dor Rondel
  Nirender
Instructor: Prof. Daniel Schlegel
Course: CSc 344
Institution: SUNY Oswego
*/

/*****************************
**** Part 1: Define World ****
*****************************/

% Borders
border(0,1).
border(0,2).
border(0,3).
border(0,4).
border(0,5).
border(0,6).
border(1,7).
border(2,7).
border(1,0).
border(2,0).
border(3,0).
border(4,0).
border(5,0).
border(6,0).
border(3,7).
border(4,7).
border(5,7).
border(6,7).
border(7,1).
border(7,2).
border(7,3).
border(7,4).
border(7,5).
border(7,6).


% Cars represented as:
% Id,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE

/* First Set up
_ _ _ _ _ _
|4|4|>|2|_|_|
|_|_|_|2|_|_|
 _|_|_|v|_|_
|_|1|1|>|^|_|
|_|_|_|_|3|_|
|_|_|_|_|3|_| */


car(1,2,4,3,4,4,4,3).
car(2,4,1,4,2,4,3,3).
car(3,5,4,5,5,5,6,3).
car(4,1,1,2,1,3,1,3).


/* Second Set up
_ _ _ _ _ _
|_|_|^|<|2|^|
|_|_|1|_|^|3|
 _|_|_|_|4|^
|_|_|<|6|_|5|
|<|7|7|8|<|9|
|_|_|_|v|_|_| */


% car(1,3,1,3,2,1111,1111,2).
% car(2,4,1,5,1,1111,1111,2).
% car(3,6,1,6,2,1111,1111,2).
% car(4,5,2,5,3,1111,1111,2).
% car(5,6,3,6,4,1111,1111,2).
% car(6,3,4,4,4,1111,1111,2).
% car(7,1,5,2,5,3,5,3).
% car(8,4,5,4,6,1111,1111,2).
% car(9,5,5,6,5,1111,1111,2).


% start and finish
finish(6,3).
beginning(1,3).


/***********************************
**** Part 1.5: Helper Functions ****
************************************/

% params are equal
equals(X,X).

% return is sum of first 2 params
add(A,B,C) :- C is A + B.

% return is difference of first 2 params
subtract(A,B,C) :- C is A - B.

% recursively removs head from lists until head of middle list == first arg.
remove(H,[H|T],T).
remove(H,[F|T],[F|R]) :- remove(H,T,R).

% put X in L1 if X in L2
place(X,L1,L2) :- remove(X,L2,L1).

% recursively checks if first arg in list by comparing head to 1st arg and passing tail
member(X,[X|_]).
member(X,[_|R]) :- member(X,R).

% recursively combines lists Z and Y
% If x in Z and x not in Y
conjoin([X|T],Z,Y) :-
	member(X,Z),
	conjoin(T,Z,Y).

% If x in w and x not in z
conjoin([X|T],Z,[X|Y]) :-
	\+ member(X,Z),
	conjoin(T,Z,Y).

% if lists are already disjoined
conjoin([],Z,Z).

% true if collision in any of the x_1,y_1 - _3 for cars of SIZE==3
collision(ID,X,Y):- car(ID,X,Y,B,C,D,E,F).
collision(ID,X,Y):- car(ID,B,C,X,Y,D,E,F).
collision(ID,X,Y):- car(ID,B,C,D,E,X,Y,F).

% true if collision in any of the x_1,y_1 - _2 for cars of SIZE==2
collision2(ID,X,Y):- car(ID,X,Y,D,E,1111,1111,2).
collision2(ID,X,Y):- car(ID,B,C,X,Y,1111,1111,2).

% cars of size 3, collision downward
coll270(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(X,X_1),
	subtract(Y_1,Z,TRUE_Y_1),
	equals(Y,TRUE_Y_1).

coll270(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(X,X_1),
	subtract(Y_2,Z,TRUE_Y_2),
	equals(Y,TRUE_Y_2).

coll270(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(X,X_1),
	subtract(Y_3,Z,TRUE_Y_3),
	equals(Y,TRUE_Y_3).

% cars of size 3 collision upward
coll90(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(X,X_1),
	add(Y_1,Z,TRUE_Y_1),
	equals(Y,TRUE_Y_1).

coll90(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(X,X_1),
	add(Y_2,Z,TRUE_Y_2),
	equals(Y,TRUE_Y_2).

coll90(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(X,X_1),
	add(Y_3,Z,TRUE_Y_3),
	equals(Y,TRUE_Y_3).

% cars of size 3 collision to the left
coll180(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(Y,Y_1),
	subtract(X_1,Z,TRUE_X_1),
	equals(X,TRUE_X_1).

coll180(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(Y,Y_1),
	subtract(X_2,Z,TRUE_X_2),
	equals(X,TRUE_X_2).

coll180(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(Y,Y_1),
	subtract(X_3,Z,TRUE_X_3),
	equals(X,TRUE_X_3).

% cars of size 3 collision to the right
coll360(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(Y,Y_1),
	add(X_1,Z,TRUE_X_1),
	equals(X,TRUE_X_1).

coll360(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(Y,Y_1),
	add(X_2,Z,TRUE_X_2),
	equals(X,TRUE_X_2).

coll360(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(Y,Y_1),
	add(X_3,Z,TRUE_X_3),
	equals(X,TRUE_X_3).

% cars of size 2, downward collision
minorColl270(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(X,X_1),
	subtract(Y_1,Z,TRUE_Y_1),
	equals(Y,TRUE_Y_1).

minorColl270(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(X,X_1),
	subtract(Y_2,Z,TRUE_Y_2),
	equals(Y,TRUE_Y_2).

% cars of size 2, upward collision
minorColl90(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(X,X_1),
	add(Y_1,Z,TRUE_Y_1),
	equals(Y,TRUE_Y_1).

minorColl90(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(X,X_1),
	add(Y_2,Z,TRUE_Y_2),
	equals(Y,TRUE_Y_2).

% cars of size 2, collision to the left
minorColl180(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(Y,Y_1),
	subtract(X_1,Z,TRUE_X_1),
	equals(X,TRUE_X_1).

minorColl180(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(Y,Y_1),
	subtract(X_2,Z,TRUE_X_2),
	equals(X,TRUE_X_2).

% cars of size 2, collision to the right
minorColl360(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(Y,Y_1),
	add(X_1,Z,TRUE_X_1),
	equals(X,TRUE_X_1).

minorColl360(ID,X,Y,Z):-
	car(ID,X_1,Y_1,X_2,Y_2,X_3,Y_3,SIZE),
	equals(Y,Y_1),
	add(X_2,Z,TRUE_X_2),
	equals(X,TRUE_X_2).


% covers all scenarios for cars to be in
option1(X,Y,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(H,3), % length == 3
	not(member([A,DIRECTION,SIZE], CurrCarConfig)), % car not in moved before in that direction
	collision(A,X,Y). % collision for car

option2(X,Y,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(H,3), % size == 3
	member([A,DIRECTION,SIZE], CurrCarConfig), % car moved before
	equals(DIRECTION,90), % facing up
	coll270(A,X,Y,SIZE). % collision down

option3(X,Y,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(H,3), % size == 3
	member([A,DIRECTION,SIZE], CurrCarConfig), % car moved
	equals(DIRECTION,270), % facing down
	coll90(A,X,Y,SIZE). % collision on up

option4(X,Y,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(H,3), % size == 3
	member([A,DIRECTION,SIZE], CurrCarConfig), % car moved
	equals(DIRECTION,180), % facing left
	coll180(A,X,Y,SIZE). % collision left

option5(X,Y,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(H,3),
	member([A,DIRECTION,SIZE], CurrCarConfig),
	equals(DIRECTION,360),
	coll360(A,X,Y,SIZE).

option6(X,Y,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(H,2),
	not(member([A,DIRECTION,SIZE], CurrCarConfig)),
	collision2(A,X,Y).

option7(X,Y,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(H,2),
	member([A,DIRECTION,SIZE], CurrCarConfig),
	equals(DIRECTION,90),
	minorColl360(A,X,Y,SIZE).

option8(X,Y,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(H,2),
	member([A,DIRECTION,SIZE], CurrCarConfig),
	equals(DIRECTION,270),
	minorColl180(A,X,Y,SIZE).

option9(X,Y,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(H,2),
	member([A,DIRECTION,SIZE], CurrCarConfig),
	equals(DIRECTION,180),
	minorColl270(A,X,Y,SIZE).

option10(X,Y,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(H,2),
	member([A,DIRECTION,SIZE], CurrCarConfig),
	equals(DIRECTION,270),
	minorColl90(A,X,Y,SIZE).

% Checks if space is occupied by a car
notOccByCar(X,Y,CurrCarConfig) :-
	not(option1(X,Y,CurrCarConfig)),
	not(option2(X,Y,CurrCarConfig)),
	not(option3(X,Y,CurrCarConfig)),
	not(option4(X,Y,CurrCarConfig)),
	not(option5(X,Y,CurrCarConfig)),
	not(option6(X,Y,CurrCarConfig)),
	not(option7(X,Y,CurrCarConfig)),
	not(option8(X,Y,CurrCarConfig)),
	not(option9(X,Y,CurrCarConfig)),
	not(option10(X,Y,CurrCarConfig)).

% space in boundries and isn't car then its free
openCoordinate(X,Y,CurrCarConfig) :-
	not(border(X,Y)),
	notOccByCar(X,Y,CurrCarConfig).

% true if car not in history list
hasntMoved(ID, DIRECTION, SIZE, CurrCarConfig) :- not(member([ID,X,Y], CurrCarConfig)).


% Checking if cars can move horizontally and/or vertically

% Check if car can move right
carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(C,E), % horizontally oriented
	equals(H,3), % size 3
	add(F,1,Q1), % x_3 += 1
	openCoordinate(Q1,C,CurrCarConfig), % space moved to is free
	ID is A, % set id
	DIRECTION is 90, % set direction
	SIZE is 1, % units moved
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig). % car not moved before in that way

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(C,E),
	equals(H,3),
	add(F,1,Q1),
	openCoordinate(Q1,C,CurrCarConfig),
	add(F,2,Q2),
	openCoordinate(Q2,C,CurrCarConfig),
	ID is A,
	DIRECTION is 360,
	SIZE is 2,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(C,E),
	equals(H,3),
	add(F,1,Q1),
	openCoordinate(Q1,C,CurrCarConfig),
	add(F,2,Q2),
	openCoordinate(Q2,C,CurrCarConfig),
	add(F,3,Q3),
	openCoordinate(Q3,C,CurrCarConfig),
	ID is A,
	DIRECTION is 360,
	SIZE is 3,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(C,E),
	equals(H,2),
	add(D,1,Q1),
	openCoordinate(Q1,C,CurrCarConfig),
	ID is A,
	DIRECTION is 360,
	SIZE is 1,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(C,E),
	equals(H,2),
	add(D,1,Q1),
	openCoordinate(Q1,C,CurrCarConfig),
	add(D,2,Q2),
	openCoordinate(Q2,C,CurrCarConfig),
	ID is A,
	DIRECTION is 360,
	SIZE is 2,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(C,E),
	equals(H,2),
	add(D,1,Q1),
	openCoordinate(Q1,C,CurrCarConfig),
	add(D,2,Q2),
	openCoordinate(Q2,C,CurrCarConfig),
	add(D,3,Q1),
	openCoordinate(Q3,C,CurrCarConfig),
	ID is A,
	DIRECTION is 360,
	SIZE is 3,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(C,E),
	equals(H,2),
	add(D,1,Q1),
	openCoordinate(Q1,C,CurrCarConfig),
	add(D,2,Q2),
	openCoordinate(Q2,C,CurrCarConfig),
	add(D,3,Q3),
	openCoordinate(Q3,C,CurrCarConfig),
	add(D,4,Q4),
	openCoordinate(Q4,C,CurrCarConfig),
	ID is A,
	DIRECTION is 360,
	SIZE is 4,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).


% check to see if car can move upward
carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(B,D),
	equals(H,3),
	subtract(C,1,Q1),
	openCoordinate(B,Q1,CurrCarConfig),
	ID is A,
	DIRECTION is 90,
	SIZE is 1,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(B,D),
	equals(H,3),
	subtract(C,1,Q1),
	openCoordinate(B,Q1,CurrCarConfig),
	subtract(C,2,Q2),
	openCoordinate(B,Q2,CurrCarConfig),
	ID is A,
	DIRECTION is 90,
	SIZE is 2,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(B,D),
	equals(H,3),
	subtract(C,1,Q1),
	openCoordinate(B,Q1,CurrCarConfig),
	subtract(C,2,Q2),
	openCoordinate(B,Q2,CurrCarConfig),
	subtract(C,3,Q3),
	openCoordinate(B,Q3,CurrCarConfig),
	ID is A,
	DIRECTION is 90,
	SIZE is 3,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(B,D),
	equals(H,2),
	subtract(C,1,Q1),
	openCoordinate(B,Q1,CurrCarConfig),
  ID is A,
	DIRECTION is 90,
	SIZE is 1,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(B,D),
	equals(H,2),
	subtract(C,1,Q1),
	openCoordinate(B,Q1,CurrCarConfig),
	subtract(C,2,Q2),
	openCoordinate(B,Q2,CurrCarConfig),
  ID is A,
	DIRECTION is 90,
	SIZE is 2,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(B,D),
	equals(H,2),
	subtract(C,1,Q1),
	openCoordinate(B,Q1,CurrCarConfig),
  subtract(C,2,Q2),
	openCoordinate(B,Q2,CurrCarConfig),
	subtract(C,3,Q3),
	openCoordinate(B,Q3,CurrCarConfig),
  ID is A,
	DIRECTION is 90,
	SIZE is 3,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(B,D),
	equals(H,2),
	subtract(C,1,Q1),
	openCoordinate(B,Q1,CurrCarConfig),
  subtract(C,2,Q2),
	openCoordinate(B,Q2,CurrCarConfig),
	subtract(C,3,Q3),
	openCoordinate(B,Q3,CurrCarConfig),
	subtract(C,4,Q4),
	openCoordinate(B,Q4,CurrCarConfig),
  ID is A,
	DIRECTION is 90,
	SIZE is 4,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).


% check to see if car can move leftward
carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(C,E),
	equals(H,3),
	subtract(B,1,Q1),
	openCoordinate(Q1,C,CurrCarConfig),
  ID is A,
	DIRECTION is 180,
	SIZE is 1,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(C,E),
	equals(H,3),
	subtract(B,1,Q1),
	openCoordinate(Q1,C,CurrCarConfig),
  subtract(B,2,Q2),
	openCoordinate(Q2,C,CurrCarConfig),
  ID is A,
	DIRECTION is 180,
	SIZE is 2,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(C,E),
	equals(H,3),
	subtract(B,1,Q1),
	openCoordinate(Q1,C,CurrCarConfig),
  subtract(B,2,Q2),
	openCoordinate(Q2,C,CurrCarConfig),
	subtract(B,3,Q3),
	openCoordinate(Q3,C,CurrCarConfig),
  ID is A,
	DIRECTION is 180,
	SIZE is 3,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(C,E),
	equals(H,2),
	subtract(B,1,Q1),
	openCoordinate(Q1,C,CurrCarConfig),
  ID is A,
	DIRECTION is 180,
	SIZE is 1,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(C,E),
	equals(H,2),
	subtract(B,1,Q1),
	openCoordinate(Q1,C,CurrCarConfig),
  subtract(B,2,Q2),
	openCoordinate(Q2,C,CurrCarConfig),
  ID is A,
	DIRECTION is 180,
	SIZE is 2,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(C,E),
	equals(H,2),
	subtract(B,1,Q1),
	openCoordinate(Q1,C,CurrCarConfig),
  subtract(B,2,Q2),
	openCoordinate(Q2,C,CurrCarConfig),
	subtract(B,3,Q3),
	openCoordinate(Q3,C,CurrCarConfig),
  ID is A,
	DIRECTION is 180,
	SIZE is 3,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(C,E),
	equals(H,2),
	subtract(B,1,Q1),
	openCoordinate(Q1,C,CurrCarConfig),
  subtract(B,2,Q2),
	openCoordinate(Q2,C,CurrCarConfig),
	subtract(B,3,Q3),
	openCoordinate(Q3,C,CurrCarConfig),
	subtract(B,4,Q4),
	openCoordinate(Q3,C,CurrCarConfig),
  ID is A,
	DIRECTION is 180,
	SIZE is 4,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).


% check to see if car can move downward
carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(B,D),
	equals(H,3),
	add(G,1,GM1),
	openCoordinate(B,GM1,CurrCarConfig),
  ID is A,
	DIRECTION is 270,
	SIZE is 1,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(B,D),
	equals(H,3),
	add(G,1,GM1),
	openCoordinate(B,GM1,CurrCarConfig),
	add(G,2,GM2),
	openCoordinate(B,GM2,CurrCarConfig),
  ID is A,
	DIRECTION is 270,
	SIZE is 2,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(B,D),
  equals(H,3),
	add(G,1,GM1),
	openCoordinate(B,GM1,CurrCarConfig),
  subtract(G,2,GM2),
	openCoordinate(B,GM2,CurrCarConfig),
	add(G,3,GM3),
	openCoordinate(B,GM3,CurrCarConfig),
  ID is A,
	DIRECTION is 270,
	SIZE is 3,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(B,D),
	equals(H,2),
	add(E,1,EM1),
	openCoordinate(B,EM1,CurrCarConfig),
  ID is A,
	DIRECTION is 270,
	SIZE is 1,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(B,D),
	equals(H,2),
	add(E,1,EM1),
	openCoordinate(B,EM1,CurrCarConfig),
	add(E,2,EM2),
	openCoordinate(B,EM2,CurrCarConfig),
  ID is A,
	DIRECTION is 270,
	SIZE is 2,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(B,D),
	equals(H,2),
	add(E,1,EM1),
	openCoordinate(B,EM1,CurrCarConfig),
  add(E,2,EM2),
	openCoordinate(B,EM2,CurrCarConfig),
	add(E,3,EM3),
	openCoordinate(B,EM3,CurrCarConfig),
  ID is A,
	DIRECTION is 270,
	SIZE is 3,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig) :-
	car(A,B,C,D,E,F,G,H),
	equals(B,D),
	equals(H,2),
	add(E,1,EM1),
	openCoordinate(B,EM1,CurrCarConfig),
  add(E,2,EM2),
	openCoordinate(B,EM2,CurrCarConfig),
	add(E,3,EM3),
	openCoordinate(B,EM3,CurrCarConfig),
	add(E,4,EM4),
	openCoordinate(B,EM4,CurrCarConfig),
  ID is A,
	DIRECTION is 270,
	SIZE is 4,
	hasntMoved(ID,DIRECTION,SIZE,CurrCarConfig).

% move car up
shift90(X,Y,Z,CurrCarConfig) :-
	subtract(Y,1,Z),
	openCoordinate(X,Z,CurrCarConfig).

% move car left
shift180(X,Y,Z,CurrCarConfig) :-
	subtract(X,1,Z),
	openCoordinate(Z,Y,CurrCarConfig).

% move car down
shift270(X,Y,Z,CurrCarConfig) :-
	add(Y,1,Z),
	openCoordinate(X,Z,CurrCarConfig).

% move car right
shift360(X,Y,Z,CurrCarConfig) :-
	add(X,1,Z),
	openCoordinate(Z,Y,CurrCarConfig).

% prevent backtracking on place
placeIn(X,Y,Z) :- place(X,Y,Z), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Path Finding %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

findGoal(X, Y,RobotMove,AllRobotMoves,CurrCarConfig) :-
	finish(X,Y), % check to see if at destination
	nl, nl, write("Path for robot: "), nl,
	write(RobotMove), nl. % output instructions

findGoal(X, Y, RobotMove,AllRobotMoves,CurrCarConfig) :-
	shift360(X,Y,Z,CurrCarConfig), % move car right
	not(member(Z+l+Y, RobotMove)), % move not made prior
	placeIn(Z+l+Y,RobotMove,NewMove), % add move to list
	keepLooking(Z,Y,NewMove,AllRobotMoves,CurrCarConfig). % keep searching

findGoal(X, Y, RobotMove,AllRobotMoves,CurrCarConfig) :-
	shift270(X,Y,Z,CurrCarConfig),
	not(member(X+l+Z,RobotMove)),
	placeIn(X+l+Z,RobotMove,NewMove),
	keepLooking(X,Z,NewMove,AllRobotMoves,CurrCarConfig).

findGoal(X, Y, RobotMove,AllRobotMoves,CurrCarConfig) :-
	shift90(X,Y,Z,CurrCarConfig),
	not(member(X+l+Z, RobotMove)),
	placeIn(X+l+Z,RobotMove,NewMove),
	keepLooking(X,Z,NewMove,AllRobotMoves,CurrCarConfig).

findGoal(X, Y, RobotMove,AllRobotMoves,CurrCarConfig) :-
	shift180(X,Y,Z,CurrCarConfig),
	not(member(Z+l+Y, RobotMove)),
	placeIn(Z+l+Y,RobotMove,NewMove),
	keepLooking(Z,Y,NewMove,AllRobotMoves,CurrCarConfig) .

% updates list and continues looking for exit
keepLooking(X,Y,RobotMove,AllRobotMoves,CurrCarConfig) :-
	not(member(RobotMove,AllRobotMoves)),
	place(RobotMove,AllRobotMoves,NEW_NewMove), !,
	findGoal(X,Y,RobotMove,NEW_NewMove,CurrCarConfig).

% true if path can be found, false otherwise
findPath(X,Y,RobotMove,AllRobotMoves,CurrCarConfig):- findGoal(X,Y,RobotMove,AllRobotMoves,CurrCarConfig).

% initializes the rush hour game with empty lists
begin() :- game([],[]).

% checks to see that move wasnt made prior
notRepeated(NewMove,AllMoves) :-
	not(member(NewMove,AllMoves)), % move not made
	placeIn(NewMove,AllMoves,ALL_NewMove), !, % add move to new list
	game(NewMove,ALL_NewMove). % keep playing with updated lists

% no cars
empty :-
	not(car(1,X,Y,Z,A,B,C,R)), % no car 1, then no cars...
	write("Robot path: straight 6 spaces"), nl.

game(CurrCarConfig, PastCarConfigs):-
	findPath(1,3,[1+l+3],[], CurrCarConfig), % true if path found from findGoal
	nl, nl, write("Path for Cars: "), nl,
	write(CurrCarConfig).

game(CurrCarConfig, PastCarConfigs) :-
	write("Searching for path.... "), nl,
	carAbleToMove(ID,DIRECTION,SIZE,CurrCarConfig), % car can move in direction
	placeIn([ID,DIRECTION,SIZE], CurrCarConfig, NewMove), % add move to history
	notRepeated(NewMove,PastCarConfigs). % if lists aren't redundant
