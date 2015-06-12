%
% Random Kingdom of Vallorath setup generator
% To use this:
%  - Open this file and the file with your playset in the editor
%  - Do compile buffer on this file, and then your playset.
%  - Run the setup predicate with the names of your characters as
%    arguments
%

:- use_module(library(random)).
:- export((setup/3, setup/4, setup/5, setup/6,
	   detail/3, needs/2, relationship/3,
	   need/1, relation/1, object/1, location/1,
	   symmetric/1, role_relation/1,
	   implies/2, contradiction/2, conflicting_roles/2)).
:- dynamic need/1, object/1, location/1,
	   symmetric/1, roles_relation/1,
	   contradiction/2, implies/2, generalization/2, conflicting_roles/2.
:- discontiguous need/1, object/1, location/1,
	         relation/1, symmetric/1, roles_relation/1,
		 contradiction/2, implies/2, generalization/2, conflicting_roles/2.
:- multifile need/1, object/1, location/1,
	     relation/1, symmetric/1, roles_relation/1,
	     contradiction/2, implies/2, generalization/2, conflicting_roles/2.

%% Call these with the names of your characters to make a random setup.
setup(A, B, C) :-
	make_setup([
		detail(A, is, _, _C1),
		detail(_C1, starts_with, _),
	    detail(A, needs, _),
	    detail(A, has, _),
	    role(A, _, _),
	    stat(A, has, _, attack),
	    stat(A, has, _, knowledge),
	    stat(A, has, _, luck),
	    backstory(A, is_from, _),

		detail(B, is, _, _C2),
		detail(_C2, starts_with, _),
	    detail(B, needs, _),
	    detail(B, has, _),
	    role(B, _, _),
	    stat(B, has, _, attack),
	    stat(B, has, _, knowledge),
	    stat(B, has, _, luck),
	    backstory(B, is_from, _),


	    detail(C, is, _, _C3),
	    detail(_C3, starts_with, _),
	    detail(C, needs, _),
	    detail(C, has, _),
	    role(C, _, _),
	    stat(C, has, _, attack),
	    stat(C, has, _, knowledge),
	    stat(C, has, _, luck),
	    backstory(C, is_from, _)
	    ]).

setup(A, B, C, D) :-
	make_setup([
		detail(A, is, _, _C1),
		detail(_C1, starts_with, _),
	    detail(A, needs, _),
	    detail(A, has, _),
	    role(A, _, _),
	    stat(A, has, _, attack),
	    stat(A, has, _, knowledge),
	    stat(A, has, _, luck),
	    backstory(A, is_from, _),

		detail(B, is, _, _C2),
		detail(_C2, starts_with, _),
	    detail(B, needs, _),
	    detail(B, has, _),
	    role(B, _, _),
	    stat(B, has, _, attack),
	    stat(B, has, _, knowledge),
	    stat(B, has, _, luck),
	    backstory(B, is_from, _),

	    detail(C, is, _, _C3),
	    detail(_C3, starts_with, _),
	    detail(C, needs, _),
	    detail(C, has, _),
	    role(C, _, _),
	    stat(C, has, _, attack),
	    stat(C, has, _, knowledge),
	    stat(C, has, _, luck),
	    backstory(C, is_from, _),

	    detail(D, is, _, _C4),
	    detail(_C4, starts_with, _),
	    detail(D, needs, _),
	    detail(D, has, _),
	    role(D, _, _),
	    stat(D, has, _, attack),
	    stat(D, has, _, knowledge),
	    stat(D, has, _, luck),
	    backstory(D, is_from, _)
	    ]).
% Facts and implications
%

% The setup generator generates objects, locations, and needs as
% "details".  But they imply the needs, has, and at facts.
detail(_Character, needs, Need) :-
	need(Need).
detail(_Character, at, X) :-
	location(X).
detail(_Character, starts_with, X):-
	holding(_Character, X).
detail(_Character, has, X) :-
	object(X).
detail(_Character, is, X, Y):-
	race(X), class(Y).

implies(detail(Character, needs, N),
	needs(Character, N)).
implies(detail(Character, has, Object),
	has(Character, Object)).
implies(detail(Character, at, Location),
	at(Character, Location)).
implies(detail(Character, is, Race, Class),
	is(Character, Class), is(Character, Race)).

role(Character, X, Y) :-
	attribute(X),
	attribute(Y),
	\+conflicting_roles(X, Y),
	\+conflicting_roles(Y, X),
	attribute(X) \= attribute(Y).

stat(Character, has, Level, attack):-
	between(0,10, Level).
stat(Character, has, Level, knowledge):-
	between(0,10, Level).
stat(Character, has, Level, luck):-
	between(0,10, Level).

backstory(Character, is_from, Story):-
	backstory(Story).

holding(_C, N):-
	starts_with(class(_C), object(N)).

% A character can need N if N is a kind of need
needs(_C, N) :-
	need(N).

setup_encounter(Difficulty):-
	make_encounter([
		num_enemies(Difficulty, _),
		enemy_vitality(Difficulty, _),
		enemy_type(_, _)
		]).

num_enemies(easy, Number):-
	between(2,2, Number).
num_enemies(medium, Number):-
	between(3,4, Number).
num_enemies(hard, Number):-
	between(5,5, Number).
num_enemies(boss, Number):-
	between(1,1, Number).
num_enemies(legendary, Number):-
	between(5,6, Number).

enemy_vitality(easy, Vitality):-
	between(10,20,Vitality).
enemy_vitality(medium, Vitality):-
	between(20,35,Vitality).
enemy_vitality(hard, Vitality):-
	between(35,55,Vitality).
enemy_vitality(boss, Vitality):-
	between(80,120,Vitality).
enemy_vitality(legendary, Vitality):-
	between(55,75, Vitality).

enemy_type(Enemy, Description):-
	enemy(Enemy, Description).
%
% Solver code
%

% Find a valid setup and print it
make_setup(Setup) :-
	choose(Setup, DB),
	writeln('Setup:'),
	forall(member(Fact, Setup),
	       writeln(Fact)),
	nl,
	writeln('Type any character followed by a period to begin your adventure!'),
	read(_),
	random_solution(quest(X)),
	writeln(X),
	%% writeln('Inferred facts:'),
	%% forall(member(Fact, DB),
	%%        writeln(Fact)),
	!.

make_encounter(Setup) :-
	choose(Setup, DB),
	writeln('Encounter:'),
	forall(member(Fact, Setup),
		writeln(Fact)),
	nl,

	writeln('Drops on Victory:'),
	random_solution(object(Object)),
	writeln(Object),
	random_solution(object(Object2)),
	writeln(Object2),
	!.

% Find a valid setup
choose(GoalList, Database) :-
	add_random_solutions(GoalList, [], Database).

%% random_solution(:P)
% Enumerates solutions to P in random order.
random_solution(P) :-
	% Find all solutions to P, put them in List
	setof(P, P, List),
	% Shuffle all the elements of list
	random_permutation(List, Shuffled),
	% Pick one
	member(P, Shuffled).

%% add_random_solutions(+GoalList, +ExistingFacts, -AllFacts)
% Find solutions to all the goals in GoalList. Solutions must be
% consistent with ExistingFacts and with one another. Then unifies
% AllFacts with ExistingFacts, plus the solutions to GoalList and all
% facts that are implied by them.
add_random_solutions([], Facts, Facts).
add_random_solutions([First | Rest], Facts, MoreFacts) :-
	add_random_solution(First, Facts, F),
	add_random_solutions(Rest, F, MoreFacts).

%% add_random_solution(:Goal, +ExistingFacts, -AllFacts)
% Find a solution to Goal that's consistent with ExistingFacts.  Unifies
% AllFacts with list of ExistingFacts, the Goal solution, and all its
% implications.
add_random_solution(P, OldFacts, NewFacts) :-
	random_solution(P),
	assertion(ground(P)),
	\+ member(P, OldFacts),
	add_fact(P, OldFacts, NewFacts).

%% add_fact(+Fact, +ExistingFacts, -AllFacts)
% Unifies AllFacts with list of ExistingFacts, Fact, and all Fact's
% implications. Fails if Fact or any of its implications are
% incompatible with any of ExistingFacts.
add_fact(Fact, OldFacts, OldFacts) :-
	assertion(ground(Fact)),
	memberchk(Fact,OldFacts),
	!.
add_fact(Fact, OldFacts, NewFacts) :-
	assertion(ground(Fact)),
	forall(member(Old, OldFacts),
	       compatible(Fact, Old)),
	findall(Implication, implies(Fact, Implication), Implications),
	add_fact_list(Implications, [Fact | OldFacts], NewFacts).

%% add_fact_list(+FactList, +ExistingFacts, -AllFacts)
% Unifies AllFacts with a list of ExistingFacts, FactList,
% and all the implications of FactList. Fails if these facts are
% contradictory
add_fact_list([], Facts, Facts).
add_fact_list([Head | Tail], Old, New) :-
	add_fact(Head, Old, T),
	add_fact_list(Tail, T, New).

%% compatible(+P, +Q)
% True if P and Q do not contradict one another. Uses contradiction/2 to
% check for compatibilty.
compatible(P, Q) :-
	\+ contradiction(P, Q),
	\+ contradiction(Q, P).

rolld20:-
	random_solution(between(1,20, Number)),writeln(Number),!.
rolld4:-
	random_solution(between(1,4, Number)),writeln(Number),!.
rolld6:-
	random_solution(between(1,6, Number)),writeln(Number),!.






