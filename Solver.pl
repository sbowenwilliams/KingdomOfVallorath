%
% Random Fiasco setup generator
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
	make_setup([relationship(A, _, B),
		    relationship(B, _, C),
		    relationship(C, _, A),
		    detail(A, _, _),
		    detail(B, _, _),
		    detail(C, _, _)]).

setup(A, B, C, D) :-
	make_setup([
		    detail(A, needs, _),
		    detail(A, at, _),
		    detail(A, is, _),
		    detail(A, has, _),

		    detail(B, _, _),
		    detail(C, _, _),
		    detail(D, _, _)]).

setup(A, B, C, D, E) :-
	make_setup([relationship(A, _, B),
		    relationship(B, _, C),
		    relationship(C, _, D),
		    relationship(D, _, E),
		    relationship(E, _, A),
		    detail(A, _, _),
		    detail(B, _, _),
		    detail(C, _, _),
		    detail(D, _, _),
		    detail(E, _, _)]).

setup(A, B, C, D, E, F) :-
	make_setup([relationship(A, _, B),
		    relationship(B, _, C),
		    relationship(C, _, D),
		    relationship(D, _, E),
		    relationship(E, _, F),
		    relationship(F, _, A),
		    detail(A, _, _),
		    detail(B, _, _),
		    detail(C, _, _),
		    detail(D, _, _),
		    detail(E, _, _),
		    detail(F, _, _)]).

%
% Facts and implications
%

% The setup generator generates objects, locations, and needs as
% "details".  But they imply the needs, has, and at facts.
detail(_Character, needs, Need) :-
	need(Need).
detail(_Character, at, X) :-
	location(X).
detail(_Character, has, X) :-
	object(X).
detail(_Character, is, X):-
	class(X).

implies(detail(Character, needs, N),
	needs(Character, N)).
implies(detail(Character, has, Object),
	has(Character, Object)).
implies(detail(Character, at, Location),
	at(Character, Location)).
implies(detail(Character, is, Class),
	is(Character, Class)).

% You can have a relationship between two characters if Relation is
% a kind of relation
relationship(_, Relation, _) :-
	relation(Relation).

% If R1 is a special kind of R2, then infer R2 from R1
implies(relationship(X, R1, Y),
	relationship(X, R2, Y)) :-
	generalization(R1, R2).

% If R is a symmetric relation, then you can swap the characters
relation(R) :- symmetric(R).
implies(relationship(X, Relation, Y),
	relationship(Y, Relation, X)) :-
	symmetric(Relation).

%
% Role relations
%

% A role relation can be either X/Y or Y/X
relation(X/Y) :-
	roles_relation(X/Y) ; roles_relation(Y/X).

% A role relationship XRole/YRole implies the X role for the first
% character, and the YRole for the second character
implies(relationship(X, XRole/_, _),
	role(X, XRole)).
implies(relationship(_, _/YRole, Y),
	role(Y, YRole)).

% A character can't have conflicting roles
contradiction(role(X, Y), role(X, Z)) :-
	conflicting_roles(Y, Z) ; conflicting_roles(Z, Y).

% A character can need N if N is a kind of need
needs(_C, N) :-
	need(N).

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
	writeln('Inferred facts:'),
	forall(member(Fact, DB),
	       writeln(Fact)),
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




