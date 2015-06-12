%% Sean Bowen-Williams, Brian Tang, Asher Rieck, Aaron Karp

%% :- export((need/1, relationship/3, location/1, object/1,
%% 	   relation/1, roles_relation/1, symmetric/1,
%% 	   implies/2, conflicting_roles/2, generalization/2,
%% 	   contradiction/2)).

:- export((class/1, race/1, conflicting_roles/2, need/1, object/1, location/1)).

class(warrior).
class(mage).
class(rouge).
class(ranger).
class(bard).

race(human).
race(elven).
race(dwarven).
race(orcish).
race(gnomish).


%% symmetric(rival_coworkers).
%% implies(relationship(X, rival_coworkers, _),
%% 	role(X, casino_employee)).
%% implies(relationship(_, rival_coworkers, Y),
%% 	role(Y, casino_employee)).
	
%% symmetric(rival_managers).
%% implies(relationship(X, rival_managers, _),
%% 	role(X, floor_manager)).
%% implies(relationship(_, rival_managers, Y),
%% 	role(Y, floor_manager)).

%% symmetric(rival_gang_leaders).
%% implies(relationship(X, rival_gang_leaders, _),
%% 	role(X, gang_leader)).
%% implies(relationship(_, rival_gang_leaders, Y),
%% 	role(Y, gang_leader)).

%% symmetric(rival_drivers).
%% implies(relationship(X, rival_drivers, _),
%% 	role(X, driver)).
%% implies(relationship(_, rival_drivers, Y),
%% 	role(Y, driver)).

%% symmetric(past_cell_mates).
%% implies(relationship(X, past_cell_mates, _),
%% 	role(X, thug)).
%% implies(relationship(_, past_cell_mates, Y),
%% 	role(Y, thug)).

%% generalizes(sibling,family).
%% generalizes(thug,gang_member).
%% generalizes(gang_leader,gang_member).
%% generalizes(casino_employee,casino_worker).
%% generalizes(floor_manager,casino_worker).
%% generalizes(politician,polical_actor).
%% generalizes(staffer,polical_actor).
%% generalizes(dirty_cop, gang_member).

conflicting_roles(hotblooded,sheepish).
conflicting_roles(patient,impulsive).
conflicting_roles(timid,adventurous).
conflicting_roles(agreeable,disagreeable).
conflicting_roles(friendly,unfriendly).
conflicting_roles(hardy,prissy).
conflicting_roles(lonely,ladykiller).
conflicting_roles(selfish,selfless).
conflicting_roles(relaxed,uptight).
conflicting_roles(bold,reserved).
conflicting_roles(serious,sassy).
conflicting_roles(jolly,morose).
conflicting_roles(naive, worldly).
conflicting_roles(addict, pious).
conflicting_roles(clumsy, deft).
conflicting_roles(graceful, tactless).
conflicting_roles(quiet, loud).
conflicting_roles(modest, arrogant).
conflicting_roles(naughty, nice).
conflicting_roles(kinky, vanilla).
conflicting_roles(gentle, rough).


contradiction(relationship(X, robber/heist_leader, Y),
	      relationship(X, robber/heist_leader, Z)) :-
	Y \= Z.

contradiction(relationship(X, thug/gang_leader, Y),
	      relationship(X, thug/gang_leader, Z)) :-
	Y \= Z.

contradiction(relationship(X, target/hitman, Y),
	      relationship(X, target/hitman, Z)) :-
	Y \= Z.

contradiction(relationship(X, staffer/politician, Y),
	      relationship(X, staffer/politician, Z)) :-
	Y \= Z.
	
contradiction(relationship(X, casino_employee/floor_manager, Y),
	      relationship(X, casino_employee/floor_manager, Z)) :-
	Y \= Z.

need(become_a_hero).
need(revenge).
need(honor_family).
need(money).
need(mission_from_god).
implies(need(C, mission_from_god),
	\+class(C, rogue)).
need(escape_the_law).
implies(need(C, escape_from_law),
	class(C, rogue)).
need(to_prove_their_love).
need(protect_their_home).
need(seek_knowledge).
implies(need(C, seek_knowledge),
	class(C, mage)).
need(prove_themselves).

object(cloak).
object(helm).
object(tent).
object(torch).
object(rope).
object(flaggon).

location(swamp).
location(volcano).
location(castle).
location(cave).

	
%% location(drunk_tank).
%% implies(at(C, drunk_tank),
%% 	\+role(C,politician)).
%% implies(at(C, drunk_tank),
%% 	\+role(C, good_cop)).

%% location(watching_bonneville_salt_flats).

%% location(drag_racing_on_bonneville_salt_flats).
%% implies(at(C, drag_racing_on_bonneville_salt_flats),
%% 	role(C, driver)).
	
%% location(in_and_out).
%% location(airport).
%% location(wathing_a_magic_show).
%% location(fabulous_las_vegas_sign).
