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

stat(attack).
stat(knowledge).
stat(luck).

attribute(hotblooded).
attribute(sheepish).
attribute(patient).
attribute(impulsive).
attribute(timid).
attribute(adventurous).
attribute(agreeable).
attribute(disagreeable).
attribute(friendly).
attribute(unfriendly).
attribute(hardy).
attribute(prissy).
attribute(lonely).
attribute(suave).
attribute(selfish).
attribute(selfless).
attribute(relaxed).
attribute(uptight).
attribute(bold).
attribute(reserved).
attribute(serious).
attribute(sassy).
attribute(jolly).
attribute(morose).
attribute(naive).
attribute(worldly).
attribute(addict).
attribute(pious).
attribute(clumsy).
attribute(deft).
attribute(graceful).
attribute(tactless).
attribute(quiet).
attribute(loud).
attribute(modest).
attribute(modest).
attribute(arrogant).
attribute(naughty).
attribute(nice).

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
object(sword).
object(shield).
object(bow).
object(arrows).
object(lute).
object(dagger).
object(staff).

implies(class(warrior), object(sword), object(shield)).
implied(class(mage), object(staff)).
implies(class(rouge), object(dagger)).
implies(class(ranger), object(bow), object(arrows)).
implies(class(bard), object(lute)).

location(swamp).
location(volcano).
location(castle).
location(cave).
location(yourmumsbutt).

enemy(ghoul).
enemy(undead_knight).
enemy(dragon).
enemy(malicious_townsfolk).
enemy(thieves).
