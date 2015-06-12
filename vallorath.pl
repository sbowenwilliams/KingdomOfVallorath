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

object(sword_and_shield).
object(bow_and_arrow).
object(staff).
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
object(bar_of_gold).
object(meat).
object(pendant).
object(lockpick).
object(potion).

starts_with(class(warrior), object(sword_and_shield)).
starts_with(class(mage), object(staff)).
starts_with(class(rogue), object(dagger)).
starts_with(class(ranger), object(bow_and_arrow)).
starts_with(class(bard), object(lute)).

encounter(easy).
encounter(medium).
encounter(hard).
encounter(legendary).
encounter(boss).

enemy(undead_knight, 'prioritizes warrior').
enemy(dragon, 'all damage is done to entire group').
enemy(malicious_townsfolk, 'prioritizes bard').
enemy(thieves, 'steal: if enemy rolls crit item is removed from player inventory at gm discretion').
enemy(ghosts, 'prioritizes mage').
enemy(demon 'prioritizes strongest').
enemy(harpy, 'prioritizes rangers').
enemy(hound, 'prioritizes bards').
enemy(witch, 'prioritizes lowest health').
enemy(goblin, 'prioritizes randomly').
enemy(troll, 'prioritizes closest player').
enemy(necromancer, 'occasionally summons 1 vitality easy skeleton, prioritizes mages').
enemy(unfriendly_elves, 'attacks from distance as archers, prioritizes closest target').

backstory(noble_founding).
backstory(noble_fallen).
backstory(beggar_bethrothed).
backstory(beggar_orphan).
backstory(farmer_no_more).
backstory(tradesman_no_more).
backstory(reformed_criminal).
backstory(rogue_mage).
backstory(naive_mage).
backstory(former_soldier).
backstory(boring).

quest('For the last 100 years, the citizens of Vallorath have enjoyed prosperous times. That is, until King Bradley died suddenly, and his brother claimed the crown. With immediate steep raises in taxes and harsh punishments for disobedience, the new King, Lester, does not have many friends among the common folk. Your party has been hired to save the kingdom from ruin. Starting location: small town to the West of the castle.').
quest('The scholars at the Mages College have noticed a sudden and unexplained decrease in their powers. Your party has been tasked with searching for any magical anomalies and fixing them before they can be exploited by those with less-than-friendly intentions. Starting location: Mages College.').
quest('Recent bandit activity in Vallorath has reached an all-time high. The biggest bandit factions have joined forces under the name The People’s Army. The high king of Vallorath has hired your party to infiltrate and crush the group in its infancy, before it turns into a full-blown rebellion. The bandit camp is located in the Northern-most region of the Kingdom. Starting location: Outside the capitol city.').
quest('A wealthy merchant has hired your party to kill someone interrupting his trade routes and stealing his merchandise. Little is known about this thief, though he is rumored to reside in a vast cave system in the East of the Kingdom. Starting location: merchant’s house, capitol city.').
quest('Legends tell of the great pirate Mineer and his buried treasure. Thought by most to only be children’s tales, a map has recently been unearthed that seems to lead the way to the treasure, containing unfathomable wealth and items. Your party is racing to find it before anyone else can. Starting location: capitol city.').