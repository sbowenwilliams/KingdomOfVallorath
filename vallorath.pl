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
conflicting_roles(hotblooded, patient).
conflicting_roles(hotblooded, timid).
conflicting_roles(hotblooded, relaxed).
conflicting_roles(sheepish, bold).
conflicting_roles(sheepish, loud).
conflicting_roles(patient,impulsive).
conflicting_roles(timid,adventurous).
conflicting_roles(timid, bold).
conflicting_roles(timid, loud).
conflicting_roles(adventurous, reserved).
conflicting_roles(agreeable,disagreeable).
conflicting_roles(agreeable, unfriendly).
conflicting_roles(agreeable, tactless).
conflicting_roles(disagreeable, friendly).
conflicting_roles(disagreeable, nice).
conflicting_roles(friendly,unfriendly).
conflicting_roles(friendly, selfish).
conflicting_roles(friendly, arrogant).
conflicting_roles(unfriendly, selfless).
conflicting_roles(unfriendly, jolly).
conflicting_roles(unfriendly, nice).
conflicting_roles(hardy,prissy).
conflicting_roles(hardy, naive).
conflicting_roles(prissy, relaxed).
conflicting_roles(prissy, jolly).
conflicting_roles(prissy, naughty).
conflicting_roles(lonely,suave).
conflicting_roles(lonely, bold).
conflicting_roles(lonely, jolly).
conflicting_roles(suave, uptight).
conflicting_roles(suave, naive).
conflicting_roles(suave, clumsy).
conflicting_roles(suave, tactless).
conflicting_roles(selfish,selfless).
conflicting_roles(selfish, pious).
conflicting_roles(selfish, modest).
conflicting_roles(selfish, nice).
conflicting_roles(selfless, addict).
conflicting_roles(selfless, arrogant).
conflicting_roles(selfless, naughty).
conflicting_roles(relaxed,uptight).
conflicting_roles(relaxed, serious).
conflicting_roles(uptight, jolly).
conflicting_roles(uptight, naughty).
conflicting_roles(bold,reserved).
conflicting_roles(bold, quiet).
conflicting_roles(reserved, loud).
conflicting_roles(serious,sassy).
conflicting_roles(serious, jolly).
conflicting_roles(sassy, pious).
conflicting_roles(jolly,morose).
conflicting_roles(jolly, quiet).
conflicting_roles(morose, loud).
conflicting_roles(naive, worldly).
conflicting_roles(worldly, tactless).
conflicting_roles(worldly, arrogant).
conflicting_roles(addict, pious).
conflicting_roles(addict, graceful).
conflicting_roles(addict, nice).
conflicting_roles(pious, naughty).
conflicting_roles(clumsy, deft).
conflicting_roles(clumsy, graceful).
conflicting_roles(deft, tactless).
conflicting_roles(deft, loud).
conflicting_roles(graceful, tactless).
conflicting_roles(graceful, loud).
conflicting_roles(graceful, naughty).
conflicting_roles(quiet, loud).
conflicting_roles(modest, arrogant).
conflicting_roles(arrogant, nice).
conflicting_roles(naughty, nice).


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

object('sword of +1 attack').
object('shield of +2 damage reduction').
object('bow of +1 attack').
object('staff of +1 attack').
object('cloack of +1 damge reduction').
object('helm of +1 damage reduction').
object('tent of +10 vitality (for all party members)').
object('torch').
object('rope - 30 feet').
object('flaggon of -1 knowledge, +1 luck (on use)').
object('lute of +1 luck').
object('dagger of +1 attack').
object('bar of gold').
object('meat of +5 vitality (on use)').
object('pendant of +2 knowledge').
object('lockpick').
object('potion of + 20 vitality (on use)').
object('potion of + 5 attack (on use)').
object('potion of + 5 luck (on use)').
object('tome of +2 knowledge').

starts_with(class(warrior), object('sword of +1 attack')).
starts_with(class(mage), object('staff of +1 attack')).
starts_with(class(rogue), object('dagger of +1 attack')).
starts_with(class(ranger), object('bow of +1 attack')).
starts_with(class(bard), object('lute of +1 luck')).

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