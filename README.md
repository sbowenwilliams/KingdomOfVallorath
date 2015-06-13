KINGDOM OF VALLORATH Usage Guide

1) Read the game guide and game master guides
2) Compile buffer the Solver.pl file, followd by the vallorath.pl file
3) In SWI-Prolog use the setup command with between three and four names:
	setup(name1, name2, name3).
	setup(name1, name2, name3, name4).

Useful Commands for Game Master:
Rolls a four sided die:			rolld4. 
Rolls a six sided die:			rolld6.
Rolls a twenty sided die:		rolld20.

Generates encounter:			setup_encounter(difficulty).
									(either easy, medium, hard, legendary, or boss)

Rolls for player damage:		roll_warrior.
								roll_ranger.
								roll_thief
								roll_mage
								roll_bard

Rolls for enemy damage:			roll_difficulty.
	Run once for each enemy still alive

Drops one random piece of loot:	drop_loot.