%
% Random Kingdom of Vallorath setup generator
% To use this:
%  - Open this file and the file with your playset in the editor
%  - Do compile buffer on this file, and then your playset.
%  - Run the setup predicate with the names of your characters as
%    arguments

KINGDOM OF VALLORATH Usage Guide

1) Read the game guide and game master guides
2) Compile buffer the Solver.pl file, followd by the vallorath.pl file
3) In SWI-Prolog use the setup command with between three and four names:
	setup(name1, name2, name3).
	setup(name1, name2, name3).

Useful Commands for Game Master:
Rolls a four sided die:       rolld4. 
Rolls a six sided die:        rolld6.
Rolls a twenty sided die:     rolld20.

Generates encounter:		  setup_encounter(difficulty).