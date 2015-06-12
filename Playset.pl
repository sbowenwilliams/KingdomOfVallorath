%
% Example Fiasco playset: DC politics
% This is a simple, short one that I threw together in an hour or so.
% Yours should be more
%

% This keeps the editor from displaying the names of these relations in
% red.
:- export((need/1, relationship/3, location/1, object/1,
	   relation/1, roles_relation/1, symmetric/1,
	   implies/2, conflicting_roles/2, generalization/2,
	   contradiction/2)).

%
% Relationships
%

roles_relation(politician/lobbyist).
symmetric(political_rivals).
implies(relationship(X, political_rivals, _),
	role(X, politician)).
implies(relationship(_, political_rivals, Y),
	role(Y, politician)).
roles_relation(politician/strategist).
roles_relation(politician/estranged_child).
symmetric(old_flames).
roles_relation(journalist/politician).
roles_relation(politician/billionaire).
roles_relation(politician/staffer).

conflicting_roles(politician, lobbyist).
conflicting_roles(politician, strategist).
conflicting_roles(strategist, lobbyist).
conflicting_roles(journalist, politician).
conflicting_roles(journalist, strategist).
conflicting_roles(journalist, lobbyist).
conflicting_roles(billionaire, politician).
conflicting_roles(billionaire, strategist).
conflicting_roles(billionaire, lobbyist).
conflicting_roles(billionaire, journalist).
conflicting_roles(staffer, politician).
conflicting_roles(staffer, strategist).
conflicting_roles(staffer, lobbyist).
conflicting_roles(staffer, journalist).
conflicting_roles(staffer, billionaire).

% A strategist can only work for one politician
contradiction(relationship(X, strategist/politician, Y),
	      relationship(X, strategist/politician, Z)) :-
	Y \= Z.

% A staffer can only work for one politician
contradiction(relationship(X, staffer/politician, Y),
	      relationship(X, staffer/politician, Z)) :-
	Y \= Z.

%
% Needs
%
need(hide_my_addiction_to(X)) :-
	drug(X).
drug(meth).
drug(crack).
drug(bath_salts).
drug(human_blood).

need(kleptomaniac).
need(streaking).

need(make_my_parent_suffer).
implies(needs(C, make_my_parent_suffer),
	role(C, estranged_child)).

need(retire_with_a_cushy_wall_street_job).
implies(needs(C, retire_with_a_cushy_wall_street_job),
	role(C, politician)).
need(become_the_first_tourettes_patient_to_be_elected_president).
implies(needs(C, become_the_first_tourettes_patient_to_be_elected_president),
	role(C, politician)).

need(get_the_big_scoop).
implies(needs(C, get_the_big_scoop),
	role(C, journalist)).

need(make_50_grand_before_tuesday).
contradiction(needs(C, make_50_grand_before_tuesday),
	      role(C, billionaire)).

need(hide_the_body).

%
% Locations
%

location(the_capitol_building).
location(a_corn_farm_in_iowa).
location(a_fact_finding_tour_in_the_bahmamas).
implies(at(C, a_fact_finding_tour_in_the_bahamas),
	role(C, politician)).
location(the_oval_office).
location(a_dc_pickup_bar).
location(the_watergate_hotel).
location(the_david_letterman_set).
location(the_police_station).

%
% Objects
%

object(ronald_reagans_ouija_board).
object(an_experimental_truth_serum).
object(half_a_kilogram_of_heroin).
object(the_nuclear_football).

