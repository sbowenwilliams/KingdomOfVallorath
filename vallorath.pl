%% Sean Bowen-Williams, Brian Tang, Asher Rieck, Aaron Karp

:- export((need/1, relationship/3, location/1, object/1,
	   relation/1, roles_relation/1, symmetric/1,
	   implies/2, conflicting_roles/2, generalization/2,
	   contradiction/2)).

symmetric(sibling).
symmetric(associate).
symmetric(rival).
symmetric(friends).
symmetric(fling).
symmetric(lover).
symmetric(partners_in_crime).

relation(traitor).

roles_relation(hitman/target).
roles_relation(gang_leader/thug).
roles_relation(floor_manager/casino_employee).
roles_relation(heist_leader/robber).
roles_relation(politician/staffer).
roles_relation(stalker/stalkee).
roles_relation(dirty_cop/clean_cop).
roles_relation(drug_dealer/drug_user).
roles_relation(black_mailer/politician).

symmetric(rival_coworkers).
implies(relationship(X, rival_coworkers, _),
	role(X, casino_employee)).
implies(relationship(_, rival_coworkers, Y),
	role(Y, casino_employee)).
	
symmetric(rival_managers).
implies(relationship(X, rival_managers, _),
	role(X, floor_manager)).
implies(relationship(_, rival_managers, Y),
	role(Y, floor_manager)).

symmetric(rival_gang_leaders).
implies(relationship(X, rival_gang_leaders, _),
	role(X, gang_leader)).
implies(relationship(_, rival_gang_leaders, Y),
	role(Y, gang_leader)).

symmetric(rival_drivers).
implies(relationship(X, rival_drivers, _),
	role(X, driver)).
implies(relationship(_, rival_drivers, Y),
	role(Y, driver)).

symmetric(past_cell_mates).
implies(relationship(X, past_cell_mates, _),
	role(X, thug)).
implies(relationship(_, past_cell_mates, Y),
	role(Y, thug)).

generalizes(sibling,family).
generalizes(thug,gang_member).
generalizes(gang_leader,gang_member).
generalizes(casino_employee,casino_worker).
generalizes(floor_manager,casino_worker).
generalizes(politician,polical_actor).
generalizes(staffer,polical_actor).
generalizes(dirty_cop, gang_member).

conflicting_roles(staffer,floor_manager).
conflicting_roles(staffer,casino_employee).
conflicting_roles(politician,floor_manager).
conflicting_roles(politician,thug).
conflicting_roles(politician,casino_employee).
conflicting_roles(gang_leader,casino_employee).
conflicting_roles(driver,thug).
conflicting_roles(sibling,friend).
conflicting_roles(heist_leader,thug).
conflicting_roles(heist_leader,casino_employee).
conflicting_roles(heist_leader,floor_manager).
conflicting_roles(heist_leader,staffer).
conflicting_roles(stalker, stalkee).
conflicting_roles(heist_leader, traitor).
conflicting_roles(drug_dealer, drug_user).
conflicting_roles(politician, black_mailer).
conflicting_roles(politician, drug_dealer).
conflicting_roles(politician, good_cop).
conflicting_roles(politician, bad_cop).
conflicting_roles(politician, hitman).
conflicting_roles(good_cop, drug_user).
conflicting_roles(good_cop, drug_dealer).
conflicting_roles(good_cop, hitman).
conflicting_roles(good_cop, thug).
conflicting_roles(good_cop, heist_leader).
conflicting_roles(good_cop, politician).
conflicting_roles(staffer, politician).




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
	
need(a_million_dollars_yesterday).
need(to_pay_off_gambling_debts).
need(to_relax).
need(to_get_payback).
need(get_outa_dodge).
need(to_prove_their_love).

need(just_to_win_big).
implies(needs(C, just_to_win_big),
	\+role(C, floor_manager)).
	
need(the_thrill_of_a_heist).
need(buy_like_five_islands).
need(get_my_car_back).
implies(needs(C, get_my_car_back),
	role(C, driver)).
need(find_the_thrill_of_love).
need(prove_ones_worth).

need(make_it_big_in_silicon_valley).
implies(needs(C, make_it_big_in_silicon_valley),
	\+role(C, politician)).

need(buy_drugs_and_lots_more_drugs).
implies(needs(C, buy_drugs_and_lots_more_drugs),
	role(C, drug_user)).

need(get_back_at_the_boss).
implies(needs(C, get_back_at_the_boss),
	role(C, casino_employee)).

need(win_next_election).
implies(needs(C, win_next_election),
	role(C, politician)).
	
need(make_a_legacy).
implies(needs(C, make_a_legacy),
	role(C, heist_leader)).
	
need(clean_my_record).
implies(needs(C, clean_my_record),
	role(C, politician)).

need(to_protect_and_serve).
implies(need(C, to_protect_and_serve),
	role(C, good_cop)).

need(peace_and_quiet).
	
object(horse_dildo).
object(donkey_dildo).
object(female_coyote_dildo).
object(silenced_pistol).
object(large_pistol).
object(assault_rifle).
object(poker_chips).
object(grenade).%yolo
object(credit_card).
object(roses).
object(keycard).
object(lots_of_rope).
object(empty_briefcase).
object(car_keys).
object(covert_listening_devices_or_bugs).
object(wedding_rings).
object(a_magic_hat).
object(roys_tiger).
object(a_few_numbers_on_a_cocktail_napkin).
object(hacksaw).
object(buzzsaw).
object(drag_racer).
object(valet_ticket).
object(torches).
object(bottle_service_at_the_encore).
object(secret_stash_of_fake_drugs).
object(pipe_wrench).
object(beer).
object(colt45).
object(black_tar_heroin).
object(dirty_underwear).
object(sex_toy).
object(smoke_bomb).
object(walkie_talkie).
object(samurai_sword).
object(lockpick).
object(earthquake_machine).
object(drill).
object(scorpion_jacket).
implies(role(C, driver),
	object(C,scorpion_jacket)).


location(feeding_the_lions_at_the_mgm).
location(why_am_in_mesquite).
location(studio_54).
location(the_bunny_ranch).
location(rollercoaster_of_the_new_york_new_york).
location(gold_and_silver_pawn).
location(the_little_chapel_on_the_hill_officiating_a_wedding).
location(the_little_chapel_on_the_hill_watching_a_wedding).
location(penthouse_of_monte_carlo).
location(watching_celine_dion_at_caesars_palace).
location(sam_woos_in_chinatown).
location(downtown_vegas_looking_at_the_lazer_show).
location(las_vegas_motor_speedway).
location(in_the_bellagio_fountains_without_clothes).
location(face_first_in_wynn_buffet).
location(the_gun_store).
location(golfing_at_vegas_municipal).
location(casino_safe).
location(funeral).
location(mens_room).
location(ladies_room).

location(drag_racing_on_the_strip).
implies(at(C, drag_racing_on_the_strip),
	role(C, driver)).
	
location(drunk_tank).
implies(at(C, drunk_tank),
	\+role(C,politician)).
implies(at(C, drunk_tank),
	\+role(C, good_cop)).

location(watching_bonneville_salt_flats).

location(drag_racing_on_bonneville_salt_flats).
implies(at(C, drag_racing_on_bonneville_salt_flats),
	role(C, driver)).
	
location(in_and_out).
location(airport).
location(wathing_a_magic_show).
location(fabulous_las_vegas_sign).
