%*******************************************************************************
%                                    AETOILE
%*******************************************************************************

/*
Rappels sur l'algorithme
 
- structures de donnees principales = 2 ensembles : P (etat pendants) et Q (etats clos)
- P est dedouble en 2 arbres binaires de recherche equilibres (AVL) : Pf et Pu
 
   Pf est l'ensemble des etats pendants (pending states), ordonnes selon
   f croissante (h croissante en cas d'egalite de f). Il permet de trouver
   rapidement le prochain etat a developper (celui qui a f(U) minimum).
   
   Pu est le meme ensemble mais ordonne lexicographiquement (selon la donnee de
   l'etat). Il permet de retrouver facilement n'importe quel etat pendant

   On gere les 2 ensembles de façon synchronisee : chaque fois qu'on modifie
   (ajout ou retrait d'un etat dans Pf) on fait la meme chose dans Pu.

   Q est l'ensemble des etats deja developpes. Comme Pu, il permet de retrouver
   facilement un etat par la donnee de sa situation.
   Q est modelise par un seul arbre binaire de recherche equilibre.

Predicat principal de l'algorithme :

   aetoile(Pf,Pu,Q)

   - reussit si Pf est vide ou bien contient un etat minimum terminal
   - sinon on prend un etat minimum U, on genere chaque successeur S et les valeurs g(S) et h(S)
	 et pour chacun
		si S appartient a Q, on l'oublie
		si S appartient a Ps (etat deja rencontre), on compare
			g(S)+h(S) avec la valeur deja calculee pour f(S)
			si g(S)+h(S) < f(S) on reclasse S dans Pf avec les nouvelles valeurs
				g et f 
			sinon on ne touche pas a Pf
		si S est entierement nouveau on l'insere dans Pf et dans Ps
	- appelle recursivement etoile avec les nouvelles valeurs NewPF, NewPs, NewQs

*/

%*******************************************************************************

:- ['avl.pl'].       % predicats pour gerer des arbres bin. de recherche   
:- ['taquin.pl'].    % predicats definissant le systeme a etudier

%*******************************************************************************

main :-
	initial_state(S0),
	heuristique(S0, H0),

	% initialisations Pf, Pu et Q 

	empty(Pf),
	empty(Pu),
	empty(Q),

	insert([[H0, H0, 0], S0], Pf, New_Pf),
	insert([S0, [H0, H0, 0], nil, nil], Pu, New_Pu),

	% lancement de Aetoile
	aetoile(New_Pf, New_Pu, Q).



%*******************************************************************************

aetoile(Pf, Ps, _) :-
	empty(Pf),
	empty(Ps),
	write("PAS de SOLUTION : L'ETAT FINAL N'EST PAS ATTEIGNABLE").

aetoile(Pf, Ps, Qs) :-

	not(empty(Pf)),
	not(empty(Ps)),

	% Cas general

	final_state(Final_state),
	suppress_min([[Fu, Hu, Gu], U], Pf, Pf_without_U),

	(U = Final_state
	% IF WE REACHED THE FINAL STATE
	->
		suppress([Final_state, [Ff, Hf, Gf], PereF, ActionF], Ps, _New_Pu),
		insert([Final_state, [Ff, Hf, Gf], PereF, ActionF], Qs, New_Qs),

		affiche_solution(New_Qs, Final_state)

	% ELSE, WE CONTINUE BY DEVELOPING THE CURRENT STATE 'U'
	;

		suppress([U, [Fu, Hu, Gu], PereU, ActionU], Ps, Pu_without_U),

		% Developpement de U
		% -- Determiner tous les noeuds contenant un etat successeur S 
		% de la situation U et calculer leur evaluation [Fs, Hs, Gs] 
		% connaissant Gu et le cout de U a S.

		expand(U, Evaluations),

		% -- Traiter chaque noeud successeur
		loop_successors(U, Gu, Evaluations, Pf_without_U, Pu_without_U, Pf_updated, Pu_updated, Qs),	

		insert([U, [Fu, Hu, Gu], PereU, ActionU],  Qs, Q_updated),

		aetoile(Pf_updated, Pu_updated, Q_updated)
	).








% ====== loop_successors
loop_successors(_, _, [], Pf, Pu, Pf, Pu, _).

% if S has already been developed (S is in Q) -> we go right to the next state
loop_successors(U, Gu, [[S | _] | T], Pf, Pu, New_Pf, New_Pu, Q) :-
	belongs([S | _], Q),
	loop_successors(U, Gu, T, Pf, Pu, New_Pf, New_Pu, Q).


loop_successors(U, Gu, [[S, H, Action] | T], Pf, Pu, New_Pf, New_Pu, Q) :-

	suppress([S, [FP, HP, GP], PereS, ActionS], Pu, Pu_tmp),

	G is Gu + 1,
	FEval is G + H,

	get_min([FP, FEval],
		[HP, H],
		[GP, G],
		[PereS, U],
		[ActionS, Action],
		[Fmin, Hmin, Gmin, Peremin, Actionmin]),

	% Update Pf and Pu with minimal values

	suppress([[FP, HP, GP], S], Pf, Pf_tmp),
	insert([[Fmin, Hmin, Gmin], S], Pf_tmp, New_Pf1),

	insert([S, [Fmin, Hmin, Gmin], Peremin, Actionmin], Pu_tmp, New_Pu1),

	loop_successors(U, Gu, T, New_Pf1, New_Pu1, New_Pf, New_Pu, Q).



loop_successors(U, Gu, [[S, H, Action] | T], Pf, Pu, New_Pf, New_Pu, Q) :-
	not(belongs([S | _], Q)),
	not(belongs([S | _], Pu)),

	G is Gu + 1,
	Fs is G + H,

	% Update Pf and Pu with minimal values

	insert([[Fs, H, G], S], Pf, New_Pf1),

	insert([S, [Fs, H, G], U, Action], Pu, New_Pu1),

	loop_successors(U, Gu, T, New_Pf1, New_Pu1, New_Pf, New_Pu, Q).
	






% ====== expand

do_expand([], []).
do_expand([[Action, S] | T], [[S, H, Action] | Evaluations]) :-
	heuristique(S, H),	
	do_expand(T, Evaluations).

expand(Current_state, Evaluations) :-
	findall([X, Next_state], rule(X, 1, Current_state, Next_state), Successors),	
	do_expand(Successors, Evaluations).






affiche_solution(Qs, U) :-
	belongs([U, _, nil, nil], Qs),
	write("\n=== Solution ===\n\n"),
	write_state(U).

affiche_solution(Qs, U) :-
	belongs([U, _, PereU, ActionU], Qs),
	PereU \= nil,
	affiche_solution(Qs, PereU),

	write("\n\t|\n\t"),
	write(ActionU),
	write("\n\t|\n"),
	write("\tv\n"),
	write_state(U).




   
get_min([F1, F2], [H1, H2], [G1, G2], [Pere1, _], [Action1, _], [F1, H1, G1, Pere1, Action1]) :-
	%min_list([F1, F2], F1).
	[F1, H1, G1] @< [F2, H2, G2].

get_min([F1, F2], [H1, H2], [G1, G2], [_, Pere2], [_, Action2], [F2, H2, G2, Pere2, Action2]) :-
	%min_list([F1, F2], F2).
	[F2, H2, G2] @< [F1, H1, G1].





% ========= UNIT TESTS

test_loop_successors :-
	initial_state(S0),

	% initialisations Pf, Pu et Q 

	empty(Pf),
	empty(Pu),
	empty(Q),

	expand(S0, Evaluations),
	write("\n=== EXPAND ===\n"),
	write_state(Evaluations),

	write("\n=== BEFORE ===\nPf :"),
	put_flat(Pf),
	write("\nPu :"),
	put_flat(Pu),

	loop_successors(S0, 0, Evaluations, Pf, Pu, New_Pf, New_Pu, Q),

	write("\n=== AFTER ==="),
	write("\nPf :"),
	put_flat(New_Pf),
	write("\nPu :"),
	put_flat(New_Pu).


test_expand :-
	initial_state(Ini),
	findall([X, Next_state], rule(X, 1, Ini, Next_state), Successors),	
	write_state(Successors),
	expand(Successors, Eval),
	write_state(Eval).

% affiche_solution tests
% avl_test_affiche_solution(0, Avl), afficher_solution(Avl, a).
avl_test_affiche_solution(0, 
	(avl(
		avl(
			avl(
				nil,
				[a, [0, 0, 0], b, left],
				nil,
				left
			), 
			[b, [0, 0, 0], c, up], 
			nil, 
			1
		), 
		[c, [0, 0, 0], nil, nil], 
		nil, 
		1
	))
).


update([], P, P).
update([H | T], P, New_P) :-
	insert([H], P, New_P1),
	update(T, New_P1, New_P).		


