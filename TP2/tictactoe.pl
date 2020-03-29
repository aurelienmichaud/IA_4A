	/*********************************
	DESCRIPTION DU JEU DU TIC-TAC-TOE
	*********************************/

	/*
	Une situation est decrite par une matrice 3x3.
	Chaque case est soit un emplacement libre, soit contient le symbole d'un des 2 joueurs (o ou x)

	Contrairement a la convention du tp pr�c�dent, pour mod�liser une case libre
	dans une matrice on n'utilise pas une constante sp�ciale (ex : nil, 'vide', 'libre','inoccupee' ...);
	On utilise plut�t une variable libre (_), c'est�-dire un terme non instanci� ('_').
	La situation initiale est donc une matrice 3x3 composee uniquement de variables libres (_). 
	Ceci est possible car le jeu consiste � instancier la grille avec des symboles et non � d�placer les symbles d�j� affect�s.
	
	
	
	Jouer un coup, c-a-d placer un symbole dans une grille S1 ne consiste pas � g�n�rer une nouvelle grille S2 obtenue 
	en copiant d'abord S1 puis en remplacant le symbole de case libre par le symbole du joueur, mais plus simplement
	� INSTANCIER (au sens Prolog) la variable libre qui repr�sentait la case libre par la valeur associ�e au joueur, ex :
	Case = Joueur, ou a realiser indirectement cette instanciation par unification via un pr�dicat comme member/2, select/3, nth1/3 ...
	
	Ainsi si on joue un coup en S, S perd une variable libre, mais peut continuer � s'appeler S (on n'a pas besoin de la d�signer
	par un nouvel identificateur).
	La situation initiale est une "matrice" 3x3 (liste de 3 listes de 3 termes chacune)
	o� chacun des 9 termes est une variable libre.	
	*/

situation_initiale([ [_,_,_],
                     [_,_,_],
                     [_,_,_] ]).

	% Convention (arbitraire) : c'est x qui commence

joueur_initial(x).


	% Definition de la relation adversaire/2

adversaire(x,o).
adversaire(o,x).


	/****************************************************
	 � l'aide du pr�dicat ground/1 comment
	 reconnaitre une situation terminale dans laquelle il
	 n'y a aucun emplacement libre : aucun joueur ne peut
	 continuer � jouer (quel qu'il soit).
	 ****************************************************/

do_situation_terminale_ligne([]).
do_situation_terminale_ligne([H | T]) :-
	ground(H),
	do_situation_terminale_ligne(T).

situation_terminale(_, []).
situation_terminale(Joueur, [Ligne | T]) :- 
	do_situation_terminale_ligne(Ligne),
	situation_terminale(Joueur, T).


/**************x*************
 DEFINITIONS D'UN ALIGNEMENT
 ***************************/

alignement(L, Matrix) :- ligne(    L,Matrix).
alignement(C, Matrix) :- colonne(  C,Matrix).
alignement(D, Matrix) :- diagonale(D,Matrix).
	
ligne(L, M) :-  
	nth1(_, M, L).


get_transpose_line(_, [], []).
get_transpose_line(Index, [L | Tm], [E | Tl]) :-
	nth1(Index, L, E),
	get_transpose_line(Index, Tm, Tl).


do_transpose(0, [], _, []).
do_transpose(IndexC, [_ | T], M, [Transposed_line | TT]) :-
	get_transpose_line(IndexC, M, Transposed_line),
	do_transpose(I, T, M, TT),
	IndexC is I + 1.

transpose(M, Transpose) :-
	do_transpose(_, M, M, Transpose_tmp),
	reverse(Transpose_tmp, Transpose).

colonne(C,M) :- 
	transpose(M, Transpose),
	ligne(C, Transpose).


	/* D�finition de la relation liant une diagonale D � la matrice M dans laquelle elle se trouve.
		il y en a 2 sortes de diagonales dans une matrice carree(https://fr.wikipedia.org/wiki/Diagonale) :
		- la premiere diagonale (principale) (descendante) : (A I)
		- la seconde diagonale  (ascendante)  : (R Z)
		A . . . . . . . Z
		. \ . . . . . / .
		. . \ . . . / . .
		. . . \ . / . . .
		. . . . X . . .
		. . . / . \ . . . 
		. . / . . . \ . .
		. / . . . . . \ .
		R . . . . . . . I
	*/

list_length([], 0).
list_length([_ | T], N) :-
	list_length(T, N_tmp),
	N is N_tmp + 1.

diagonale(D, M) :- premiere_diag(1,D,M).
diagonale(D, M) :- 
	list_length(M, Length),
	seconde_diag(Length,D,M).

premiere_diag(_,[],[]).
premiere_diag(K,[E|D],[Ligne|M]) :-
	nth1(K,Ligne,E),
	K1 is K+1,
	premiere_diag(K1,D,M).

% definition de la seconde diagonale
seconde_diag(_,[],[]).
seconde_diag(K, [E|D], [Ligne|M]) :- 
	nth1(K,Ligne,E),
	K1 is K-1,
	seconde_diag(K1,D,M).


	/***********************************
	 DEFINITION D'UN ALIGNEMENT POSSIBLE
	 POUR UN JOUEUR DONNE
	 **********************************/

possible([X|L], J) :- unifiable(X,J), possible(L,J).
possible([   ], _).

	/* Attention 
	il faut juste verifier le caractere unifiable
	de chaque emplacement de la liste, mais il ne
	faut pas realiser l'unification.
	*/

unifiable(X,_) :- var(X).
unifiable(X,J) :- ground(X), X == J.
	
	/**********************************
	 DEFINITION D'UN ALIGNEMENT GAGNANT
	 OU PERDANT POUR UN JOUEUR DONNE J
	 **********************************/

	/*
	Un alignement gagnant pour J est un alignement
possible pour J qui n'a aucun element encore libre.
Un alignement perdant pour J est gagnant
pour son adversaire.
	*/

alignement_gagnant([], _).
alignement_gagnant([H | T], J) :-
	ground(H),
	H == J,
	alignement_gagnant(T, J).


alignement_perdant(Ali, J) :-
	adversaire(J, Adv),
	alignement_gagnant(Ali, Adv).









%%%%%% ============================
%%%%%%  TEST UNITAIRE A FAIRE ......
%%%%%% ============================
















	/******************************
	DEFINITION D'UN ETAT SUCCESSEUR
	******************Etat*************/

     /*Il faut definir quelle op�ration subit une matrice M representant la situation courante
	lorsqu'un joueur J joue en coordonnees [L,C]
     */	

successeur(J, Etat, [L,C]) :-
	nth1(L, Etat, Ligne),
	nth1(C, Ligne, Case),
	unifiable(Case, J),
	Case = J.

	/**************************************
   	 EVALUATION HEURISTIQUE D'UNE SITUATION
  	 **************************************/

/*
1/ l'heuristique est +infini si la situation J est gagnante pour J
2/ l'heuristique est -infini si la situation J est perdante pour J
3/ sinon, on fait la difference entre :
	   le nombre d'alignements possibles pour J
	moins
 	   le nombre d'alignements possibles pour l'adversaire de J
*/

nb_alignement_gagnant(_, [], 0).

nb_alignement_gagnant(J, [Alig | TAlig], N) :-
	not(possible(Alig, J)),
	nb_alignement_gagnant(J, TAlig, N).

nb_alignement_gagnant(J, [Alig | TAlig], N) :-
	possible(Alig, J),
	nb_alignement_gagnant(J, TAlig, N_tmp),
	N is N_tmp + 1.

heuristique(J,Situation,H) :-		% cas 1
   H = 10000,				% grand nombre approximant +infini
   alignement(Alig,Situation),
   alignement_gagnant(Alig,J), !.
	
heuristique(J,Situation,H) :-		% cas 2
   H = -10000,				% grand nombre approximant -infini
   alignement(Alig,Situation),
   alignement_perdant(Alig,J),!.	


% on ne vient ici que si les cut precedents n'ont pas fonctionne,
% c-a-d si Situation n'est ni perdante ni gagnante.


heuristique(J, Situation, H) :-		% cas 3
	
	findall(Alig, alignement(Alig, Situation), Alig_list),

	nb_alignement_gagnant(J, Alig_list, NJ),

	adversaire(J, Adv),
	nb_alignement_gagnant(Adv, Alig_list, NAdv),

	H is NJ - NAdv.

% ===== TESTS UNITAIRES heuristique

test_heuristique_gagnant :-
	M = [
			[x, o, _],
			[x, x, o],
			[x, o, _]
		], 

	statistics(cputime, BEFORE),
	heuristique(x, M, H),
	statistics(cputime, AFTER),
	CPUTIME is AFTER - BEFORE,
	write("CPU TIME = "),
	write(CPUTIME),
	write("\nH = "),
	write(H).

test_heuristique_perdant :-
	M = [
			[x, o, _],
			[x, x, o],
			[x, o, _]
		], 

	statistics(cputime, BEFORE),
	heuristique(o, M, H),
	statistics(cputime, AFTER),
	CPUTIME is AFTER - BEFORE,
	write("CPU TIME = "),
	write(CPUTIME),
	write("\nH = "),
	write(H).

test_heuristique_courant :-
	M = [
			[x, o, _],
			[o, x, o],
			[x, o, _]
		], 

	statistics(cputime, BEFORE),
	heuristique(x, M, H),
	statistics(cputime, AFTER),
	CPUTIME is AFTER - BEFORE,
	write("CPU TIME = "),
	write(CPUTIME),
	write("\nH = "),
	write(H).
	
	



