/*%:- lib(listut).       % Placer cette directive en commentaire si vous utilisez Swi-Prolog */
   
                      % Sinon ne pas modifier si vous utilisez ECLiPSe Prolog :
                      % -> permet de disposer du predicat nth1(N, List, E)
                      % -> permet de disposer du predicat sumlist(List, S)
                      % (qui sont predefinis en Swi-Prolog)

                      
%***************************
%DESCRIPTION DU JEU DU TAKIN
%***************************

   %********************
   % ETAT INITIAL DU JEU
   %********************   
   % format :  initial_state(+State) ou State est une matrice (liste de listes)
   


initial_state([ [b, h, c],       % C'EST L'EXEMPLE PRIS EN COURS
                [a, f, d],       % 
                [g,vide,e] ]).   % h1=4,   h2=5,   f*=5


/*
initial_state([ [b, a, c],       % C'EST L'EXEMPLE PRIS EN COURS
                [g,vide,d],       % 
                [h,f,e] ]).   % h1=4,   h2=5,   f*=5

*/
/*
initial_state([ [vide, b, c, d],       % Exemple TAQUIN 4X4
                [a, e, f, g],
                [i, j, k, h],
		[m, n, o, l] ]).
*/


% AUTRES EXEMPLES POUR LES TESTS DE  A*


/*
initial_state([ [ a, b, c],        
                [ g, h, d],
                [vide,f, e] ]). % h2=2, f*=2


initial_state([ [b, c, d],
                [a,vide,g],
                [f, h, e]  ]). % h2=10 f*=10
			

-initial_state([ [f, g, a],
                [h,vide,b],
                [d, c, e]  ]). % h2=16, f*=20
			

-initial_state([ [e, f, g],
                [d,vide,h],
                [c, b, a]  ]). % h2=24, f*=30 


-initial_state([ [a, b, c],
                [g,vide,d],
                [h, f, e]]). % etat non connexe avec l'etat final (PAS DE SOLUTION)

*/  


   %******************
   % ETAT FINAL DU JEU
   %******************
   % format :  final_state(+State) ou State est une matrice (liste de listes)
   

final_state([[a, b,    c],
             [h, vide, d],
             [g, f,    e]]).


/*
final_state([[a, b, c, d],
             [e, f, g, h],
             [i, j, k, l],
		[m, n, o, vide]]).
*/
			 
   %********************
   % AFFICHAGE D'UN ETAT
   %********************
   % format :  write_state(?State) ou State est une liste de lignes a afficher

write_state([]).
write_state([Line|Rest]) :-
   writeln(Line),
   write_state(Rest).
   

%**********************************************
% REGLES DE DEPLACEMENT (up, down, left, right)             
%**********************************************
   % format :   rule(+Rule_Name, ?Rule_Cost, +Current_State, ?Next_State)
   
rule(up,   1, S1, S2) :-
   vertical_permutation(_X,vide,S1,S2).

rule(down, 1, S1, S2) :-
   vertical_permutation(vide,_X,S1,S2).

rule(left, 1, S1, S2) :-
   horizontal_permutation(_X,vide,S1,S2).

rule(right,1, S1, S2) :-
   horizontal_permutation(vide,_X,S1,S2).

   %***********************
   % Deplacement horizontal            
   %***********************
    % format :   horizontal_permutation(?Piece1,?Piece2,+Current_State, ?Next_State)
	
horizontal_permutation(X,Y,S1,S2) :-
   append(Above,[Line1|Rest], S1),
   exchange(X,Y,Line1,Line2),
   append(Above,[Line2|Rest], S2).

   %***********************************************
   % Echange de 2 objets consecutifs dans une liste             
   %***********************************************
   
exchange(X,Y,[X,Y|List], [Y,X|List]).
exchange(X,Y,[Z|List1],  [Z|List2] ):-
   exchange(X,Y,List1,List2).

   %*********************
   % Deplacement vertical            
   %*********************
   
vertical_permutation(X,Y,S1,S2) :-
   append(Above, [Line1,Line2|Below], S1), % decompose S1
   delete(N,X,Line1,Rest1),    % enleve X en position N a Line1,   donne Rest1
   delete(N,Y,Line2,Rest2),    % enleve Y en position N a Line2,   donne Rest2
   delete(N,Y,Line3,Rest1),    % insere Y en position N dans Rest1 donne Line3
   delete(N,X,Line4,Rest2),    % insere X en position N dans Rest2 donne Line4
   append(Above, [Line3,Line4|Below], S2). % recompose S2 

   %***********************************************************************
   % Retrait d'une occurrence X en position N dans une liste L (resultat R) 
   %***********************************************************************
   % use case 1 :   delete(?N,?X,+L,?R)
   % use case 2 :   delete(?N,?X,?L,+R)   
   
delete(1,X,[X|L], L).
delete(N,X,[Y|L], [Y|R]) :-
   delete(N1,X,L,R),
   N is N1 + 1.


   
   
   %*******************
   % PARTIE A COMPLETER
   %*******************
   
   %*******************************************************************
   % Coordonnees X(colonne),Y(Ligne) d'une piece P dans une situation U
   %*******************************************************************
	% format : coordonnees(?Coord, +Matrice, ?Element)
	% Définit la relation entre des coordonnees [Ligne, Colonne] et un element de la matrice
	/*
	Exemples
	
	?- coordonnees(Coord, [[a,b,c],[d,e,f]],  e).        % quelles sont les coordonnees de e ?
	Coord = [2,2]
	yes
	
	?- coordonnees([2,3], [[a,b,c],[d,e,f]],  P).        % qui a les coordonnees [2,3] ?
	P=f
	yes
	*/

	
	%coordonnees([L,C], Mat, Elt) :- true.    %********
											 % A FAIRE
											 %********

											 
   %*************
   % HEURISTIQUES
   %*************
   
heuristique(U,H) :-
%    heuristique1(U, H).  % au debut on utilise l'heuristique 1 
   heuristique2(U, H).  % ensuite utilisez plutot l'heuristique 2  
   
   
   %****************
   %HEURISTIQUE no 1
   %****************
   % Nombre de pieces mal placees dans l'etat courant U
   % par rapport a l'etat final F

   
   /*
   isCorrectlyPlaced(E) :-
	   initial_state(Ini), nth1(L, Ini, Ligne), nth1(C, Ligne, E),
	   final_state(Fin), nth1(L, Fin, Ligne2), nth1(C, Ligne2, E).
   */


   diff(vide, _, 0).
   diff(E1, E1, 0) :-
        E1 \= vide.
   diff(E1, E2, 1) :-
	   E1 \= vide,
	   E1 \= E2.
   
   diff_ligne([], [], 0).
   diff_ligne([H1|T1], [H2|T2], Dl) :-
	   diff(H1, H2, D),
	   diff_ligne(T1, T2, Rdl),
	   Dl is Rdl + D.

   do_heuristique1([], [], 0).
   do_heuristique1([L1|R1], [L2|R2], H) :-
	   diff_ligne(L1, L2, D),
	   do_heuristique1(R1, R2, Rd),
	   H is D + Rd.

   heuristique1(M1, H) :- 
	    final_state(M2),
	    do_heuristique1(M1, M2, H).
   
   
   %****************
   %HEURISTIQUE no 2
   %****************
   
   % Somme des distances de Manhattan à parcourir par chaque piece
   % entre sa position courante et sa positon dans l'etat final

	coordonnees([L, C], Mat, Elt) :-
		nth1(L, Mat, Ligne), nth1(C, Ligne, Elt).

	do_heuristique2_ligne(_, [], [], []).
	do_heuristique2_ligne(Mat, [vide|T1], [_|T2], T3) :- do_heuristique2_ligne(Mat, T1, T2, T3).
	do_heuristique2_ligne(Mat, [H1|T1], [H2|T2], [H3|T3]) :-
		H1 \= vide,
		coordonnees([L1, C1], Mat, H1),
		coordonnees([L2, C2], Mat, H2),

		do_heuristique2_ligne(Mat, T1, T2, T3),

		H3 is abs(L2-L1) + abs(C2-C1).



	do_heuristique2(_, [], [], 0).
	do_heuristique2(Init, [L1|R1], [L2|R2], H) :-
		do_heuristique2_ligne(Init, L1, L2, D),

		sumlist(D, Tmph),

		do_heuristique2(Init, R1, R2, Rd),
		H is Tmph + Rd.

	heuristique2(U, H) :- 
		final_state(Fin),
		do_heuristique2(U, U, Fin, H).
        




