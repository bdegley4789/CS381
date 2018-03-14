% Team Members: Bryce Egley ONID: egleyb, Kenneth Price ONID: pricek, Kenneth Thompson ONID: thomkenn
% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(C, P) :- parent(P, C).

% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(Mother) :- female(Mother), parent(Mother, _).
isFather(Father) :- male(Father), parent(Father, _).

% 3. Define a predicate `grandparent/2`.
grandparent(G, C) :- parent(X, C), parent(G, X).

% 4. Define a predicate `sibling/2`. Ss share at least one parent.
sibling(X, S) :- parent(P, S), parent(P, X), X \= S.

% 5. Define two predicates `brother/2` and `sister/2`.
brother(X, S) :- sibling(X, S), male(X).
sister(X, S) :- sibling(X, S), female(X).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw_(X, SIL) :- married(Spouse, SIL), sibling(Spouse, X).

siblingInLaw(X, Y) :- siblingInLaw_(X, Y).
siblingInLaw(X, Y) :- siblingInLaw_(Y, X). 

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt_(X,A) :- parent(P,X), sister(P,A).
uncle_(X,U) :- parent(P,X), brother(P,U).
aunt(A,X) :- aunt_(X,A).
aunt(A,X) :- married(A, U), uncle_(X,U).
uncle(U,X) :- uncle_(X,U).
uncle(U,X) :- married(U, A), aunt_(X,A).

% 8. Define the predicate `cousin/2`.
cousin(C,X) :- aunt(A,X), child(C, A).
cousin(C,X) :- uncle(U,X), child(C, U).

% 9. Define the predicate `ancestor/2`.
ancestor(A,X) :- parent(A,X).
ancestor(A,X) :- parent(P,X),ancestor(A,P).

% Extra credit: Define the predicate `related/2`.

getRel(R,X) :- parent(R,X).
getRel(R,X) :- child(R,X).
getRel(R,X) :- married(R,X).

isNotMember(_,[]).
isNotMember(X,[H|T]) :- X\=H, isNotMember(X,T).

findRelative(R,L,[H|_]) :- getRel(R,H), isNotMember(R,L), !.
findRelative(R,L,[_|T]) :- findRelative(R,L,T).

related_(_,R,[R|T]) :- findRelative(R,T,T).
related_(X,R,[R|T]) :- findRelative(Rm,T,T), !, related_(X,R,[R,Rm|T]).

related(X,R) :- related_(X,R,[R,X]).

%%
% Part 2. Language implementation
%%

% 1. Define the predicate `cmd/3`, which describes the effect of executing a
%    command on the stack.
cmd(add, [I1,I2|T1], S2) :- cmd(R, T1, S2), R is I1+I2, !.
cmd(lte, [I1,I2|T1], S2) :- cmd(t, T1, S2), I1@=<I2, !.
cmd(lte, [I1,I2|T1], S2) :- cmd(f, T1, S2), I1@>I2, !.
cmd(if(P1,_), [t|T1], S2) :- prog(P1, T1, S2), !.
cmd(if(_,P2), [f|T1], S2) :- prog(P2, T1, S2), !.
cmd(Lit, S1, [Lit|S1]).

% 2. Define the predicate `prog/3`, which describes the effect of executing a
%    program on the stack.
prog([H|[]], S1, S2) :- cmd(H,S1,S2).
prog([Hp|Tp], S1, S2) :- cmd(Hp,S1,Rs), prog(Tp,Rs,S2),!. 

