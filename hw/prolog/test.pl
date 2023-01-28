%misc func
max([X],X).
max([X|Xs],X):- max(Xs,Y), X >=Y.
max([X|Xs],N):- max(Xs,N), N > X.

findlen([],X):-X=0.
findlen([X|Tail],Count):-findlen(Tail,Prev), Count is Prev + 1.
find_board_size([TopRow|Others],Width,Height):-findlen(TopRow,Width),findlen([[TopRow]|Others],Height).

%board before
n0_row([],NumB,NumW):-NumB=0, NumW=0.
n0_row(H,[],NumB,NumW):- H=@=b,NumB=1,NumW=0,!; H=@=w,NumB=0,NumW=1,!; (H=@=m;H=@=s),NumB=0,NumW=0,!.
n0_row([H|T],NumB,NumW):- 
	n0_row(H,[],NumB1,NumW1), n0_row(T,NumB2,NumW2), 
	NumB is NumB1+NumB2, NumW is NumW1+NumW2.

n0_board([],NumB,NumW):- NumB=0,NumW=0.
n0_board([H|T],NumB,NumW):-
	n0_row(H,NumB1,NumW1), n0_board(T,NumB2,NumW2), 
	NumB is NumB1+NumB2, NumW is NumW1+NumW2.
%board after
n1_row([],NumB,NumW):-NumB=0, NumW=0.
n1_row(H,[],NumB,NumW):- H=@=bL,NumB=1,NumW=0,!; H=@=wL,NumB=0,NumW=1,!; (H=@=aL;H=@=bD;H=@=wD),NumB=0,NumW=0,!.
n1_row([H|T],NumB,NumW):- 
	n1_row(H,[],NumB1,NumW1), n1_row(T,NumB2,NumW2), 
	NumB is NumB1+NumB2, NumW is NumW1+NumW2.

n1_board([],NumB,NumW):- NumB=0,NumW=0.
n1_board([H|T],NumB,NumW):-
	n1_row(H,NumB1,NumW1), n1_board(T,NumB2,NumW2), 
	NumB is NumB1+NumB2, NumW is NumW1+NumW2.
%given before and after, return score
score_move(Play,BoardOri,BoardNow,Score):-
	Play=@=b, n0_board(BoardOri,C0,C2), n1_board(BoardNow,C1,C3), Score is (C1-C0)+(C2-C3);
	Play=@=w, n0_board(BoardOri,C2,C0), n1_board(BoardNow,C3,C1), Score is (C1-C0)+(C2-C3).

find_good_move(Play,Depth,BoardOri,BestMove,ScoreOfBestMove):-
	Depth < 0, BestMove=error, ScoreOfBestMove=[];
	Depth =@= 0 , BestMove=nil, ScoreOfBestMove=inf.
%Depth =@= 1, 
%find_moveset(H,Ans):- H=@=m, Ans=
%find_row_moveset([H|T],[Ans|AnsRow]):-find_moveset(H,Ans),fund_row_moveset(T,AnsRow)
%find_board_moveset(Board,[MoveRow|MoveBoard]):-find_row_moveset(), find_board_moveset().
append([ ], L, L).
append([X|L1], L2, [X|L3]) :- append(L1,L2,L3).

find_m(H,RowID,ColID,Ans,Score):-
	H=@=m, Ans=[RowID,ColID] ,Score is -1000; 
	(H=@=s;H=@=b;H=@=w), Ans=[n,n], Score is -1000.

find_m_row([],_,_,[],[]).
find_m_row([H|T],RowID,ColID,[Ans|AnsRow],[Score|Stail]):- 
	find_m(H,RowID,ColID,Ans,Score), C is ColID+1, find_m_row(T,RowID,C,AnsRow,Stail).

find_m_board([],_,_,[],[]).
find_m_board([H|T],RowID,ColID,MoveSet,ScoreSet):-
	find_m_row(H,RowID,ColID,M1,S1),
	R is RowID+1,
	find_m_board(T,R,ColID,M2,S2),
	append(M1,M2,MoveSet),
	append(S1,S2,ScoreSet).

indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1+1.
get_row_ele([H|T],LocationX,Ans):- LocationX=0, Ans=H ; NewX is LocationX-1, get_row_ele(T,NewX,Ans).


put_stone(H,RowID,ColID,Play,Ans):- 
	RowID=@=0, ColID=@=0, Ans=Play,!;
	Ans=H.
put_stone_row([],_,_,_,[]).
put_stone_row([H|T],RowID,ColID,Play,[Ans|AnsRow]):- 
	ColID=@=0, put_stone(H,RowID,ColID,Play,Ans), AnsRow=T,!;
	C is ColID-1, put_stone_row(T,RowID,C,Play,AnsRow), Ans=H.
put_stone_board([],_,_,_,[]).
put_stone_board([H|T],RowID,ColID,Play,[AnsRow|AnsBoard]):-
	RowID=@=0, put_stone_row(H,RowID,ColID,Play,AnsRow), AnsBoard=T,!; 
	R is RowID-1, put_stone_board(T,R,ColID,Play,AnsBoard), AnsRow=H.
put_stone_board(BoardOri,[RowID,ColID],Play,BoardNew):-put_stone_board(BoardOri,RowID,ColID,Play,BoardNew).
