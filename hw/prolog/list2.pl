init_row([],[]).
init_row(H,[],Ans):- H=@=b,Ans=bD ; H=@=w,Ans=wD ; H=@=s,Ans=aL;H=@=m,Ans=aL.
init_row([H|T],[Ans1|Ans2]):- init_row(H,[],Ans1), init_row(T,Ans2).
%Ans1/2 is a list, H is a list, T is list of list
init_board([],[]).
init_board([H|T],[AnsRow|AnsBoard],Play):- init_row(H,AnsRow), init_board(T,AnsBoard).
%find curr turn color
get_row_ele([H|T],LocationX,Ans):- LocationX=0, Ans=H ; NewX is LocationX-1, get_row_ele(T,NewX,Ans).
get_board_ele([Hlist|Tboard],LocationY,LocationX,Ans):- LocationY=0, get_row_ele(Hlist,LocationX,Ans); NewY is LocationY-1, get_board_ele(Tboard,LocationX,NewY,Ans).
/*======================*/
%only current death will check
%err when contradiction
check_neighbour(Color,Cell,Ans):-
	Color=@=bD, (Cell=@=bL;Cell=@=aL), Ans=bL; 
	Color=@=bD, (Cell=@=bD;Cell=@=wL), Ans=bD;
	Color=@=bD, Cell=@=wD, Ans=err;

	Color=@=wD, (Cell=@=wL;Cell=@=aL), Ans=wL;
	Color=@=wD, (Cell=@=wD;Cell=@=bL), Ans=wD;
	Color=@=wD, Cell=@=bD, Ans=err.

update_cell([Tleft, Tmid|Ttail],[Mleft,Center,Mright|Mtail],[Bleft,Bmid|Btail],Play,Ans):-
	Center=@=aL, Ans=aL;
	Center=@=bL, Ans=bL;
	Center=@=wL, Ans=wL;
	Center=@=bD, (Mleft=@=x;Tmid=@=x;Mright=@=x,Bmid=@=x), Ans=bL;
	Center=@=wD, (Mleft=@=x;Tmid=@=x;Mright=@=x,Bmid=@=x), Ans=wL,!;

	Center=@=bD, (check_neighbour(bD,Mleft,Left),check_neighbour(bD,Tmid,Top),check_neighbour(bD,Mright,Right),check_neighbour(bD,Bmid,Bottom)), ((Left=@=bL;Left=@=aL;Top=@=bL;Top=@=aL;Right=@=bL;Right=@=aL;Botton=@=bL;Bottom=@=aL), Ans=bL; (Left=@=bD,Top=@=bD,Right=@=bD,Bottom=@=bD), Ans=bD; (Left=@=err;Top=@=err;Right=@=err;Bottom=@=err), Play=@=b, Ans=bL ;(Left=@=err;Top=@=err;Right=@=err;Bottom=@=err), Play=@=w, Ans=x); Ans=bD;

	Center=@=wD, (check_neighbour(wD,Mleft,Left),check_neighbour(wD,Tmid,Top),check_neighbour(wD,Mright,Right),check_neighbour(wD,Bmid,Bottom)), ((Left=@=wL;Left=@=aL;Top=@=wL;Top=@=aL;Right=@=wL;Right=@=aL;Bottom=@=wL;Bottom=@=aL), Ans=wL; (Left=@=wD,Top=@=wD,Right=@=wD,Bottom=@=wD), Ans=wD; (Left=@=err;Top=@=err;Right=@=err;Bottom=@=err), Play=@=w, Ans=wL ;(Left=@=err;Top=@=err;Right=@=err;Bottom=@=err), Play=@=b, Ans=x); Ans=wD.

/*======================*/
%need to find out the color of move()
%Play=w/b
check_board([],_,[]).
/*

*/
check_3row([],TopRow,MidRow,Play,AnsRow):-.
check_3row(MidRow,BotRow,[],[AnsRow|[]]):-.
/*
m/s->aL
	self:wL -> wL
	self:wD
		topMid:wL,bD,aL->wL | topMid:bL,wD->wD
		btmMid:wL,bD,aL->wL	| topMid:bL,wD->wD
		midLeft:w:,bD,aL->wL 
		right ...
	
	self:bL -> bL
		...

*/
check_neighbour(Color,TMid,Ans):-
	Color=@=w, (TMid=@=wL ; TMid=@=bD ; TMid=@=aL), Ans=wL; (TMid=@=bL ; TMid=@=wD), Ans=wD;
	Color=@=b, (TMid=@=bL ; TMid=@=wD ; TMid=@=aL), Ans=bL; (TMid=@=bD ; TMid=@=wL), And=bD.
%normal case
check_3row([TLeft,TMid|TTail],[MLeft,MMid,MRight|MTail],[BLeft,BMid|BTail],Play,Ans):-
	MMid=@=aL, Ans=aL;
	%MMid=@=wL, Ans=wL;
	%MMid=@=bL, Ans=bL;
	MMid=@=wD, (check_neighbour(w,TMid,A), check_neighbour(w,BMid,B), check_neighbour(w,MLeft,C), check_neighbour(w,MRight,D) ), (A=@=wL;B=@=wL;C=@=wL;D=@=wL), Ans=wL, (A=@=wD,B=@=wD,C=@=wD,D=@=wD), Ans=wD;
	MMid=@=bD, (check_neighbour(b,TMid,A), check_neighbour(b,BMid,B), check_neighbour(b,MLeft,C), check_neighbour(b,MRight,D) ), (A=@=bL;B=@=bL;C=@=bL;D=@=bL), Ans=bL, (A=@=bD,B=@=bD,C=@=bD,D=@=bD), Ans=bD.
%top cases
check_3row([],[MLeft,MMid,MRight|MTail],[BLeft,BMid|BTail],Play,Ans):-
	MMid=@=aL, Ans=aL;
	MMid=@=wL, Ans=wL;
	MMid=@=bL, Ans=bL;
	MMid=@=wD, (check_neighbour(w,BMid,B), check_neighbour(w,MLeft,C), check_neighbour(w,MRight,D) ), (A=@=wL;B=@=wL;C=@=wL;D=@=wL), Ans=wL, (A=@=wD,B=@=wD,C=@=wD,D=@=wD), Ans=wD;
	MMid=@=bD, (check_neighbour(b,BMid,B), check_neighbour(b,MLeft,C), check_neighbour(b,MRight,D) ), (A=@=bL;B=@=bL;C=@=bL;D=@=bL), Ans=bL, (A=@=bD,B=@=bD,C=@=bD,D=@=bD), Ans=bD.

/*
check_3row([TLeft,TMid|TTail],[MLeft,MMid,MRight|MTail],[BLeft,BMid|BTail],Play,[Ans|AnsRow]):-
	MMid=@=aL, Ans=aL;
	MMid=@=wL, Ans=wL;
	MMid=@=wD,	(TMid=@=wL ; TMid=@=bD ; TMid=@=aL), Ans=wL; (TMid=@=bL ; TMid=@=wD), Ans=wD; 
				(BMid=@=wL ; BMid=@=bD ; BMid=@=wL), Ans=wL; (BMid=@=bL ; BMid=@=wD), Ans=wD;
				(MLeft=@=wL ; MLeft=@=bD ; MLeft=@=wL), Ans=wL; (MLeft=@=bL ; MLeft=@=wD), Ans=wD;
				(MRight=@=wL ; MRight=@=bD ; MRight=@=wL), Ans=wL; (MRight=@=bL ; MRight=@=wD), Ans=wD;
	MMid=@=bL, Ans=bL;
	MMid=@=bD,	(TMid=@=bL ; TMid=@=wD ; TMid=@=bL), Ans=bL; (TMid=@=wL ; TMid=@=bD), Ans=bD; 
				(BMid=@=bL ; BMid=@=wD ; BMid=@=bL), Ans=bL; (BMid=@=wL ; BMid=@=bD), Ans=bD;
				(MLeft=@=bL ; MLeft=@=wD ; MLeft=@=bL), Ans=bL; (MLeft=@=wL ; MLeft=@=bD), Ans=bD;
				(MRight=@=bL ; MRight=@=wD ; MRight=@=bL), Ans=bL; (MRight=@=wL ; MRight=@=bD), Ans=bD;
*/
check_board([TopRow,MidRow,[BotRow|TailRows],Play,[AnsRow|AnsBoard]):-
	check_3row(TopRow,MidRow,BotRow,Play,AnsRow),
	check_board(TailRows,Play,AnsBoard).
%[board],move(x,y),[LDboard]
check_board([TopRow,MidRow|TailRows],Play,[AnsRow|AnsBoard]):-
	check_3row([],TopRow,MidRow,Play,AnsRow),
	check_board([MidRow|TailRows],Play,AnsBoard).

check_3([],M,R,Ans):- M=@=b,R=@=b,Ans=bL;M=@=w,R=@=w,Ans=wL ; M=@=b,R=@=w,Ans=bD;M=@=w,R=@=b,Ans=wD ; M=@=m,Ans=aL;M=@=s,Ans=aL ; M=@=b,R=@=s,Ans=bL;M=@=b,R=@=m,Ans=bL ; M=@=w,R=@=s,Ans=wL;M=@=w,R=@=m,Ans=wL.

check_3(L,M,[],[Ans|[]]):- L=@=b,M=@=b,Ans=bL ; L=@=w,M=@=w,Ans=wL ; L=@=b,M=@=w,Ans=wD ; L=@=w,M=@=b,Ans=bD ; M=@=m,Ans=aL; M=@=s,Ans=aL ; L=@=s,M=@=b,Ans=bL;L=@=m,M=@=b,Ans=bL ; L=@=s,M=@=w,Ans=wL;L=@=m,M=@=w,Ans=wL.

check_3(L,M,R,[],Ans):- L=@=b,M=@=b,Ans=bL ; M=@=b,R=@=b,Ans=bL ; L=@=w,M=@=b,R=@=w,Ans=bD ; L=@=w,M=@=w,Ans=wL ; M=@=w,R=@=w,Ans=wL ; L=@=b,M=@=w,R=@=b,Ans=wD ; M=@=m,Ans=aL; M=@=s,Ans=aL ; L=@=s,M=@=b,Ans=bL;L=@=m,M=@=b,Ans=bL ; L=@=s,M=@=w,Ans=wL;L=@=m,M=@=w,Ans=wL ; M=@=b,R=@=s,Ans=bL;M=@=b,R=@=m,Ans=bL ; M=@=w,R=@=s,Ans=wL;M=@=w,R=@=m,Ans=wL.

check_3(L,M,[H|T],[AnsM|AnsR]):-check_3(L,M,H,[],AnsM),check_3(M,H,T,AnsR),!.
check_3([L,R|T],[AnsL|AnsR]):-check_3([],L,R,AnsL),check_3(L,R,T,AnsR).

check_test(X):- (X=1;X=2), write('correct'),nl ; write('error'),nl.

%check rightmost
%check_4([],M,R,Ans):- (M=@=bL),Ans=bL,!;(M=@=wL),Ans=wL,!; (M=@=bL;M=@=bD),(R=@=bL;R=@=wD;R=@=aL),Ans=bL ,!; (M=@=bL;M=@=bD),Ans=bD; (M=@=wL;M=@=wD),(R=@=wL;R=@=bD;R=@=aL),Ans=wL ,!; (M=@=wL;M=@=wD),Ans=wD.
%check leftmost
%check_4(L,M,[],[Ans|[]]):- (M=@=bL),Ans=bL,!;(M=@=wL),Ans=wL,!; (M=@=bL;M=@=bD),(L=@=bL,L=@=wD,L=@=aL),(M=@=bL;M=@=bD); M=@=b,Ans=bD ; (M=@=wL;M=@=wD),(L=@=wL,L=@=bD,L=@=aL),And=wL; (M=@=wL;M=@=wD),Ans=wD.
%check middle(recursive)
%check_4(L,M,R,[],Ans):- (M=@=bL),Ans=bL,!;(M=@=wL),Ans=wL,!; (M=@=bL;M=@=bD),(L=@=aL,L=@=bL,L=@=wD),Ans=bL; (M=@=bL;M=@=bD),(R=@=aL,R=@=bL,R=@=wD),Ans=bL; (M=@=bL;M=@=bD),Ans=bD; (M=@=wL;M=@=wD),(L=@=aL,L=@=wL,L=@=bD), Ans=wL; (M=@=wL;M=@=wD),(R=@=aL,R=@=wL,R=@=bD),Ans=wL; (M=@=wL;M=@=wD),Ans=wD.
check_4([],M,R,Ans):- M=@=bL,Ans=bL ; M=@=wL,Ans=wL ; M=@=wD,(R=@=wD;R=@=bL),Ans=wD ,!; Ans=wL; M=@=bD,((R=@=bD,R=@=wL),Ans=bD,!; Ans=bL).
check_4(L,M,[],[Ans|[]]):- M=@=bL,Ans=bL ; M=@=wL,Ans=wL ; M=@=wD,(L=@=wD;L=@=bL),Ans=wD ; M=@=wD,(L=@=wL;L=@=aL),Ans=wL ; M=@=bD,(L=@=bD;L=@=wL),Ans=bD ; M=@=bD,(L=@=bL;L=@=aL), Ans=bL.
%check_4(L,M,[],[Ans|[]]):- M=@=bL, Ans=bL; M=@=wL, Ans=wL; M=@=bD, L=@=bL, Ans=bL; M=@=bD, Ans=@=bD.
check_4(L,M,R,[],Ans):- M=@=bL,Ans=bL ; M=@=wL,Ans=wL ; M=@=wD,(L=@=wD;L=@=bL),(R=@=wD;R=@=bL),Ans=wD ,!; M=@=wD, Ans=wL; M=@=bD,(L=@=bD;L=@=wL),(R=@=bD,R=@=wL),Ans=bD,!; M=@=bD, Ans=bL.
%general
check_4(L,M,[H|T],[Ans|AnsR]):- check_4(L,M,H,[],Ans),check_4(M,H,T,AnsR),!.
check_4([L,R|T],[AnsL|AnsR]):-check_4([],L,R,AnsL),check_4(L,R,T,AnsR).

/*
*/
