init_row([],[]).
init_row(H,[],Ans):- H=@=b,Ans=bD ; H=@=w,Ans=wD ; H=@=s,Ans=aL;H=@=m,Ans=aL.
init_row([H|T],[Ans1|Ans2]):- init_row(H,[],Ans1), init_row(T,Ans2).
%Ans1/2 is a list, H is a list, T is list of list
init_board([],[]).
init_board([H|T],[AnsRow|AnsBoard]):- init_row(H,AnsRow), init_board(T,AnsBoard).
%find curr turn color
get_row_ele([H|T],LocationX,Ans):- LocationX=0, Ans=H ; NewX is LocationX-1, get_row_ele(T,NewX,Ans).
get_board_ele([Hlist|Tboard],LocationY,LocationX,Ans):- LocationY=@=0, get_row_ele(Hlist,LocationX,Ans); NewY is LocationY-1, get_board_ele(Tboard,NewY,LocationX,Ans).
/*======================*/

check_neighbour(Color,Cell,Ans):-
	Color=@=bD, (Cell=@=bL;Cell=@=aL), Ans=bL; 
	Color=@=bD, (Cell=@=bD;Cell=@=wL), Ans=bD;
	Color=@=bD, Cell=@=wD, Ans=err;

	Color=@=wD, (Cell=@=wL;Cell=@=aL), Ans=wL;
	Color=@=wD, (Cell=@=wD;Cell=@=bL), Ans=wD;
	Color=@=wD, Cell=@=bD, Ans=err.
%without left
update_cell_noleft([Tmid|Ttail],[Center,Mright|Mtail],[Bmid|Btail],Play,Ans):-
	Center=@=aL, Ans=aL,!;
	Center=@=x, Ans=x,!;
	
	(Center=@=bL; Center=@=bx), (Tmid=@=wL,Mright=@=wL,Bmid=@=wL), Ans=bD,!;
	(Center=@=wL; Center=@=wx), (Tmid=@=bL,Mright=@=bL,Bmid=@=bL), Ans=wD,!;

	Center=@=bL, Ans=bL,!;
	Center=@=wL, Ans=wL,!;

	Center=@=bD, (Tmid=@=wx;Mright=@=xw;Bmid=@=wx), Ans=bL,!;
	Center=@=wD, (Tmid=@=bx;Mright=@=bx;Bmid=@=bx), Ans=wL,!;

	(Center=@=bD; Center=@=bx),  
	(check_neighbour(bD,Tmid,Top),check_neighbour(bD,Mright,Right),check_neighbour(bD,Bmid,Bottom)), 
	(
		(Top=@=bL; Top=@=aL; Right=@=bL; Right=@=aL; Bottom=@=bL; Bottom=@=aL), Ans=bL,!; 
		(Top=@=bD, Right=@=bD, Bottom=@=bD), Ans=bD,! ;
		(Top=@=err;Right=@=err;Bottom=@=err), Play=@=b, Ans=bL,!;
		(Top=@=err;Right=@=err;Bottom=@=err), Play=@=w, Ans=bx,!;
		Ans=bD
	);
	Center=@=bx, Ans=bD,!;
	(Center=@=wD; Center=@=wx),
	(check_neighbour(wD,Tmid,Top),check_neighbour(wD,Mright,Right),check_neighbour(wD,Bmid,Bottom)), 
	(
		(Top=@=wL; Top=@=aL; Right=@=wL; Right=@=aL; Bottom=@=wL; Bottom=@=aL), Ans=wL,!; 
		(Top=@=wD, Right=@=wD, Bottom=@=wD), Ans=wD,!;
		(Top=@=err;Right=@=err;Bottom=@=err), Play=@=w, Ans=wL,!;
		(Top=@=err;Right=@=err;Bottom=@=err), Play=@=b, Ans=wx,!;
		Ans=wD
	);
	Center=@=wx, Ans=wD,!.
update_cell_noright([Tleft, Tmid],[Mleft,Center],[Bleft,Bmid],Play,Ans):-
	Center=@=aL, Ans=aL,!;
	Center=@=x, Ans=x,!;
	
	(Center=@=bL; Center=@=bx), (Mleft=@=wL,Tmid=@=wL,Bmid=@=wL), Ans=bD,!;
	(Center=@=wL; Center=@=wx), (Mleft=@=bL,Tmid=@=bL,Bmid=@=bL), Ans=wD,!;

	Center=@=bL, Ans=bL,!;
	Center=@=wL, Ans=wL,!;

	Center=@=bD, (Mleft=@=wx;Tmid=@=wx;Bmid=@=wx), Ans=bL,!;
	Center=@=wD, (Mleft=@=bx;Tmid=@=bx;Bmid=@=bx), Ans=wL,!;

	(Center=@=bD; Center=@=bx), 
	(check_neighbour(bD,Mleft,Left),check_neighbour(bD,Tmid,Top),check_neighbour(bD,Bmid,Bottom)), 
	(
		(Left=@=bL;	Left=@=aL; Top=@=bL; Top=@=aL; Bottom=@=bL; Bottom=@=aL), Ans=bL,!; 
		(Left=@=bD, Top=@=bD, Bottom=@=bD), Ans=bD,! ;
		(Left=@=err;Top=@=err;Bottom=@=err), Play=@=b, Ans=bL,!;
		(Left=@=err;Top=@=err;Bottom=@=err), Play=@=w, Ans=bx,!;
		Ans=bD
	);
	Center=@=bx, Ans=bD,!;
	(Center=@=wD; Center=@=wx),
	(check_neighbour(wD,Mleft,Left),check_neighbour(wD,Tmid,Top),check_neighbour(wD,Bmid,Bottom)),
	(
		(Left=@=wL; Left=@=aL; Top=@=wL; Top=@=aL; Bottom=@=wL; Bottom=@=aL), Ans=wL,!; 
		(Left=@=wD, Top=@=wD, Bottom=@=wD), Ans=wD,!;
		(Left=@=err;Top=@=err;Bottom=@=err), Play=@=w, Ans=wL,!;
		(Left=@=err;Top=@=err;Bottom=@=err), Play=@=b, Ans=wx,!;
		Ans=wD
	);
	Center=@=wx, Ans=wD,!.
update_cell_noleftbottom([Tmid|Ttail],[Center,Mright|Mtail],Play,Ans):-
	Center=@=aL, Ans=aL,!;
	Center=@=x, Ans=x,!;
	
	(Center=@=bL; Center=@=bx), (Tmid=@=wL,Mright=@=wL), Ans=bD,!;
	(Center=@=wL; Center=@=wx), (Tmid=@=bL,Mright=@=bL), Ans=wD,!;

	Center=@=bL, Ans=bL,!;
	Center=@=wL, Ans=wL,!;

	Center=@=bD, (Tmid=@=wx;Mright=@=wx), Ans=bL,!;
	Center=@=wD, (Tmid=@=bx;Mright=@=bx), Ans=wL,!;

	(Center=@=bD; Center=@=bx), 
	(check_neighbour(bD,Tmid,Top),check_neighbour(bD,Mright,Right)), 
	(

		(Top=@=bL; Top=@=aL; Right=@=bL;	Right=@=aL), Ans=bL,!; 
		(Top=@=bD, Right=@=bD), Ans=bD,! ;
		(Top=@=err;Right=@=err), Play=@=b, Ans=bL,!;
		(Top=@=err;Right=@=err), Play=@=w, Ans=bx,!;
		Ans=bD
	);
	Center=@=bx, Ans=bD,!;
	(Center=@=wD; Center=@=wx),
	(check_neighbour(wD,Tmid,Top),check_neighbour(wD,Mright,Right)), 
	(

		(Top=@=wL; Top=@=aL; Right=@=wL; Right=@=aL), Ans=wL,!; 
		(Top=@=wD, Right=@=wD), Ans=wD,!;
		(Top=@=err;Right=@=err), Play=@=w, Ans=wL,!;
		(Top=@=err;Right=@=err), Play=@=b, Ans=wx,!;
		Ans=wD
	);
	Center=@=wx, Ans=wD,!.
update_cell_norightbottom([Tleft, Tmid],[Mleft,Center],Play,Ans):-
	Center=@=aL, Ans=aL,!;
	Center=@=x, Ans=x,!;

	(Center=@=bL; Center=@=bx), (Mleft=@=wL,Tmid=@=wL), Ans=bD,!;
	(Center=@=wL; Center=@=wx), (Mleft=@=bL,Tmid=@=bL), Ans=wD,!;

	Center=@=bL, Ans=bL,!;
	Center=@=wL, Ans=wL,!;

	Center=@=bD, (Mleft=@=wx;Tmid=@=wx), Ans=bL,!;
	Center=@=wD, (Mleft=@=bx;Tmid=@=bx), Ans=wL,!;

	(Center=@=bD; Center=@=bx), 
	(check_neighbour(bD,Mleft,Left),check_neighbour(bD,Tmid,Top)), 
	(

		(Left=@=bL;	Left=@=aL; Top=@=bL; Top=@=aL), Ans=bL,!; 
		(Left=@=bD, Top=@=bD), Ans=bD,! ;
		(Left=@=err;Top=@=err), Play=@=b, Ans=bL,!;
		(Left=@=err;Top=@=err), Play=@=w, Ans=bx,!;
		Ans=bD
	);
	Center=@=bx, Ans=bD,!;
	(Center=@=wD; Center=@=wx), 
	(check_neighbour(wD,Mleft,Left),check_neighbour(wD,Tmid,Top)), 
	(

		(Left=@=wL; Left=@=aL; Top=@=wL; Top=@=aL), Ans=wL,!; 
		(Left=@=wD, Top=@=wD), Ans=wD,!;
		(Left=@=err;Top=@=err), Play=@=w, Ans=wL,!;
		(Left=@=err;Top=@=err), Play=@=b, Ans=wx,!;
		Ans=wD
	);
	Center=@=wx, Ans=wD,!.
update_cell_nobottom([Tleft, Tmid|Ttail],[Mleft,Center,Mright|Mtail],Play,Ans):-
	Center=@=aL, Ans=aL,!;
	Center=@=x, Ans=x,!;
	
	(Center=@=bL; Center=@=bx), (Mleft=@=wL,Tmid=@=wL,Mright=@=wL), Ans=bD,!;
	(Center=@=wL; Center=@=wx), (Mleft=@=bL,Tmid=@=bL,Mright=@=bL), Ans=wD,!;

	Center=@=bL, Ans=bL,!;
	Center=@=wL, Ans=wL,!;

	Center=@=bD, (Mleft=@=wx;Tmid=@=wx;Mright=@=wx), Ans=bL,!;
	Center=@=wD, (Mleft=@=bx;Tmid=@=bx;Mright=@=bx), Ans=wL,!;

	(Center=@=bD; Center=@=bx),  
	(check_neighbour(bD,Mleft,Left),check_neighbour(bD,Tmid,Top),check_neighbour(bD,Mright,Right)), 
	(

		(Left=@=bL;	Left=@=aL; Top=@=bL; Top=@=aL; Right=@=bL;	Right=@=aL), Ans=bL,!; 
		(Left=@=bD, Top=@=bD, Right=@=bD), Ans=bD,! ;
		(Left=@=err;Top=@=err;Right=@=err), Play=@=b, Ans=bL,!;
		(Left=@=err;Top=@=err;Right=@=err), Play=@=w, Ans=bx,!;
		Ans=bD
	);
	Center=@=bx, Ans=bD,!;
	(Center=@=wD; Center=@=wx), 
	(check_neighbour(wD,Mleft,Left),check_neighbour(wD,Tmid,Top),check_neighbour(wD,Mright,Right)), 
	(

		(Left=@=wL; Left=@=aL; Top=@=wL; Top=@=aL; Right=@=wL; Right=@=aL), Ans=wL,!; 
		(Left=@=wD, Top=@=wD, Right=@=wD), Ans=wD,!;
		(Left=@=err;Top=@=err;Right=@=err), Play=@=w, Ans=wL,!;
		(Left=@=err;Top=@=err;Right=@=err), Play=@=b, Ans=wx,!;
		Ans=wD
	);
	Center=@=wx, Ans=wD,!.
update_cell_nolefttop([Center,Mright|Mtail],[Bmid|Btail],Play,Ans):-
	Center=@=aL, Ans=aL,!;

	(Center=@=bL; Center=@=bx), (Mright=@=wL,Bmid=@=wL), Ans=bD,!;
	(Center=@=wL; Center=@=wx), (Mright=@=bL,Bmid=@=bL), Ans=wD,!;


	Center=@=bL, Ans=bL,!;
	Center=@=wL, Ans=wL,!;

	Center=@=bD, (Mright=@=wx;Bmid=@=wx), Ans=bL,!;
	Center=@=wD, (Mright=@=bx;Bmid=@=bx), Ans=wL,!;

	(Center=@=bD; Center=@=bx), 
	(check_neighbour(bD,Mright,Right),check_neighbour(bD,Bmid,Bottom)), 
	(

		(Right=@=bL;	Right=@=aL; Bottom=@=bL; Bottom=@=aL), Ans=bL,!; 
		(Right=@=bD, Bottom=@=bD), Ans=bD,! ;
		(Right=@=err;Bottom=@=err), Play=@=b, Ans=bL,!;
		(Right=@=err;Bottom=@=err), Play=@=w, Ans=bx,!;
		And=bD
	);
	Center=@=bx, Ans=bD,!;
	(Center=@=wD; Center=@=wx), 
	(check_neighbour(wD,Mright,Right),check_neighbour(wD,Bmid,Bottom)), 
	(

		(Right=@=wL; Right=@=aL; Bottom=@=wL; Bottom=@=aL), Ans=wL,!; 
		(Right=@=wD, Bottom=@=wD), Ans=wD,!;
		(Right=@=err;Bottom=@=err), Play=@=w, Ans=wL,!;
		(Right=@=err;Bottom=@=err), Play=@=b, Ans=wx,!;
		Ans=wD
	);
	Center=@=wx, Ans=wD,!.
update_cell_norighttop([Mleft,Center],[Bleft,Bmid],Play,Ans):-
	Center=@=aL, Ans=aL,!;

	(Center=@=bL; Center=@=bx), (Mleft=@=wL,Bmid=@=wL), Ans=bD,!;
	(Center=@=wL; Center=@=wx), (Mleft=@=bL,Bmid=@=bL), Ans=wD,!;

	Center=@=bL, Ans=bL,!;
	Center=@=wL, Ans=wL,!;

	Center=@=bD, (Mleft=@=wx;Bmid=@=wx), Ans=bL,!;
	Center=@=wD, (Mleft=@=bx;Bmid=@=bx), Ans=wL,!;

	(Center=@=bD; Center=@=bx),
	(check_neighbour(bD,Mleft,Left),check_neighbour(bD,Bmid,Bottom)), 
	(

		(Left=@=bL;	Left=@=aL; Bottom=@=bL; Bottom=@=aL), Ans=bL,!; 
		(Left=@=bD, Bottom=@=bD), Ans=bD,! ;
		(Left=@=err;Bottom=@=err), Play=@=b, Ans=bL,!;
		(Left=@=err;Bottom=@=err), Play=@=w, Ans=bx,!;
		Ans=bD
	);
	Center=@=bx, Ans=bD,!;
	(Center=@=wD; Center=@=wx), 
	(check_neighbour(wD,Mleft,Left),check_neighbour(wD,Bmid,Bottom)), 
	(

		(Left=@=wL; Left=@=aL; Bottom=@=wL; Bottom=@=aL), Ans=wL,!; 
		(Left=@=wD,Bottom=@=wD), Ans=wD,!;
		(Left=@=err;Bottom=@=err), Play=@=w, Ans=wL,!;
		(Left=@=err;Bottom=@=err), Play=@=b, Ans=wx,!;
		Ans=wD
	);
	Center=@=wx, Ans=wD,!.
update_cell_notop([Mleft,Center,Mright|Mtail],[Bleft,Bmid|Btail],Play,Ans):-
	Center=@=aL, Ans=aL,!;

	(Center=@=bL; Center=@=bx), (Mleft=@=wL,Mright=@=wL,Bmid=@=wL), Ans=bD,!;
	(Center=@=wL; Center=@=wx), (Mleft=@=bL,Mright=@=bL,Bmid=@=bL), Ans=wD,!;

	Center=@=bL, Ans=bL,!;
	Center=@=wL, Ans=wL,!;

	Center=@=bD, (Mleft=@=wx;Mright=@=wx;Bmid=@=wx), Ans=bL,!;
	Center=@=wD, (Mleft=@=bx;Mright=@=bx;Bmid=@=bx), Ans=wL,!;

	(Center=@=bD; Center=@=bx),
	(check_neighbour(bD,Mleft,Left),check_neighbour(bD,Mright,Right),check_neighbour(bD,Bmid,Bottom)), 
	(

		(Left=@=bL;	Left=@=aL; Right=@=bL;	Right=@=aL; Bottom=@=bL; Bottom=@=aL), Ans=bL,!; 
		(Left=@=bD, Right=@=bD, Bottom=@=bD), Ans=bD,! ;
		(Left=@=err;Right=@=err;Bottom=@=err), Play=@=b, Ans=bL,!;
		(Left=@=err;Right=@=err;Bottom=@=err), Play=@=w, Ans=bx,!;
		Ans=bD
	);
	Center=@=bx, Ans=bD,!;
	(Center=@=wD; Center=@=wx), 
	(check_neighbour(wD,Mleft,Left),check_neighbour(wD,Mright,Right),check_neighbour(wD,Bmid,Bottom)), 
	(

		(Left=@=wL; Left=@=aL; Right=@=wL; Right=@=aL; Bottom=@=wL; Bottom=@=aL), Ans=wL,!; 
		(Left=@=wD, Right=@=wD, Bottom=@=wD), Ans=wD,!;
		(Left=@=err;Right=@=err;Bottom=@=err), Play=@=w, Ans=wL,!;
		(Left=@=err;Right=@=err;Bottom=@=err), Play=@=b, Ans=wx,!;
		Ans=wD
	);
	Center=@=wx, Ans=wD,!.
%normal
update_cell([Tleft, Tmid|Ttail],[Mleft,Center,Mright|Mtail],[Bleft,Bmid|Btail],Play,Ans):-
	Center=@=aL, Ans=aL,!;

	(Center=@=bL; Center=@=bx), (Mleft=@=wL,Tmid=@=wL,Mright=@=wL,Bmid=@=wL), Ans=bD,!;
	(Center=@=wL; Center=@=wx), (Mleft=@=bL,Tmid=@=bL,Mright=@=bL,Bmid=@=bL), Ans=wD,!;

	Center=@=bL, Ans=bL,!;
	Center=@=wL, Ans=wL,!;

	Center=@=bD, (Mleft=@=wx;Tmid=@=wx;Mright=@=wx;Bmid=@=wx), Ans=bL,!;
	Center=@=wD, (Mleft=@=bx;Tmid=@=bx;Mright=@=bx;Bmid=@=bx), Ans=wL,!;

	(Center=@=bD; Center=@=bx), 
	(check_neighbour(bD,Mleft,Left),check_neighbour(bD,Tmid,Top),check_neighbour(bD,Mright,Right),check_neighbour(bD,Bmid,Bottom)), 
	(

		(Left=@=bL;	Left=@=aL; Top=@=bL; Top=@=aL; Right=@=bL;	Right=@=aL; Bottom=@=bL; Bottom=@=aL), Ans=bL,!; 
		(Left=@=bD, Top=@=bD, Right=@=bD, Bottom=@=bD), Ans=bD,! ;
		(Left=@=err;Top=@=err;Right=@=err;Bottom=@=err), Play=@=b, Ans=bL,!;
		(Left=@=err;Top=@=err;Right=@=err;Bottom=@=err), Play=@=w, Ans=bx,!;
		Ans=bD
	);
	Center=@=bx, Ans=bD,!;
	(Center=@=wD; Center=@=wx),
	(check_neighbour(wD,Mleft,Left),check_neighbour(wD,Tmid,Top),check_neighbour(wD,Mright,Right),check_neighbour(wD,Bmid,Bottom)), 
	(

		(Left=@=wL; Left=@=aL; Top=@=wL; Top=@=aL; Right=@=wL; Right=@=aL; Bottom=@=wL; Bottom=@=aL), Ans=wL,!; 
		(Left=@=wD, Top=@=wD, Right=@=wD, Bottom=@=wD), Ans=wD,!;
		(Left=@=err;Top=@=err;Right=@=err;Bottom=@=err), Play=@=w, Ans=wL,!;
		(Left=@=err;Top=@=err;Right=@=err;Bottom=@=err), Play=@=b, Ans=wx,!;
		Ans=wD
	);
	Center=@=wx, Ans=wD,!.
%without bottom.
update_row_nobottom([Thead,Ttail],[Mhead,Mtail],Play,[Ans|[]]):-
	update_cell_norightbottom([Thead,Ttail],[Mhead,Mtail],Play,Ans).

update_row_nobottom([],[Thead|Ttail],[Mhead|Mtail],Play,[Ans|AnsRow]):-
	update_cell_noleftbottom([Thead|Ttail],[Mhead|Mtail],Play,Ans), 
	update_row_nobottom([Thead|Ttail],[Mhead|Mtail],Play,AnsRow).

update_row_nobottom([Thead|Ttail],[Mhead|Mtail],Play,[Ans|AnsRow]):-
	update_cell_nobottom([Thead|Ttail],[Mhead|Mtail],Play,Ans),
	update_row_nobottom(Ttail,Mtail,Play,AnsRow).
%without top.
update_row_notop([Mhead,Mtail],[Bhead,Btail],Play,[Ans|[]]):-
	update_cell_norighttop([Mhead,Mtail],[Bhead,Btail],Play,Ans).

update_row_notop([],[Mhead|Mtail],[Bhead|Btail],Play,[Ans|AnsRow]):-
	update_cell_nolefttop([Mhead|Mtail],[Bhead|Btail],Play,Ans),
	update_row_notop([Mhead|Mtail],[Bhead|Btail],Play,AnsRow).

update_row_notop([Mhead|Mtail],[Bhead|Btail],Play,[Ans|AnsRow]):-
	update_cell_notop([Mhead|Mtail],[Bhead|Btail],Play,Ans),
	update_row_notop(Mtail,Btail,Play,AnsRow).

%dummy for 2, should for 1 only
update_row([Thead,Ttail],[Mhead,Mtail],[Bhead,Btail],Play,[Ans|[]]):-
	update_cell_noright([Thead,Ttail],[Mhead,Mtail],[Bhead,Btail],Play,Ans).

update_row([],[Thead|Ttail],[Mhead|Mtail],[Bhead|Btail],Play,[Ans|AnsRow]):- 
	update_cell_noleft([Thead|Ttail],[Mhead|Mtail],[Bhead|Btail],Play,Ans), 
	update_row([Thead|Ttail],[Mhead|Mtail],[Bhead|Btail],Play,AnsRow).

update_row([Thead|Ttail],[Mhead|Mtail],[Bhead|Btail],Play,[Ans|AnsRow]):- 
	update_cell([Thead|Ttail],[Mhead|Mtail],[Bhead|Btail],Play,Ans), 
	update_row(Ttail,Mtail,Btail,Play,AnsRow).
%dummy for 2, should for 1 only
update_board([L1,L2],Play,[AnsRow|[]]):-update_row_nobottom([],L1,L2,Play,AnsRow).

update_board([L1,L2,L3|L4],Play,[AnsRow|AnsBoard]):-
	update_row([],L1,L2,L3,Play,AnsRow), 
	update_board([L2,L3|L4],Play,AnsBoard).

update_board([],[L1,L2|L3],Play,[AnsRow|AnsBoard]):-
	update_row_notop([],L1,L2,Play,AnsRow),
	update_board([L1,L2|L3],Play,AnsBoard).
%update_row([Top|Mid|BtmTail],Play,[Ans|AnsRow]):-update_cell(Top,Mid,BtmTail,Play,[Ans]), update_row(Top,Mid,BtmTail,Play,AnsRow).
check_board(Board,Play,AnsBoard):- 
	update_board([],Board,Play,AnsBoard1), 
	(	Board=@=AnsBoard1, AnsBoard=AnsBoard1 ,!;
 		check_board(AnsBoard1,Play,AnsBoard)).
%life_and_death([[s,s,s,s,s],[w,w,w,w,s],[w,b,b,w,s],[b,m,b,w,s],[m,b,b,w,s]],move(2,0),LDBoard).
life_and_death(Raw,move(Y,X),LDBoard):-get_board_ele(Raw,Y,X,Play), init_board(Raw,Board), check_board(Board,Play,LDBoard).
