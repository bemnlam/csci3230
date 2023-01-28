init_row([],[],Num):-Num=0.
init_row(H,[],Ans,Num):- H=@=b,Ans=bD, Num=0 ; H=@=w,Ans=wD, Num=0 ; (H=@=s;H=@=m),Ans=aL, Num=1.
init_row([H|T],[Ans1|Ans2],Num):- init_row(H,[],Ans1,Num1), init_row(T,Ans2,Num2), Num is Num1+Num2.
%Ans1/2 is a list, H is a list, T is list of list
init_board([],[],Num):-Num=0.
init_board([H|T],[AnsRow|AnsBoard],Num):-init_row(H,AnsRow,Num1), init_board(T,AnsBoard,Num2), Num is Num1+Num2.
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
fill_row([],[],Play).
fill_row(H,[],Ans,Play):- 
	Play=@=b, (H=@=b,Ans=bL ; H=@=w,Ans=wD),!;
	Play=@=w, (H=@=b,Ans=bD ; H=@=w,Ans=wL).
fill_row([H|T],[Ans1|Ans2],Play):- fill_row(H,[],Ans1,Play), fill_row(T,Ans2,Play).

fill_board([],[],Play).
fill_board([H|T],[AnsRow|AnsBoard],Play):- fill_row(H,AnsRow,Play), fill_board(T,AnsBoard,Play).

life_and_death(Raw,move(Y,X),LDBoard):- 
(get_board_ele(Raw,Y,X,Play), init_board(Raw,Board,Num), Num=@=0, fill_board(Raw,LDBoard,Play)),! ;
(get_board_ele(Raw,Y,X,Play), init_board(Raw,Board,Num), check_board(Board,Play,LDBoard)),! .

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
	append(S1,S2,ScoreSet),!.

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

cal_score(BoardOri,[RowID,ColID],Play,Score):- 
	put_stone_board(BoardOri,[RowID,ColID],Play,Board), 
	life_and_death(Board,move(RowID,ColID),LDBoard),
	score_move(Play,BoardOri,LDBoard,Score),!.
%write(Board),write(' : '),write(LDBoard),write(' : '),write(Score),nl.

findlen([],X):-X=0.
findlen([X|Tail],Count):-findlen(Tail,Prev), Count is Prev + 1, write(X).

cal_all_score(_,[],_,[]).
cal_all_score(BoardOri,[[RowID,ColID]|T],Play,[Score|SRow]):-
	(RowID=@=n, ColID=@=n, Score is -1000,!;
	cal_score(BoardOri,[RowID,ColID],Play,Score)),
	cal_all_score(BoardOri,T,Play,SRow).

max([X],X).
max([X|Xs],X):- max(Xs,Y), X >=Y.
max([X|Xs],N):- max(Xs,N), N > X.

min([X],X).
min([X|Xs],X):- min(Xs,Y), X =<Y.
min([X|Xs],N):- min(Xs,N), N < X.

indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1+1.

get_row_ele([H|T],LocationX,Ans):- LocationX=0, Ans=H ; NewX is LocationX-1, get_row_ele(T,NewX,Ans).

%find_m_board([H|T],RowID,ColID,MoveSet,ScoreSet):-
%find_good_move(Play,Depth,BoardOri,BestMove,ScoreOfBestMove):-
	%Depth < 0, BestMove=error, ScoreOfBestMove=[];
	%Depth =@= 0 , BestMove=nil, ScoreOfBestMove=inf.
%find_good_move(_,_,_,move(0,3),10).
find_good_move(Play,Depth,BoardOri,BestMove,BestScore):-
Depth < 0, BestMove=error, BestScore=[],!;
Depth =@= 0 , BestMove=nil, BestScore=inf,!;
Depth =@= 1 ,
	find_m_board(BoardOri,0,0,MoveSet1,_),
	delete(MoveSet1,[n,n],MoveSet),
	cal_all_score(BoardOri,MoveSet,Play,ScoreSet),
	max(ScoreSet,BestScore),
	indexOf(ScoreSet, BestScore, Index),
	get_row_ele(MoveSet,Index,[RowID,ColID]),
	BestMove=move(RowID,ColID),
	!;
Depth > 1, Depth < 4,
	(Play=@=b, NextPlay=w; Play=@=w, NextPlay=b),
	find_m_board(BoardOri,0,0,MoveSet1,_),
	delete(MoveSet1,[n,n],MoveSet),
	D is Depth-1,
	find_next_scoreset(BoardOri,MoveSet,NextPlay,D,NextScoreSet),
	min(NextScoreSet,Bs),
	indexOf(NextScoreSet, Bs, Index),
	get_row_ele(MoveSet,Index,[RowID,ColID]),
	BestMove=move(RowID,ColID),
	BestScore is 0-Bs,
	!.
	%find_next_scoreset(NextScoreSet),
	%write(NextScoreSet),nl,
	%given MoveSet , ScoreSet,
find_next_scoreset(_,[],_,_,[]).
find_next_scoreset(Board,[Move|Tmove],Play,Depth,[NextBestScore|Tscore]):-
	(Play=@=b,PrevPlay=w;Play=@=w,PrevPlay=b),
	put_stone_board(Board,Move,PrevPlay,TempBoard),
	
	find_good_move(Play,Depth,TempBoard,_,TempBestScore),
	NextBestScore is TempBestScore-1,
	write(TempBoard),nl,
	find_next_scoreset(Board,Tmove,Play,Depth,Tscore),!.
