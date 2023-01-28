p([H|T],H,T).

max(X, [X]).
max(H, [H|T]) :- max(N,T), H >= N.
max(N, [H|T]) :- max(N,T), N > N.

make_date(Y,M,D,date(Y,M,D)).
get_year(date(Y,_,_),Y).
get_month(date(_,M,_),M).
get_day(date(_,_,D),D).

set_year(Y,date(_,M,D),date(Y,M,D)).
set_month(M,date(Y,_,D),date(Y,M,D)).
set_day(D,date(Y,M,_),date(Y,M,D)).

next_year(Today,NextYear):-get_year(Today,Y), NY is Y+1, set_year(NY,Today,NextYear).

check_black(B,Ans):-B=@=a,Ans=k ; B=@=w,Ans=l.
life_and_death([H|T],BoardCopy):- check_black(H,HY), BoardCopy=[HY|T].
life_and_death2([H|T],BoardCopy):- check_black2(H,HY,T,Ans), BoardCopy=Ans.


check_black2([],[]).
check_black2(H,[],Ans):- H=@=b,Ans=bL ; H=@=w,Ans=wL ; H=@=s,Ans=aL;H=@=m,Ans=aL.
check_black2([H|T],[Ans1|Ans2]):- check_black2(H,[],Ans1), check_black2(T,Ans2).

%check_3([],[]).
%check_3(L,[],[],[]).
%check_3([],M,R,Ans):- M=@=b,R=@=b,Ans=bL,! ; M=@=w,R=@=w,Ans=wL,! ; M=@=b,R=@=w,Ans=wD,! ; M=@=w,R=@=b,Ans=bD,!.
%check_3(L,M,[],Ans):- L=@=b,M=@=b,Ans=bL,! ; L=@=w,M=@=w,Ans=wL,! ; L=@=b,M=@=w,Ans=wD,! ; L=@=w,M=@=b,Ans=bD,!.

%check_3([L,M,R,R2|T],[AnsL,AnsM,AnsR]):-check_3(L,M,R,AnsL),check_3(M,R,R2,AnsM),check_3([M,R|T],AnsR).

check_3([w],[Ans]):-Ans=wD.
check_3([b],[Ans]):-Ans=bD.
check_3([s],[Ans]):-Ans=aL.
check_3([m],[Ans]):-Ans=aL.

%check_3([L,R],[Ans1,Ans2]):- L=@=b,R=@=b,Ans1=bL,Ans2=bL,! ; L=@=w,R=@=w,Ans1=wL,Ans2=wL,! ; L=@=b,R=@=w,Ans1=bD,Ans2=wD,! ; L=@=w,R=@=b,Ans1=wD,Ans2=bD,!.

%check_3([],M,R,Ans):- M=@=b,R=@=b,Ans=bL,! ; M=@=w,R=@=w,Ans=wL,! ; M=@=b,R=@=w,Ans=bD,! ; M=@=w,R=@=b,Ans=wD,!.
%check_3(L,M,[],Ans):- L=@=b,M=@=b,Ans=bL,! ; L=@=w,M=@=w,Ans=wL,! ; L=@=b,M=@=w,Ans=wD,! ; L=@=w,M=@=b,Ans=bD,!.
%check_3(L,M,R,Ans):- L=@=b,M=@=b,Ans=bL,! ; M=@=b,R=@=b,Ans=bL,! ; L=@=w,M=@=b,R=@=w,Ans=bD,! ; L=@=w,M=@=w,Ans=wL,! ; M=@=w,R=@=w,Ans=wL,! ; L=@=b,M=@=w,R=@=b,Ans=wD,!.
%check_3(M,[],[],AnsR):-AnsR=x.
check_3([],M,R,Ans):- M=@=b,R=@=b,Ans=bL;M=@=w,R=@=w,Ans=wL ; M=@=b,R=@=w,Ans=bD;M=@=w,R=@=b,Ans=wD ; M=@=m,Ans=aL;M=@=s,Ans=aL ; M=@=b,R=@=s,Ans=bL;M=@=b,R=@=m,Ans=bL ; M=@=w,R=@=s,Ans=wL;M=@=w,R=@=m,Ans=wL.
check_3(L,M,[],[Ans|[]]):- L=@=b,M=@=b,Ans=bL ; L=@=w,M=@=w,Ans=wL ; L=@=b,M=@=w,Ans=wD ; L=@=w,M=@=b,Ans=bD ; M=@=m,Ans=aL; M=@=s,Ans=aL ; L=@=s,M=@=b,Ans=bL;L=@=m,M=@=b,Ans=bL ; L=@=s,M=@=w,Ans=wL;L=@=m,M=@=w,Ans=wL.
check_3(L,M,R,[],Ans):- L=@=b,M=@=b,Ans=bL ; M=@=b,R=@=b,Ans=bL ; L=@=w,M=@=b,R=@=w,Ans=bD ; L=@=w,M=@=w,Ans=wL ; M=@=w,R=@=w,Ans=wL ; L=@=b,M=@=w,R=@=b,Ans=wD ; M=@=m,Ans=aL; M=@=s,Ans=aL ; L=@=s,M=@=b,Ans=bL;L=@=m,M=@=b,Ans=bL ; L=@=s,M=@=w,Ans=wL;L=@=m,M=@=w,Ans=wL ; M=@=b,R=@=s,Ans=bL;M=@=b,R=@=m,Ans=bL ; M=@=w,R=@=s,Ans=wL;M=@=w,R=@=m,Ans=wL.
check_3(L,M,[H|T],[AnsM|AnsR]):-check_3(L,M,H,[],AnsM),check_3(M,H,T,AnsR),!.
check_3([L,R|T],[AnsL|AnsR]):-check_3([],L,R,AnsL),check_3(L,R,T,AnsR).

sub_test(X,Y):-sub_string(X,0,1,_,-Y).
test(X):- X=10 ; Y is X+1 ,write(X), test(Y),!.
test1(X):- X=end ; write(X),nl, read(W), test1(W).

check_it(Board,Board2):- Board=Board2 ; read(I1),nl,read(I2), check_it(I1,I2).

check_board(Board,[],Ans):-check_3(Board,List), write('List1='), write(List),nl,check_3(List,List2) , write('List2='), write(List2),nl, List=List2, Ans=List2 ; check_board(List2,[],Ans).
check_board(Board,Ans):- check_black2(Board,List),write('List1='), write(List), check_3(Board,List2),write('List2='), write(List2), List=List2.% ; check_board(List2,[],Ans).
