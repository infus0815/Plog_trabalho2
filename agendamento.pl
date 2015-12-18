:- use_module(library(clpfd)).
:- use_module(library(lists)).



% tipo de sala
% 1 - normal
% 2 - c/ videoconferencia


%Result = [Idsala1(tam4),Id1,Id2,Id3,0,Idsala2,Id4,Id5,...]


%reuniao [duracao,[ids obg],[ids obg dist],[ids pref]]
reuniao0([15,[1],[],[2]]).
reuniao1([15,[1,2,3],[],[4,5,6]]).
reuniao2([10,[9,10,11],[],[12,13,14]]).
reuniao3([11,[1,2,3],[7],[4,5,6]]).
reuniao4([12,[1,2,3],[],[4,5,6]]).
reuniao5([15,[1,2,3,4,5,6,7,8,9,10,11,12,13],[],[14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]]).
reuniao6([10,[1,2,3,4,5,6,7,14,15,16,17,18,19],[],[8,9,10,11,12,13,20,21,22,23,24,25,26,27,28,29,30]]).


%sala[[capacidades]-[tipos]]. id da sala e a posiçao na lista
salasteste0([20,20]-[1,2]).
salasteste1([10,10,20,10,10,20]-[1,1,2,1,1,2]).



%%%%%%%%%%%%UTILS%%%%%%%%%%%%%%
getMaxValue(R_ids,Value) :- 
	maximum(Value,R_ids).

getTotalSize([],0).
getTotalSize([Sala1|T],Size) :- 
	getTotalSize(T,TSize),
	element(2,Sala1,X),
	X1 #= X + 1,
	Size #= X1 + TSize.
	
	
	
count_equals(_,[],0).
count_equals(V,[H|T],C) :- 
	count_equals(V,T,C1),
	(H #= V) #<=> C2,
	C #= C1 + C2.

flatten(List, FlatList) :-
 flatten(List, [], FlatList0), !,
 FlatList = FlatList0.

 flatten(Var, Tl, [Var|Tl]) :- var(Var), !.
 flatten([], Tl, Tl) :- !.
 flatten([Hd|Tl], Tail, List) :- !,
     flatten(Hd, FlatHeadTail, List),
     flatten(Tl, Tail, FlatHeadTail).
flatten(NonList, Tl, [NonList|Tl]).


integer_div(N,Q,M) :- M #= N/Q , integer(M).
integer_div(N,Q,M) :- N1 #= N mod Q , N2 #= N-N1 , M #= N2/Q.

reset_timer :- statistics(walltime,_).  
print_time :-
        statistics(walltime,[_,T]),
        TS is ((T//10)*10)/1000,
        nl, write('Time: '), write(TS), write('s'), nl, nl.

%%%%%%%%%inits%%%%%%%%%%%%%
createDuration([],[]).
createDuration([[Duration,_,_,_]|TReunioesPretendidas],D) :- 
	createDuration(TReunioesPretendidas,DT),
	append([Duration],DT,D).
	
createTasks([],_,_,_,[]).
createTasks([Hss|Tss],[Hd|Td],[He|Te],[Hm|Tm],L) :-
	createTasks(Tss,Td,Te,Tm,Ltemp),
	append([task(Hss,Hd,He,1,Hm)],Ltemp,L).

createMachines([],[],_).	
createMachines([_Hsala|Tsala],L,Count) :-
	Count1 is Count + 1,
	createMachines(Tsala,Ltemp,Count1),
	append([machine(Count,1)],Ltemp,L).
	
	
%%%%%%%%%%%%%tests%%%%%%%%%%%%%
teste0 :-
        reuniao1(A),
        L = [A],
        salasteste0(S),
        agendamento(L,S).

teste1 :-
	reuniao1(A),
	reuniao2(B),
	reuniao3(C),
	reuniao4(D),
	L = [A,B,C,D],
	salasteste1(S),
	agendamento(L,S).

teste2 :-
        reuniao1(A),
        reuniao2(B),
        reuniao3(C),
        reuniao4(D),
        reuniao1(E),
        reuniao2(F),
        reuniao3(G),
        reuniao4(H),
        L = [A,B,C,D,E,F,G,H],
        salasteste1(S),
        agendamento(L,S).

teste3 :-
        reuniao1(A),
        reuniao2(B),
        reuniao3(C),
        reuniao4(D),
        reuniao1(E),
        reuniao2(F),
        reuniao3(G),
        reuniao4(H),
        reuniao1(I),
        reuniao2(J),
        reuniao3(K),
        reuniao4(M),
        L = [A,B,C,D,E,F,G,H,I,J,K,M],
        salasteste1(S),
        agendamento(L,S).

teste4 :-
        reuniao1(A),
        reuniao2(B),
        reuniao3(C),
        reuniao4(D),
        reuniao5(E),
        reuniao6(F),
        L = [A,B,C,D,E,F],
        salasteste1(S),
        agendamento(L,S).

teste5 :-
        reuniao1(A),
        reuniao2(B),
        reuniao3(C),
        reuniao4(D),
        reuniao1(E),
        reuniao2(F),
        reuniao3(G),
        reuniao4(H),
        reuniao1(I),
        reuniao2(J),
        reuniao5(K),
        reuniao6(M),
        L = [A,B,C,D,E,F,G,H,I,J,K,M],
        salasteste1(S),
        agendamento(L,S).

teste6 :-
        reuniao1(A),
        reuniao2(B),
        reuniao3(C),
        reuniao4(D),
        reuniao1(E),
        reuniao2(F),
        reuniao3(G),
        reuniao4(H),
        reuniao5(K),
        reuniao6(M),
        L = [A,B,C,D,E,F,G,H,K,M],
        salasteste1(S),
        agendamento(L,S).
teste7 :-
        reuniao1(A),
        reuniao2(B),
        reuniao3(C),
        reuniao4(D),
        reuniao5(E),
        reuniao6(F),
        L = [A,B,C,D,E,F],
        salasteste0(S),
        agendamento(L,S).	


%%%%%%%%%%%%%%prints%%%%%%%%%%%%%%
getMembros([],[],[]).
getMembros([HR|TR],[HRP|TRP],Result) :- 
	HR =:= 1,!,
	getMembros(TR,TRP,R1),
	append([HRP],R1,Result).
	
getMembros([HR|TR],[_HRP|TRP],Result) :- 
	HR =:= 0,!,
	getMembros(TR,TRP,Result).
	
	

printReuniao(HReunioes,HSS,HES,HReunioesPretendidas,HSalasReuniao,SalasCap-SalasCar) :- 
	Hinicio is 8 + div(HSS, 60),
        Minicio is mod(HSS, 60),
        Hfim is 8 + div(HES, 60),
        Mfim is mod(HES, 60),
        write('Reuniao tera inicio as '),write(Hinicio),write(':'),write(Minicio),nl,
        write('Reuniao tera fim as '),write(Hfim),write(':'),write(Mfim),nl,
        write('Reuniao a efectuar na sala '),write(HSalasReuniao),write(' com a capacidade '),element(HSalasReuniao,SalasCap,X),write(X),
        write(' do tipo '),element(HSalasReuniao,SalasCar,Y),write(Y),nl,
        flatten_reunioes_pretendidas(HReunioesPretendidas,HRP),
        getMembros(HReunioes,HRP,Membros),
        write('Lista de ids de pessoas a participar '),write(Membros),nl,nl.
        
printResult([],[],[],[],[],_,_).
printResult([HReunioes|TReunioes],[HSS|TSS],[HES|TES],[ HReunioesPretendidas |TReunioesPretendidas],[HSalasReuniao|TSalasReuniao],SalasCap-SalasCar,Count) :- 
	Count1 is Count + 1,
	write('Reuniao id '),write(Count1),nl,
	printReuniao(HReunioes,HSS,HES,HReunioesPretendidas,HSalasReuniao,SalasCap-SalasCar),
	printResult(TReunioes,TSS,TES,TReunioesPretendidas,TSalasReuniao,SalasCap-SalasCar,Count1).


%%%%%%%%%%%%%%%%CODE%%%%%%%%%	
agendamento(ReunioesPretendidas,SalasCap-SalasCar) :- 
	length(ReunioesPretendidas,Rsize),
	length(SalasCap,NSalas),
	length(SS,Rsize),
	length(ES,Rsize),
	length(SalasReuniao,Rsize),
	length(Reunioes,Rsize),
	createDuration(ReunioesPretendidas,D),
	createTasks(SS,D,ES,SalasReuniao,ReunioesTask),
	createMachines(SalasCap,Machines,1),
	
	
	domain(SS,1,1000),
	domain(D,1,1000),
	domain(ES,1,1000),
	domain(SalasReuniao,1,NSalas),
	domain([End],1,1000),
	domain([Evaluation],-100000,100000),
	maximum(End,ES),
	
	assign_salas_reuniao(SalasReuniao,ReunioesPretendidas,SalasCap-SalasCar),
	assign_reuniao_participantes(Reunioes,ReunioesPretendidas,SalasReuniao,SalasCap,CountOpt),
	time_constraint(Reunioes,ReunioesPretendidas,SS,ES),
	
	cumulatives(ReunioesTask,Machines,[bound(upper)]),
	
	%evaluation
	integer_div(CountOpt,End,R),
	Evaluation #= R,
	
	append(SS,[End],Vars1),
	append(Vars1,SalasReuniao,Vars2),
	append(Vars2,Reunioes,Vars3),
	append(Vars3,[CountOpt],Vars4),
	append(Vars4,[Evaluation],Vars5),
	
	
	
	flatten(Vars5,Vars),
	
	
	reset_timer,
	labeling([maximize(Evaluation)],Vars),
        print_time,
        fd_statistics,write('----------------------'), nl,
	printResult(Reunioes,SS,ES,ReunioesPretendidas,SalasReuniao,SalasCap-SalasCar,0).
	

assign_salas_reuniao(_,[],_).
assign_salas_reuniao([HSalasReuniao|TSalasReuniao],[ [_,MO,MOD,_MP] |TReunioesPretendidas],SalasCap-SalasCar) :- 
	element(I1,SalasCar,1),
	element(I1,SalasCar,1),
	element(I2,SalasCar,2),
	element(I2,SalasCar,2),
	element(I3,SalasCap,Cap),
	length(MOD,X),
	length(MO,Y),
	Cap in 0..100,
	Y  #=< Cap,
	(X #> 0 #/\ HSalasReuniao #= I2) #\/ (X #= 0 #/\ (HSalasReuniao #= I1 #\/ HSalasReuniao #= I2)),
	I3 #= HSalasReuniao,
	assign_salas_reuniao(TSalasReuniao,TReunioesPretendidas,SalasCap-SalasCar).


force_obrigatorios(_HReunioes,[],[],_).
force_obrigatorios(HReunioes,[],[_HMOD|TMOD],Id) :- 
	Id1 is Id + 1,
	element(Id1,HReunioes,1), %força que user HMOD exista na reuniao
	force_obrigatorios(HReunioes,[],TMOD,Id1).
force_obrigatorios(HReunioes,[_HMO|TMO],MOD,Id) :- 
	Id1 is Id + 1,
	element(Id1,HReunioes,1), %força que user HMO exista na reuniao
	force_obrigatorios(HReunioes,TMO,MOD,Id1).




	
assign_reuniao_participantes(_,[],[],_,0).	
assign_reuniao_participantes([HReunioes|TReunioes],[ [_,MO,MOD,MP] |TReunioesPretendidas],[HSR|TSR],SalasCap,TotalOpt) :- 
	length(MO,NumObg),
	length(MOD,NumObgDist),
	length(MP,NumPref),
	
	NumParticipantes is NumObg + NumObgDist + NumPref,
	length(HReunioes,NumParticipantes),
	domain(HReunioes,0,1),

	
	force_obrigatorios(HReunioes,MO,MOD,0),
	
	
	%evita ultrapassar limite da sala
	sum(HReunioes,#=,CountPartipantes),
	CountPartipantesE #= CountPartipantes - NumPref,
	element(HSR,SalasCap,Cap),
	CountPartipantesE #=< Cap,
	
	NumParticipantesObg #= NumObg + NumObgDist,
	length(ListObg,NumParticipantesObg),
	append(ListObg,ListOpt,HReunioes),
	sum(ListOpt,#=,CountOpt1),
	
	assign_reuniao_participantes(TReunioes,TReunioesPretendidas,TSR,SalasCap,CountOpt2),
	
	TotalOpt #= CountOpt1 + CountOpt2.
	
flatten_reunioes_pretendidas([_,MO,MOD,MP],Result) :- 
	append(MO,MOD,R1),
	append(R1,MP,Result).
	

%count = 0 if intercept and 1 if no interception
check_intercept_reunioes(SS1,ES1,SS2,ES2,Count) :- 
	(SS1 #>= ES2) #<=> C1, %Reuniao2 antes de Reuniao1
	(SS2 #>= ES1) #<=> C2, %Reuniao1 antes de Reuniao2
	Count #= C1 + C2.
	

count_equals2(_,_,[],_,0).
count_equals2(V,B,[H1|T1],[H2|T2],C) :- 
	count_equals2(V,B,T1,T2,C1),
	(H1 #= V) #<=> C2,
	C3 #= B * H2 * C2,
	C #= C1 + C3.
	
		
%returns number of same people	
count_equals3([],_,_,_,0).
count_equals3([HV|TV],[HB|TB],RP2,R2,C) :-
	count_equals3(TV,TB,RP2,R2,C1),
	count_equals2(HV,HB,RP2,R2,C2),
	C #= C1 + C2.
	
	

%evita mesmas pessoas em duas reunioes ao mesmo tempo
avoid_same_time(_Reuniao,_ReuniaoPret,_SS,_ES,[],[],[],[]).
avoid_same_time(Reuniao,ReuniaoPret,SS,ES,[HReunioes|TReunioes],[HReunioesPretendidas|TReunioesPretendidas],[HSS|TSS],[HES|TES]) :- 
	check_intercept_reunioes(SS,ES,HSS,HES,C1),
	flatten_reunioes_pretendidas(ReuniaoPret,ReuniaoPret1),
	flatten_reunioes_pretendidas(HReunioesPretendidas,ReuniaoPret2),
	count_equals3(ReuniaoPret1,Reuniao,ReuniaoPret2,HReunioes,C2),
	C1 #= 1 #\/ C2 #= 0,
	avoid_same_time(Reuniao,ReuniaoPret,SS,ES,TReunioes,TReunioesPretendidas,TSS,TES).

time_constraint([],[],[],[]).
time_constraint([HReunioes|TReunioes],[HReunioesPretendidas|TReunioesPretendidas],[HSS|TSS],[HES|TES]) :- 
	avoid_same_time(HReunioes,HReunioesPretendidas,HSS,HES,TReunioes,TReunioesPretendidas,TSS,TES),
	time_constraint(TReunioes,TReunioesPretendidas,TSS,TES).
	
	
	
        
        
        
        