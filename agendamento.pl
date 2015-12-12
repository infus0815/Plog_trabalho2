:- use_module(library(clpfd)).
:- use_module(library(lists)).



% tipos de participantes
% 1 - presenca obrigatoria
% 2 - presenca obrigatoria + distancia
% 3 - presenca preferencial
% 4 - presenca preferencial + distancia
% 5 - presenca opcional

% tipo de sala
% 1 - normal
% 2 - c/ videoconferencia


%Result = [Idsala1(tam4),Id1,Id2,Id3,0,Idsala2,Id4,Id5,...]


%reuniao [duracao,[ids obg],[ids obg dist],[ids pref]]
reuniao1([15,[1,2,3],[7],[4,5,6]]).
reuniao2([10,[1,2,3],[],[4,5,6]]).
reuniao3([11,[1,2,3],[7],[4,5,6]]).
reuniao4([12,[1,2,3],[],[4,5,6]]).

reuniaoteste1([1,2,3],[1,4,5]).
reuniaoprefteste1([1,2,3],[3,5,5]).
%sala[id,capacidade,tipo].
salasteste1([10,10,20,10,10,20]-[1,1,2,1,1,2]).


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
	H #= V #<=> C2,
	C #= C1 + C2.
	
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
	
teste :-
	reuniao1(A),
	reuniao2(B),
	reuniao3(C),
	reuniao4(D),
	L = [A,B,C,D],
	salasteste1(S),
	agendamento(L,S).	

	
agendamento(ReunioesPretendidas,SalasCap-SalasCar) :- 
	length(ReunioesPretendidas,Rsize),
	length(SS,Rsize),
	length(ES,Rsize),
	length(SalasReuniao,Rsize),
	length(Reunioes,Rsize),
	createDuration(ReunioesPretendidas,D),
	createTasks(SS,D,ES,SalasReuniao,ReunioesTask),
	createMachines(SalasCap,Machines,1),
	
	
	domain(SS,1,100),
	domain(D,1,100),
	domain(ES,1,100),
	domain(SalasReuniao,1,Rsize),
	domain([End],1,100),
	maximum(End,ES),
	
	assign_salas_reuniao(SalasReuniao,ReunioesPretendidas,SalasCap-SalasCar),
	assign_reuniao_participantes(Reunioes,ReunioesPretendidas,SalasReuniao,SalasCap,CountOpt),
	
	
	cumulatives(ReunioesTask,Machines,[bound(upper)]),
	
	
	
	append(SS,[End],Vars1),
	append(Vars1,SalasReuniao,Vars),
	labeling([],Vars),
	write(SS),nl,write(End),nl,write(SalasReuniao).
	

assign_salas_reuniao(_,[],_).
assign_salas_reuniao([HSalasReuniao|TSalasReuniao],[ [_,MO,MOD,_MP] |TReunioesPretendidas],SalasCap-SalasCar) :- 
	element(I,SalasCar,V),
	element(I,SalasCap,Cap),
	length(MOD,X),
	length(MO,Y),
	Cap in 0..100,
	Y  #=< Cap,
	(V #= 2 #/\ X #> 0) #\/ X #= 0,
	HSalasReuniao #= I,
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
	
	
	
/*avoid_same_time(SS,ES,Result) :- */
	
        
        
        
        