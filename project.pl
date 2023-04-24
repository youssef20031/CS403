% Predicate to update the teaching load of a TA
ta_slot_assignment(TAs, RemTAs, Name) :-
    % Find the TA with the given name
    member(TA, TAs),
    TA = ta(Name, Load),
    % Decrement the teaching load of the TA by 1
    NewLoad is Load - 1,
    % Create a new TA structure with the updated teaching load
    NewTA = ta(Name, NewLoad),
    % Replace the old TA with the new TA in the list of TAs
    select(TA, TAs, NewTA, RemTAs).


max_slots_per_day(DaySched, Max) :-
    flatten(DaySched, [X|T]),
    max_slots_per_day_helper([X|T], Max).


max_slots_per_day_helper([],_).	
max_slots_per_day_helper([X|T],Max):-
    count_occurrences(X, [X|T], Count),
    Count =< Max,
    max_slots_per_day_helper(T,Max).

count_occurrences(_, [], 0).
count_occurrences(X, [X|T], N) :-
    count_occurrences(X, T, N1),
    N is N1 + 1.
count_occurrences(X, [Y|T], N) :-
    X \= Y,
    count_occurrences(X, T, N).

sum_ta_s(TA,TAs,0):-
    \+member(TA, TAs).
sum_ta_s(TA,TAs,1):-
    member(TA, TAs).


sum_ta_d(TA,[DSH],N):-  
    sum_ta_s(TA,DSH,N).

sum_ta_d(TA,[DSH|DSR],N):-
    sum_ta_d(TA,DSR,NX),
    sum_ta_s(TA,DSH,X),
    N is NX + X.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
maximum(A,B,A):-A>B.
maximum(A,B,B):-A=<B.

max_tas_d([],_,0).
max_tas_d([ta(Name,_)],DS,M):-
    sum_ta_d(Name,DS,M).

max_tas_d([ta(Name,_)|RTA],DS,M):-
    sum_ta_d(Name,DS,A),
    max_tas_d(RTA,DS,B),
    maximum(A,B,M).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sum_ta_w(TA,[DSH],N):-sum_ta_d(TA,DSH,N).    
sum_ta_w(TA,[WSH|WSR],N):-
    sum_ta_w(TA,WSR,NX),
    sum_ta_d(TA,WSH,X),
    N is NX + X.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_ta_s(ta(Name,Load),SL,Z):-
    Z=ta(Name,Load1),
    sum_ta_s(Name,SL,N),
    Load1 is Load - N.

update_tas_s([],_,[]).
update_tas_s([HTAs|RTAs],SL,NRTAs):-
    update_ta_s(HTAs,SL,NHTAs),
    NHTAs=ta(_,Load),
    Load=<0,
    update_tas_s(RTAs,SL,NRTAs).

update_tas_s([HTAs|RTAs],SL,NTAs):-
    update_ta_s(HTAs,SL,NHTAs),
    NHTAs=ta(_,Load),
    Load>0,
    update_tas_s(RTAs,SL,NRTAs),
    append([NHTAs],NRTAs,NTAs).    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_ta_d(ta(Name,Load),DS,Z):-
    Z=ta(Name,Load1),
    sum_ta_d(Name,DS,N),
    Load1 is Load - N.

update_tas_d([],_,[]).
update_tas_d([HTAs|RTAs],DS,NRTAs):-
    update_ta_d(HTAs,DS,NHTAs),
    NHTAs=ta(_,Load),
    Load=<0,
    update_tas_d(RTAs,DS,NRTAs).

update_tas_d([HTAs|RTAs],DS,NTAs):-
    update_ta_d(HTAs,DS,NHTAs),
    NHTAs=ta(_,Load),
    Load>0,
    update_tas_d(RTAs,DS,NRTAs),
    append([NHTAs],NRTAs,NTAs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_ta_w(ta(Name,Load),WS,Z):-
    Z=ta(Name,Load1),
    sum_ta_w(Name,WS,N),
    Load1 is Load - N.

update_tas_w([],_,[]).

update_tas_w([HTAs|RTAs],WS,NRTAs):-
    update_ta_w(HTAs,WS,NHTAs),
    NHTAs=ta(_,Load),
    Load=<0,
    update_tas_w(RTAs,WS,NRTAs).

update_tas_w([HTAs|RTAs],WS,NTAs):-
    update_ta_w(HTAs,WS,NHTAs),
    NHTAs=ta(_,Load),
    Load>0,
    update_tas_w(RTAs,WS,NRTAs),
    append([NHTAs],NRTAs,NTAs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
slot_schedule(0,_,_,[]):-!.
slot_schedule(_,_,[],[]).
slot_schedule(N,M,[_|RTAs],SlotSched):-
    slot_schedule(N,M,RTAs,SlotSched).

slot_schedule(N,M,[ta(Name,Load)|RTAs],SlotSched):-
    Load>0,
    NX is N-1,
    slot_schedule(NX,M,RTAs,XSlotSched),
    append([Name],XSlotSched,SlotSched).

slot_schedule_x(N,TAs,SlotSched):-
    length(TAs,M),
    slot_schedule(N,M,TAs,SlotSched),
    length(SlotSched,X),
    (M=<X;N=X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
day_schedule([],_,_,[]).
day_schedule([DSH|DSR],TAs,DayMax,DaySched):-
    slot_schedule_x(DSH,TAs,SlotSched),
    update_tas_s(TAs,SlotSched,NTAs),
    day_schedule(DSR,NTAs,DayMax,XDaySched),
    append([SlotSched],XDaySched,DaySched),
    max_tas_d(TAs,DaySched,M),
    M=<DayMax
    .
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
week_schedule([],_,_,[]).
week_schedule([WSH|WSR],TAs,DayMax,WeekSched):-
    day_schedule(WSH,TAs,DayMax,DaySched),
    update_tas_d(TAs,DaySched,NTAs),
    week_schedule(WSR,NTAs,DayMax,XWeekSched),
    append([DaySched],XWeekSched,WeekSched).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
:- table day_schedule/4.    