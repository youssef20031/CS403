% % Predicate to update the teaching load of a TA
% ta_slot_assignment(TAs, RemTAs, Name) :-
%     % Find the TA with the given name
%     member(TA, TAs),
%     TA = ta(Name, Load),
%     % Decrement the teaching load of the TA by 1
%     NewLoad is Load - 1,
%     % Create a new TA structure with the updated teaching load
%     NewTA = ta(Name, NewLoad),
%     % Replace the old TA with the new TA in the list of TAs
%     select(TA, TAs, NewTA, RemTAs).


% max_slots_per_day(DaySched, Max) :-
%     flatten(DaySched, [X|T]),
%     max_slots_per_day_helper([X|T], Max).


% max_slots_per_day_helper([],_).	
% max_slots_per_day_helper([X|T],Max):-
%     count_occurrences(X, [X|T], Count),
%     Count =< Max,
%     max_slots_per_day_helper(T,Max).

% count_occurrences(_, [], 0).
% count_occurrences(X, [X|T], N) :-
%     count_occurrences(X, T, N1),
%     N is N1 + 1.
% count_occurrences(X, [Y|T], N) :-
%     X \= Y,
%     count_occurrences(X, T, N).

sum_ta_s(TA,TAs,0):-
    \+member(TA, TAs).
sum_ta_s(TA,TAs,1):-
    member(TA, TAs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sum_ta_d(TA,[DSH],N):-  
    sum_ta_s(TA,DSH,N).

sum_ta_d(TA,[DSH|DSR],N):-
    sum_ta_d(TA,DSR,NX),
    sum_ta_s(TA,DSH,X),
    N is NX + X.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
maximum(A,B,A):-A>B.
maximum(A,B,B):-A=<B.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
max_tas_d([],_,0).

max_tas_d([Name|RTA],DS,M):-
    sum_ta_d(Name,DS,A),
    max_tas_d(RTA,DS,B),
    maximum(A,B,M).

max_slots_per_day(DS,Max):-
    flatten(DS,FDS),sort(FDS,TAs),
    max_tas_d(TAs,DS,Max).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ta_slot_assignment([ta(Name,Load)|RTAs],[ta(Name,Load1)|RTAs],Name):-
    Load>0,
    Load1 is Load-1.

ta_slot_assignment([HTAs|RTAs],[HTAs|NRTAs],X):-
    ta_slot_assignment(RTAs,NRTAs,X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
slot_assignment(0,TAs,TAs,[]).
   
slot_assignment(N,TAs,NTAs,SlotSched):-
    ta_slot_assignment(TAs,XNTAs,TA),
    nth0(P,XNTAs,ta(TA,_)),
    select(ta(TA,NLoad),XNTAs,XTAs),
    NX is N-1,
    slot_assignment(NX,XTAs,NRTAs,XSlotSched),
    append([TA],XSlotSched,SlotSched),
    nth0(P,NTAs,ta(TA,NLoad),NRTAs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
day_schedule([],TAs,TAs,[]).
day_schedule([DSH|DSR],TAs,NTAs,DaySched):-
    slot_assignment(DSH,TAs,XNTAs,SlotSched),
    day_schedule(DSR,XNTAs,NTAs,XDaySched),
    append([SlotSched],XDaySched,DaySched).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%day_schedule_filter(DS,TAs,DayMax,NTAs,X1):-
%    bagof(X1, (day_schedule(DS,TAs,NTAs,X1), max_slots_per_day(X1,M), M=<DayMax),X1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
week_schedule([],_,_,[]).
week_schedule([WSH|WSR],TAs,DayMax,WeekSched):-
    day_schedule(WSH,TAs,XNTAs,DaySched),
    max_slots_per_day(DaySched,M),
    M=<DayMax,
    week_schedule(WSR,XNTAs,DayMax,XWeekSched),
    append([DaySched],XWeekSched,WeekSched).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     