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
