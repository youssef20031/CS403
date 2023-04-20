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
