% Define the flight facts: flight(ID, Type, ScheduledTime, FlightNumber, PriorityStatus).
flight(2, departure, time(8, 0), 'FL100', scheduled).
flight(3, arrival, time(8, 10), 'FL200', scheduled).
flight(1, departure, time(8, 5), 'FL101', scheduled).
flight(4, arrival, time(8, 15), 'FL201', scheduled).
flight(5, departure, time(8, 20), 'FL102', scheduled).
flight(6, arrival, time(8, 25), 'FL202', scheduled).
flight(7, departure, time(8, 30), 'FL103', scheduled).
flight(8, arrival, time(8, 35), 'FL203', scheduled).
flight(9, departure, time(8, 40), 'FL104', scheduled).
flight(10, arrival, time(8, 45), 'FL204', scheduled).
flight(11, departure, time(8, 50), 'FL105', scheduled).
flight(12, arrival, time(8, 55), 'FL205', scheduled).
flight(13, departure, time(9, 0), 'FL106', scheduled).
flight(14, arrival, time(9, 5), 'FL206', scheduled).
flight(15, departure, time(9, 10), 'FL107', scheduled).


time_difference(time(H1, M1), time(H2, M2), Diff) :-
    Minutes1 is H1 * 60 + M1,
    Minutes2 is H2 * 60 + M2,
    Diff is abs(Minutes1 - Minutes2).

% Rule to check if two flights conflict.
conflict(Flight1, Flight2) :-
    flight(Flight1, Type1, Time1, _, _),
    flight(Flight2, Type2, Time2, _, _),
    Type1 = Type2,
    time_difference(Time1, Time2, Diff),
    Diff < 5.

% Rule to find all conflicts for a given flight.
find_conflicts(Flight, Conflicts) :-
    findall(Conflict, (flight(Conflict, _, _, _, _), conflict(Flight, Conflict), Conflict \= Flight), Conflicts).

% Rule to generate a flight plan without conflicts.
generate_flight_plan([], []).

generate_flight_plan([Flight|Rest], [Flight|Plan]) :-
    find_conflicts(Flight, Conflicts),
    Conflicts = [],
    generate_flight_plan(Rest, Plan).

generate_flight_plan([Flight|Rest], Plan) :-
    find_conflicts(Flight, Conflicts),
    Conflicts \= [],
    generate_flight_plan(Rest, Plan).

% Rule to sort flights by scheduled time.
sort_flights_by_time(Flights, SortedFlights) :-
    predsort(compare_flights_by_time, Flights, SortedFlights).

compare_flights_by_time(Order, flight(ID1, Type1, Time1, FlightNumber1, PriorityStatus1), flight(ID2, Type2, Time2, FlightNumber2, PriorityStatus2)) :-
    compare(Order, Time1, Time2).

% Rule to create the final flight plan.
create_flight_plan(Plan) :-
    findall(flight(ID, Type, ScheduledTime, FlightNumber, PriorityStatus), flight(ID, Type, ScheduledTime, FlightNumber, PriorityStatus), Flights),
    sort_flights_by_time(Flights, SortedFlights),
    generate_flight_plan(SortedFlights, Plan).

%Predikat za ispisivanje jednog leta
print_flight(flight(ID, Type, time(H, M), FlightNumber, Status)) :-
    format('Flight ID: ~|~t~d~2+, Type: ~|~t~w~9+, Time: ~|~`0t~d~2+:~|~`0t~d~2+, Flight Number: ~w, Status: ~w~n', [ID, Type, H, M, FlightNumber, Status]).

print_plan([]).
print_plan([Flight | Rest]) :-
    print_flight(Flight),
    print_plan(Rest).

display_flight_plan() :-
    create_flight_plan(Plan),
    print_plan(Plan).