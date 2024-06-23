% Define the flight facts: flight(ID, Type, ScheduledTime, FlightNumber, PriorityStatus).
flight(1, departure, 800, 'FL100', scheduled).
flight(2, arrival, 805, 'FL200', scheduled).
flight(3, departure, 810, 'FL101', scheduled).
flight(4, arrival, 815, 'FL201', scheduled).
flight(5, departure, 820, 'FL102', scheduled).
flight(6, arrival, 825, 'FL202', scheduled).
flight(7, departure, 830, 'FL103', scheduled).
flight(8, arrival, 835, 'FL203', scheduled).
flight(9, departure, 840, 'FL104', scheduled).
flight(10, arrival, 845, 'FL204', scheduled).
flight(11, departure, 850, 'FL105', scheduled).
flight(12, arrival, 855, 'FL205', scheduled).
flight(13, departure, 900, 'FL106', scheduled).
flight(14, arrival, 905, 'FL206', scheduled).
flight(15, departure, 910, 'FL107', scheduled).


% Rule to check the time difference between two times (in minutes).
time_difference(Time1, Time2, Diff) :-
    Diff is abs(Stamp1 - Stamp2).

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
print_flight(flight(ID, Type, Time, FlightNumber, Status)) :-
    format('Flight ID: ~d, Type: ~w, Time: ~w, Flight Number: ~w, Status: ~w~n', [ID, Type, Time, FlightNumber, Status]).

print_plan([]).
print_plan([Flight | Rest]) :-
    print_flight(Flight),
    print_plan(Rest).

display_flight_plan() :-
    create_flight_plan(Plan),
    print_plan(Plan).