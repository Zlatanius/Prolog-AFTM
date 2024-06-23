% Define the flight facts: flight(ID, Type, ScheduledTime, FlightNumber, PriorityStatus).
flight(2, departure, date(2024, 6, 22), time(8, 0), 'FL100', scheduled).
flight(3, arrival, date(2024, 6, 21), time(8, 0), 'FL200', scheduled).
flight(1, departure, date(2024, 6, 22), time(8, 5), 'FL101', scheduled).
flight(4, arrival, date(2024, 6, 22), time(8, 15), 'FL201', scheduled).
flight(5, departure, date(2024, 6, 22), time(8, 20), 'FL102', scheduled).
flight(6, arrival, date(2024, 6, 22), time(8, 25), 'FL202', scheduled).
flight(7, departure, date(2024, 6, 22), time(8, 30), 'FL103', scheduled).
flight(8, arrival, date(2024, 6, 22), time(8, 35), 'FL203', scheduled).
flight(9, departure, date(2024, 6, 22), time(8, 40), 'FL104', scheduled).
flight(10, arrival, date(2024, 6, 22), time(8, 45), 'FL204', scheduled).
flight(11, departure, date(2024, 6, 22), time(8, 50), 'FL105', scheduled).
flight(12, arrival, date(2024, 6, 22), time(8, 55), 'FL205', scheduled).
flight(13, departure, date(2024, 6, 22), time(9, 0), 'FL106', scheduled).
flight(14, arrival, date(2024, 6, 22), time(9, 5), 'FL206', scheduled).
flight(15, departure, date(2024, 6, 22), time(9, 10), 'FL107', scheduled).

% Pravilo za ispisivanje datuma
display_date(date(Year, Month, Day)) :-
    format('~d-~|~`0t~d~2+-~|~`0t~d~2+~n', [Year, Month, Day]).

time_to_minutes(time(H, M), Minutes) :-
    Minutes is H * 60 + M.

time_difference(Time1, Time2, Diff) :-
    time_to_minutes(Time1, Minutes1),
    time_to_minutes(Time2, Minutes2),
    Diff is abs(Minutes1 - Minutes2).

% Rule to check if two flights conflict.
conflict(Flight1, Flight2) :-
    flight(Flight1, Type1, Date1, Time1, _, _),
    flight(Flight2, Type2, Date2, Time2, _, _),
    Type1 = Type2,
    Date1 = Date2,
    time_difference(Time1, Time2, Diff),
    Diff < 5.

% Rule to find all conflicts for a given flight.
find_conflicts(Flight, Conflicts) :-
    findall(Conflict, (flight(Conflict, _, _, _, _, _), conflict(Flight, Conflict), Conflict \= Flight), Conflicts).

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
    predsort(compare_flights_by_dateTime, Flights, SortedFlights).

compare_flights_by_dateTime(Order, flight(ID1, Type1, Date1, Time1, FlightNumber1, PriorityStatus1), flight(ID2, Type2, Date2, Time2, FlightNumber2, PriorityStatus2)) :-
    (Time1 @< Time2 -> Order = '<';
    Time1 @> Time2 -> Order = '>';
    Date1 @< Date2 -> Order = '<';
    Date1 @> Date2 -> Order = '>';
    ID1 @< ID2 -> Order = '<';
    ID1 @> ID2 -> Order = '>';
    Order = '=').

% Rule to create the final flight plan.
create_flight_plan(Plan) :-
    findall(flight(ID, Type, Date, ScheduledTime, FlightNumber, PriorityStatus), flight(ID, Type, Date, ScheduledTime, FlightNumber, PriorityStatus), Flights),
    sort_flights_by_time(Flights, SortedFlights),
    generate_flight_plan(SortedFlights, Plan).

%Predikat za ispisivanje jednog leta
print_flight(flight(ID, Type, date(Year, Month, Day), time(Hour, Minute), FlightNumber, Status)) :-
    format('Flight ID: ~|~t~d~2+, Type: ~|~t~w~9+, Date: ~d-~|~`0t~d~2+-~|~`0t~d~2+, Time: ~|~`0t~d~2+:~|~`0t~d~2+, Flight Number: ~w, Status: ~w~n', [ID, Type, Year, Month, Day, Hour, Minute, FlightNumber, Status]).

print_plan([]).
print_plan([Flight | Rest]) :-
    print_flight(Flight),
    print_plan(Rest).

display_flight_plan() :-
    create_flight_plan(Plan),
    print_plan(Plan).