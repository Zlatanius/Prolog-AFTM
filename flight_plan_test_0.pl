% Define the flight facts: flight(ID, Type, ScheduledTime, FlightNumber, PriorityStatus).
flight(2, departure, date(2024, 6, 22), time(8, 5), 'FL100', scheduled).
flight(3, departure, date(2024, 6, 22), time(8, 5), 'FL200', scheduled).
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

% Pravilo za racunanje minuta
time_to_minutes(time(H, M), Minutes) :-
    Minutes is H * 60 + M.

% Pravilo za racunanje razliek izmedju dva vremena u minutama
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


%-------------------------------------------generate_flight_plan WITH CONFLICTS-----------------------------------

generate_flight_plan([], [], []).
generate_flight_plan([flight(FlightID, Type, Date, Time, FlightNumber, Status)|Rest], [flight(FlightID, Type, Date, Time, FlightNumber, Status)|Plan], ConflictIDs) :-
    find_conflicts(FlightID, Conflicts),
    Conflicts = [],
    generate_flight_plan(Rest, Plan, ConflictIDs).

generate_flight_plan([flight(FlightID, Type, Date, Time, FlightNumber, Status)|Rest], Plan, [FlightID|ConflictIDs]) :-
    find_conflicts(FlightID, Conflicts),
    Conflicts \= [],
    generate_flight_plan(Rest, Plan, ConflictIDs).

%------------------------------------------------------------------------------------------------------------------


% Rule to sort flights by scheduled time.
sort_flights_by_time(Flights, SortedFlights) :-
    predsort(compare_flights_by_dateTime, Flights, SortedFlights).

% Pravilo za poredjenje dva leta po kriteriju datum > vrijeme > ID
compare_flights_by_dateTime(Order, flight(ID1, Type1, Date1, Time1, FlightNumber1, PriorityStatus1), flight(ID2, Type2, Date2, Time2, FlightNumber2, PriorityStatus2)) :-
    Date1 @< Date2 -> Order = '<';
    Date1 @> Date2 -> Order = '>';
    (Time1 @< Time2 -> Order = '<';
    Time1 @> Time2 -> Order = '>';
    ID1 @< ID2 -> Order = '<';
    ID1 @> ID2 -> Order = '>';
    Order = '=').

% Rule to create the final flight plan.
create_flight_plan(Plan, ConflictIDs) :-
    findall(flight(ID, Type, Date, ScheduledTime, FlightNumber, PriorityStatus), flight(ID, Type, Date, ScheduledTime, FlightNumber, PriorityStatus), Flights),
    sort_flights_by_time(Flights, SortedFlights),
    generate_flight_plan(SortedFlights, Plan, ConflictIDs).

% Pravilo za ispisivanje jednog leta
print_flight(flight(ID, Type, date(Year, Month, Day), time(Hour, Minute), FlightNumber, Status)) :-
    format('Flight ID: ~|~t~d~2+, Type: ~|~t~w~9+, Date: ~d-~|~`0t~d~2+-~|~`0t~d~2+, Time: ~|~`0t~d~2+:~|~`0t~d~2+, Flight Number: ~w, Status: ~w~n', [ID, Type, Year, Month, Day, Hour, Minute, FlightNumber, Status]).

% Pravilo za ispisivanje liste letova
print_flights([]).
print_flights([Flight | Rest]) :-
    print_flight(Flight),
    print_flights(Rest).

% Pravilo za pronalazenje leta preko IDa
find_flight_by_id(FlightID, Flight) :-
    flight(FlightID, Type, Date, Time, FlightNumber, Status),
    Flight = flight(FlightID, Type, Date, Time, FlightNumber, Status).

% Rekurzivno pravilo za pravljenje liste letova od liste IDeva
get_flights_from_ids([], []).
get_flights_from_ids([FlightID|RestIDs], [Flight|RestFlights]) :-
    find_flight_by_id(FlightID, Flight),
    get_flights_from_ids(RestIDs, RestFlights).

% Pravilo za ispisivanje letova iz liste IDeva
print_flights_from_id(FlightIDs) :-
    get_flights_from_ids(FlightIDs, Flights),
    print_flights(Flights).

% Pravilo za ispisivanje citavog plana letova i pronadjenih konflikta
display_flight_plan() :-
    create_flight_plan(Plan, ConflictIDs),
    write('FLIGHT SCHEDULE: '), nl,
    print_flights(Plan), nl,
    write('CONFLICTS: '), nl,
    print_flights_from_id(ConflictIDs).
