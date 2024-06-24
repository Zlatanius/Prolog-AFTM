% --------------------------------------------FACTS--------------------------------------------
% Define the flight facts: flight(ID, Type, ScheduledTime, FlightNumber, PriorityStatus).
flight(2, departure, date(2024, 6, 22), time(8, 5), 'FL100', priority).
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
flight(13, departure, date(2024, 6, 21), time(8, 5), 'FL106', scheduled).
flight(14, arrival, date(2024, 6, 22), time(9, 5), 'FL206', scheduled).
flight(15, departure, date(2024, 6, 21), time(8, 5), 'FL107', scheduled).


% --------------------------------------------FLIGHT GETTERS--------------------------------------------

get_flight_id(flight(FlightId, _, _, _, _, _), FlightId).
get_flight_type(flight(_, FlightType, _, _, _, _), FlightType).
get_flight_date(flight(_, _, FlightDate, _, _, _), FlightDate).
get_flight_time(flight(_, _, _, FlightTime, _, _), FlightTime).
get_flight_number(flight(_, _, _, _, FlightNumber, _), FlightNumber).
get_flight_status(flight(_, _, _, _, _, FlightStatus), FlightStatus).


% --------------------------------------------FLIGHT SETTERS--------------------------------------------

set_flight_type(flight(FlightID, _, FlightDate, FlightTime, FlightNumber, FlightStatus), NewType, flight(FlightID, NewType, FlightDate, FlightTime, FlightNumber, FlightStatus)).
set_flight_date(flight(FlightID, FlightType, _, FlightTime, FlightNumber, FlightStatus), NewDate, flight(FlightID, FlightType, NewDate, FlightTime, FlightNumber, FlightStatus)).
set_flight_time(flight(FlightID, FlightType, FlightDate, _, FlightNumber, FlightStatus), NewTime, flight(FlightID, FlightType, FlightDate, NewTime, FlightNumber, FlightStatus)).
set_flight_number(flight(FlightID, FlightType, FlightDate, FlightTime, _, FlightStatus), NewNumber, flight(FlightID, FlightType, FlightDate, FlightTime, NewNumber, FlightStatus)).
set_flight_status(flight(FlightID, FlightType, FlightDate, FlightTime, FlightNumber, _), NewStatus, flight(FlightID, FlightType, FlightDate, FlightTime, FlightNumber, NewStatus)).


% --------------------------------------------PLAN GENERATION--------------------------------------------

% Rekurzivno pravilo za generisanje plana i liste konflikta
generate_flight_plan([], [], []).
generate_flight_plan([Flight|Rest], [Flight|Plan], ConflictIDs) :-
    get_flight_id(Flight, FlightID),
    find_conflicts(FlightID, Conflicts),
    Conflicts = [],
    generate_flight_plan(Rest, Plan, ConflictIDs).

generate_flight_plan([Flight|Rest], Plan, [FlightID|ConflictIDs]) :-
    get_flight_id(Flight, FlightID),
    find_conflicts(FlightID, Conflicts),
    Conflicts \= [],
    generate_flight_plan(Rest, Plan, ConflictIDs).


% Pravilo za poredjenje dva leta po kriteriju datum > vrijeme > ID
compare_flights(Order, flight(ID1, Type1, Date1, Time1, FlightNumber1, PriorityStatus1), flight(ID2, Type2, Date2, Time2, FlightNumber2, PriorityStatus2)) :-
    Date1 @< Date2 -> Order = '<';
    Date1 @> Date2 -> Order = '>';
    (Time1 @< Time2 -> Order = '<';
    Time1 @> Time2 -> Order = '>';
    ID1 @< ID2 -> Order = '<';
    ID1 @> ID2 -> Order = '>';
    Order = '=').

% Pravilo za pravljanje finalnog plana
create_flight_plan(Plan, ConflictIDs) :-
    findall(flight(ID, Type, Date, ScheduledTime, FlightNumber, PriorityStatus), flight(ID, Type, Date, ScheduledTime, FlightNumber, PriorityStatus), Flights),
    predsort(compare_flights, Flights, SortedFlights),
    generate_flight_plan(SortedFlights, Plan, ConflictIDs).


% --------------------------------------------CONFLICT DETECTION--------------------------------------------

% Pravilo za provjeru konflikta
conflict(FlightID1, FlightID2) :-
    flight(FlightID1, Type1, Date1, Time1, _, Status1),
    flight(FlightID2, Type2, Date2, Time2, _, Status2),

    Type1 = Type2,
    Date1 = Date2,
    time_difference(Time1, Time2, Diff),
    Diff < 5,
    (Status1 = Status2 ; (Status1 = scheduled, Status2 = priority)). % Konflik je samo ako je status letova isti ili drugi let ima prioritet

% Pravilo za pronalazenje svih konflikta sa trenutnim letom
find_conflicts(Flight, Conflicts) :-
    findall(Conflict, (flight(Conflict, _, _, _, _, _), conflict(Flight, Conflict), Conflict \= Flight), Conflicts).


% --------------------------------------------CONFLICT RESOLUTION--------------------------------------------

resolve_conflict(ScheduledFlights, FlightID) :-
    get_flight_by_id(FlightID, Flight),
    get_flight_time(Flight, FlightTime),
    add_five_minutes(FlightTime, NewFlightTime),
    NewFlight is flight().

    % write(NewFlightTime).




% --------------------------------------------DISPLAY FUNCTIONS--------------------------------------------

% Pravilo za ispisivanje jednog leta
print_flight(flight(ID, Type, date(Year, Month, Day), time(Hour, Minute), FlightNumber, Status)) :-
    format('Flight ID: ~|~t~d~2+, Type: ~|~t~w~9+, Date: ~d-~|~`0t~d~2+-~|~`0t~d~2+, Time: ~|~`0t~d~2+:~|~`0t~d~2+, Flight Number: ~w, Status: ~w~n', [ID, Type, Year, Month, Day, Hour, Minute, FlightNumber, Status]).

% Pravilo za ispisivanje liste letova
print_flights([]).
print_flights([Flight | Rest]) :-
    print_flight(Flight),
    print_flights(Rest).

% Pravilo za ispisivanje letova iz liste IDeva
print_flights_from_id(FlightIDs) :-
    get_flights_from_ids(FlightIDs, Flights),
    print_flights(Flights).

% Pravilo za ispisivanje datuma (UNUSED)
display_date(date(Year, Month, Day)) :-
    format('~d-~|~`0t~d~2+-~|~`0t~d~2+~n', [Year, Month, Day]).


% --------------------------------------------HELPER FUNTIONS--------------------------------------------

% Pravilo za pronalazenje leta preko IDa
get_flight_by_id(FlightID, Flight) :-
    flight(FlightID, Type, Date, Time, FlightNumber, Status),
    Flight = flight(FlightID, Type, Date, Time, FlightNumber, Status).

% Rekurzivno pravilo za pravljenje liste letova od liste IDeva
get_flights_from_ids([], []).
get_flights_from_ids([FlightID|RestIDs], [Flight|RestFlights]) :-
    get_flight_by_id(FlightID, Flight),
    get_flights_from_ids(RestIDs, RestFlights).

% Pravilo za racunanje minuta
time_to_minutes(time(H, M), Minutes) :-
    Minutes is H * 60 + M.

% Pravilo za racunanje razlike izmedju dva vremena u minutama
time_difference(Time1, Time2, Diff) :-
    time_to_minutes(Time1, Minutes1),
    time_to_minutes(Time2, Minutes2),
    Diff is abs(Minutes1 - Minutes2).

add_five_minutes(time(Hour, Minute), time(NewHour, NewMinute)) :-
    TotalMinutes is Hour * 60 + Minute + 5,
    NewHour is (TotalMinutes // 60) mod 24,
    NewMinute is TotalMinutes mod 60.


% --------------------------------------------MAIN PROGRAM--------------------------------------------

% Pravilo za ispisivanje citavog plana letova i pronadjenih konflikta
display_flight_plan() :-
    create_flight_plan(Plan, ConflictIDs),
    writeln('FLIGHT SCHEDULE: '),
    print_flights(Plan), nl,
    writeln('CONFLICTS: '),
    print_flights_from_id(ConflictIDs).
