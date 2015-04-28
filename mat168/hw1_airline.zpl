set Flight := {"Ithaca to Newark", "Newark to Boston", "Ithaca to Boston"};
set Fare := {"Y", "B", "M"};

param prices[Fare * Flight] :=
    |"Ithaca to Newark", "Newark to Boston", "Ithaca to Boston"|
|"Y"|               300,                160,                360|
|"B"|               220,                130,                280|
|"M"|               100,                 80,                140|;

param seating[Fare * Flight] :=
    |"Ithaca to Newark", "Newark to Boston", "Ithaca to Boston"|
|"Y"|                 4,                  8,                  3|
|"B"|                 8,                 13,                 10|
|"M"|                22,                 20,                 18|;

var passenger[Fare * Flight] integer;

maximize revenue: sum <fare, flight> in Fare * Flight:
    prices[fare, flight] * passenger[fare, flight];

subto seating_limit: forall <fare, flight> in Fare * Flight:
    passenger[fare, flight] <= seating[fare, flight];
subto flight_limit: forall <flight> in Flight:
    (sum <fare> in Fare: passenger[fare, flight]) <= 30;

sos s1: type1: 100 * x[1] + 200 * x[2] + 400 * x[3];
sos s2: type2 priority 100 : sum <i> in I: a[i] * x[i];
sos s3: forall <i> in I with i > 2:
type1: (100 + i) * x[i] + i * x[i-1];
