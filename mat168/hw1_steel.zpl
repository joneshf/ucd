set Product := { "Band", "Coil" };
set Params := { "Profit", "Rate" };

param limit[Product] := <"Band"> 6000, <"Coil"> 4000;

param data[Product * Params] :=
       |"Profit", "Rate"|
|"Band"|      25,    200|
|"Coil"|      30,    140|;

var time[Product] real >= 0;

maximize profit: sum <product> in Product:
    data[product, "Profit"] * data[product, "Rate"] * time[product];

subto production_limit: forall <product> in Product:
    time[product] * data[product, "Rate"] <= limit[product];
subto time_limit: (sum <product> in Product: time[product]) <= 40;
