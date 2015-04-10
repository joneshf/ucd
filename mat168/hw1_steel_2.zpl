var band real;
var coil real;

maximize profit: 5000 * band + 4200 * coil;

subto hours: band + coil <= 40;
subto rate_band: 200 * band <= 6000;
subto rate_coil: 140 * coil <= 4000;
