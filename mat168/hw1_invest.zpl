var domestic real;
var foreign real;

maximize returns: 0.11 * domestic + 0.17 * foreign;

subto total_both: domestic + foreign <= 12;
subto total_domestic: domestic <= 10;
subto total_foreign: foreign <= 7;
subto twice_foreign: 2 * domestic >= foreign;
subto twice_domestic: 2 * foreign >= domestic;
