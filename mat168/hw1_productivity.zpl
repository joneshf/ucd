set Designer := { "Designer 1", "Designer 2", "Designer 3" };
set Project := { "Project 1", "Project 2", "Project 3", "Project 4" };

param productivity[Designer * Project] :=
             |"Project 1", "Project 2", "Project 3", "Project 4"|
|"Designer 1"|         90,          80,          10,          50|
|"Designer 2"|         60,          70,          50,          65|
|"Designer 3"|         70,          40,          80,          85|;

param estimate[Project] :=
    <"Project 1"> 70, <"Project 2"> 50, <"Project 3"> 85, <"Project 4"> 35;

var hours[Designer * Project] real;

maximize productivity: sum <designer, project> in Designer * Project:
    productivity[designer, project] * hours[designer, project];

subto max_hours: forall <designer> in Designer:
    (sum <project> in Project: hours[designer, project]) <= 80;

subto requirement: forall <project> in Project:
    (sum <designer> in Designer: hours[designer, project]) == estimate[project];
