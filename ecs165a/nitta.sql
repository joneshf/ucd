CREATE TABLE Car(
model VARCHAR(10),
city INT,
highway INT,
passengers INT,
trunk INT,
price INT
);
CREATE TABLE Product(
maker VARCHAR(10),
model VARCHAR(10),
type VARCHAR(10)
);
CREATE TABLE Pickup(
model VARCHAR(10),
city INT,
highway INT,
passengers INT,
cargo INT,
price INT
);
CREATE TABLE EV(
model VARCHAR(10),
EVrange INT,
battery INT,
passengers INT,
price INT
);

INSERT INTO Car VALUES ('car1', 10, 20, 5, 20, 10000);
INSERT INTO Car VALUES ('car2', 38, 40, 4, 15, 40000);
INSERT INTO Car VALUES ('car3', 10, 20, 5, 10, 20000);
INSERT INTO Car VALUES ('car4', 30, 20, 4, 10, 30000);
INSERT INTO Car VALUES ('car5', 5, 6, 2, 10, 100000);
INSERT INTO Pickup VALUES ('pu1', 10, 20, 4, 10, 15000);
INSERT INTO Pickup VALUES ('pu2', 25, 40, 3, 10, 20000);
INSERT INTO Pickup VALUES ('pu3', 15, 20, 2, 10, 16700);
INSERT INTO Pickup VALUES ('pu4', 12, 18, 5, 10, 100000);
INSERT INTO Pickup VALUES ('pu5', 7, 14, 4, 10, 200000);
INSERT INTO EV VALUES ('ev1', 150, 30, 5, 19000);
INSERT INTO EV VALUES ('ev2', 180, 45, 4, 100000);
INSERT INTO EV VALUES ('ev3', 120, 50, 5, 40000);
INSERT INTO EV VALUES ('ev4', 170, 30, 4, 50000);
INSERT INTO EV VALUES ('ev5', 200, 40, 5, 100000);
INSERT INTO Product VALUES ('BMW', 'car1', 'c');
INSERT INTO Product VALUES ('BMW', 'pu1', 'p');
INSERT INTO Product VALUES ('BMW', 'pu2', 'p');
INSERT INTO Product VALUES ('BMW', 'ev1', 'e');
INSERT INTO Product VALUES ('LEXUS', 'car2', 'c');
INSERT INTO Product VALUES ('LEXUS', 'car3', 'c');
INSERT INTO Product VALUES ('LEXUS', 'pu3', 'p');
INSERT INTO Product VALUES ('LEXUS', 'ev2', 'e');
INSERT INTO Product VALUES ('HONDA', 'car4', 'c');
INSERT INTO Product VALUES ('HONDA', 'pu4', 'p');
INSERT INTO Product VALUES ('HONDA', 'ev3', 'e');
INSERT INTO Product VALUES ('ACURA', 'car5', 'c');
INSERT INTO Product VALUES ('ACURA', 'pu5', 'p');
INSERT INTO Product VALUES ('ACURA', 'ev4', 'e');
INSERT INTO Product VALUES ('ACURA', 'ev5', 'e');
