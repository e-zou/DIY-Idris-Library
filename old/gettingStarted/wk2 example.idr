data Bool = True | False;

id : Bool -> Bool;
id True = True;
id False = False;

constTrue : Bool -> Bool;
constTrue True = True;
consTrue False = True;

constFalse : Bool -> Bool;
constFalse True = False;
constFalse False = False;

not : Bool -> Bool;
not True = False;
not False = True;


data Fruit = Apple | Banana | Strawberry | Blueberry | Watermelon | Lime
isYummy : Fruit -> Bool
