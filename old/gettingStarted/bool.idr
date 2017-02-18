module bool

public export
data Bool = {True | False}

public export
bool_id : Bool -> Bool;
bool_id  True = True;
bool_id  False = False;

bool_idTrue : Bool -> Bool;
bool_idTrue True = True;
bool_idTrue  False = True;

bool_idFalse : Bool -> Bool;
bool_idFalse True = False;
bool_idFalse False = False;

bool_idNot : Bool -> Bool;
bool_idNot True = False;
bool_idNot False = True;
