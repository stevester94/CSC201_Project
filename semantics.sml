type Variable = string;
type Integer_Constant = int;
type Boolean_Constant = bool;

datatype Arithmatic_Op = Plus | Minus | Times | Div;
datatype Relational_Op = Lt | Le | Eq | Ne | Ge | Gt;
datatype Boolean_Op    = And | Or;

datatype IntegerExpression = 
    IntegerExpression_1 of Integer_Constant
|   IntegerExpression_2 of Variable
|   IntegerExpression_3 of (IntegerExpression*Arithmatic_Op*IntegerExpression);

datatype BooleanExpression = 
    BooleanExpression_1 of Boolean_Constant
|   BooleanExpression_2 of Variable
|   BooleanExpression_3 of (IntegerExpression*Relational_Op*IntegerExpression)
|   BooleanExpression_4 of (BooleanExpression*Boolean_Op*BooleanExpression);

datatype Expression = Expression_1 of IntegerExpression | Expression_2 of BooleanExpression;


datatype Skip_Const = Skip;
datatype Instruction = 
    Instruction_1 of Skip_Const 
|   Instruction_2 of (Variable * Expression)
|   Instruction_3 of Instruction list
|   Instruction_4 of (BooleanExpression * Instruction * Instruction)
|   Instruction_5 of (BooleanExpression * Instruction);

datatype Type = BooleanType | IntegerType;

type Declaration = Variable * Type;

type DeclarationList = Declaration list;

type Program = (DeclarationList * Instruction);

(****************************************************************************************)


(* Variable names *)
val v1 = "n";
val v2 = "reverse";
val v3 = "sum";
val v4 = "flag";
val v5 = "j";
val v6 = "tprime";

(* Declarations *)
val declares = [(v1, IntegerType),
        (v2, IntegerType),
        (v3, IntegerType),
        (v4, IntegerType),
        (v5, IntegerType),
        (v6, BooleanType)];



datatype IType = IntRep | BoolRep | NoDeclarationRep
type TypeMapImpl = (Variable * IType) list

(* Saving these for future reference
(* fun 
    AddOneToTypeMap( _:(string * BooleanType) ) = 69
|   AddOneToTypeMap( _:(string * IntegerType) ) = 42; *)

(* fun AddOneToTypeMap(D:()) *)

(* This is valid *)
fun  AddOneToTypeMap(D:Declaration) = 
    if #2(D) = BooleanType then BoolRep else IntRep;

(* But I think this looks better *)
fun 
    GetDeclarationInternalType( (_:string, BooleanType) ) = BoolRep
|   GetDeclarationInternalType( (_:string, IntegerType) ) = IntRep;
*)

datatype IType = IntRep | BoolRep | NoDeclarationRep
type TypeMapImpl = (Variable * IType) list

fun 
    DecListToTypeMapImp([]) = [] (*dec list empty, so is your symbol table lol *)
|   DecListToTypeMapImp(declist_head::declist_tail : DeclarationList) = 
    [
        (fn
            (v:Variable, BooleanType) => (v, BoolRep)
        |   (v:Variable, IntegerType) => (v, IntRep)
        )(declist_head)
    ] @ DecListToTypeMapImp(declist_tail);

DecListToTypeMapImp(declares);



        (* [(#1(declist_head), GetDeclarationInternalType(#2 declist_head))] @ DecListToTypeMapImp(declist_tail); *)
    


(* DecListToTypeMapImp(declares); *)




(* fun 
    DecListToTypeMapImp([]) = [] (*dec list empty, so is your symbol table lol *)
|   DecListToTypeMapImpl(declist_head::declist_tail):DeclarationList =>
	TypeMapPlusOne                                                                   (* Fucking what *)
		(DecListTotypeMapImple(declist_tail)
		(declist_head) *)