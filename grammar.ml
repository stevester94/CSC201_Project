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

(* Instantiation *)
val inst1 = Instruction_2(v3, Expression_1(IntegerExpression_1(0)));
val inst2 = Instruction_2(v1, Expression_1(IntegerExpression_1(13)));

(* While condition *)
val cond1 = Expression_2(BooleanExpression_3(IntegerExpression_2(v1), Ne, IntegerExpression_1(0)));

(* While innards *)
val arith1 = IntegerExpression_3(
        IntegerExpression_3(IntegerExpression_2(v1), Minus, IntegerExpression_1(10)),
        Times,
        IntegerExpression_3(IntegerExpression_2(v1), Div, IntegerExpression_1(10))); 


