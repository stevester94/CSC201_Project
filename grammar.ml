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
val insts1 = Instruction_3([inst1, inst2]);

(* While condition *)
val whileCond = BooleanExpression_3(IntegerExpression_2(v1), Ne, IntegerExpression_1(0));

(* While innards *)
val whileArith1 = IntegerExpression_3(
        IntegerExpression_3(IntegerExpression_2(v1), Minus, IntegerExpression_1(10)),
        Times,
        IntegerExpression_3(IntegerExpression_2(v1), Div, IntegerExpression_1(10))); 

val whileInst1 = Instruction_2(v2, Expression_1(whileArith1)); 


val whileArith2 = IntegerExpression_3(
        IntegerExpression_3(IntegerExpression_2(v3), Times, IntegerExpression_1(10)),
        Plus,
        IntegerExpression_2(v2));

val whileInst2 = Instruction_2(v3, Expression_1(whileArith2)); 

val whileArith3 = IntegerExpression_3(IntegerExpression_2(v1), Div, IntegerExpression_1(10));

val whileInst3 = Instruction_2(v1, Expression_1(whileArith3));

(* While Block *)
val whileBlock = Instruction_5(whileCond, Instruction_3([whileInst1, whileInst2, whileInst3]));

(* More Instantiations *)
val inst3 = Instruction_2(v4, Expression_1(IntegerExpression_1(0)));
val inst4 = Instruction_2(v5, Expression_1(IntegerExpression_1(2)));
val insts2 = Instruction_3([inst3, inst4]);




(* We need the stuff outisde Repeat Loop before writing repeat loop in order to implement "break" *)

(*If Condition *)
val ifCond2 = BooleanExpression_3(IntegerExpression_2(v4), Eq, IntegerExpression_1(0));
val thenStatement2 = Instruction_2(v6, Expression_2(BooleanExpression_1(true)));
val elseStatement2 = Instruction_2(v6, Expression_2(BooleanExpression_1(false)));

(* If Block2 *)
val ifBlock2 = Instruction_4(ifCond2, thenStatement2, elseStatement2); 


(* Repeat Innards *)

(* If Condition *)
val ifArith1 = IntegerExpression_3(IntegerExpression_3(IntegerExpression_2(v3), Minus, IntegerExpression_2(v5)), Times, IntegerExpression_3(IntegerExpression_2(v3), Div, IntegerExpression_2(v5)));
val ifCond1 = BooleanExpression_3(ifArith1, Eq, IntegerExpression_1(0));

(* If Innards *)
val thenStatement1 = Instruction_2(v4, Expression_1(IntegerExpression_1(1)));

(* If Block *)
val ifBlock1 = Instruction_4(ifCond1, thenStatement1, ifBlock2);


val repeatArith1 = IntegerExpression_3(IntegerExpression_2(v5), Plus, IntegerExpression_1(1));
val repeatInst1 = Instruction_2(v5, Expression_1(repeatArith1));

(* Repeat Body *)
val repeatBody = Instruction_3([ifBlock1, repeatInst1]); 

(* Repeat Cond *)
val repeatCond = BooleanExpression_3(IntegerExpression_2(v5), Le, IntegerExpression_3(IntegerExpression_2(v3), Div, IntegerExpression_1(2)));

(* Repeat Block *)
val repeatBlock1 = Instruction_3([repeatBody, Instruction_5(repeatCond, repeatBody)]);



(* Putting it all together *)
val ourProgram = Program(declares, Instruction_3([
		insts1,
		whileBlock,
		insts2,
		repeatBlock1,
		ifBlock2
		]));
				

