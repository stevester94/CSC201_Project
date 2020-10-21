(* CSC 201 Section 3, Team 4 *)
(* Alex Nosenko *)
(* Jeffrey Byrnes *)
(* Nitin Nath *)
(* Steven Mackey *)

(*Part 1 - Defenitions *)
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

(*Part - 3*)

(* Sample Program
PROGRAM twisted_prime
{
n Integer;
reverse Integer;
sum Integer;
flag Integer;
j Integer;
tprime Boolean;
sum = 0;
n = 13;
WHILE (n Ne 0)
{
reverse = (n Minus 10 Times (n Div 10));
sum = sum Times 10 Plus reverse;
n = n Div 10;
}
flag = 0;
j = 2;
REPEAT
IF ((sum Minus j Times (sum Div j)) Eq 0)
{
flag = 1;
break;
}
j = j Plus 1;
UNTIL (j Le (sum Div 2))
IF (flag Eq 0) THEN ans=True;
ELSE ans=False;
}
*)

(*Implementation of Sample Program*)

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




(* We need the code outisde Repeat Loop before writing repeat loop in order to implement "break" *)

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
val ourProgram = (
			declares, 
			Instruction_3([
				insts1,
				whileBlock,
				insts2,
				repeatBlock1,
				ifBlock2
			])
		);

(*Step 2*)

(*step 2.1 *)
datatype IType = IntRep | BoolRep | NoDecRep;

(*step 2.2 *)
type TypeMapImp = (Variable*IType)list;

(*step 2.3 *)
(*varITypeSearch: TypeMap->Variable->IType;*)
val rec varITypeSearch = fn([]:TypeMapImp)=>(fn(u:Variable)=>NoDecRep) | (((v,w)::map_tail):TypeMapImp)=>
			(fn(u:Variable)=>if(u=v) then w
					else
		 			varITypeSearch(map_tail)(u));

(*step 2.4 *)
(*TypeMapPlusOne:TypeMap->Declaration->TypeMap *)
val TypeMapPlusOne = (fn(TMOld:TypeMapImp)=>(fn(v:Variable,IntegerType)=>[(v,IntRep)]@TMOld | (v:Variable,BooleanType)=>[(v,BoolRep)]@TMOld));

(*step 2.5 *)
fun DecListToTypeMapImp([])=[] |
	DecListToTypeMapImp((declist_head::declist_tail):DeclarationList)=TypeMapPlusOne(DecListToTypeMapImp(declist_tail))(declist_head);

(*Testing *)
val testVariables = DecListToTypeMapImp(declares);
val result = varITypeSearch(testVariables);

val var1 = varITypeSearch([]);


(*step 2.6 *)
val rec VarNotInDecList = fn([]:DeclarationList)=>(fn(v:Variable)=>true) |
			((x:Variable,y:Type)::declist_tail)=>(fn(v:Variable)=>VarNotInDecList(declist_tail)(v) andalso (v<>x));

(*step 2.7 *)
val rec ValidDecList= fn([])=> true |
	((x:Variable,y:Type)::declist_tail)=>VarNotInDecList(declist_tail)(x) andalso ValidDecList(declist_tail);


(*step 2.8*)
print("\nBeginning step 2.8\n");

val rec VIntExp = 
	fn(IntegerExpression_1(i)) => (fn(tmi:TypeMapImp) => true) |
	(IntegerExpression_2(v)) => (fn(tmi:TypeMapImp) => varITypeSearch(tmi)(v) = IntRep) |
	(IntegerExpression_3(ie1, aop, ie2)) => (fn(tmi:TypeMapImp) => VIntExp(ie1)(tmi) 
								andalso VIntExp(ie2)(tmi));
	 
(*Testing 2.8 *)
print("\nTesting 2.8\n");

VIntExp(IntegerExpression_1(0));				(* good test IntegerExpression_1 *)
VIntExp(IntegerExpression_2(v1)); 				(* good test IntegerExpression_2 *)
VIntExp(whileArith2);						(* good test IntegerExpression_3 *)

VIntExp(IntegerExpression_2(v6));				(* bad test IntegerExpression_2 *)
(* VIntExp(IntegerExpression_3(whileCond, Plus, whileArith2));	(* bad test IntegerExpression_3 *) ERROR*)


(*step 2.9*)
print("\nBeginning step 2.9\n");
val rec VBoolExp =
        fn(BooleanExpression_1(i)) => (fn(tmi:TypeMapImp) => true) |
        (BooleanExpression_2(v)) => (fn(tmi:TypeMapImp) => varITypeSearch(tmi)(v) = BoolRep) |
        (BooleanExpression_3(ie1, rop, ie2)) => (fn(tmi:TypeMapImp) => VIntExp(ie1)(tmi)
                                                                andalso VIntExp(ie2)(tmi)) |
        (BooleanExpression_4(be1, bop, be2)) => (fn(tmi:TypeMapImp) => VBoolExp(be1)(tmi)
                                                                andalso VBoolExp(be2)(tmi));

(*Testing 2.9*)
print("\nTesting 2.9\n");

VBoolExp(BooleanExpression_1(true));				(* good test BooleanExpression_1 *)
VBoolExp(BooleanExpression_2(v6));				(* good test BooleanExpression_2 *)
VBoolExp(ifCond2);						(* good test BooleanExpression_3 *)
VBoolExp(BooleanExpression_4(whileCond, And, ifCond2));		(* good test BooleanExpression_4 *)

VBoolExp(BooleanExpression_2(v1));				(* bad test IntegerExpression_2 *)
(* VBoolExp(BooleanExpression_2()); ERROR*)
