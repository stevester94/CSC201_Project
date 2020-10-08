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
    Instruction_0 of Skip_Const 
|   Instruction_1 of (Variable * Expression)
|   Instruction_2 of Instruction list
|   Instruction_3 of (BooleanExpression * Instruction * Instruction)
|   Instruction_4 of (BooleanExpression * Instruction);

datatype Type = BooleanType | IntegerType;

datatype Declaration = Declaration_1 of (Variable * Type);

datatype DeclarationList = DeclarationList_1 of Declaration list;

datatype Program = Program_1 of (DeclarationList * Instruction);


Program_1(
    DeclarationList_1([
        Declaration_1(
            "Muh_Variable",
            IntegerType
        )
    ]),
    Instruction_0(Skip)
);
