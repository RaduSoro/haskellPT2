{ 
module Parser where 
import Tokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }

%token
    -- syntax begins    
    nl   { TokenNewLine _}
    ';'  { TokenEndLine _ }
    '{'  { TokenLBracket _ }
    '}'  { TokenRBracket _ }
    '('  { TokenLParan _ } 
    ')'  { TokenRParan _ }
    comm { TokenLineComment _ $$ }
    block_comm { TokenBlockComment _ $$ }
    '\'' { TokenQuotes _ }
    ','  { TokenComma _ }
    '['  { TokenArrayBegin _ }
    ']'  { TokenArrayEnd _ }
  -- syntax end

  -- keywords begin
    var      { TokenVar _ }
    bool     { TokenBool _ $$}
    if       { TokenIf _ }
    else     { TokenElse _ }
    endif    { TokenEndIf _ }
    while    { TokenWhile _ }
    endwhile { TokenEndWhile _ }
    print    { TokenPrint _ }
    println  { TokenPrintNewLine _ }
    function { TokenFunction _ }
    and      { TokenAnd _ }
    or       { TokenOr _ }

    fct_size { TokenFunctionSize _ }
    fct_read { TokenFunctionReadInput _  }
    fct_pop { TokenFunctionRemoveLast _  }
    fct_push { TokenFunctionAddFirst _  }
    fct_enq { TokenFunctionAddLast _  }
    fct_deq { TokenFunctionRemoveFirst _  }
    fct_ins { TokenFunctionAddIndex _  }
    fct_rmv { TokenFunctionRemoveIndex _  }
    
    int      { TokenInt _ $$ }
    float    { TokenFloat _ $$ }
    str      { TokenString _ $$ } 
    sym      { TokenSym _ $$}
    null     { TokenNull _ }
  -- keywords end
  
  -- mathematical signs begin
    '==' { TokenEqual _ }
    '<'  { TokenLess _ }
    '>'  { TokenGreater _ }
    '<=' { TokenLessEqual _ }
    '>=' { TokenGreaterEqual _ } 
    '='  { TokenAssign _ } 
    '!=' { TokenDiff _ } 
    '+'  { TokenPlus _ } 
    '-'  { TokenMinus _ } 
    '*'  { TokenMultiply _ }
    '/'  { TokenDivide _ }
    '%'  { TokenModulo _ }
    '^'  { TokenPower _ }
    '.'  { TokenDecimal _ }
  -- mathematical signs end


%left '+' '-'
%left '*' '/'
%left '<' '>' '<=' '>='
%left '^'
%left NEG 


%% 

-- define expression types
prog : {- empty -}                                           { [] }
     | exp prog                                              { $1 : $2 } 
     | exp nl prog                                           { $1 : $3 } 
     | nl prog                                               { $2 }

exp : if_expression                                          { IfExp $1 }
    | var sym '=' value ';'                                  { Assign $2 $4 }
    | var sym '=' '[' list_values ']' ';'                    { Assign $2 (AccType (Arr $5 )) }
    | var sym '=' '['']' ';'                                 { Assign $2 (AccType (Arr [] )) }
    | var sym '[' value ']' '=' value ';'                    { ChangeArr $2 $4 $7 }
    | sym '=' value ';'                                      { Assign $1 $3 }
    | sym '[' value ']' '=' value ';'                        { ChangeArr $1 $3 $6 }
    | print '(' value ')' ';'                                { Print $3 }
    | println '(' value ')' ';'                              { PrintNewLine $3 }
    | while_expr                                             { WhileExp $1 }
    | comm                                                   { Comm $1 }
    | block_comm                                             { BlockComm $1 }
    

if_expression : if '('if_cond')' nl if_body endif                     { If $3 $6 }
              | if '('if_cond')' nl if_body else nl else_body endif   { IfElse $3 $6 $9 }

while_expr : while '('while_cond')' nl while_body endwhile            { WhileLoop $3 $6 }          


if_cond :  cond                                              {$1}
if_body : prog                                               {$1}
else_body : prog                                             {$1}

while_cond : cond                                           { $1 }
while_body : prog                                           { $1 }

list_values : basic_value ',' list_values                    { $1:$3 }
            | basic_value                                    { [$1] }

cond : '(' cond ')'                                         { $2 }
     | cond and cond                                        { And $1 $3 }
     | cond or cond                                         { Or $1 $3 }
     | values_bool                                          { Condition ( CondTerm ( $1 ) ) }
     | values_sym                                           { Condition ( CondTerm ( $1 ) ) }
     | math_comp                                            { Condition ( CondMath $1 ) }

math_comp : value '<'  value                                { Less $1 $3 }
          | value '<=' value                                { LessEqual $1 $3 }
          | value '>'  value                                { Greater $1 $3 }
          | value '>=' value                                { GreaterEqual $1 $3 }
          | value '==' value                                { Equal $1 $3 }
          | value '!=' value                                { Diff $1 $3 }

value : '(' value ')'                                       { $2 } 
      | value '+' value                                     { Plus $1 $3 } 
      | value '-' value                                     { Minus $1 $3 }
      | value '*' value                                     { Multiply $1 $3 }
      | value '/' value                                     { Divide $1 $3 }
      | value '%' value                                     { Modulo $1 $3 }
      | value '^' value                                     { Power $1 $3 }
      | values_all                                          { $1 }
      | values_sym '[' value ']'                            { ExtractIndex $1 $3 }
      | values_sym '[' value ']'                            { ExtractIndex $1 $3 }
      | function_call                                       { $1 }
      | '[' list_values ']'                                 { AccType (Arr $2 ) }
      | fct_read                                            { Fun(Func "read") }
      | int int                                             { Plus (AccType (Int $1)) (AccType (Int $2)) } 


function_call : values_sym '.' func_name                            { ApplyFunction $1 $3 }
              | values_sym '.' fct_ins '(' value ',' value ')'      { ApplyFunctionArg $1 (Func "ins") [$5, $7] } -- index, value
              | values_sym '.' fct_rmv '(' value ')'                { ApplyFunctionArg $1 (Func "rmv") [$5] }
              | values_sym '.' fct_push '(' value ')'               { ApplyFunctionArg $1 (Func "ins") [(AccType ( Int 0 )) ,$5] }
              | values_sym '.' fct_enq '(' value ')'                { ApplyFunctionArg $1 (Func "ins") [(ApplyFunction $1 (Func "size")), $5 ] }
              | values_sym '.' fct_pop                              { ApplyFunctionArg $1 (Func "rmv") [(AccType ( Int 0 ))] }
              | values_sym '.' fct_deq                              { ApplyFunctionArg $1 (Func "rmv") [(ApplyFunction $1 (Func "size_intern"))] }

values_all : values_num                                     { $1 }
           | values_bool                                    { $1 }
           | values_sym                                     { $1 }

values_bool : bool                                          { AccType (Bool $1) }

values_num : int                                            { AccType (Int $1) }
           | float                                          { AccType (Float $1) }
           | str                                            { AccType (Str $1) }

values_sym : sym                                            { AccType (Sym $1) }

values_null : null                                          { AccType ( NullVal ) }

basic_value : int                                           { Int $1 }
            | float                                         { Float $1 }
            | str                                           { Str $1 }
            | sym                                           { Sym $1 }
            | bool                                          { Bool $1 }
            | null                                          { NullVal }

func_name : fct_size                                        { Func "size" }
     
{ 
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error \n" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ " " ++(show t))

-- define what data the cek machine can use

data Exp = Assign String Term
         | ChangeArr String Term Term
         | Print Term
         | PrintNewLine Term
         | IfExp IfBody
         | WhileExp WhileBody
         | Comm String
         | BlockComm String
         deriving (Eq, Show)

data IfBody = If Conditional [Exp]
            | IfElse Conditional [Exp] [Exp]
            deriving (Eq, Show)

data WhileBody = WhileLoop Conditional [Exp]
               deriving (Eq, Show)

data Conditional = And Conditional Conditional
                 | Or Conditional Conditional
                 | Condition Condition
                 deriving (Eq, Show)

data Condition = CondTerm Term
               | CondMath MathComparisons
               deriving (Eq, Show)

data MathComparisons = Less Term Term
                     | LessEqual Term Term
                     | Greater Term Term
                     | GreaterEqual Term Term
                     | Equal Term Term
                     | Diff Term Term
                     deriving (Eq, Show)

data Term = Plus Term Term
           | Minus Term Term
           | Multiply Term Term
           | Divide Term Term
           | Modulo Term Term
           | Power Term Term
           | AccType AccType
           | ExtractIndex Term Term
           | ApplyFunction Term Func
           | ApplyFunctionArg Term Func [Term]
           | Fun Func
           deriving (Eq, Show)

data AccType = Int Int
             | Float Float
             | Bool Bool
             | Sym String
             | Str String
             | Arr [AccType]
             | NullVal
             deriving (Eq, Show)

data Func = Func String
     deriving (Eq, Show)



} 