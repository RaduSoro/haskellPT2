{
module Tokens where
}

%wrapper "posn"

$digit = [0-9]
$alpha = [a-zA-Z]
$whiteSpace = [\ ]
$decimalPoint = \.
$doubleQuotes = \"
$singleQuotes = \'
$newLine = \n

tokens :-
  -- syntax begins
  $whiteSpace+                  ;
  \t+                           ;
  \n+             { tok(\p s -> TokenNewLine p)}
  \;              { tok(\p s -> TokenEndLine p)}
  \{							{ tok(\p s -> TokenLBracket p)}
  \}							{ tok(\p s -> TokenRBracket p)}
  \(							{ tok(\p s -> TokenLParan p)}
  \)							{ tok(\p s -> TokenRParan p)}

  \/\/ [$alpha $digit \_ \- \' \, \( \) \[ \] \= \; $decimalPoint $whiteSpace \< \> \% $doubleQuotes \+ \+ \* \% \^]* { tok(\p s -> TokenLineComment p s)}
  
  "/*" [$alpha $digit \_ \- \' \, \( \) \[ \] \= \; \! $decimalPoint $whiteSpace \< \> \% $doubleQuotes $newLine \+ \* \% \^]* "*/"	  { tok(\p s -> TokenBlockComment p s)}
  \'							{ tok(\p s -> TokenQuotes p)}
  \,							{ tok(\p s -> TokenComma p)}
  \[							{ tok(\p s -> TokenArrayBegin p) }
  \]							{ tok(\p s -> TokenArrayEnd p) }
  -- syntax end
  -- keywords begin
  var                           { tok(\p s -> TokenVar p)}
  True                          { tok(\p s -> TokenBool p True)}
  False                         { tok(\p s -> TokenBool p False)}
  if                            { tok(\p s -> TokenIf p)}
  else                          { tok(\p s -> TokenElse p)}
  endif                         { tok(\p s -> TokenEndIf p)}
  while                         { tok(\p s -> TokenWhile p)}
  endwhile                      { tok(\p s -> TokenEndWhile p)}
  print                         { tok(\p s -> TokenPrint p)}
  println                       { tok(\p s -> TokenPrintNewLine p)}
  function						          { tok(\p s -> TokenFunction p)}
  and                           { tok(\p s -> TokenAnd p)}
  or                            { tok(\p s -> TokenOr p)}
  -- keywords end
  -- custom functions
  size\(\)                      { tok(\p s -> TokenFunctionSize p)}
  readInput\(\)                 { tok(\p s -> TokenFunctionReadInput p)}
  pop\(\)                       { tok(\p s -> TokenFunctionRemoveLast p)}
  push                          { tok(\p s -> TokenFunctionAddFirst p)}
  enque                         { tok(\p s -> TokenFunctionAddLast p)}
  deque\(\)                     { tok(\p s -> TokenFunctionRemoveFirst p)}
  insert                        { tok(\p s -> TokenFunctionAddIndex p)}
  remove                        { tok(\p s -> TokenFunctionRemoveIndex p)}
  -- custom functions
  -- mathematical signs begin
  \=\=                          { tok(\p s -> TokenEqual p)}
  \<                            { tok(\p s -> TokenLess p)}
  \>                            { tok(\p s -> TokenGreater p)}
  \<=                           { tok(\p s -> TokenLessEqual p)}
  \>=                           { tok(\p s -> TokenGreaterEqual p)}
  \=                            { tok(\p s -> TokenAssign p)}
  \!\=                          { tok(\p s -> TokenDiff p)}
  \+                            { tok(\p s -> TokenPlus p)}
  \-                            { tok(\p s -> TokenMinus p)}
  \*                            { tok(\p s -> TokenMultiply p)}
  \/                            { tok(\p s -> TokenDivide p)}
  \%                            { tok(\p s -> TokenModulo p)}
  \^                            { tok(\p s -> TokenPower p)}
  $decimalPoint                 { tok(\p s -> TokenDecimal p )}

  -- mathematial signs end
  -- variables begin
  [\-]?$digit+                                                                                    { tok(\p s -> TokenInt p (read s) )}
  [\-]?$digit+ $decimalPoint $digit+                                                              { tok(\p s -> TokenFloat p (read s) )}
  $doubleQuotes [.~$doubleQuotes]* $doubleQuotes                                                  { tok(\p s -> TokenString p (init(tail s)) ) } 
  $alpha [$alpha $digit \_]*                                                                      { tok(\p s -> TokenSym p s)}
  null                                                                                            { tok(\p s -> TokenNull p)}
  -- variables end

{

-- Helper function
tok f p s = f p s

-- The token type:
data Token = TokenNewLine AlexPosn
           | TokenEndLine AlexPosn
           | TokenLBracket AlexPosn
           | TokenRBracket AlexPosn
           | TokenLParan AlexPosn
           | TokenRParan AlexPosn
           | TokenLineComment AlexPosn String
           | TokenBlockComment AlexPosn String
           | TokenQuotes AlexPosn
           | TokenComma AlexPosn
           | TokenArrayBegin AlexPosn
           | TokenArrayEnd AlexPosn
           | TokenVar AlexPosn
           | TokenBool AlexPosn Bool
           | TokenIf AlexPosn
           | TokenElse AlexPosn
           | TokenEndIf AlexPosn
           | TokenWhile AlexPosn
           | TokenEndWhile AlexPosn
           | TokenPrint AlexPosn
           | TokenPrintNewLine AlexPosn
           | TokenFunction AlexPosn
           | TokenAnd AlexPosn
           | TokenOr AlexPosn
           | TokenFunctionSize AlexPosn
           | TokenFunctionReadInput AlexPosn
           | TokenFunctionRemoveLast AlexPosn
           | TokenFunctionAddFirst AlexPosn
           | TokenFunctionAddLast AlexPosn
           | TokenFunctionRemoveFirst AlexPosn
           | TokenFunctionAddIndex AlexPosn
           | TokenFunctionRemoveIndex AlexPosn
           | TokenEqual AlexPosn
           | TokenDiff AlexPosn
           | TokenLess AlexPosn
           | TokenGreater AlexPosn
           | TokenLessEqual AlexPosn
           | TokenGreaterEqual AlexPosn
           | TokenAssign AlexPosn
           | TokenPlus AlexPosn
           | TokenMinus AlexPosn
           | TokenMultiply AlexPosn
           | TokenDivide AlexPosn 
           | TokenModulo AlexPosn 
           | TokenPower AlexPosn
           | TokenDecimal AlexPosn
           | TokenInt AlexPosn Int
           | TokenFloat AlexPosn Float
           | TokenString AlexPosn String
           | TokenSym AlexPosn String
           | TokenNull AlexPosn
           deriving (Eq,Show)

tokenPosn :: Token -> String

-- syntax
tokenPosn (TokenNewLine (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEndLine (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLBracket (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRBracket (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLineComment (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBlockComment (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenQuotes (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenArrayBegin (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenArrayEnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

-- keywords
tokenPosn (TokenVar (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBool (AlexPn a l c) b) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEndIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEndWhile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPrint (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPrintNewLine (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFunction (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

-- built-in functions
tokenPosn (TokenFunctionSize (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFunctionReadInput (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFunctionRemoveLast (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFunctionAddFirst (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFunctionAddLast (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFunctionRemoveFirst (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFunctionAddIndex (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFunctionRemoveIndex (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

-- mathematical
tokenPosn (TokenEqual (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLess (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreater (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessEqual (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreaterEqual (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAssign (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDiff (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMinus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMultiply (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDivide (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenModulo (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPower (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDecimal (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

-- values
tokenPosn (TokenInt (AlexPn a l c) n) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFloat (AlexPn a l c) f) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenString (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSym (AlexPn a l c) v) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNull (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

scanTokens = alexScanTokens

}