{-------------------------------------------------------------------------------------
-
- An XQuery parser
- Programmer: Leonidas Fegaras
- Email: fegaras@cse.uta.edu
- Web: http://lambda.uta.edu/
- Creation: 02/15/08, last update: 01/07/09
- 
- Copyright (c) 2008 by Leonidas Fegaras, the University of Texas at Arlington. All rights reserved.
- This material is provided as is, with absolutely no warranty expressed or implied.
- Any use is at your own risk. Permission is hereby granted to use or copy this program
- for any purpose, provided the above notices are retained on all copies.
-
--------------------------------------------------------------------------------------}

{
module Text.XML.HXQ.Parser(Ast(..),scan,parse,call,concatenateAll,ppAst) where
import Char
}

%name parse
%tokentype { Token }
%error { parseError }

%token
	'return'	{ RETURN }
	'some'		{ SOME }
	'every'		{ EVERY }
	'if' 		{ IF }
	'then' 		{ THEN }
	'else' 		{ ELSE }
	'[' 		{ LB }
	']' 		{ RB }
	'(' 		{ LP }
	')' 		{ RP }
	'{' 		{ LSB }
	'}' 		{ RSB }
        'to' 		{ TO }
	'+' 		{ PLUS }
	'-' 		{ MINUS }
	'*' 		{ TIMES }
	'div' 		{ DIV }
	'idiv' 		{ IDIV }
	'mod' 		{ MOD }
	'=' 		{ TEQ }
	'!=' 		{ TNE }
	'<' 		{ TLT }
	'<=' 		{ TLE }
	'>' 		{ TGT }
	'>=' 		{ TGE }
	'<<' 		{ PRE }
	'>>' 		{ POST }
	'is' 		{ IS }
	'eq' 		{ SEQ }
	'ne' 		{ SNE }
	'lt' 		{ SLT }
	'le' 		{ SLE }
	'gt' 		{ SGT }
	'ge' 		{ SGE }
	'and' 		{ AND }
	'or' 		{ OR }
	'not' 		{ NOT }
	'union' 	{ UNION }
	'intersect' 	{ INTERSECT }
	'except' 	{ EXCEPT }
	'for' 		{ FOR }
	'let' 		{ LET }
	'in' 		{ IN }
	'as' 		{ AS }
	',' 		{ COMMA }
	':=' 		{ ASSIGN }
	'where' 	{ WHERE }
	'order' 	{ ORDER }
        'by'    	{ BY }
	'ascending' 	{ ASCENDING }
	'descending' 	{ DESCENDING }
	'element' 	{ ELEMENT }
	'attribute' 	{ ATTRIBUTE }
	'</' 		{ STAG }
	'/>' 		{ ETAG }
	'satisfies' 	{ SATISFIES }
	'@' 		{ ATSIGN }
	'/' 		{ SLASH }
	'QName' 	{ QName $$ }
	'declare'       { DECLARE }
	'function'      { FUNCTION }
	'variable'      { VARIABLE }
	'at' 		{ AT }
	'..' 		{ DOTS }
	'.' 		{ DOT }
	';' 		{ SEMI }
	':' 		{ COLON }
	'insert'        { INSERT }
	'delete'	{ DELETE }
	'replace'	{ REPLACE }
	'into'		{ INTO }
	'from'		{ FROM}
	'with'		{ WITH }
        'instance'      { INSTANCE }
        'of'            { OF }
        '?'             { QMARK }
        'cast'          { CAST }
        'castable'      { CASTABLE }
        'case'          { CASE }
        'default'       { DEFAULT }
        'typeswitch'    { TYPESWITCH }
	'Variable' 	{ Variable $$ }
	'XMLtext' 	{ XMLtext $$ }
	'Integer' 	{ TInteger $$ }
	'Double' 	{ TFloat $$ }
	'String' 	{ TString $$ }
	'EOF' 	        { TokenEOF }


%nonassoc	'for' 'let' 'satisfies' 'return' 'typeswitch'
%nonassoc	'with' 'from' 'into' 'instance' 'cast' 'castable' 'case' 'default'
%nonassoc	'else'
%left		'intersect' 'union' 'except'
%right		'or'
%right		'and'
%nonassoc	'not'
%left		'to'
%left		'=' '<' '>' '<=' '>=' '!=' '>>' '<<' 'is' 'eq' 'ne' 'lt' 'le' 'gt' 'ge'
%left		'+' '-'
%left		'*' 'div' 'idiv' 'mod'
%nonassoc	UMINUS


%%
main           :: { [ Ast ] }
main            :   prog 'EOF'                         { $1 }

prog           :: { [ Ast ] }
prog            :   def                                { [$1] }
                |   def 'XMLtext'                      { [$1] }
                |   prog ';' def                       { $1++[$3] }
                |   prog ';' def 'XMLtext'             { $1++[$3] }

def            :: { Ast }
def             :   expr                                { $1 }
                |   'declare' 'variable' var ':=' expr  { Ast "variable" [$3,$5] }
                |   'declare' 'variable' var 'as' type
                      ':=' expr                         { Ast "variable" [$3,$7] }
                |   'declare' 'function' qname
                      '(' params ')' '{' expr '}'       { Ast "function" ([Avar $3,$8]++$5) }
                |   'declare' 'function' qname
                      '(' params ')' 'as' type
		      '{' expr '}'                      { Ast "function" ([Avar $3,$10]++$5) }

qname          :: { String }
qname          :    'QName'                             { $1 }
               |    'QName' ':' 'QName'                 { $1++":"++$3 }

params         :: { [ Ast ] }
params          :   var                                 { [$1] }
                |   var 'as' type                       { [$1] }
                |   params ',' var                      { $1++[$3] }
                |   params ',' var 'as' type            { $1++[$3] }

type           ::  { Ast }
type            :  qname               %prec 'case'     { Avar $1 }
                |  qname '+'                            { Ast "+" [Avar $1] }
                |  qname '*'                            { Ast "*" [Avar $1] }
                |  qname '?'                            { Ast "?" [Avar $1] }
                |  sequence_type       %prec 'case'     { $1 }
                |  sequence_type '+'                    { Ast "+" [$1] }
                |  sequence_type '*'                    { Ast "*" [$1] }
                |  sequence_type '?'                    { Ast "?" [$1] }

sequence_type  ::  { Ast }
sequence_type   :  'element' '(' ')'                    { Ast "element" [] }
                |  'attribute' '(' ')'                  { Ast "attribute" [] }
                |  qname '(' ')'                        { Ast $1 [] }
                |  'element' '(' typeparams ')'         { Ast "element" $3 }
                |  'attribute' '(' typeparams ')'       { Ast "attribute" $3 }

typeparams     :: { [ Ast ] }
                :  '*'                                  { [Avar "*"] }
                |  qname                                { [Avar $1] }
                |  '*' ',' qname                        { [Avar "*",Avar $3] }
                |  qname ',' qname                      { [Avar $1,Avar $3] }
                |  '*' ',' qname '?'                    { [Avar "*",Ast "?" [Avar $3]] }
                |  qname ',' qname '?'                  { [Avar $1,Ast "?" [Avar $3]] }

var            :: { Ast }
var		:   'Variable' 				{ Avar $1 }

expr           :: { Ast }
expr		:   clauses opt_where opt_order
                         'return' expr			{ (snd $3) ($1 ($2 ((fst $3) $5))) }
		|   'some' for_bindings
                         'satisfies' expr		{ call "some" [$2 $4] }
		|   'every' for_bindings 
                         'satisfies' expr		{ call "not" [call "some" [$2 (call "not" [$4])]] }
		|   'if' expr 'then' expr 'else' expr	{ Ast "if" [$2,$4,$6] }
		|   full_path				{ $1 }
		|   element				{ $1 }
		|   computed				{ $1 }
		|   expr 'to' expr			{ call "to" [$1,$3] }
		|   expr '+' expr			{ call "+" [$1,$3] }
		|   expr '-' expr			{ call "-" [$1,$3] }
		|   expr '*' expr			{ call "*" [$1,$3] }
		|   expr 'div' expr			{ call "div" [$1,$3] }
		|   expr 'idiv' expr			{ call "idiv" [$1,$3] }
		|   expr 'mod' expr			{ call "mod" [$1,$3] }
		|   expr '=' expr			{ call "=" [$1,$3] }
		|   expr '!=' expr			{ call "!=" [$1,$3] }
		|   expr '<' expr			{ call "<" [$1,$3] }
		|   expr '<=' expr			{ call "<=" [$1,$3] }
		|   expr '>' expr			{ call ">" [$1,$3] }
		|   expr '>=' expr			{ call ">=" [$1,$3] }
		|   expr '<<' expr			{ call "<<" [$1,$3] }
		|   expr '>>' expr			{ call ">>" [$1,$3] }
		|   expr 'is' expr			{ call "is" [$1,$3] }
		|   expr 'eq' expr			{ call "eq" [$1,$3] }
		|   expr 'ne' expr			{ call "ne" [$1,$3] }
		|   expr 'lt' expr			{ call "lt" [$1,$3] }
		|   expr 'le' expr			{ call "le" [$1,$3] }
		|   expr 'gt' expr			{ call "gt" [$1,$3] }
		|   expr 'ge' expr			{ call "ge" [$1,$3] }
		|   expr 'and' expr			{ call "and" [$1,$3] }
		|   expr 'or' expr			{ call "or" [$1,$3] }
		|   expr 'not' expr			{ call "not" [$1,$3] }
		|   expr 'union' expr			{ call "union" [$1,$3] }
		|   expr 'intersect' expr		{ call "intersect" [$1,$3] }
		|   expr 'except' expr			{ call "except" [$1,$3] }
                |   expr 'instance' 'of' type           { call "instance-of" [$1,Ast "type" [$4]] }
                |   expr 'cast' 'as' type               { call "cast-as" [$1,Ast "type" [$4]] }
                |   expr 'castable' 'as' type           { call "castable-as" [$1,Ast "type" [$4]] }
                |   'typeswitch' '(' expr ')' typecases { let v = "_tc" in Ast "let" [Avar v,$3,$5 v] }
		|   '+' expr       %prec UMINUS		{ call "uplus" [$2] }
		|   '-' expr       %prec UMINUS		{ call "uminus" [$2] }
		|   'not' expr     %prec UMINUS		{ call "not" [$2] }
		|   string				{ $1 }
		|   'Integer'				{ Aint $1 }
		|   'Double'				{ Afloat $1 }
		|   'insert' expr 'into' expr		{ Ast "insert" [$2,Ast "destination" [$4]] }
		|   'delete' 'from' expr		{ Ast "delete" [$3] }
		|   'replace' expr 'with' expr		{ Ast "replace" [$2,$4] }

expl           :: { [ Ast ] }
expl		:   expr				{ [$1] }
		|   expl ',' expr 			{ $1++[$3] }

clauses        :: { Ast -> Ast }
clauses		:   'for' for_bindings			{ $2 }
		|   'let' let_bindings			{ $2 }
		|   clauses 'for' for_bindings		{ $1 . $3 }
		|   clauses 'let' let_bindings		{ $1 . $3 }

for_bindings   :: { Ast -> Ast }
for_bindings	:   var 'in' expr			{ \x -> Ast "for" [$1,Avar "$",$3,x] }
		|   var 'at' var 'in' expr		{ \x -> Ast "for" [$1,$3,$5,x] }
		|   for_bindings ',' var 'in' expr	{ \x -> $1(Ast "for" [$3,Avar "$",$5,x]) }
		|   for_bindings ',' var 'at' var
                        'in' expr			{ \x -> $1(Ast "for" [$3,$5,$7,x]) }

let_bindings   :: { Ast -> Ast }
let_bindings	:   var ':=' expr			{ \x -> Ast "let" [$1,$3,x] }
		|   let_bindings ','
			var ':=' expr			{ \x -> $1(Ast "let" [$3,$5,x]) }

opt_where      :: { Ast -> Ast }
opt_where 	:   'where' expr			{ \x -> Ast "predicate" [$2,x] }
		|   {- empty -}				{ id }

opt_order      :: { ( Ast -> Ast, Ast -> Ast ) }
opt_order	:   'order' 'by' order_list		{ (\x -> Ast "sortTuple" (x:(fst $3)),
                                                           \x -> Ast "sort" (x:(snd $3))) }
		|   {- empty -}				{ (id,id) }

order_list     :: { ( [ Ast ], [ Ast ] ) }
order_list	:   expr mode				{ ([$1],[$2]) }
		|   order_list ',' expr mode		{ ((fst $1)++[$3],(snd $1)++[$4]) }

mode           :: { Ast }
mode		:   'ascending'				{ Avar "ascending" }
		|   'descending'			{ Avar "descending" }
		|   {- empty -}				{ Avar "ascending" }

computed       :: { Ast }
computed	:   'element' '(' qname ')'		{ call "element" [Avar $3] }
		|   'attribute' '(' qname ')'		{ call "attribute" [Avar $3] }

element        :: { Ast }
element         :   stag '>' content '</' qname '>'     { if head $1 == Astring $5
                                                             then Ast "element_construction" ($1++[Ast "append" $3])
                                                          else parseError [TError ("Unmatched tags in element construction: "
                                                                                   ++(show (head $1))++" '"++$5++"'")] }
                |   stag '>' '</' qname '>'             { if head $1 == Astring $4
                                                             then Ast "element_construction" ($1++[Ast "append" []])
                                                          else parseError [TError ("Unmatched tags in element construction: "
                                                                                   ++(show (head $1))++" '"++$4++"'")] }
                |   stag '/>'                           { Ast "element_construction" ($1++[Ast "append" []]) }
                |   'element' '{' expr '}' '{' expl '}' { Ast "element_construction" [$3,Ast "attributes" [],concatenateAll $6] }
                |   'attribute' '{' expr '}''{' expl '}'{ Ast "attribute_construction" [$3,concatenateAll $6] }
                |   'element' qname '{' expl '}'        { Ast "element_construction" [Astring $2,Ast "attributes" [],concatenateAll $4] }
                |   'attribute' qname '{' expl '}'      { Ast "attribute_construction" [Astring $2,concatenateAll $4] }

stag           :: { [ Ast ] }
stag		:   '<' qname				{ [Astring $2,Ast "attributes" []] }
                |   '<' qname attributes		{ [Astring $2,Ast "attributes" $3] }

content        :: { [ Ast ] }
content		:   '{' expl '}'			{ [concatenateAll $2] }
		|   'String'				{ [Astring $1] }
		|   'XMLtext'				{ [Astring $1] }
		|   element				{ [$1] }
		|   content '{' expl '}'		{ $1++[concatenateAll $3] }
                |   content 'String'		        { $1++[Astring $2] }
		|   content 'XMLtext'			{ $1++[Astring $2] }
		|   content element			{ $1++[$2] }

string         :: { Ast }
string          : stringc                               { if length $1 == 0 then Astring ""
                                                          else if length $1 == 1 then head $1 else Ast "append" $1 }

stringc        :: { [Ast] }
stringc         :   'String'                            { if $1=="" then [] else [Astring $1] }
                |   '{' expl '}'                        { [concatAll $2] }
                |   stringc 'String'                    { if $2=="" then $1 else $1++[Astring $2] }
                |   stringc '{' expl '}'                { $1++[concatAll $3] }

attributes     :: { [ Ast ] }
attributes	:   qname '=' string  	                { [Ast "pair" [Astring $1,$3]] }
		|   attributes qname '=' string	        { $1++[Ast "pair" [Astring $2,$4]] }

full_path      :: { Ast }
full_path       :   simple_step predicates              { $1 "child" (Avar ".") $2 }
                |   '@' simple_step predicates          { $2 "attribute" (Avar ".") $3 }
                |   simple_step predicates path         { $3 ($1 "child" (Avar ".") $2) }
                |   '@' simple_step predicates path     { $4 ($2 "attribute" (Avar ".") $3) }

path           :: { Ast -> Ast }
path            :   step                                { $1 }
                |   path step                           { $2 . $1 }

step           :: { Ast -> Ast }
step            :   '/' simple_step predicates          { \e -> $2 "child" e $3 }
                |   '/' '@' simple_step predicates      { \e -> $3 "attribute" e $4 }
                |   '/' '/' simple_step predicates      { \e -> $3 "descendant" e $4 }
                |   '/' '/' '@' simple_step predicates  { \e -> $4 "attribute-descendant" e $5 }
                |   '/' '..'                            { \e -> Ast "step" [Avar "parent",Astring "*",e] }

predicates      :: { [ Ast ] }
predicates      :   predicates '[' expr ']'             { $1 ++ [$3] }
		|   {- empty -}				{ [] }

simple_step    :: { String -> Ast -> [ Ast ] -> Ast }
simple_step     :   primary_expr                        { \t e ps -> if null ps
								     then $1 t e
                                                                     else Ast "filter" ($1 t e:ps) }
                |   '*'                                 { \t e ps -> Ast "step" ((Avar t):(Astring "*"):e:ps) }
                |   qname                               { \t e ps -> if elem $1 path_steps
                                                                     then parseError [TError ("Axis "++$1++" is missing a node step")]
                                                                     else Ast "step" ((Avar t):(Astring $1):e:ps) }
                |   'QName' ':' ':' qname               { \t e ps -> if elem $1 path_steps
                                                                     then if t == "child"
                                                                          then Ast "step" ((Avar $1):(Astring $4):e:ps)
                                                                          else parseError [TError ("The navigation step must be /"++$1++"::"++$4)]
                                                                     else parseError [TError ("Not a valid axis name: "++$1)] }
                |   'QName' ':' ':' '*'                 { \t e ps -> if elem $1 path_steps
                                                                     then if t == "child"
                                                                          then Ast "step" ((Avar $1):(Astring "*"):e:ps)
                                                                          else parseError [TError ("The navigation step must be /"++$1++"::*")]
                                                                     else parseError [TError ("Not a valid axis name: "++$1)] }

primary_expr   :: { String -> Ast -> Ast }
primary_expr    :   var                                 { \_ _ -> $1 }
                |   '.'                                 { \_ e -> e }
                |   '(' expl ')'                        { \t e -> if e == Avar "."
                                                                  then concatenateAll $2
	                                                          else Ast "context" [e,Astring t,concatenateAll $2] }
                |   '(' ')'                             { \_ _ -> call "empty" [] }
                |   qname '(' expl ')'                  { \t e -> if e == Avar "."
                                                                     then call $1 $3
                                                                  else Ast "context" [e,Astring t,call $1 $3] }
                |   qname '(' ')'                       { \_ e -> if elem $1 ["last","position","true","false","empty","select"]
                                                                  then call $1 []
                                                                  else call $1 [e] }

typecases      :: { String -> Ast }
typecases       :   'case' type 'return' expr
                           'default' 'return' expr      { \v -> Ast "if" [call "instance-of" [Avar v,Ast "type" [$2]],$4,$7] }
                |   'case' type 'return' expr typecases { \v -> Ast "if" [call "instance-of" [Avar v,Ast "type" [$2]],$4,$5 v] }


{

-- Abstract Syntax Tree for XQueries
data Ast = Ast String [Ast]
         | Avar String
         | Aint Int
         | Afloat Double
         | Astring String
         deriving Eq


instance Show Ast
  where show (Ast s []) = s ++ "()"
        show (Ast s (x:xs)) = s ++ "(" ++ show x
                              ++ foldr (\a r -> ","++show a++r) "" xs
                              ++ ")"
        show (Avar s) = s
        show (Aint n) = show n
        show (Afloat n) = show n
        show (Astring s) = "\'" ++ s ++ "\'"


screenSize = 80::Int

prettyAst :: Ast -> Int -> (String,Int)
prettyAst (Avar s) p = (s,(length s)+p)
prettyAst (Aint n) p = let s = show n in (s,(length s)+p)
prettyAst (Afloat n) p = let s = show n in (s,(length s)+p)
prettyAst (Astring s) p = ("\'" ++ s ++ "\'",(length s)+p+2)
prettyAst (Ast s args) p
    = let (ps,np) = prettyArgs args
      in (s++"("++ps++")",np+1)
    where prettyArgs [] = ("",p+1)
          prettyArgs xs = let ss = show (head xs) ++ foldr (\a r -> ","++show a++r) "" (tail xs)
                              np = (length s)+p+1
                          in if (length ss)+p < screenSize
                             then (ss,(length ss)+p)
                             else let ds = map (\x -> let (s,ep) = prettyAst x np
                                                      in (s ++ ",\n" ++ space np,ep)) (init xs)
                                      (ls,lp) = prettyAst (last xs) np
                                  in (concatMap fst ds ++ ls,lp)
          space n = replicate n ' '


ppAst :: Ast -> String
ppAst e = let (s,_) = prettyAst e 0 in s


call :: String -> [Ast] -> Ast
call name args = Ast "call" ((Avar name):args)


concatenateAll :: [Ast] -> Ast
concatenateAll [x] = x
concatenateAll (x:xs) = foldl (\a r -> call "concatenate" [a,r]) x xs
concatenateAll _ = call "empty" []


concatAll :: [Ast] -> Ast
concatAll [x] = call "string" [x]
concatAll (x:xs) = foldl (\a r -> call "concatenate" [call "string" [a],r]) x xs
concatAll _ = call "empty" []


path_steps = ["child", "descendant", "attribute", "self", "descendant-or-self", "following-sibling", "following",
              "attribute-descendant", "parent", "ancestor", "preceding-sibling", "preceding", "ancestor-or-self" ]


data Token
  = RETURN | SOME | EVERY | IF | THEN | ELSE | LB | RB | LP | RP | LSB | RSB
  | TO | PLUS | MINUS | TIMES | DIV | IDIV | MOD | AS | QMARK
  | TEQ | TNE | TLT | TLE | TGT | TGE | SEQ | SNE | SLT | SLE | SGT | SGE
  | AND | OR | NOT | UNION | INTERSECT | EXCEPT | FOR | LET | IN | COMMA
  | ASSIGN | WHERE | ORDER | BY | ASCENDING | DESCENDING | ELEMENT
  | ATTRIBUTE | STAG | ETAG | SATISFIES | ATSIGN | SLASH | DECLARE | SEMI | COLON
  | FUNCTION | VARIABLE | AT | DOT | DOTS | TokenEOF | PRE | POST | IS
  | INSERT | INTO | DELETE | FROM | REPLACE | WITH | INSTANCE | OF
  | CAST | CASTABLE | CASE | DEFAULT | TYPESWITCH
  | QName String | Variable String | XMLtext String | TInteger Int
  | TFloat Double | TString String | TError String
    deriving Eq


instance Show Token
    where show (QName s) = "QName("++s++")"
	  show (Variable s) = "Variable("++s++")"
	  show (XMLtext s) = "XMLtext("++s++")"
	  show (TInteger n) = "Integer("++(show n)++")"
	  show (TFloat n) = "Double("++(show n)++")"
	  show (TString s) = "String("++s++")"
	  show (TError s) = "'"++s++"'"
          show t = case filter (\(n,_) -> n==t) tokenList of
                     (_,b):_ -> b
                     _ -> "Illegal token"


printToken (QName s) = s
printToken (Variable s) = "$"++s
printToken (XMLtext s) = "'"++s++"'"
printToken (TInteger n) = show n
printToken (TFloat n) = show n
printToken (TString s) = "\""++s++"\""
printToken (TError s) = "error("++s++")"
printToken t = case filter (\(n,_) -> n==t) tokenList of
           (_,b):_ -> b
           _ -> "Illegal token"


tokenList :: [(Token,String)]
tokenList = [(TokenEOF,"EOF"),(RETURN,"return"),(SOME,"some"),(EVERY,"every"),(IF,"if"),(THEN,"then"),(ELSE,"else"),
             (LB,"["),(RB,"]"),(LP,"("),(RP,")"),(LSB,"{"),(RSB,"}"),(QMARK,"?"),
             (TO,"to"),(PLUS,"+"),(MINUS,"-"),(TIMES,"*"),(DIV,"div"),(IDIV,"idiv"),(MOD,"mod"),
             (TEQ,"="),(TNE,"!="),(TLT,"<"),(TLE,"<="),(TGT,">"),(TGE,">="),(PRE,"<<"),(POST,">>"),
             (IS,"is"),(SEQ,"eq"),(SNE,"ne"),(SLT,"lt"),(SLE,"le"),(SGT,"gt"),(SGE,"ge"),(AND,"and"),
             (OR,"or"),(NOT,"not"),(UNION,"|"),(INTERSECT,"intersect"),(EXCEPT,"except"),
             (FOR,"for"),(LET,"let"),(IN,"in"),(AS,"as"),(COMMA,"','"),(ASSIGN,":="),(WHERE,"where"),(ORDER,"order"),
             (BY,"by"),(ASCENDING,"ascending"),(DESCENDING,"descending"),(ELEMENT,"element"),
             (ATTRIBUTE,"attribute"),(STAG,"</"),(ETAG,"/>"),(SATISFIES,"satisfies"),(ATSIGN,"@"),
             (SLASH,"/"),(DECLARE,"declare"),(FUNCTION,"function"),(VARIABLE,"variable"),
  	     (INSERT,"insert"),(INTO,"into"),(DELETE,"delete"),(FROM,"from"),(REPLACE,"replace"),(WITH,"with"),
             (AT,"at"),(DOTS,".."),(DOT,"."),(SEMI,";"),(COLON,":"),(INSTANCE,"instance"),(OF,"of"),
             (CAST,"cast"),(CASTABLE,"castable"),(CASE,"case"),(DEFAULT,"default"),(TYPESWITCH,"typeswitch")]


parseError tk = error (case tk of
                         ((TError s):_) -> "Parse error: "++s
                         (TokenEOF:_) -> "Parse error: Unexpected end of file"
		         _ -> "Parse error: "++(foldr (\a r -> (printToken a)++" "++r) "" (init (take 11 tk))))


scan :: String -> [Token]
scan cs = lexer cs ""


xmlText :: String -> [Token]
xmlText "" = []
xmlText text = [XMLtext text]


-- scans XML syntax and returns an XMLtext token with the text
xml :: String -> String -> String -> [Token]
xml ('{':cs) text n = (xmlText text)++(LSB : lexer cs ('{':n))
xml ('<':'/':cs) text n = (xmlText text)++(STAG : lexer cs ('<':'/':n))
xml ('<':'!':'-':cs) text n = xmlComment cs (text++"<!-") n
xml ('<':cs) text n = (xmlText text)++(TLT : lexer cs ('<':n))
xml ('(':':':cs) text n = xqComment cs text n
xml (c:cs) text n = xml cs (text++[c]) n
xml [] text _ = xmlText text


xqComment :: String -> String -> String -> [Token]
xqComment (':':')':cs) text n = xml cs text n
xqComment (_:cs) text n = xqComment cs text n
xqComment [] text _ = xmlText text


xmlComment :: String -> String -> String -> [Token]
xmlComment ('-':'>':cs) text n = xml cs (text++"->") n
xmlComment (c:cs) text n = xmlComment cs (text++[c]) n
xmlComment [] text _ = xmlText text


isQN :: Char -> Bool
isQN c = elem c "_-." || isDigit c || isAlpha c


isVar :: Char -> Bool
isVar c = elem c "_-." || isDigit c || isAlpha c


inXML :: String -> Bool
inXML ('>':'<':_) = True
inXML _ = False


-- the XQuery scanner
lexer :: String -> String -> [Token]
lexer [] "" = [ TokenEOF ]
lexer [] _ = [ TError "Unexpected end of input" ]
lexer (' ':'>':' ':cs) n = TGT : lexer cs n
lexer (c:cs) n
      | isSpace c = lexer cs n
      | isAlpha c || c=='_' = lexVar (c:cs) n
      | isDigit c = lexNum (c:cs) n
lexer ('$':c:cs) n | isAlpha c
      = let (var,rest) = span isVar (c:cs)
        in (Variable var) : lexer rest n
lexer (':':'=':cs) n = ASSIGN : lexer cs n
lexer ('<':'/':cs) n = STAG : lexer cs ('<':'/':n)
lexer ('<':'=':cs) n = TLE : lexer cs n
lexer ('>':'=':cs) n = TGE : lexer cs n
lexer ('<':'<':cs) n = PRE : lexer cs n
lexer ('>':'>':cs) n = POST : lexer cs n
lexer ('/':'>':cs) m = case m of
                         '<':n -> ETAG : (if inXML n then xml cs "" n else lexer cs n)
                         _ -> [ TError "Unexpected token: '/>'" ]
lexer ('(':':':cs) n = lexComment cs n
lexer ('<':'!':'-':cs) n = lexXmlComment cs "<!-" n
lexer ('.':'.':cs) n = DOTS : lexer cs n
lexer ('.':cs) n = DOT : lexer cs n
lexer ('!':'=':cs) n = TNE : lexer cs n
lexer ('\'':cs) n = lexString cs "" ('\'':n)
lexer ('\"':cs) n = lexString cs "" ('\"': n)
lexer ('[':cs) n = LB : lexer cs n
lexer (']':cs) n = RB : lexer cs n
lexer ('(':cs) n = LP : lexer cs n
lexer (')':cs) n = RP : lexer cs n
lexer ('}':cs) m = case m of
                     '{':'\"':n -> RSB : lexString cs "" ('\"':n)
                     '{':'\'':n -> RSB : lexString cs "" ('\'':n)
                     '{':n -> RSB : (if inXML n then xml cs "" n else lexer cs n)
                     _ -> [ TError "Unexpected token: '}'" ]
lexer ('+':cs) n = PLUS : lexer cs n
lexer ('-':cs) n = MINUS : lexer cs n
lexer ('*':cs) n = TIMES : lexer cs n
lexer ('=':cs) n = TEQ : lexer cs n
lexer ('<':c:cs) n = TLT : (lexer (c:cs) (if isAlpha c then ('<':n) else n))
lexer ('>':cs) m = case m of
                     '<':'/':'>':'<':n -> TGT : (if inXML n then xml cs "" n else lexer cs n)
                     '<':n -> TGT : xml cs "" ('>':m) 
                     _ -> TGT : lexer cs m
lexer (',':cs) n = COMMA : lexer cs n
lexer ('@':cs) n = ATSIGN : lexer cs n
lexer ('?':cs) n = QMARK : lexer cs n
lexer ('/':cs) n = SLASH : lexer cs n
lexer ('{':cs) n = LSB : lexer cs ('{':n)
lexer ('|':cs) n = UNION : lexer cs n
lexer (';':cs) n = SEMI : lexer cs n
lexer (':':cs) n = COLON : lexer cs n
lexer (c:cs) n = TError ("Illegal character: '"++[c,'\'']) : lexer cs n


lexExp :: String -> (String,String)
lexExp (e:cs)
    | e == 'e' || e == 'E'
    = case cs of
        '+':rest -> span isDigit rest
        '-':rest -> let (s,rest1) = span isDigit rest
                    in ('-':s,rest1)
        rest -> span isDigit rest
lexExp cs = ("",cs)


lexNum :: String -> String -> [Token]
lexNum cs n
    = let (si,rest) = span isDigit cs
      in case rest of
           '.':rest1
               -> let (sd,rest2) = span isDigit rest1
                  in case lexExp rest2 of
                       ("",_) -> (TFloat (read $ si ++ "." ++ sd)) : lexer rest2 n
                       (exp,rest3) -> (TFloat (read $ si ++ "." ++ sd ++ "e" ++ exp)) : lexer rest3 n
           _ -> case lexExp rest of
                  ("",_) -> (TInteger (read si)) : lexer rest n
                  (exp,rest3) -> (TFloat (read $ si ++ "e" ++ exp)) : lexer rest3 n


lexString :: String -> String -> String -> [Token]
lexString ('\"':cs) s m = case m of
                            '\"':n -> (TString s) : (lexer cs n)
                            _ -> lexString cs (s++"\"") m
lexString ('\'':cs) s m = case m of
                            '\'':n -> (TString s) : (lexer cs n)
                            _ -> lexString cs (s++"\'") m
-- a string in an attribute value must evaluate between {}
lexString ('{':cs) s (c:'<':n) = (TString s) : LSB : (lexer cs ('{':c:'<':n))
lexString ('\\':'n':cs) s n = lexString cs (s++['\n']) n
lexString ('\\':'r':cs) s n = lexString cs (s++['\r']) n
lexString (c:cs) s n = lexString cs (s++[c]) n
lexString [] s n = [ TError "End of input while in string" ]


lexComment :: String -> String -> [Token]
lexComment (':':')':cs) n = lexer cs n
lexComment (_:cs) n = lexComment cs n
lexComment [] n = [ TError "End of input while in comment" ]


lexXmlComment :: String -> String -> String -> [Token]
lexXmlComment ('-':'>':cs) text n = (xmlText (text++"->"))++(lexer cs n)
lexXmlComment (c:cs) text n = lexXmlComment cs (text++[c]) n
lexXmlComment [] text _ = xmlText text


lexVar :: String -> String -> [Token]
lexVar cs n =
    let (nm,rest) = span isQN cs
        token = case nm of
          "return" -> RETURN
          "some" -> SOME
          "every" -> EVERY
          "if" -> IF
          "then" -> THEN
          "else" -> ELSE
          "to" -> TO
          "div" -> DIV
          "idiv" -> IDIV
          "mod" -> MOD
          "and" -> AND
          "or" -> OR
          "not" -> NOT
          "union" -> UNION
          "intersect" -> INTERSECT
          "except" -> EXCEPT
          "for" -> FOR
          "let" -> LET
          "in" -> IN
          "as" -> AS
          "where" -> WHERE
          "order" -> ORDER
          "by" -> BY
          "ascending" -> ASCENDING
          "descending" -> DESCENDING
          "element" -> ELEMENT
          "attribute" -> ATTRIBUTE
          "satisfies" -> SATISFIES
          "declare" -> DECLARE
          "function" -> FUNCTION
          "variable" -> VARIABLE
          "at" -> AT
          "eq" -> SEQ
          "ne" -> SNE
          "lt" -> SLT
          "le" -> SLE
          "gt" -> SGT
          "ge" -> SGE
          "is" -> IS
	  "insert" -> INSERT
	  "into" -> INTO
	  "delete" -> DELETE
	  "from" -> FROM
	  "replace" -> REPLACE
	  "with" -> WITH
          "instance" -> INSTANCE
          "of" -> OF
          "cast" -> CAST
          "castable" -> CASTABLE
          "case" -> CASE
          "default" -> DEFAULT
          "typeswitch" -> TYPESWITCH
          var -> QName var
    in case token of
         QName v1 -> case rest of
                       ':':rest2 -> let (v2,rest3) = span isQN rest2
                                    in [QName v1,COLON,QName v2] ++ lexer rest3 n
                       _ -> QName v1 : lexer rest n
         x -> x : lexer rest n
}
