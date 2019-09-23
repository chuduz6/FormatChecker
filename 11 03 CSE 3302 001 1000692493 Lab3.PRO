%	Name:- Chudamani Aryal
%	ID:-   1000692493
%	Lab:-  Prolog Lab3
%	This lab takes the text to determine whether or not a piece of code is a well-formed CLITE program. 	
%	Input will consists of CLITE code segments and CLITE wannabe code segments.
% 	In this program I have used the concept of list to take the text as an input. 
%	Since the text is already separated by comma, I dont have to work on tokenizing the input text.
%	Also I have some test cases within the program.
%	Conversion of EBNF to BNF is mostly done using the concept of tail recursion in the case of kleene star operation
% To run the program, you have to enter program([List]). Here, List means the input string that is broken down into a list separated by comma



% These are few of the test cases I tried and which all works.
%input(A):- char(A-[]), write('well formed').
%input(A):- write('your input is: '),write(A),nl, program(A-[]), write('well formed').
%input1(A):- program([int, main, '(', ')', '{',int,a,b,c,d,e,f,;,int, g,;,a,=,!,b,+,c,==,d,>,e,&&,f,;,'}']-[]).
%input2(A):- program(['int main () {',int,a,;,b,=,1,;,x,=,x,+,1,;,'}']-[]).
%input4(A):- program(['int main () {',int, a,;,a,=,a,+,1,;,'}']-[]).
%input5(A):- program(['int main () {',int,a,;,x,=,x,+,1,;,'}']-[]).

%These are few of the test cases I tried and which should print as no, since they are not CLITE program.
%input6(A):- program(['int main () {',int,a,int,x,;, x,=,x,+,1,;,'}']-[]).




% The main project starts from here.

program1(P):- program([P]-[]), write('well formed').

%	Since I am testing whether the given input text is a well formed CLITE program, it is necessary to write the CLITE grammar as a rule in prolog
%	Hence, it is necessary to convert the CLITE grammar into equivalent prolog rules

% Program :- int main ( ) { Declarations Statements }    This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar.

program(A-A1):- cint(A-A2), cmain(A2-A3), parenthesis(A3-A4), clparenthesis(A4-A5), curly(A5-A6), declarations(A6-A7), statements(A7-A8), clcurly(A8-A1). 
cint(['int'|A]-A).
cmain(['main'|A]-A).
parenthesis(['('|A]-A).
clparenthesis([')'|A]-A).
curly(['{'|A]-A).
clcurly(['}'|A]-A).


%	Declarations :- { Declaration } This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
declarations(A-A1):- declaration(A-A1).
declarations(A-A1):- declaration(A-A2), declarations(A2-A1).

%	Declaration :- Type Identifier [ [ Integer ] ] { , Identifier [ [ Integer ] ] } 	This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
declaration(A-A1):- type(A-A2), identifier(A2-A3), semicolon(A3-A1).
declaration(A-A1):- type(A-A2), identifier(A2-A3), square(A3-A4), integer(A4-A5), clsquare(A5-A6), semicolon(A6-A1).
declaration(A-A1):- type(A-A2), identifier(A2-A3), repeat(A3-A4), semicolon(A4-A1). 
declaration(A-A1):- type(A-A2), identifier(A2-A3), square(A3-A4), integer(A4-A5), clsquare(A5-A6),  repeat(A6-A7), semicolon(A7-A1).
repeat(A-A1):- cmcomma(A-A2), identifier(A2-A1). 
repeat(A-A1):- cmcomma(A-A2), identifier(A2-A3), square(A3-A4), integer(A4-A5), clsquare(A5-A1). 
repeat(A-A1):- cmcomma(A-A2), identifier(A2-A3), repeat(A3-A1).  
repeat(A-A1):- cmcomma(A-A2), identifier(A2-A3), square(A3-A4), integer(A4-A5), clsquare(A5-A6), repeat(A6-A1). 
cmcomma([','|A]-A).
square(['['|A]-A).
clsquare([']'|A]-A).
semicolon([';'|A]-A).

%	Type :- int | bool | float | char	This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
type(['int'|A]-A).
type(['bool'|A]-A).
type(['float'|A]-A).
type(['char'|A]-A).

%	Statements :- { Statement }		This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
statements(I-I1) :- statement(I-I1).
statements(I2-I3):- statement(I2-I4), statements(I4-I3). 

%	Statement :- ; | Block | Assignment | IfStatement | WhileStatement 	This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
statement([';'|A]-A).
statement(A-A1) :- block(A-A1). 
statement(A-A1):- assignment(A-A1). 
statement(A-A1):- ifstatement(A-A1). 
statement(A-A1):- whilestatement(A-A1).

%	Block :- '{' Statements '}'		This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
block(A-A1) :- curly(A-A2),statements(A2-A3),clcurly(A3-A1).

%	Assignment :- Identifier [ [ Expression ] ] = Expression ;	This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
assignment(A-A1):- identifier(A-A2), aequals(A2-A3), expression(A3-A4), semicolon(A4-A1). 
assignment(A-A1):- identifier(A-A2), square(A2-A3), expression(A3-A4), clsquare(A4-A5), aequals(A5-A6), expression(A6-A7), semicolon(A7-A1).  
aequals(['='|A]-A).

%	IfStatement :- if ( Expression ) Statement [ else Statement ]		This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
ifstatement(A-A1) :- cmif(A-A2),parenthesis(A2-A3), expression(A3-A4), clparenthesis(A4-A5), statement(A5-A1). 
ifstatement(A-A1) :- cmif(A-A2),parenthesis(A2-A3), expression(A3-A4), clparenthesis(A4-A5), statement(A5-A6), aelse(A6-A7), statement(A7-A1). 
cmif(['if'|A]-A).

%	WhileStatement :- while ( Expression ) Statement	This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
whilestatement(A-A1):- cmwhile(A-A2), parenthesis(A2-A3), expression(A3-A4), clparenthesis(A4-A5), statement(A5-A1). 
cmwhile(['while'|A]-A).

%	Expression :- Conjunction { || Conjunction }	This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
expression(A-A1) :- conjunction(A-A1). 
expression(A2-A3):- conjunction(A2-A5), orexpression(A5-A6), expression(A6-A3). 
orexpression(['||'|A]-A).

%	Conjunction :- Equality { && Equality }	This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
conjunction(A-A1) :- equality(A-A1).
conjunction(A2-A3):- equality(A2-A5), andexpression(A5-A6), conjunction(A6-A3). 
andexpression(['&&'|A]-A).

%	Equality :- Relation [ EquOp Relation ]		This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
equality(A-A1) :- relation(A-A1).
equality(A2-A3) :- relation(A2-A4), equop(A4-A5), relation(A5-A3).

%	EquOp :- == | !=  	This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
equop(['!='|A]-A).
equop(['=='|A]-A).

%	Relation :- Addition [ RelOp Addition ]		This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
relation(A-A1) :- addition(A-A1).
relation(A2-A3):- addition(A2-A4), relop(A4-A5), addition(A5-A3).

%	RelOp :-   < | <= | > | >= 	This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
relop(['>='|A]-A).
relop(['>'|A]-A).
relop(['<='|A]-A).
relop(['<'|A]-A).

%	Addition :- Term { AddOp Term }		This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
addition(A-A1) :- term(A-A1). 
addition(A-A1):- term(A-A2), addoperation(A2-A3), addition(A3-A1). 

%	AddOp :- + | - 	This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
addoperation(['-'|A]-A).
addoperation(['+'|A]-A).

%	Term :- Factor { MulOp Factor }		This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
term(A-A1):- factor(A-A1). 
term(A-A1):- factor(A-A2), muloperation(A2-A3), term(A3-A1). 

%	MulOp :-  * | / | %		This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
muloperation(['%'|A]-A).
muloperation(['/'|A]-A).
muloperation(['*'|A]-A).

%	Factor :- [ UnaryOp ] Primary 	This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
factor(A-A1) :-	primary(A-A1). 
factor(A-A1):- 	unaryoperation(A-A2) ,primary(A2-A1).

%	UnaryOp :-  - | ! 	This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
unaryoperation(['!'|A]-A).
unaryoperation(['-'|A]-A).

%	Primary :- Identifier [  [ Expression ] ] | Literal |  ( Expression )  | Type ( Expression )	This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
primary(A-A1):- identifier(A-A1).
primary(A-A1):- identifier(A-A2), square(A2-A3), expression(A3-A4), clsquare(A4-A1).
primary(A-A1):- literal(A-A1).
primary(A-A1):- parenthesis(A-A2),expression(A2-A3),clparenthesis(A3-A1). 
primary(A-A1):- type(A-A2), parenthesis(A2-A3),expression(A3-A4),clparenthesis(A4-A1). 

%	Identifier :- Letter { Letter | Digit }		This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
identifier(A-A1):- 	letter(A-A1). 
identifier(A-A1):- 	letter(A-A2), letDig(A2-A1). 
letDig(A-A1):- 		letter(A-A1).  
letDig(A-A1):- 		letter(A-A2), letDig(A2-A1). 
letDig(A-A1):- 		digit(A-A1).
letDig(A-A1):- 		digit(A-A2), letDig(A2-A1). 

%	Literal :- Integer | Boolean | Float | Char		This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
literal(A-A1):- cinteger(A-A1).
literal(A-A1):- floats(A-A1).
literal(A-A1):- boolean(A-A1).  
literal(A-A1):- char(A-A1). 

%	Integer :- Digit { Digit }		This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
cinteger(A-A1) :- 	digit(A-A1). 
cinteger(A-A1):- 	dig(A-A1). 
dig(A-A1):- 		digit(A-A2), dig(A2-A1).
dig(A-A1):- 		digit(A-A1). 

%	Boolean :- true | False	This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
boolean([true|A]-A).
boolean([false|A]-A).

%	Float :- Integer . Integer		This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
floats(A-A1):- cinteger(A-A2), dotInt(A2-A1). 
dotInt(A-A1):- dot(A-A2), cinteger(A2-A1).
dot(['.'|A]-A).

%	Letter :- a | b | … | z | A | B | … | Z 		This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
letter([a|A]-A).
letter([b|A]-A).
letter([c|A]-A).
letter([d|A]-A).
letter([e|A]-A).
letter([f|A]-A).
letter([g|A]-A).
letter([h|A]-A).
letter([i|A]-A).
letter([j|A]-A).
letter([k|A]-A).
letter([l|A]-A).
letter([m|A]-A).
letter([n|A]-A).
letter([o|A]-A).
letter([p|A]-A).
letter([q|A]-A).
letter([r|A]-A).
letter([s|A]-A).
letter([t|A]-A).
letter([u|A]-A).
letter([v|A]-A).
letter([w|A]-A).
letter([x|A]-A).
letter([y|A]-A).
letter([z|A]-A).
letter(['A'|A]-A).
letter(['B'|A]-A).
letter(['C'|A]-A).
letter(['D'|A]-A).
letter(['E'|A]-A).
letter(['F'|A]-A).
letter(['G'|A]-A).
letter(['H'|A]-A).
letter(['I'|A]-A).
letter(['J'|A]-A).
letter(['K'|A]-A).
letter(['L'|A]-A).
letter(['M'|A]-A).
letter(['N'|A]-A).
letter(['O'|A]-A).
letter(['P'|A]-A).
letter(['Q'|A]-A).
letter(['R'|A]-A).
letter(['S'|A]-A).
letter(['T'|A]-A).
letter(['U'|A]-A).
letter(['V'|A]-A).
letter(['W'|A]-A).
letter(['X'|A]-A).
letter(['Y'|A]-A).
letter(['Z'|A]-A).

%	Digit :-   0 | 1 | ... | 9		This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
digit([0|A]-A). 
digit([1|A]-A).
digit([2|A]-A).
digit([3|A]-A).
digit([4|A]-A).
digit([5|A]-A).
digit([6|A]-A).
digit([7|A]-A).
digit([8|A]-A).
digit([9|A]-A).

%	Char :- ' ASCII Char '			This is the CLITE grammar and the code below is the equivalent prolog rules to that grammar
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char(['        '|X]-X).
char([' '|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([''|X]-X).
char([' '|X]-X).
char(['!'|X]-X).
char(['"'|X]-X).
char(['#'|X]-X).
char(['$'|X]-X).
char(['%'|X]-X).
char(['&'|X]-X).
char(['\''|X]-X).
char(['('|X]-X).
char([')'|X]-X).
char(['*'|X]-X).
char(['+'|X]-X).
char([','|X]-X).
char(['-'|X]-X).
char(['.'|X]-X).
char(['/'|X]-X).
char(['0'|X]-X).
char(['1'|X]-X).
char(['2'|X]-X).
char(['3'|X]-X).
char(['4'|X]-X).
char(['5'|X]-X).
char(['6'|X]-X).
char(['7'|X]-X).
char(['8'|X]-X).
char(['9'|X]-X).
char([':'|X]-X).
char([';'|X]-X).
char(['<'|X]-X).
char(['='|X]-X).
char(['>'|X]-X).
char(['?'|X]-X).
char(['@'|X]-X).
char(['A'|X]-X).
char(['B'|X]-X).
char(['C'|X]-X).
char(['D'|X]-X).
char(['E'|X]-X).
char(['F'|X]-X).
char(['G'|X]-X).
char(['H'|X]-X).
char(['I'|X]-X).
char(['J'|X]-X).
char(['K'|X]-X).
char(['L'|X]-X).
char(['M'|X]-X).
char(['N'|X]-X).
char(['O'|X]-X).
char(['P'|X]-X).
char(['Q'|X]-X).
char(['R'|X]-X).
char(['S'|X]-X).
char(['T'|X]-X).
char(['U'|X]-X).
char(['V'|X]-X).
char(['W'|X]-X).
char(['X'|X]-X).
char(['Y'|X]-X).
char(['Z'|X]-X).
char(['['|X]-X).
char(['\\'|X]-X).
char([']'|X]-X).
char(['^'|X]-X).
char(['_'|X]-X).
char(['`'|X]-X).
char(['a'|X]-X).
char(['b'|X]-X).
char(['c'|X]-X).
char(['d'|X]-X).
char(['e'|X]-X).
char(['f'|X]-X).
char(['g'|X]-X).
char(['h'|X]-X).
char(['i'|X]-X).
char(['j'|X]-X).
char(['k'|X]-X).
char(['l'|X]-X).
char(['m'|X]-X).
char(['n'|X]-X).
char(['o'|X]-X).
char(['p'|X]-X).
char(['q'|X]-X).
char(['r'|X]-X).
char(['s'|X]-X).
char(['t'|X]-X).
char(['u'|X]-X).
char(['v'|X]-X).
char(['w'|X]-X).
char(['x'|X]-X).
char(['y'|X]-X).
char(['z'|X]-X).
char(['{'|X]-X).
char(['|'|X]-X).
char(['}'|X]-X).
char(['~'|X]-X).
char([''|X]-X).