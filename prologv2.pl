%Programmer:- Raju karki.

%input(S):- char(S-[]), write('well formed').
%input(S):- write('your input is: '),write(S),nl, program(S-[]), write('well formed').
%input1(S):- program(['int main () {',int,a,b,c,d,e,f,;,a,=,!,b,+,c,==,d,>,e,&&,f,;,'}']-[]).
%input2(S):- program(['int main () {',int,a,;,b,=,1,;,x,=,x,+,1,;,'}']-[]).
%input4(S):- program(['int main () {',int, a,;,a,=,a,+,1,;,'}']-[]).
%input5(S):- program(['int main () {',int,a,;,x,=,x,+,1,;,'}']-[]).

program1(P):- program([P]-[]).


% 1. Program :- int main ( ) { Declarations Statements }.
program(V-V1):- astart(V-V2), declarations(V2-V3), statements(V3-V4), clbrac1(V4-V1). 
astart(['int main () {'|X]-X).
%braces terminals
brac(['('|X]-X).
clbrac([')'|X]-X).
brac1(['{'|X]-X).
clbrac1(['}'|X]-X).
brac2(['['|X]-X).
clbrac2([']'|X]-X).
aend([';'|X]-X).

%2. Declarations :- { Declaration }
declarations(U-U1):- declaration(U-U1).
declarations(T-T1):- declaration(T-T2), declarations(T2-T1).

%3. Declaration :- Type Identifier [ [ Integer ] ] { , Identifier [ [ Integer ] ] }
declaration(S-S1):- type(S-S2), identifier(S2-S1).
declaration(R-R1):- type(R-R2), identifier(R2-R3), brac2(R3-R4), integer(R4-R5), clbrac2(R5-R1).
declaration(Q-Q1):- type(Q-Q2), identifier(Q2-Q3), kleene(Q3-Q1). 
declaration(P3-P4):- type(P3-P5), identifier(P5-P6), brac2(P6-P7), integer(P7-P8), clbrac2(P8-P9),  kleene(P9-P4).
kleene(P-P1):- acomma(P-P2), identifier(P2-P1). 
kleene(N14-N15):- acomma(N14-N16), identifier(N16-N17), brac2(N17-N18), integer(N18-N19), clbrac2(N19-N15). 
kleene(N10-N11):- acomma(N10-N12), identifier(N12-N13), kleene(N13-N11).  
kleene(N-N1):- acomma(N-N2), identifier(N2-N3), brac2(N3-N4), integer(N4-N5), clbrac2(N5-N6), kleene(N6-N1). 
acomma([','|X]-X).

%4. Type :- int | bool | float | char
type(['int'|X]-X).
type(['bool'|X]-X).
type(['float'|X]-X).
type(['char'|X]-X).

%5. Statements :- { Statement }
statements(I-I1) :- statement(I-I1).
statements(I2-I3):- statement(I2-I4), statements(I4-I3). 

%6. Statement :- ; | Block | Assignment | IfStatement | WhileStatement
statement([';'|X]-X).
statement(I6-I7) :- block(I6-I7). 
statement(I8-I9):- assignment(I8-I9). 
statement(J-J1):- ifstatement(J-J1). 
statement(J1-J2):- whilestatement(J1-J2).

%7. Block :- '{' Statements '}'
block(J3-J4) :- brac1(J3-J5),statements(J5-J6),clbrac1(J6-J4).

%8. Assignment :- Identifier [ [ Expression ] ] = Expression ;
assignment(K-K1):- identifier(K-K2), aequals(K2-K3), expression(K3-K4), aend(K4-K1). 
assignment(K5-K6):- identifier(K5-K7), brac2(K7-K8), expression(K8-K9), clbrac2(K9-K10), aequals(K10-K11), expression(K11-K12), aend(K12-K6).  
aequals(['='|X]-X).

%9. IfStatement :- if ( Expression ) Statement [ else Statement ]
ifstatement(L-L1) :- aif(L-L2),brac(L2-L3), expression(L3-L4), clbrac(L4-L5), statement(L5-L1). 
ifstatement(M-M1) :- aif(M-M2),brac(M2-M3), expression(M3-M4), clbrac(M4-M5), statement(M5-M6), aelse(M6-M7), statement(M7-M1). 
aif(['if'|X]-X).

%10. WhileStatement :- while ( Expression ) Statement
whilestatement(I-I1):- whale(I-I2), brac(I2-I3), expression(I3-I4), clbrac(I4-I5), statement(I5-I1). 
whale(['while'|X]-X).

%11. Expression :- Conjunction { || Conjunction }
expression(G-G1) :- conjunction(G-G1). 
expression(G2-G3):- conjunction(G2-G5), orexp(G5-G6), expression(G6-G3). 
orexp(['||'|X]-X).

%12. Conjunction :- Equality { && Equality }
conjunction(H-H1) :- equality(H-H1).
conjunction(H2-H3):- equality(H2-H5), andexp(H5-H6), conjunction(H6-H3). 
andexp(['&&'|X]-X).

%13. Equality :- Relation [ EquOp Relation ]
equality(E-E1) :- relation(E-E1).
equality(E2-E3) :- relation(E2-E4), equop(E4-E5), relation(E5-E3).

%14. EquOp :- == | != 
equop(['=='|X]-X).
equop(['!='|X]-X).

%15. Relation :- Addition [ RelOp Addition ]
relation(E-E1) :- addition(E-E1).
relation(E2-E3):- addition(E2-E4), relop(E4-E5), addition(E5-E3).

%16. RelOp :-   < | <= | > | >= 
relop(['<'|X]-X).
relop(['<='|X]-X).
relop(['>'|X]-X).
relop(['>='|X]-X).

%17. Addition :- Term { AddOp Term }
addition(D5-D6) :- term(D5-D6). 
addition(D7-D8):- term(D7-D9), addop(D9-D10), addition(D10-D8). 

%18. AddOp :- + | -
addop(['+'|X]-X).
addop(['-'|X]-X).

%19. Term :- Factor { MulOp Factor }
term(B3-B4):- factor(B3-B4). 
term(B5-B6):- factor(B5-B4), mulop(B4-B3), term(B3-B6). 

%20. MulOp :-  * | / | %
mulop(['*'|X]-X).
mulop(['/'|X]-X).
mulop(['%'|X]-X).

%21. Factor :- [ UnaryOp ] Primary
factor(C3-C4) :-	primary(C3-C4). 
factor(C5-C6):- 	unaryop(C5-C7) ,primary(C7-C6).

%22. UnaryOp :-  - | !
unaryop(['-'|X]-X).
unaryop(['!'|X]-X).

%23. Primary :- Identifier [  [ Expression ] ] | Literal |  ( Expression )  | Type ( Expression )
primary(A18-A19):- identifier(A18-A19).
primary(Z-Z1):- identifier(Z-Z2), brac2(Z2-Z3), expression(Z3-Z4), clbrac2(Z4-Z1).
primary(A3-A4):- literal(A3-A4).
primary(S-S1):- brac(S-S2),expression(S2-S3),clbrac(S3-S1). 
primary(S-S1):- type(S-S2), brac(S2-S3),expression(S3-S4),clbrac(S4-S1). 

%24. Identifier :- Letter { Letter | Digit }
identifier(A-A1):- 	letter(A-A1). 
identifier(B-B1):- 	letter(B-B2), letDig(B2-B1). 
letDig(C-C1):- 		letter(C-C1).  
letDig(D-D1):- 		letter(D-D2), letDig(D2-D1). 
letDig(E-E1):- 		digit(E-E1).
letDig(F-F1):- 		digit(F-F2), letDig(F2-F1). 

%27. Literal :- Integer | Boolean | Float | Char
literal(M-M1):- cinteger(M-M1).
literal(O-O1):- floats(O-O1).
literal(N-N1):- boolean(N-N1).  
literal(P-P1):- char(P-P1). 

%28. Integer :- Digit { Digit }
cinteger(G-G1) :- 	digit(G-G1). 
cinteger(H-H1):- 	dig(H-H1). 
dig(I-I1):- 		digit(I-I2), dig(I2-I1).
dig(J-J1):- 		digit(J-J1). 

%29. Boolean :- true | False
boolean([true|X]-X).
boolean([false|X]-X).

%30. Float :- Integer . Integer
floats(K-K1):- cinteger(K-K2), dotInt(K2-K1). 
dotInt(L-L1):- dot(L-L2), cinteger(L2-L1).
dot(['.'|X]-X).

%31. Char :- ' ASCII Char '
char(['a'|X]-X).

%25. Letter :- a | b | … | z | A | B | … | Z 
letter([a|X]-X).
letter([b|X]-X).
letter([c|X]-X).
letter([d|X]-X).
letter([e|X]-X).
letter([f|X]-X).
letter([g|X]-X).
letter([h|X]-X).
letter([i|X]-X).
letter([j|X]-X).
letter([k|X]-X).
letter([l|X]-X).
letter([m|X]-X).
letter([n|X]-X).
letter([o|X]-X).
letter([p|X]-X).
letter([q|X]-X).
letter([r|X]-X).
letter([s|X]-X).
letter([t|X]-X).
letter([u|X]-X).
letter([v|X]-X).
letter([w|X]-X).
letter([x|X]-X).
letter([y|X]-X).
letter([z|X]-X).
letter(['A'|X]-X).
letter(['B'|X]-X).
letter(['C'|X]-X).
letter(['D'|X]-X).
letter(['E'|X]-X).
letter(['F'|X]-X).
letter(['G'|X]-X).
letter(['H'|X]-X).
letter(['I'|X]-X).
letter(['J'|X]-X).
letter(['K'|X]-X).
letter(['L'|X]-X).
letter(['M'|X]-X).
letter(['N'|X]-X).
letter(['O'|X]-X).
letter(['P'|X]-X).
letter(['Q'|X]-X).
letter(['R'|X]-X).
letter(['S'|X]-X).
letter(['T'|X]-X).
letter(['U'|X]-X).
letter(['V'|X]-X).
letter(['W'|X]-X).
letter(['X'|X]-X).
letter(['Y'|X]-X).
letter(['Z'|X]-X).

%26. Digit :-   0 | 1 | ... | 9
digit([0|X]-X). 
digit([1|X]-X).
digit([2|X]-X).
digit([3|X]-X).
digit([4|X]-X).
digit([5|X]-X).
digit([6|X]-X).
digit([7|X]-X).
digit([8|X]-X).
digit([9|X]-X).