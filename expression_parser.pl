% CS 345 Assignment 2 Starter Code

% This code fully implements the expression and term parsing for
% mySIMPL. No modifications to the parse code itself should be necessary.
% It will also return to you an AST node for the expression. The only
% thing that might make this an issue is that you may not want to use
% the AST format provided. You are free to change it to your own
% implementation.
%
% If there are questions about this code, please feel free to ask
% the Dr. Rellermeyer or the TA.

% parse: note you will have to change this to have it work for general
% expressionrams; at the moment, it will only parse expressions and return to
% you an expression node in the AST variable
parse(TokenList, AST) :- phrase(prog(AST), TokenList).



% base: NOTE THIS IS INCOMPLETE; still need to have with ID and expression
% parsing
% This says to parse anything in the token list as long as that thing is
% a number (i.e. bind N to the token, but then check if it is a number
base(base(N)) --> [N], {number(N)}.

base(base(B)) --> {not(number(B))},
    identifier(B).

base(base(B)) --> ['('],
	expression(B),
	[')'].

base(base(B)) --> funcCall(B).



% simple case: just a term
expression(expression(T)) --> term(T).
% recursive case: expression then term. Since we can't do the following...
% expression --> expression ...
% due to left recursion, we have to build up the expression by parsing
% a term and passing this term along to the left_assoc call which
% will biuild the expression node for us
expression(E) --> term(T1), addOp(AO), left_assoc(E, expression(T1), AO).

% left assoc base case: there is only a term left, so we take what was
% already passed in with the newly parsed term and form the expression
% node
left_assoc(expression(T1, AO, T2), T1, AO) --> term(T2).
% left assoc recursive case; there is more than just a term left, so
% we must continue parsing using left_assoc
left_assoc(E, T1, AO) --> term(T2), addOp(AO2),
                          left_assoc(E, expression(T1, AO, T2), AO2).

% simple case: just a factor
term(term(F)) --> factor(F).
% recursive case: like the expression case, we can't have left-recursion
% so we build up the parse factor by factor
term(T) --> factor(F1), mulOp(MO), left_assoc_term(T, term(F1), MO).

% left assoc term base case
left_assoc_term(term(F1, MO, F2), F1, MO) --> factor(F2).
% left assoc term recursive case
left_assoc_term(T, F1, MO) --> factor(F2), mulOp(MO2),
                               left_assoc_term(T, term(F1, MO, F2), MO2).

% factor parse; a factor is merely a base
factor(factor(B)) --> base(B).

% parse add ops and mul ops; parses the operation, and unifies the
% passed in variable to an op node
addOp(addOp('+')) --> ['+'].
addOp(addOp('-')) --> ['-'].
mulOp(mulOp('*')) --> ['*'].
mulOp(mulOp('/')) --> ['/'].

%Booleans and Comps
logOp(logOp('&&')) --> ['&&'].
logOp(logOp('||')) --> ['||'].
comp(comp('==')) --> ['=='].
comp(comp('<')) --> ['<'].
comp(comp('>')) --> ['>'].
comp(comp('<=')) --> ['<='].
comp(comp('>=')) --> ['>='].
comp(comp('!=')) --> ['!='].
notOp(notOp('!')) --> ['!'].
bool(bool('true')) --> ['true'].
bool(bool('false')) --> ['false'].


% You need to finish adding the rest of the parsing rules.
% After that, you need to define evaluation rules that will take an AST and
% "run" your expressionram.

%keywords
keywords(+).
keywords(-).
keywords(*).
keywords(/).
keywords(<-).
keywords(;).
keywords(.).
keywords('(').
keywords(')').
keywords(var).
keywords(return).

funcDecl(funcDecl(ID,PAR,PROG)) --> ['function'],
    identifier(ID),
    ['('],
    [PAR],
    [')'],
    ['{'],
    prog(PROG),
    ['}'].

prog(prog(R)) --> retStatement(R),
    ['.'].

prog(prog(S,P)) --> funcDecl(S),
    [';'],
    prog(P).

prog(prog(S,P)) --> statement(S),
    [';'],
    prog(P).

statement(statement(S)) --> declaration(S).

statement(statement(S)) --> declAssignment(S).

statement(statement(S)) --> assignment(S).

statement(statement(S)) --> conditional(S).

statement(statement(S)) --> loop(S).

statementSeq(statementSeq(S)) --> statement(S),
    ['.'].

statementSeq(statementSeq(S,ST)) --> statement(S),
    [';'],
    statementSeq(ST).

retStatement(return(B)) --> ['return'],
    base(B).

declaration(declaration(ID)) --> ['var'],
    identifier(ID).

assignment(assignment(ID,B)) --> identifier(ID),
    ['<-'],
    base(B).

declAssignment(declAssignment(ID,B)) --> ['var'],
    identifier(ID),
    ['<-'],
    base(B).

funcCall(funcCall(ID,B)) --> identifier(ID),
    ['('],
    base(B),
    [')'].

conditional(conditional(C,S)) --> ['if'],
    ['('],
    condition(C),
    [')'],
    ['then'],
    statementSeq(S),
    ['endif'].

conditional(conditional(C,S1,S2)) --> ['if'],
    ['('],
    condition(C),
    [')'],
    ['then'],
    statementSeq(S1),
    ['else'],
    statementSeq(S2),
    ['endif'].

loop(loop(C,S)) --> ['while'],
    ['('],
    condition(C),
    [')'],
    ['do'],
    statementSeq(S),
    ['done'].

condition(condition(B1,OP,B2)) --> base(B1),
    comp(OP),
    base(B2).

condition(condition(C1,OP,C2)) --> ['('],
    condition(C1),
    logOp(OP),
    condition(C2),
    [')'].

condition(condition(B)) --> boolean(B).

condition(condition(OP,C)) --> notOp(OP),
    ['('],
    condition(C),
    [')'].

identifier(identifier(ID)) --> [ID],
    {not(keywords(ID))},
    {not(number(ID))}.

boolean(boolean(B)) --> bool(B).

% Evaluation rules
evaluate(AST, Number):-
    empty_assoc(Var_list_Glob),
    empty_assoc(FirstScope),
    put_assoc(0, Var_list_Glob, FirstScope, Var_list_GlobA),
    empty_assoc(Func_list),
    evaluateProg(AST, Var_list_GlobA, Var_list_Glob_out, Func_list, Func_list_out, 0, Number).

evaluateProg(prog(return(R)), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number):-
    eval(R, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, X),
    %write('x is equal to'),writeln(X),
    not(X = 'NULL'),
    Number = X.

evaluateProg(prog(S,P), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number):-
    TEMPSCOPE = SCOPE,
    eval(S, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number),
    evaluateProg(P, Var_list_Glob_out, Var_list_Glob_new, Func_list_out, Func_list_new, TEMPSCOPE, Number).

%Eval for statementSeq and statement
eval(statementSeq(S), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number):-
    eval(S, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number).

eval(statementSeq(S,SQ), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number):-
    TEMPSCOPE = SCOPE,
    eval(S, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number),
    eval(SQ, Var_list_Glob_out, Var_list_Glob_new, Func_list_out, Func_list_new, TEMPSCOPE, Number).

eval(statement(S), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number):-
    eval(S, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number).

%rule for assignment
eval(assignment(identifier(ID), base(B)), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number):-
    eval(base(B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RB),
    %TODO::
    %NEED TO FIX checkScopes and then fix this
    %if you don't fix this ur a dumbass lol
    (checkScopes(Var_list_Glob, ID, SCOPE, X)),
    (number(X); X = 'NULL'),
    setScopes(Var_list_Glob, ID, RB, SCOPE, Var_list_Glob_out).

%rule for declaration
eval(declaration(identifier(ID)), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number):-
    declScopes(Var_list_Glob, ID, 'NULL', SCOPE, Var_list_Glob_out).

%rule for declaration assignment
eval(declAssignment(identifier(ID), base(B)), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number):-
    eval(base(B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, R),
    declScopes(Var_list_Glob, ID, R, SCOPE, Var_list_Glob_out).

%base case for base
eval(base(B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    number(B),
    Ret = B.

%base case for ID
eval(base(B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    not(number(B)),
    eval(B, Var_list_Glob, Var_list_Glob, Func_list, Func_list_out, SCOPE, Ret).

%for expression +
eval(expression(A, addOp('+'),B),Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    eval(A, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RA),
    eval(B, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RB),
    not(RA = 'NULL'),
    not(RB = 'NULL'),
    Ret is RA + RB.

%for expression -
eval(expression(A, addOp('-'),B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    eval(A, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RA),
    eval(B, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RB),
    not(RA = 'NULL'),
    not(RB = 'NULL'),
    Ret is RA - RB.

%for expression base case
eval(expression(T), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    eval(T, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RT),
    Ret = RT.

%for term base case
eval(term(F), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    eval(F,Var_list_Glob,Var_list_Glob_out, Func_list, Func_list_out,SCOPE,RF),
    Ret = RF.

%for factor base case
eval(factor(B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    eval(B, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RB),
    Ret = RB.

%for term /
eval(term(A, mulOp('/'), B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    eval(A, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RA),
    eval(B, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RB),
    not(RA = 'NULL'),
    not(RB = 'NULL'),
    Ret is RA/RB.

%for term *
eval(term(A, mulOp('*'), B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    eval(A, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RA),
    eval(B, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RB),
    not(RA = 'NULL'),
    not(RB = 'NULL'),
    Ret is RA*RB.

%for eval of ID
eval(identifier(ID), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    checkScopes(Var_list_Glob, ID, SCOPE, R),
    Ret = R.

%for factor base case
eval(factor(B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number):-
    eval(B, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, R),
    Number = R.

%for term(factor()) base case
eval(term(factor(F)), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number):-
    eval(F, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, R),
    Number = R.

%eval for function declarations
eval(funcDecl(ID,PAR,PROG), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number):-
    (not(get_assoc(ID, Func_list, V))),
    FuncPack = [PAR,PROG],
    put_assoc(ID, Func_list, FuncPack, Func_list_out).

eval(funcCall(ID, B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number):-
    get_assoc(ID, Func_list, FuncPack),
    [Param|_] = FuncPack,
    last(FuncPack, FuncLogic),
    eval(B, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Val),
    FUNCSCOPE is SCOPE+1,
    declScopes(Var_list_Glob, Param, Val, FUNCSCOPE, Var_list_Glob_out),
    eval(FuncLogic, Var_list_Glob_out, Var_list_Glob_temp, Func_list, Func_list_out, FUNCSCOPE, Number).


%eval for conditionals
eval(conditional(C,S), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number):-
    eval(C, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, N),
    NEWSCOPE is SCOPE+1,
    empty_assoc(NewScopeList),
    put_assoc(NEWSCOPE, Var_list_Glob, NewScopeList, Var_list_Glob_temp),
    condHelper(N, S, Var_list_Glob_temp, Var_list_Glob_out, Func_list, Func_list_out, NEWSCOPE, Number).

condHelper(N, S, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    (N==1),
    eval(S, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret).

condHelper(N, S, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    (N==0),
    Var_list_Glob_out = Var_list_Glob,
    Func_list_out = Func_list,
    true.

eval(conditional(C,S1,S2), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number):-
    eval(C, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, N),
    NEWSCOPE is SCOPE+1,
    empty_assoc(NewScopeList),
    put_assoc(NEWSCOPE, Var_list_Glob, NewScopeList, Var_list_Glob_temp),
    condEHelper(N, S1, S2, Var_list_Glob_temp, Var_list_Glob_out, Func_list, Func_list_out, NEWSCOPE, Number).

condEHelper(N, S1, S2, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    (N==1),
    eval(S1, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret).

condEHelper(N, S1, S2, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    (N==0),
    eval(S2, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret).

%For checking, getting, and setting the assoc list of scopes for a variable
checkScopes(Var_list_Glob, ID, SCOPE, Ret):-
    NEXTSCOPE is SCOPE-1,
    get_assoc(SCOPE, Var_list_Glob, ScopeAssoc),
    (get_assoc(ID, ScopeAssoc, R) -> Ret = R ; checkScopes(Var_list_Glob, ID, NEXTSCOPE, Ret)).

setScopes(Var_list_Glob, ID, VAL, SCOPE, Var_list_Glout):-
    NEXTSCOPE is SCOPE-1,
    get_assoc(SCOPE, Var_list_Glob, ScopeAssoc),
    (get_assoc(ID, ScopeAssoc, R) -> put_assoc(ID, ScopeAssoc, VAL, ScopeAssocT), put_assoc(SCOPE, Var_list_Glob, ScopeAssocT, Var_list_Glout) ; setScopes(Var_list_Glob, ID, VAL, NEXTSCOPE, Var_list_Glout)).

declScopes(Var_list_Glob, ID, VAL, SCOPE, Var_list_Glout):-
    get_assoc(SCOPE, Var_list_Glob, ScopeAssoc),
    (not(get_assoc(ID, ScopeAssoc, R))),
    put_assoc(ID, ScopeAssoc, VAL, ScopeAssocT),
    put_assoc(SCOPE, Var_list_Glob, ScopeAssocT, Var_list_Glout).


%eval for loops
%still need to account for scoping but this is a very basic loop setup
eval(loop(C,S), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number):-
    Var_list_copy = Var_list_Glob,
    eval(C, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret),
    (Ret==1 -> eval(S, Var_list_copy, Var_list_temp_out, Func_list, Func_list_out, SCOPE, Number), eval(loop(C,S), Var_list_temp_out, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Number); Var_list_Glob_out=Var_list_Glob; true).

%eval conditions
%NOTE: Conditionals don't currently work in the parser, so it's
%entirely possible this won't work
%ALSO: Still not 100% sure how to handle booleans in our evaluation,
%so we might need to make adjustments.
%base case

eval(condition(A, comp('=='), B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    eval(A, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RA),
    eval(B, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RB),
    ((RA=:=RB) -> Ret=1 ; Ret=0).

eval(condition(A, comp('<'), B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    eval(A, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RA),
    eval(B, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RB),
    ((RA<RB) -> Ret=1 ; Ret=0).

eval(condition(A, comp('>'), B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    eval(A, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RA),
    eval(B, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RB),
    ((RA>RB) -> Ret=1 ; Ret=0).

eval(condition(A, comp('<='), B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    eval(A, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RA),
    eval(B, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RB),
    ((RA=<RB) -> Ret=1 ; Ret=0).

eval(condition(A, comp('>='), B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    eval(A, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RA),
    eval(B, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RB),
    ((RA>=RB) -> Ret=1 ; Ret=0).

eval(condition(A, comp('!='), B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    eval(A, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RA),
    eval(B, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RB),
    ((RA=\=RB) -> Ret=1 ; Ret=0).

eval(condition(B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    eval(B, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret).

eval(condition(notOp('!'), C), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    eval(C, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, R),
    ((R=1) -> Ret=0 ; Ret=1).

%eval for logOps
%once again, not sure about the return values
eval(condition(A, logOp('&&'), B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    eval(A, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RA),
    eval(B, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RB),
    andHelper(RA,RB,R),
    Ret = R.

%Prolog boolean suck so we had to unroll the if statement
%perhaps there is a better way, but whatever
andHelper(A, B, Ret):-
    A==1,
    B==1,
    Ret = 1.

andHelper(A, B, Ret):-
    A==1,
    B==0,
    Ret = 0.

andHelper(A, B, Ret):-
    A==0,
    B==1,
    Ret = 0.

andHelper(A, B, Ret):-
    A==0,
    B==0,
    Ret = 0.


eval(condition(A,logOp('||'), B), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Ret):-
    eval(A, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RA),
    eval(B, Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, RB),
    orHelper(RA,RB,R),
    Ret = R.

orHelper(A, B, Ret):-
    A==1,
    B==1,
    Ret = 1.

orHelper(A, B, Ret):-
    A==1,
    B==0,
    Ret = 1.

orHelper(A, B, Ret):-
    A==0,
    B==1,
    Ret = 1.

orHelper(A, B, Ret):-
    A==0,
    B==0,
    Ret = 0.



%eval for Booleans
%haven't figureout exactly how to handle the eval, but for now we're using
%numbers 1 and 0 for true and false.
eval(boolean(bool('true')), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Res):-
    Res = 1.

eval(boolean(bool('false')), Var_list_Glob, Var_list_Glob_out, Func_list, Func_list_out, SCOPE, Res):-
    Res = 0.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simple test running code for the mySIMPL assignments
%
% Author: Loc Hoang <loc@cs.utexas.edu>, based on the CS345 test suite 
% runner for Assignment 2/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HOW TO:
% Paste this code at the end of your Prolog file.
% Define tests using the do_test interface. See t1, t2, and t3 below.
% Add the tests you define to the test_list.
% Upon loading your file in SWI-Prolog, the tests should run. (assuming you
% copy-pasted this infrastructure to the bottom of your file)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TEST DEFINING RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% do_test for parse failures
do_test(TokenList, p) :-
    format("Testing ~k, expect parse failure~n", [TokenList]),

    (parse(TokenList, AST) ->
        format("Exepcted parse failure, got ~k, FAIL~n", [AST]);
        format("Got parse failure, PASS~n")).

% do_test for eval failures
do_test(TokenList, e) :-
    format("Testing ~k, expect evaluate failure~n", [TokenList]),

    (parse(TokenList, AST) ->
        (evaluate(AST, A) -> 
            format("Expected evaluate failure, got ~w, FAIL~n", [A]);
            format("Got evaluate failure, PASS~n"));
        format("Parse failure, expected eval failure, FAIL~n")).

% do_test for programs that have some expected value
do_test(TokenList, Expected) :-
    format("Testing ~k~n", [TokenList]),

    (parse(TokenList, AST) ->

        (evaluate(AST, A) -> 
            (format("Expected ~w, got ~w, ", [Expected, A]),
             (Expected =:= A -> writeln('PASS'); writeln('FAIL')));
            format("Evaluate failure, expected ~w, FAIL~n", [Expected]));

        format("Parse failure, expected ~w, FAIL~n", [Expected])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% define tests as their own rules + by using the do_tests defined above;
% p for expected parse failure
% e for expected eval failure
% Add the expected value return if one is expected
% return
t1 :- do_test([return,1,'.'], 1).
t2 :- do_test([return,1.0,'.'], 1.0).

% parenthesis
t3 :- do_test([return,'(',1,'.'], p).
t4 :- do_test([return,1,')','.'], p).
t5 :- do_test([return,'(',42,')','.'], 42).
t6 :- do_test([return,'(','(',42,')',')','.'], 42).
t7 :- do_test([return,'(',42,'(',')',')','.'], p).
t8 :- do_test([return,'(','(','(','(',42,')',')',')',')','.'], 42).

% arithmetic
t9 :- do_test([return,'(',1,+,1,')','.'], 2.0).
t10 :- do_test([return,'(',2,*,2,')','.'], 4.0).
t11 :- do_test([return,'(',1,+,1,+,1,')','.'], 3.0).
t12 :- do_test([return,'(',2,*,4,*,6,')','.'], 48.0).
t13 :- do_test([return,'(',1,-,1,')','.'], 0.0).
t14 :- do_test([return,'(',2,/,2,')','.'], 1.0).
t15 :- do_test([return,'(',1,-,1,+,1,')','.'], 1.0).
t16 :- do_test([return,'(',64,/,2,/,8,')','.'], 4.0).
t17 :- do_test([return,'(',1,+,'(',1,+,1,')',')','.'], 3.0).
t18 :- do_test([return,'(',2,*,'(',2,/,2,')',')','.'], 2.0).
t19 :- do_test([return,'(',4,-,'(',2,-,1,')',')','.'], 3.0).
t20 :- do_test([return,'(',4,/,'(',2,-,-2,')',')','.'], 1.0).
t21 :- do_test([return,'(',1,*,'(',3,/,2,')',')','.'], 1.5).
t22 :- do_test([return,'(',2.5,*,'(',3,/,2,')',')','.'], 3.75).
t23 :- do_test([return,'(',2.5,+,3.66,')','.'], 6.16).
t24 :- do_test([return,'(',2.5,-,1.01,')','.'], 1.49).
t25 :- do_test([return,'(',2.5,-,1.01,+,'(',3,*,1.5,')',')','.'], 5.99).
t26 :- do_test([return,'(',2,*,9,/,6,')','.'], 3.0).
t27 :- do_test([return,'(','(','(',1,+,1,+,1,')',')',')','.'], 3.0).
t28 :- do_test([return,'(','(','(',1,-,1,-,1,')',')',')','.'], -1.0).

% Right Associativity
t29 :- do_test([return,'(',1,-,1,+,1,+,10,')','.'], 11.0).
t30 :- do_test([return,'(',10,-,1,+,2,*,10,')','.'], 29.0).
t31 :- do_test([return,'(',10,-,10,/,2,+,10,')','.'], 15.0).
t32 :- do_test([return,'(',10,-,10,*,5,-,10,')','.'], -50.0).
t33 :- do_test([return,'(',10,+,10,-,5,-,10,')','.'], 5.0).
t34 :- do_test([return,'(',10,+,10,-,5,+,10,')','.'], 25.0).
t35 :- do_test([return,'(',1,+,10,-,5,+,10,-,11,')','.'], 5.0).
t36 :- do_test([return,'(',1,+,8,+,5,-,10,+,9,')','.'], 13.0).
t37 :- do_test([return,'(',10,*,8,/,2,/,10,')','.'], 4.0).
t38 :- do_test([return,'(',8,*,10,/,2,*,10,')','.'], 400.0).
t39 :- do_test([return,'(',5,/,10,*,2,*,210,')','.'], 210.0).
t40 :- do_test([return,'(',3,/,4,*,20,*,2,')','.'], 30.0).
t41 :- do_test([return,'(',3,/,4,*,8,+,2,')','.'], 8.0).
t42 :- do_test([return,'(',3,/,4,*,8,-,2,')','.'], 4.0).
t43 :- do_test([return,'(',3,*,4,/,8,+,1,')','.'], 2.5).
t44 :- do_test([return,'(',3,*,40,/,4,*,10,+,1,')','.'], 301.0).

% Var
t45 :- do_test([var,x,;,x,<-,1,;,return,x,'.'], 1).
t46 :- do_test([return,x,'.'], e).
t47 :- do_test([var,x,;,return,x,'.'], e).
t48 :- do_test([x,<-,1,;,return,x,'.'], e).
t49 :- do_test([x,<-,1,;,return,1,'.'], e).
t50 :- do_test([var,x,;,var,y,;,x,<-,1,;,y,<-,2,;,return,'(',x,+,y,')','.'], 3.0).
t51 :- do_test([var,x,;,var,y,;,x,<-,1,;,return,'(',x,+,y,')','.'], e).
t52 :- do_test([var,x,;,var,y,;,x,<-,'(',1,+,1,+,1,')',;,return,x,'.'], 3.0).
t53 :- do_test([var,x,;,var,y,;,var,z,;,x,<-,1,;,y,<-,2,;,z,<-,'(',x,+,y,')',;,return,z,'.'], 3.0).
t54 :- do_test([var,x,;,var,y,;,x,<-,1,;,y,<-,2,;,return,y,'.'], 2).
t55 :- do_test([var,x,;,var,y,;,x,<-,1,;,x,<-,2,;,return,x,'.'], 2).
t56 :- do_test([var,x,;,var,y,;,x,<-,1,;,y,<-,1000,;,x,<-,y,;,return,x,'.'], 1000).
t57 :- do_test([var,x,;,var,y,;,x,<-,1,;,y,<-,1000,;,x,<-,y,;,y,<-,2,;,return,x,'.'], 1000).
t58 :- do_test([var,x,;,var,y,;,x,<-,a,;,y,<-,1000,;,x,<-,y,;,return,x,'.'], e).
t59 :- do_test([var,x,;,var,y,;,x,<-,1,;,y,<-,a,;,x,<-,y,;,return,y,'.'], e).
t60 :- do_test([var,x,;,x,<-,'(','(',2,+,6,+,1,')',+,1,')',;,return,x,'.'], 10.0).
t61 :- do_test([var,x,;,x,<-,'(','(',2,+,6,+,1,')',*,2,*,2,')',;,return,x,'.'], 36.0).
t62 :- do_test([var,a,;,var,b,;,a,<-,5,;,b,<-,'(',a,+,5,')',;,b,<-,'(',a,+,b,')',;,return,b,'.'], 15.0).
t63 :- do_test([var,a,;,var,b,;,a,<-,b,;,b,<-,37,;,return,a,'.'], e).
t64 :- do_test([var,a,;,var,b,;,b,<-,37,;,a,<-,30,;,return,b,'.'], 37).
t65 :- do_test([var,b,;,b,<-,37.1245,;,return,b,'.'], 37.1245).

% Declaration Assignment
t66 :- do_test([var,x,<-,1,;,return,x,'.'], 1).
t67 :- do_test([var,x,<-,1,;,var,x,;,return,x,'.'], e).
t68 :- do_test([var,x,<-,1,;,x,<-,100,;,return,x,'.'], 100).
t69 :- do_test([var,x,<-,1,;,var,x,<-,4,;,return,x,'.'], e).
t70 :- do_test([var,x,<-,1,;,var,y,;,y,<-,10,;,return,'(',x,+,y,')','.'], 11.0).
t71 :- do_test([var,x,<-,1,;,var,y,;,y,<-,1000,;,x,<-,y,;,return,x,'.'], 1000).
t72 :- do_test([var,y,;,y,<-,1000,;,var,x,<-,y,;,return,x,'.'], 1000).
t73 :- do_test([var,x,<-,a,;,return,10,'.'], e).
t74 :- do_test([var,x,<-,'(',10,+,5,')',;,return,x,'.'], 15.0).
t75 :- do_test([var,y,;,y,<-,2,;,var,x,<-,'(',10,+,y,')',;,return,x,'.'], 12.0).
t76 :- do_test([var,x,<-,x,;,return,10,'.'], e).
t77 :- do_test([var,x,;,x,<-,2,;,var,x,<-,10,;,return,10,'.'], e).
t78 :- do_test([var,x,<-,1.12555,;,return,x,'.'], 1.12555).

% Var Assignment
t79 :- do_test([var,a,;,var,b,;,var,c,;,a,<-,5,;,b,<-,'(',a,+,5,')',;,c,<-,'(',10,+,a,+,b,')',;,return,c,'.'], 25.0).
t80 :- do_test([var,a,;,var,b,;,var,c,;,a,<-,5,;,b,<-,'(',a,+,5,')',;,c,<-,'(',10,*,a,*,b,')',;,return,c,'.'], 500.0).
t81 :- do_test([var,a,;,var,b,;,var,c,;,a,<-,5,;,b,<-,'(',a,+,5,')',;,c,<-,'(',10,-,a,-,b,')',;,return,c,'.'], -5.0).
t82 :- do_test([var,a,;,var,b,;,var,c,;,a,<-,5,;,b,<-,'(',a,+,5,')',;,c,<-,'(',10,-,a,+,b,')',;,return,c,'.'], 15.0).
t83 :- do_test([var,a,;,var,b,;,var,c,;,a,<-,5,;,b,<-,'(',a,+,5,')',;,c,<-,'(',100,/,a,/,b,')',;,return,c,'.'], 2.0).
t84 :- do_test([var,a,;,var,b,;,var,c,;,a,<-,5,;,b,<-,'(',a,+,5,')',;,c,<-,'(',100,/,a,*,b,')',;,return,c,'.'], 200.0).
t85 :- do_test([var,a,;,var,b,;,var,c,;,a,<-,5,;,b,<-,'(',a,+,5,')',;,c,<-,'(',100,+,a,*,b,-,200,')',;,return,c,'.'], -50.0).
t86 :- do_test([var,a,;,var,b,;,var,c,;,a,<-,5,;,b,<-,'(',a,+,5,')',;,c,<-,'(','(',100,+,a,')',*,b,-,200,')',;,return,c,'.'], 850.0).
t87 :- do_test([var,a,;,var,b,;,var,c,;,a,<-,5,;,b,<-,'(',a,+,5,')',;,c,<-,'(','(',10,+,a,')',*,'(',b,-,20,')',')',;,return,c,'.'], -150.0).
t88 :- do_test([var,a,;,var,b,;,var,c,;,a,<-,5,;,b,<-,'(',a,+,5,')',;,c,<-,'(','(',10,+,a,')',+,'(',b,-,20,')',')',;,return,c,'.'], 5.0).
t89 :- do_test([var,a,;,var,b,;,var,c,;,a,<-,5,;,b,<-,'(',a,+,5,')',;,c,<-,'(','(',10,+,a,')',-,'(',b,-,20,')',')',;,return,c,'.'], 25.0).
t90 :- do_test([var,a,;,var,b,;,var,c,;,a,<-,5,;,b,<-,'(',a,+,5,')',;,c,<-,'(','(',10,+,a,')',/,'(',b,-,20,')',')',;,return,c,'.'], -1.5).
t91 :- do_test([var,a,;,var,b,;,var,c,;,a,<-,5,;,b,<-,'(',a,+,5,')',;,c,<-,'(','(',10,*,a,')',/,'(',b,-,15,+,10,')',')',;,return,c,'.'], 10.0).
t92 :- do_test([var,a,;,var,b,;,var,c,;,a,<-,5,;,b,<-,'(',a,+,5,')',;,c,<-,'(','(',10,*,'(',a,+,1,')',')',/,'(','(',b,-,20,')',/,2,')',')',;,return,c,'.'], -12.0).

% Base
t93 :- do_test([return,3,+,5,'.'], p).
t94 :- do_test([var,x,;,x,<-,3,+,5,;,return,10,'.'], p).
t95 :- do_test([var,x,<-,3,+,5,;,return,10,'.'], p).

% Parse
t96 :- do_test([var,x,x,<-,'(',3,+,5,')',;,return,10,'.'], p).
t97 :- do_test([var,x,;,;,x,<-,'(',3,+,5,')',;,return,10,'.'], p).
t98 :- do_test([var,x,;,x,<-,'(',3,+,5,')',;,return,10], p).
t99 :- do_test([var,x,;,x,<-,'(',3,+,5,')',;,return,10,'.','.'], p).
t100 :- do_test([var,x,;,x,<-,'(',3,a,5,')',;,return,10,'.'], p).
t101 :- do_test([var,x,;,x,<-,'(',3,5,')',;,return,10,'.'], p).
t102 :- do_test([var,x,;,x,<-,'(',3,+,+,5,')',;,return,10,'.'], p).
t103 :- do_test([var,x,;,3,;,x,<-,'(',3,+,5,')',;,return,10,'.'], p).
t104 :- do_test([var,x,'8',x,<-,'(',3,+,5,')',;,return,10,'.'], p).

% Return
t105 :- do_test([return,'.'], p).
t106 :- do_test([return,;,return,1,'.'], p).
t107 :- do_test([return,1,'.',return,1,'.'], p).
t108 :- do_test([a,<-,1,;], p).
t109 :- do_test([var,a,;,a,<-,1,;], p).
t110 :- do_test([var,a,;,a,<-,1,;,return,a,'.',a,<-,0], p).
t111 :- do_test([var,a,;,a,<-,1,;,return,a,;,a,<-,0,'.'], p).

% Invalid variable names
t112 :- do_test([var,<-,;,<-,<-,100,;,return,10,'.'], p).
t113 :- do_test([var,var,;,var,<-,100,;,return,10,'.'], p).
t114 :- do_test([var,return,;,return,<-,100,;,return,10,'.'], p).
t115 :- do_test([var,+,;,+,<-,100,;,return,10,'.'], p).
t116 :- do_test([var,-,;,-,<-,100,;,return,10,'.'], p).
t117 :- do_test([var,*,;,*,<-,100,;,return,10,'.'], p).
t118 :- do_test([var,/,;,/,<-,100,;,return,10,'.'], p).
t119 :- do_test([var,'.',;,'.',<-,100,;,return,10,'.'], p).
t120 :- do_test([var,;,;,;,<-,100,;,return,10,'.'], p).
t121 :- do_test([var,'(',;,'(',<-,100,;,return,10,'.'], p).
t122 :- do_test([var,')',;,')',<-,100,;,return,10,'.'], p).
t123 :- do_test([var,10,;,10,<-,100,;,return,10,'.'], p).
t124 :- do_test([var,10.0,;,10.0,<-,100,;,return,10,'.'], p).

% Assign3 Tests
t125 :- do_test([var, x, ;, x,<-, '(', 5, *, 2, ')', ;, return, '(', x, +, 1, ')', '.'],11).
t126 :- do_test(['var', 'x', '<-', 1, ';', 'if', '(', 'true', ')', 'then', 'x', '<-', 2, '.', 'endif', ';', 'return', 'x', '.'],2).
t127 :- do_test(['var', 'x', '<-', 1, ';', 'if', '(', 8, '>', 6, ')', 'then', 'x', '<-', 2, '.', 'endif', ';', 'return', 'x', '.'],2).
t128 :- do_test([function, f, '(', x, ')', '{', return, x, '.', '}', ';', return, f, '(', '(', 10, +, 1, ')',  ')',  '.'],11).
t129 :- do_test(['var', 'x', '<-', 1, ';', 'while', '(', 'x', '<', 5, ')', 'do', 'x', '<-', '(', 'x', '+', 1, ')', '.', 'done', ';', 'return', 'x', '.'],5).
t130 :- do_test(['var', 'x', '<-', 1, ';', 'if', '(', '(', 'true', '||', 'false', ')', ')', 'then', 'x', '<-', 2, '.', 'endif', ';', 'return', 'x', '.'], 2).
t131 :- do_test(['var', 'x', '<-', 1, ';', 'if', '(', '!', '(', 'false', ')', ')', 'then', 'x', '<-', 2, '.', 'endif', ';', 'return', 'x', '.'],2).
t132 :- do_test(['var', 'x', '<-', 1, ';', 'if', '(', '(', 'true', '&&', 'true', ')', ')', 'then', 'var', 'x', '<-', 2, '.', 'endif', ';', 'return', 'x', '.'],1).
t133 :- do_test(['var', x, '<-', 1, ';', 'if', '(', x, '<', 0, ')', 'then', x, '<-', 10, '.', 'else', x, '<-', 20, '.', 'endif', ';', return, x, '.'],20).
t134 :- do_test([function, f, '(', x, ')', '{', return, x, '.', '}', ';', return, f, '(', '(', 10, +, 1,')', ')', '.'],11).

% add tests to this list
test_list([t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50, t51, t52, t53, t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77, t78, t79, t80, t81, t82, t83, t84, t85, t86, t87, t88, t89, t90, t91, t92, t93, t94, t95, t96, t97, t98, t99, t100, t101, t102, t103, t104, t105, t106, t107, t108, t109, t110, t111, t112, t113, t114, t115, t116, t117, t118, t119, t120, t121, t122, t123, t124, t125, t126, t127, t128, t129, t130, t131, t132, t133, t134]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TEST RUNNER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test runner
run_tests([]).  % no tests left
run_tests([Head|Tail]) :-
    catch(call_with_time_limit(1, Head), _, test_error(Head)),
    writeln('-----'),
    run_tests(Tail).

test_error(Head) :- format('Error with test ~w, FAIL~n', [Head]).

% basically a boolean saying to run the tests
% PLEASE TURN IT OFF FOR TURNIN
tests_on :- true.

% will run the tests upon loading the file
:- (tests_on -> (writeln('Going to run tests:'), writeln('----------'),
                test_list(TestList), run_tests(TestList)), 
                writeln('----------'), writeln('Done running tests.') ; 
                true).


%test cases
%parse(['var', 'x', ';', 'x','<-', '(', 5, '*', 2, ')', ';', 'return', '(', 'x', '+', 1, ')', '.'], AST), evaluate(AST,N).
% This command evaluates, so if it doesn't work for you, it's your fault
%parse([function, f, '(', x, ')', '{', return, x, '.', '}', ';', return, f, '(', '(', 10, +, 1, ')',  ')',  '.'],AST), evaluate(AST,N).
%
%parse(['var', x, '<-', 1, ';', 'if', '(', x, '<', 0, ')', 'then', x, '<-', 10, '.', 'else', x, '<-', 20, '.', 'endif', ';', return, x, '.'],AST), evaluate(AST,N).
%
%parse(['var', 'x', '<-', 1, ';', 'if', '(', 'true', ')', 'then', 'x', '<-', 2, '.', 'endif', ';', 'return', 'x', '.'], AST).
%
%parse(['var', 'x', '<-', 1, ';', 'if', '(', 8, '>', 6, ')', 'then', 'x', '<-', 2, '.', 'endif', ';', 'return', 'x', '.'], AST).
%
%parse(['var', 'x', '<-', 1, ';', 'while', '(', 'x', '<', '5', ')', 'do', 'x', '<-', '(', 'x', '+', 1, ')', '.', 'done', ';', 'return', 'x', '.'], AST).
%
%parse(['var', 'x', '<-', 1, ';', 'if', '(', '(', 'true', '||', 'false', ')', ')', 'then', 'x', '<-', 2, '.', 'endif', ';', 'return', 'x', '.'], AST), evaluate(AST,N).
%
%parse(['var', 'x', '<-', 1, ';', 'if', '(', '!', '(', 'false', ')', ')', 'then', 'x', '<-', 2, '.', 'endif', ';', 'return', 'x', '.'], AST), evaluate(AST,N).
%
%parse(['var', 'x', '<-', 1, ';', 'if', '(', '(', 'true', '&&', 'true', ')', ')', 'then', 'var', 'y', '<-', 2, '.', 'endif', ';', 'return', 'y', '.'], AST), evaluate(AST,N).