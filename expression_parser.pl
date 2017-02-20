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

base(base(B)) --> identifier(B),
	{not(number(B))},
    {not(keywords(B))}.

base(base(B)) --> ['('],
	expression(B),
	[')'].



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
keywords(var).
keywords(return).


prog(prog(R)) --> retStatement(R),
    ['.'].

prog(prog(ID,P)) --> declaration(ID),
    [';'],
	prog(P).

prog(prog(ID,B,P)) --> declAssignment(ID,B),
    [';'],
    prog(P).

prog(prog(ID,B,P)) --> assignment(ID,B),
    [';'],
    prog(P).

retStatement(return(B)) --> ['return'],
    base(B).

declaration(declaration(ID)) --> ['var'],
    identifier(ID).

assignment(identifier(ID), base(B)) --> identifier(ID),
    ['<-'],
    base(B).

declAssignment(identifier(ID), base(B)) --> ['var'],
    identifier(ID),
    ['<-'],
    base(B).

identifier(identifier(ID)) --> [ID].

% Evaluation rules
evaluate(AST, Number):-
    empty_assoc(var_list_in),
    evaluateProg(AST, var_list_in, var_list_out, Number).

%rule for assignment
evaluateProg(prog(assignment(identifier(ID), base(B))), var_list_in, var_list_out):-
    evalBase(B, var_list_in, R),
    put_assoc(ID, var_list_in, R, var_list_out).

%rule for declaration
evaluateProg(prog(declaration(identifier(ID))), var_list_in, var_list_out):-
    put_assoc(ID, var_list_in, 'NULL', var_list_out).


%rule for declaration assignment
evaluateProg(prog(declAssignment(identifier(ID), base(B))), var_list_in, var_list_out):-
    evalBase(B, var_list_in, R),
    put_assoc(ID, var_list_in, R, var_list_out).

evalBase(base(B), var_list_in, ret):-
    ret is B.

evalBase(identifier(ID), var_list_in, ret):-
    get_assoc(ID, var_list_in, val),
    ret is val.

%evalBase()


