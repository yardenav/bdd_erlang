-module(bdd_301812038).
-export([exp_to_bdd/2,solve_bdd/2]).





% spec is detailed in the assignment doc
exp_to_bdd(BoolFunc,treeHeight) -> Rank = rank(BoolFunc), StartTime = now(),
	{ElapsedTime, Tree} = rotateIte(fun measureHeight/1,Rank,seqPerm(Rank),BoolFunc,StartTime),
	io:fwrite("~n Total Time elapsed - ~w micro seconds ~n", [ElapsedTime]),
	Tree;

exp_to_bdd(BoolFunc,numOfNodes) -> Rank = rank(BoolFunc), StartTime = now(),
	{ElapsedTime, Tree} = rotateIte(fun measureNodes/1,Rank,seqPerm(Rank),BoolFunc,StartTime),
	io:fwrite("~n Total Time elapsed - ~w micro seconds ~n", [ElapsedTime]),
	Tree;

exp_to_bdd(BoolFunc,numOfLeafs) -> Rank = rank(BoolFunc), StartTime = now(),
	{ElapsedTime, Tree} = rotateIte(fun measureLeafs/1,Rank,seqPerm(Rank),BoolFunc,StartTime),
	io:fwrite("~n Total Time elapsed - ~w micro seconds ~n", [ElapsedTime]),
	Tree;
%used for elapsed time measurments.
exp_to_bdd(BoolFunc,measureTimeElapsed) -> Rank = rank(BoolFunc), StartTime = now(),
	{ElapsedTime, Tree} = rotateIte1(fun measureLeafs/1,Rank,seqPerm(Rank),BoolFunc,StartTime),
	io:fwrite("~n Total Time elapsed - ~w micro seconds ~n", [ElapsedTime]),
	Tree.

% spec is detailed in the assignment doc
solve_bdd(Func,[]) -> eval(Func);
solve_bdd(Func,[{Var,Value}|T]) ->  solve_bdd(assignNext(Func,Value,Var),T).

%% ============================== Evaluating assigned function and assigning variables. =======================

% performs the action of not.
toggle(A) -> 
	case A of 
		0 -> 1;
		1 -> 0
	end.

% returns the final asnwer of a given an assigned boolean function (with x1..xn is replaces with 0,1).
% It's done recursivly while each recursive call deals with evaluation of one operation (not,or,and).
eval(Num) when is_integer(Num) -> Num;
eval({'not',B}) -> toggle(eval(B));
eval({'and',{A,B}}) -> eval(A)*eval(B);
eval({'or',{A,B}}) ->  Result = eval(A)+eval(B), 
	if 
		Result > 1 -> 1;
		true -> Result
	end.



%% Assigning a value inside the Boolean FUnction.
%% input - Boolean function {operaion,{A,B}} , Value (1\0) , atom (x1,x2...) which represent a variable in the function.
%% output - The initial boolean function with the given 'Value' instead of every instance of the given atom.
%% example - assignNext({'or',{x1,x2}},1,x1) -> {'or',{1,x2}}
assignNext(TheAtom,Val,TheAtom) -> Val;
assignNext(A,_,_) when is_atom(A) -> A;
assignNext(A,_,_) when is_integer(A) -> A;
assignNext({TheAtom,TheAtom},Val,TheAtom) -> {Val,Val};
assignNext({TheAtom,B},Val,TheAtom) -> {Val,assignNext(B,Val,TheAtom)};
assignNext({A,TheAtom},Val,TheAtom) -> {assignNext(A,Val,TheAtom),Val};
assignNext({A,B},Val,TheAtom) -> {assignNext(A,Val,TheAtom),assignNext(B,Val,TheAtom)}.


%input - Boolean Function, Rank of the function (how many different variables), wanted Index.
% returns the Index'd Variable from the boolean func in order of apearence.
varNum(Func,Rank,Index) when Index =< Rank -> lists:nth(Index,fNr(Func));
varNum(_,_,_) -> something.


%% ============================== Boolean function Manipulation. =======================



% takes a boolean function and returns a list with all the atom variables in it, excluding the 'or','and','not' operators. the list is sorted in the order of apearence. may include duplicated variables.
%example flatten({'or',{x1,x2}}) -> [x1,x2]
flatten(A) when is_atom(A) -> [A];
flatten(A) when is_integer(A) -> [A];
flatten({A,B}) when (A =:= 'not') or (A =:= 'or') or (A =:= 'and') -> flatten(B);
flatten({A,B}) when (is_atom(A) and is_atom(B)) -> [A,B];
flatten({A,B}) -> flatten(A) ++ flatten(B).


% takes a list of atoms and removes any duplicated atoms. keeps the same order.
removeDups([]) -> [];
removeDups(List) ->
    Set = sets:from_list(List),
    sets:to_list(Set).

% flattening the Func and removing duplicated vars.
fNr(Func) -> removeDups(flatten(Func)).

% takes a boolean functio and returns the rank of it (how many different variables are present)
%example rank({'or',{x1,x2}}) -> 2
rank(Func) -> length(fNr(Func)).

% given a list, the function returns the index of a given Val(atom).
findIndex([Val|_T],Val,Index) -> Index;
findIndex([_H|T],Val,Index) -> findIndex(T,Val,Index+1).


%% ============================== Tree construction =======================

% the main function that builds a tree out of a boolean function.
% input :
%	OrigFunc - The original boolean function that is sent from exp_to_bdd. contains only operations (or,and,not) and variable atoms (x1,x2...)
%	CurrFunc - The boolean function that recursivly changing and being assigned with 1 and 0. contains operations (or,and,not) 1,0 integers, and variable atoms (x1,x2...)
%	Rank(integer) - the rank of the boolean function (number of different variables)
%	Step(integer) - The stage (from 1 to Rank) that we currently dealing with. different steps deal with different variables. 
%	Perm(List of integers) - determines the order of variables that the tree should be constructed.
% output : 
%	A BDD according to all the inputs. 
ite(_,CurrFunc,Rank,Step,_) when Rank+1 =:= Step-> eval(CurrFunc);
ite(OrigFunc,CurrFunc,Rank,Step,Perm) ->
	CurrVar = varNum(OrigFunc,Rank,lists:nth(Step,Perm)),
	Low = ite(OrigFunc,assignNext(CurrFunc,0,CurrVar),Rank,Step+1,Perm),
	High = ite(OrigFunc,assignNext(CurrFunc,1,CurrVar),Rank,Step+1,Perm),
	if
		Low =:= High  -> Low;
		true -> {CurrVar,{Low,High}}
	end.

% This function generates all the different trees according to the different permutations of order, chooses the optimal tree according to the pre-determined measuring function.
% input: 
%	MeasureFunc - which measurment to use (treeHeight,numOfNodes,numOfLeafs)
%	Rank(integer) - same as explained before.
%	PermList(list of lists of integers) - contains lists of all the different permutations for the list [1..Rank].
%	BoolFunc - The boolean function to process
%	StartTime - The time that exp_to_bdd was invoked.
% output:
%	The tuple - {Time elapsed from StartTime , The optimal tree according to the measurment function (MeasureFunc)}
rotateIte(MeasureFunc,Rank,PermList,BoolFunc,StartTime) -> 
	TreeList = [ite(BoolFunc,BoolFunc,Rank,1,Perm) || Perm <- PermList],
	MeasuredList = [MeasureFunc(Tree) || Tree <- TreeList],
	BestMeasure = lists:min(MeasuredList),
	IndexOfBest = findIndex(MeasuredList,BestMeasure,1),
	{timeDiff(StartTime),lists:nth(IndexOfBest,TreeList)}.

%used only for elapsed time measurment.
rotateIte1(MeasureFunc,Rank,PermList,BoolFunc,StartTime) -> 
	TreeList = [ite1(BoolFunc,BoolFunc,Rank,1,Perm) || Perm <- PermList],
	MeasuredList = [MeasureFunc(Tree) || Tree <- TreeList],
	BestMeasure = lists:min(MeasuredList),
	IndexOfBest = findIndex(MeasuredList,BestMeasure,1),
	{timeDiff(StartTime),lists:nth(IndexOfBest,TreeList)}.

% For measuring specific permutation elapsed time.
ite1(BoolFunc,BoolFunc,Rank,1,Perm) ->
	PermStartTime  = now(),
	{Tree,ElapsedTime} = {ite(BoolFunc,BoolFunc,Rank,1,Perm),timeDiff(PermStartTime)},
	io:fwrite("~n *Perm: ~w, Time elapsed - ~w micro secs ~n", [Perm,ElapsedTime]),
	Tree.
	

%% ============================== Ordering Functions =======================

% returns the height of a given tree.
measureHeight(Tree) when is_integer(Tree) -> 1;
measureHeight({_Var,{A,B}}) -> myMax(1 + measureHeight(A),1 + measureHeight(B)).

% returns the number of nodes of a given tree.
measureNodes(Tree) when is_integer(Tree) -> 1;
measureNodes({_Var,{A,B}}) -> measureNodes(A) + measureNodes(B) + 1.

% returns the number of leafs of a given tree.
measureLeafs(Tree) when is_integer(Tree) -> 1;
measureLeafs({_Var,{A,B}}) -> measureNodes(A) + measureNodes(B).


%% ============================== Helping Functions =======================

% calculates the time elapsed between the time input tuple and the now() output tuple.
timeDiff({_,Secs,MicSecs}) -> {_N,NewSecs,NewMicSecs} = now(), (NewSecs-Secs)*1000000 + (NewMicSecs-MicSecs).
	
% generates all the permutations of a given list L.
perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

% generates all permutation for the sequence [1..Num].
seqPerm(0) -> [];
seqPerm(Num) -> perms(lists:seq(1,Num)).

% returns the greater between A and B
myMax(A,B) -> if A > B -> A; true -> B end.










% example func - {'or',{{'or',{{'and',{x1 , {'not',x2}}},{'and',{x2,x3}}}},x3}}
