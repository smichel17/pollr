-module(concurrency).
-export([map/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%      Parallel Map       %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Calls a helper function that recursively spawns threads to process each 
% element that rebuild the new list through message passing. Receives the
% final list through a message passed from the initially spawned process
% and returns the list
map(F, XS) -> 
    ElementProcessorPID = spawn(fun() -> element_processor(self(), F, XS) end),
    receive
        {ElementProcessorPID, YS} -> YS
    end.

% Element processor takes a list and recursively applies a function across
% all elements in the list and sends the transformed list to the passed PID

% Receive an empty list, reply with an empty list
element_processor(ReplyPid, _F, []) -> 
    ReplyPid ! {self(), []};
% Receive a list, spawn a new process and send it the tail of the list. 
% Process the first element and then when the new process sends back the rest
% of the list, concatenate them together and reply with the processed list
element_processor(ReplyPid,  F, [H | T]) ->
    NextElemPid = spawn(fun() -> element_processor(self(), F, T) end),
    X = F(H),
    receive
        {NextElemPid, XS} -> ReplyPid ! {self(), [X | XS]}
    end.
