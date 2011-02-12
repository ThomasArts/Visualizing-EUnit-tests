%% st = states, alph = alphabet, iSt = initialState, tr = transitions
%% fSt = finalStates <- The final states will be the ones that crash
%% The rest would be acceptance states. But is quicker if we store
%% them this way.
-record(fa, {st, alph = [], iSt, tr = [], fSt = []}).
