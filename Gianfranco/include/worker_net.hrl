%%% @author Gianfranco <zenon@zen.local>
%%% @copyright (C) 2010, Gianfranco
%%% Created : 10 Dec 2010 by Gianfranco <zenon@zen.local>

-record(wn_resource,
      {name :: string(),
       type :: [{atom(), non_neg_integer() | infinity}],
       resides :: node()
      }).

%% added st stage 2

-record(wn_file,
        {id :: string(),
         file :: string(),
         resides :: node()
        }).
