-module(download_blocks).
-export([sync_cron/0, sync_cron/1, sync_all/2, 
	 sync/3, absorb_txs/1, tuples2lists/1]).

sync_cron() -> sync_cron(30000).
sync_cron(N) -> %30000 is 30 second.
    timer:sleep(N),
    Height = block:height(block:read(top:doit())),
    P = peers:all(),
    P2 = rank_filter(P),
    sync_all(P2, Height),
    sync_cron(N).

rank_filter(P) ->
    %probabilistically select a peer. prefer lower ranked pers.
    P.
    
    
sync_all([], _) -> success;
sync_all([{IP, Port}|T], Height) ->
    spawn(fun() ->
		  sync(IP, Port, Height)
	  end),
    %spawn(download_blocks, sync, [IP, Port, Height]),
    %timer:sleep(3000),
    sync_all(T, Height).
sync(IP, Port, MyHeight) ->
    %lower their ranking
    %peers:update_score(IP, Port, peers:initial_score()),
    io:fwrite("top of sync\n"),
    %S = erlang:timestamp(),
    talk({top}, IP, Port, 
	 fun(X) ->
		 case X of
		     {error, failed_connect} -> 
			 io:fwrite("failed connect"),
			 ok;
		     {ok, TopBlock, Height}  ->
			 %io:fwrite("got topblock\n"),
			 DBB = free_constants:download_blocks_batch(),
			 HH = MyHeight + DBB,
			 if
			     HH < Height ->
				 %{ok, Block} = talker:talk({block, HH}, IP, Port),
				 %io:fwrite("HH < Height\n"),
				 talk({block_sizecap, HH, free_constants:download_blocks_sizecap()}, IP, Port,
				      fun(Y) -> trade_blocks(IP, Port, Y) end);
			     true ->
				 trade_blocks(IP, Port, [TopBlock]),
				 get_txs(IP, Port)
				     
			 end,
                         TopHash = block:hash(TopBlock),
	                 send_blocks(IP, Port, top:doit(), TopHash, [], 0),
			 trade_peers(IP, Port);
			 %Time = timer:now_diff(erlang:timestamp(), S),%1 second is 1000000.
			 %Score = abs(Time)*(1+abs(Height - MyHeight));
		     X -> io:fwrite(X)
		 end
	 end).


    %peers:update_score(IP, Port, Score).
    %raise their ranking.
get_blocks(Height, N, IP, Port, _) ->
    %heigh is the heighest we download. Height-n is lowest.
    
    talk({block, max(Height-N, 0), N}, IP, Port, 
	 fun(X) -> X end).

%get_blocks(_, 0, _, _, L) -> L;
%get_blocks(H, _, _, _, L) when H < 1 -> L;
%get_blocks(Height, N, IP, Port, L) ->
    %should send multliple blocks at a time!!
%    talk({block, Height}, IP, Port,
%	 fun(X) -> get_blocks(Height-1, N-1, IP, Port, [X|L])
%	 end).
    
trade_blocks(IP, Port, [PrevBlock|PBT]) ->
    %io:fwrite("trade blocks"),
    %"nextBlock" is from earlier in the chain than prevblock. we are walking backwards
    case block:height(PrevBlock) of 
        1 ->  %if PrevBlock is block 1 we stop
            sync3([PrevBlock|PBT]);
        _ ->
            PrevHash = block:hash(PrevBlock),
            %{ok, PowBlock} = talker:talk({block, Height}, IP, Port),
            {PrevHash, NextHash} = block:check1(PrevBlock),
            M = block:read(PrevHash),%check if it is in our memory already.
            case M of
	        empty -> 
	            talk({block_sizecap, NextHash, free_constants:download_blocks_sizecap()}, IP, Port,
	    	        fun(NextBatch) ->  %Heighest first, lowest last. We need to reverse
		    	    %[NextBlock | _] = NextBatch,     
                            %NextHash = block:hash(NextBlock), We do this kind of checks with check1 function when we do sync3 and blockabsorber is invoked
		            trade_blocks(IP, Port, lists:append(lists:reverse(NextBatch),[PrevBlock|PBT]))
		         end);
	        _ -> 
                    NewBlocks = remove_known_blocks(PBT),
	            sync3(NewBlocks)
            end
    end.

remove_known_blocks([]) ->
    [];
remove_known_blocks([PrevBlock | PBT]) ->
    PrevHash = block:hash(PrevBlock),
    M = block:read(PrevHash),
    case M of 
        empty ->
            [PrevBlock | PBT];
        _ ->
            remove_known_blocks(PBT)
    end.

send_blocks(IP, Port, T, T, L, _N) -> 
    send_blocks2(IP, Port, L);
send_blocks(IP, Port, TopHash, CommonHash, L, N) ->
    if
	TopHash == 0 -> send_blocks2(IP, Port, L);
	%N>4000 -> send_blocks2(IP, Port, L);
	true -> 
	    BlockPlus = block:read(TopHash),
	    PrevHash = block:prev_hash(BlockPlus),
	    send_blocks(IP, Port, PrevHash, CommonHash, [BlockPlus|L], N+1)
    end.
send_blocks2(_, _, []) -> ok;
send_blocks2(IP, Port, [Block|T]) -> 
    talker:talk({give_block, Block}, IP, Port),
    timer:sleep(20),
    send_blocks2(IP, Port, T).
    
sync3([]) -> ok;
sync3([B|T]) -> 
    block_absorber:doit(B),
    sync3(T).
absorb_txs([]) -> ok;
absorb_txs([H|T]) -> 
    tx_pool_feeder:absorb(H),
    absorb_txs(T).
talk(CMD, IP, Port, F) ->
    %io:fwrite("start talk\n"),
    talk(CMD, IP, Port, F, 1).
talk(_, _, _, _, 0) -> 
    io:fwrite("talk error \n"),
    error;
talk(CMD, IP, Port, F, N) ->
    %io:fwrite("talk number "),
    %io:fwrite(integer_to_list(N)),
    %io:fwrite("\n"),
    case talker:talk(CMD, IP, Port) of
	{error, failed_connect} -> talk(CMD, IP, Port, F, N-1);
	{ok, X} -> F(X);
	X -> F(X)
    end.
	   
get_txs(IP, Port) ->
    %io:fwrite("download blocks get txs\n"),
    talk({txs}, IP, Port, 
	 fun(X) ->
		 absorb_txs(X),
		 {_,_,Mine} = tx_pool:data(),
		 talker:talk({txs, Mine}, IP, Port)
	 end).
trade_peers(IP, Port) ->
    talk({peers}, IP, Port,
	 fun(X) ->
		 MyPeers = tuples2lists(peers:all()),
		 talker:talk({peers, MyPeers}, IP, Port),
		 peers:add(X)
	 end).
tuples2lists(X) when is_tuple(X) ->
    tuples2lists(tuple_to_list(X));
tuples2lists([]) -> [];
tuples2lists([H|T]) -> 
    [tuples2lists(H)|tuples2lists(T)];
tuples2lists(X) -> X.
    
    
    


