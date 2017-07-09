-module(download_blocks).
-export([sync_all/2, sync/2, do_send_blocks/2]).

%% TODO: introduce ranking mechanism. It may be based on initial top request.
%%       The quickest and the longest chains are promoted
%%       There is no point of quering thousands of peers and updating our chain at once from all of them

sync_all([], _) -> ok;
sync_all([Peer|T], Height) ->
    spawn(fun() ->
                  sync(Peer, Height)
          end),
    sync_all(T, Height).
sync(Peer, MyHeight) ->
    %lower their ranking
    %peers:update_score(Peer, peers:initial_score()),
    io:fwrite("top of sync\n"),
    %S = erlang:timestamp(),
    talk({top}, Peer, 
	 fun(X) ->
		 case X of
		     {error, failed_connect} -> 
			 io:fwrite("failed connect"),
			 ok;
		     {ok, TopBlock, Height}  ->
			 %io:fwrite("got topblock\n"),
			 DBB = application:get_env(ae_core, download_blocks_batch),
			 HH = MyHeight + DBB,
			 if
			     HH < Height ->
				 %{ok, Block} = talker:talk({block, HH}, IP, Port),
				 %io:fwrite("HH < Height\n"),
				 talk({block_sizecap, HH, application:get_env(ae_core, download_blocks_sizecap),}, Peer,
				      fun(Y) -> trade_blocks(Peer, Y) end);
			     true ->
				 trade_blocks(Peer, [TopBlock]),
				 get_txs(Peer)
			 end,
			 trade_peers(Peer);
			 %Time = timer:now_diff(erlang:timestamp(), S),%1 second is 1000000.
			 %Score = abs(Time)*(1+abs(Height - MyHeight));
		     X -> io:fwrite(X)
		 end
	 end).




trade_blocks(Peer, [PrevBlock|PBT]) ->
    %io:fwrite("trade blocks"),
    %"nextBlock" is from earlier in the chain than prevblock. we are walking backwards
    case block:height(PrevBlock) of 
        1 ->  %if PrevBlock is block 1 we stop
            send_blocks(Peer, top:doit(), block:genesis_hash(), [], 0),
            block_absorber:doit([PrevBlock|PBT]);
        _ ->
            PrevHash = block:hash(PrevBlock),
            %{ok, PowBlock} = talker:talk({block, Height}, Peer),
            {PrevHash, NextHash} = block:check1(PrevBlock),
            M = block:read(PrevHash),%check if it is in our memory already.
            case M of
	        empty -> 
	            talk({block_sizecap, NextHash, application:get_env(ae_core, download_blocks_sizecap)}, Peer,
	    	        fun(NextBatch) ->  %Heighest first, lowest last. We need to reverse
		    	    %[NextBlock | _] = NextBatch,     
                            %NextHash = block:hash(NextBlock), We do this kind of checks with check1 function when we do blockabsorber is invoked
		            trade_blocks(Peer, lists:append(lists:reverse(NextBatch),[PrevBlock|PBT]))
		         end);
	        _ -> 
                    LastCommonHash = block:hash(last_known_block([PrevBlock|PBT])),
                    %We send blocks before absorbing to make sure we don't send any downloaded blocks
                    send_blocks(Peer, top:doit(), LastCommonHash, [], block:height(M)),
                    NewBlocks = remove_known_blocks(PBT),
	            block_absorber:doit(NewBlocks)
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

last_known_block([Block]) ->
    Block;
last_known_block([First | [Second | Other]]) ->
    SecondHash = block:hash(Second),
    M = block:read(SecondHash),
    case M of
        empty ->
            First;
        _ ->
            last_known_block([Second | Other])
    end.

send_blocks(Peer, T, T, L, _) -> 
    send_blocks_external(Peer, L);
send_blocks(Peer, 0, _, L, _) ->
    send_blocks_external(Peer, L);
send_blocks(Peer, TopHash, CommonHash, L, CommonHeight) ->
    BlockPlus = block:read(TopHash),
    PrevHash = block:prev_hash(BlockPlus),
    case block:height(BlockPlus) of
        CommonHeight -> %if we realize, we are on diffrent fork then the peer
            BlockCommon = block:read(CommonHash),
            NewCommon = block:prev_hash(BlockCommon),
            send_blocks(Peer, PrevHash, NewCommon, [BlockPlus|L], block:height(BlockCommon)-1); %we send one more block till we find one common with our main chain and the fork
        _ ->
            send_blocks(Peer, PrevHash, CommonHash, [BlockPlus|L], CommonHeight)
    end.

send_blocks_external(Peer, Blocks) ->
    spawn(?MODULE, do_send_blocks, [Peer, Blocks]).

do_send_blocks(_, []) -> ok;
do_send_blocks(Peer, [Block|T]) ->
    remote_peer({give_block, Block}, Peer),
    timer:sleep(20),
    do_send_blocks(Peer, T).

get_txs(Peer) ->
    Txs = remote_peer({txs}, Peer),
    tx_pool_feeder:absorb(Txs),
    {_,_,Mine} = tx_pool:data(),
    remote_peer({txs, Mine}, Peer).

trade_peers(Peer) ->
    TheirsPeers = remote_peer({peers}, Peer),
    MyPeers = ae_utils:tuples2lists(peers:all()),
    remote_peer({peers, MyPeers}, Peer),
    peers:add(TheirsPeers).

remote_peer(Transaction, Peer) ->
    case talker:talk(Transaction, Peer) of
        {ok, Return0} -> Return0;
        Return1 -> Return1
    end.
