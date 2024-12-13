-module(day9).

-export([run/1, compact/1, compact2/1, expand_blocks/1]).

run(Filename) ->
    {ok, Data} = file:read_file(Filename),

    Blocks = to_blocks([X - $0 || X <- binary_to_list(Data), X >= $0, X =< $9], 0),

    Part1 = checksum(compact(Blocks)),
    io:format("Part 1: ~p~n", [Part1]),

    Part2 = checksum(compact2(Blocks)),
    io:format("Part 2: ~p~n", [Part2]).

to_blocks([Length, GapLength | Rest], Id) ->
    [{Length, Id}, {GapLength, gap} | to_blocks(Rest, Id + 1)];
to_blocks([Length], Id) ->
    [{Length, Id}];
to_blocks([], _) ->
    [].

compact2(Blocks) ->
    compact_blocks2(queue:from_list(Blocks), []).

compact_blocks2(BlockQueue, Acc) ->
    case queue:out_r(BlockQueue) of
        {empty, _} ->
            Acc;
        {{value, {Length, gap}}, NewBlockQueue} ->
            compact_blocks2(NewBlockQueue, [{Length, gap} | Acc]);
        {{value, {Length, Id}}, NewBlockQueue} when is_integer(Id) ->
            case insert_block({Length, Id}, NewBlockQueue) of
                {ok, NewNewBlockQueue} ->
                    compact_blocks2(NewNewBlockQueue, [{Length, gap} | Acc]);
                {error, NewNewBlockQueue} ->
                    compact_blocks2(NewNewBlockQueue, [{Length, Id} | Acc])
            end
    end.

insert_block(Block, BlockQueue) ->
    insert_block(Block, BlockQueue, []).

insert_block({BlockLength, Id}, BlockQueue, Skipped) ->
    case queue:out(BlockQueue) of
        {empty, _} ->
            {error, queue:from_list(lists:reverse(Skipped))};
        {{value, {GapLength, gap}}, NewBlockQueue} ->
            case GapLength of
                L when L == BlockLength ->
                    {ok, add_all(Skipped, queue:in_r({BlockLength, Id}, NewBlockQueue))};
                L when L < BlockLength ->
                    insert_block({BlockLength, Id}, NewBlockQueue, [{GapLength, gap} | Skipped]);
                L when L > BlockLength ->
                    {ok, add_all(Skipped, queue:in_r({BlockLength, Id}, queue:in_r({L - BlockLength, gap}, NewBlockQueue)))}
            end;
        {{value, Block}, NewBlockQueue}  ->
            insert_block({BlockLength, Id}, NewBlockQueue, [Block | Skipped])
    end.

add_all([], Queue) ->
    Queue;
add_all([Block | Rest], Queue) ->
    add_all(Rest, queue:in_r(Block, Queue)).


compact(Blocks) ->
    compact_blocks(queue:from_list(Blocks)).

compact_blocks(BlockQueue) ->
    case queue:out(BlockQueue) of
        {empty, _} ->
            [];
        {{value, {Length, Id}}, NewBlockQueue} when is_integer(Id) ->
            [{Length, Id} | compact_blocks(NewBlockQueue)];
        {{value, {Length, gap}}, NewBlockQueue} ->
            {RightBlocks, NewNewBlockQueue} = take_blocks(NewBlockQueue, Length),
            RightBlocks ++ compact_blocks(NewNewBlockQueue)
    end.

take_blocks(BlockQueue, 0) ->
    {[], BlockQueue};
take_blocks(BlockQueue, GapLength) ->
    case queue:out_r(BlockQueue) of
        {empty, _} ->
            {[], BlockQueue};
        {{value, {BlockLength, Id}}, NewBlockQueue} when is_integer(Id) ->
            case BlockLength of
                L when L =< GapLength ->
                    {Blocks, Next} = take_blocks(NewBlockQueue, GapLength - L),
                    {[{BlockLength, Id} | Blocks], Next};
                L when L > GapLength ->
                    {[{ GapLength, Id }], queue:in({L - GapLength, Id}, NewBlockQueue)}
            end;
        {{value, {_BlockLength, gap}}, NewBlockQueue} ->
            take_blocks(NewBlockQueue, GapLength)
    end.

expand_blocks([]) ->
    [];
expand_blocks([{Length, Id} | Rest]) ->
    lists:duplicate(Length, Id) ++ expand_blocks(Rest).

checksum(Blocks) -> checksum(Blocks, 0, 0).

checksum([], _, Acc) ->
    Acc;
checksum([{Length, gap} | Rest], Position, Acc) ->
    checksum(Rest, Position + Length, Acc);
checksum([{Length, _Id} | Rest], Position, Acc) when Length =:= 0 ->
    checksum(Rest, Position, Acc);
checksum([{Length, Id} | Rest], Position, Acc) ->
    checksum([{Length - 1, Id} | Rest], Position + 1, Acc + Position * Id).
