-module(student).

-export([char_count/1, split_on/2, is_utf8_string/1]).

-import(lists, [sort/1, reverse/1]).
-import(maps, [find/2]).

char_count(S) -> char_count(sort(S), #{}).

char_count([], M) -> M;
char_count([H | T], M) ->
	case find(H, M) of 
	{_, V} -> char_count(T, M#{H := V + 1});
	_ -> char_count(T, M#{H => 1}) end.

split_on(S, C) -> split_on(reverse(S), [], [], C).

split_on([], [], L, _) -> L;
split_on([], S, L, _) -> [S | L];
split_on([C | T], [], L, C) -> split_on(T, [], L, C);
split_on([C | T], S, L, C) -> split_on(T, [], [S | L], C);
split_on([H | T], S, L, C) -> split_on(T, [H | S], L, C).

is_utf8_string(<<>>) -> true;
is_utf8_string(<<2#0:1, _:7, T/binary>>) -> is_utf8_string(T);
is_utf8_string(<<2#110:3, _:5, 2#10:2, _:6, T/binary>>) -> is_utf8_string(T);
is_utf8_string(<<2#1110:4, _:4, 2#10:2, _:6, 2#10:2, _:6, T/binary>>) -> is_utf8_string(T);
is_utf8_string(<<2#11110:5, _:3, 2#10:2, _:6, 2#10:2, _:6, 2#10:2, _:6,T/binary>>) -> is_utf8_string(T);
is_utf8_string(<<2#111110:6, _:2, 2#10:2, _:6, 2#10:2, _:6, 2#10:2, _:6, 2#10:2, _:6, T/binary>>) -> is_utf8_string(T);
is_utf8_string(<<2#1111110:7, _:1, 2#10:2, _:6, 2#10:2, _:6, 2#10:2, _:6, 2#10:2, _:6, 2#10:2, _:6, T/binary>>) -> is_utf8_string(T);
is_utf8_string(_) -> false.
