atom.         % atom
atoms_rule.   % atoms_rule
true and false.  % false
false or true.   % true
true xor false.  % true
not false.    % true
5 =:= 5.      % true
1 =/= 0.      % true
5 =/= 5.0.    % true
5 == 5.0.     % true
5 /= 5.0.     % false
1 < false.    % true

atom < reference < fun < port < pid < tuple < list < bit string.

X = 10, Y = 4.
Point = {X,Y}.


%% lists
[1, 2, 3, {numbers,[4,5,6]}, 5.34, atom].
[1,2,3] ++ [4,5].
[1,2,3,4,5] -- [1,2,3].     % [4,5]
[2,4,2] -- [2,4].           % [2]
[2,4,2] -- [2,4,2].         % []
[1,2,3] -- [1,2] -- [3].    % [3]
List = [2,3,4].
NewList = [1|List].
[1 | []].
[2 | [1 | []]].             % [2,1]
[3 | [2 | [1 | []]]].       % [3,2,1]


%% list comprehensions
[2*N || N <- [1,2,3,4]].    % [2,4,6,8]
[X || X <- [1,2,3,4,5,6,7,8,9,10], X rem 2 =:= 0].    % [2,4,6,8,10]

RestaurantMenu = [
  {steak, 5.99}, {beer, 3.99}, {poutine, 3.50}, {kitten, 20.99}, {water, 0.00}
].
[{Item, Price*1.07} || {Item,Price} <- RestaurantMenu, Price >= 3, Price =< 10].
% [{steak,6.409300000000001},{beer,4.2693},{poutine,3.745}]
[X+Y || X <- [1,2], Y <- [3,4]].    % [4,5,5,6]

Weather = [
  {toronto, rain}, {montreal, storms}, {london, fog}, {paris, sun}, {boston, fog}, {vancouver, snow}
].
FoggyPlaces = [X || {X, fog} <- Weather].   % [london,boston]


%% binary data
Color = 16#F09A29.      % 15768105
Pixel = <<Color:24>>.   % <<240,154,41>>
Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>.
% <<213,45,132,64,76,32,76,0,0,234,32,15>>
<<Pix1:24, Pix2:24, Pix3:24, Pix4:24>> = Pixels.
% <<213,45,132,64,76,32,76,0,0,234,32,15>>
<<R:8, G:8, B:8>> = <<Pix1:24>>.
% <<213,45,132>>
R.
% 213
<<R:8, Rest/binary>> = Pixels.
% <<213,45,132,64,76,32,76,0,0,234,32,15>>
R.
% 213
