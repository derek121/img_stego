-module(img_png_inspect).

-export([get_pixels/1]).

-include_lib("erl_img/include/erl_img.hrl").

get_pixels(ImgPath) ->
  {ok, Img} = erl_img:load(ImgPath),
  Pixmap = hd(Img#erl_image.pixmaps),
  Pixmap#erl_pixmap.pixels.




