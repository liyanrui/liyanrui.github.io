def llc = llcorner enddef;

vardef make_canvas(expr pic, margin) =
  save ll, ul, ur, lr; pair ll, ul, ur, lr;
  ll := llcorner pic; ll := ((xpart ll) - margin, (ypart ll) - margin);
  ul := ulcorner pic; ul := ((xpart ul) - margin, (ypart ul) + margin);
  ur := urcorner pic; ur := ((xpart ur) + margin, (ypart ur) + margin);
  lr := lrcorner pic; lr := ((xpart lr) + margin, (ypart lr) - margin);
  ll -- ul -- ur -- lr -- cycle
enddef;

vardef text_node(expr s, text_center, text_color) =
  image(draw textext(s) shifted text_center withcolor text_color;)
enddef;

vardef box_path(expr box) =
  (llcorner box) -- (ulcorner box) -- (urcorner box) -- (lrcorner box) -- cycle
enddef;

vardef proc_node(expr s, box, text_center, text_color, bgcolor) =
  save frame; picture proc_text; path frame;
  frame := box_path(box) shifted (text_center - (center box));
  image(fill frame withcolor bgcolor;
        drawpath frame;
        draw text_node(s, text_center, text_color);)
enddef;

vardef move_right(expr pic, ref, offset) =
  save d, refcenter; pair center_of_ref; numeric d;
  center_of_ref := center ref;
  d := xpart center_of_ref + 0.5(bbwidth ref) + 0.5(bbwidth pic) + offset;
  pic shifted (d, ypart center_of_ref)
enddef;

vardef move_upper(expr pic, ref, offset) =
  save d, refcenter; pair refcenter; numeric d;
  refcenter := center ref;
  d := xpart refcenter + 0.5(bbwidth ref) + 0.5(bbwidth pic) + offset;
  pic shifted (d, ypart refcenter)
enddef;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
drawpathoptions(withpen pencircle scaled 1.5 withcolor darkgray);
numeric margin, hdist; margin := 4; hdist := 1.5cm;

picture a, b;
pair a.out, b.out; path canvas.a, canvas.b;
a := text_node("元文档", origin, .5green);
b := text_node("文献数据库", (0, -0.6cm), .3white);
canvas.a := make_canvas(a, margin);
canvas.b := make_canvas(b, margin);
a.out := 0.5[urcorner canvas.a, lrcorner canvas.a];
b.out := 0.5[urcorner canvas.b, lrcorner canvas.b];

picture c; pair c.a.in, c.b.in, c.out;
picture ab; ab := image(draw a; draw b;);
c := proc_node("awkbib", bbox(ab), origin, darkred, .9white);
c := move_r(c, ab, hdist);
c.a.in := (xpart (llcorner c), ypart a.out);
c.b.in := (xpart (llcorner c), ypart b.out);
c.out := 0.5[lrcorner c, urcorner c];

picture d; pair d.out; path cavans.d;
d := text_node("模板", origin, .3white);

picture e; pair e.in; path canvas.e;
e := text_node("\myframe{含文献引用的元文档}", origin, .5green);
e := move_r(e, c, hdist);
canvas.e := make_canvas(e, margin);
e.in := 0.5[llc canvas.e, ulcorner canvas.e];

draw a; draw b; draw c; draw e;
drawarrowpath a.out -- c.a.in;
drawarrowpath b.out -- c.b.in;
drawarrowpath c.out -- e.in;
