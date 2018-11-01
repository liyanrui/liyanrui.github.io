<!--
.. title: 流程图
.. slug: flow-charts
.. date: 2018-10-31 17:42:15 UTC+08:00
.. tags: MetaFun
.. category: 技术
.. link: 
.. description: 
.. type: text
-->

![自出入](/images/flow-charts/demo.png)

```TeX
\usemodule[zhfonts]
\defineframed
  [myframe]
  [frame=off, offset=overlay,
    width=2.5cm, autowidth=force,
    align={middle, lohi, broad}]
\startMPpage
input chart.mp;
picture t; pair t.out, t.in;
t := procedure("foo", like(fullsquare xysized (2cm,1cm))) shifted (10cm, 0);
t.out := anchor(t, "right", 0);
t.in  := anchor(t, "top", 0);

path t.self;
t.self := t.out
          && right * margin
          && up * (margin + ypart t.in)
          && left * (H t.in)
          -- t.in;

draw t; tagged_flow ("bar", "top", 0.6) t.self;

% 结点
picture a, b, c, d, e, f;
a := data("元文档");
b := put(data("文献数据库"), a, "bottom") shifted (0, 0.5vgap);
c := put(procedure("nar", like(a +++ b)), a +++ b, "right");
d := put(data("\myframe{含文献引用的元文档}"), c, "right");
e := put(procedure("排版引擎", like(d) yscaled 1.5), d, "bottom");
f := put(data("格式化文档"), e, "right");

path a.border, b.border, d.border, f.border;
forsuffixes i = a, b, d, f: i.border := border(i); endfor; 

% 绘制流程图
for i = a, b, c, d, e, f: draw i; endfor;
a.border ==> c; b.border ==> c; c ==> d.border; d.border ==> e; e ==> f.border;

\stopMPpage
```

chart.mp:

```metapost
tertiarydef a +++ b = image(draw a; draw b;) enddef;

vardef like(expr p) =
  save q; path q;
  q := (llcorner p) -- (ulcorner p) -- (urcorner p) -- (lrcorner p) -- cycle;
  q := q shifted -(center p);
  q
enddef;

vardef border(expr p) =
  save w; numeric w;
  w := bbwidth p;
  like(p) scaled ((w + padding) / w) shifted (center p)
enddef;

vardef data(expr s) =
  image(draw textext(s) withcolor datacolor; )
enddef;

vardef procedure(expr s, frame) =
  image(fill frame withcolor backgroundcolor;
        drawpath frame withcolor framecolor;
        draw textext(s) withcolor procedurecolor;
  )
enddef;

vardef put(expr p, ref, toward) =
  save center_of_ref, center_of_p, d;
  pair center_of_ref, center_of_p; numeric d;
  center_of_ref := center ref;
  if toward = "left":
    d := xpart center_of_ref - 0.5(bbwidth ref) - 0.5(bbwidth p) - hgap;
    center_of_p := (d, ypart center_of_ref);
  elseif toward = "right":
    d := xpart center_of_ref + 0.5(bbwidth ref) + 0.5(bbwidth p) + hgap;
    center_of_p := (d, ypart center_of_ref);
  elseif toward = "top":
    d := ypart center_of_ref + 0.5(bbheight ref) + 0.5(bbheight p) + vgap;
    center_of_p := (xpart center_of_ref, d);
  elseif toward = "bottom":
    d := ypart center_of_ref - 0.5(bbheight ref) - 0.5(bbheight p) - vgap;
    center_of_p := (xpart center_of_ref, d);
  fi;
  p shifted center_of_p
enddef;

vardef anchor(expr p, base, location) =
  save edge_origin, anchor, ll, ul, ur, lr;
  pair edge_origin, anchor, ll, ul, ur, lr;
  ll := llcorner p; ul := ulcorner p; ur := urcorner p; lr := lrcorner p;
  if base = "left":
    edge_origin := 0.5[ll, ul];
    if location >= 0:
      anchor := location[edge_origin, ul];
    else:
      anchor := location[ll, edge_origin];
    fi;
  elseif base = "right":
    edge_origin := 0.5[lr, ur];
    if location >= 0:
      anchor := location[edge_origin, ur];
    else:
      anchor := location[lr, edge_origin];
    fi;
  elseif base = "top":
    edge_origin := 0.5[ul, ur];
    if location >= 0:
      anchor := location[edge_origin, ur];
    else:
      anchor := location[ul, edge_origin];
    fi;
  elseif base = "bottom":
    edge_origin := 0.5[ll, lr];
    if location >= 0:
      anchor := location[edge_origin, lr];
    else:
      anchor := location[ll, edge_origin];
    fi;
  fi;
  anchor
enddef;

vardef mate_in(expr p, base, mate) =
  save anchor, ll, ul, ur, lr;
  pair anchor, ll, ul, ur, lr;
  ll := llcorner p; ul := ulcorner p; ur := urcorner p; lr := lrcorner p;
  if base = "left":
    anchor := (xpart ll, ypart mate);
  elseif base = "right":
    anchor := (xpart lr, ypart mate);
  elseif base = "top":
    anchor := (xpart mate, ypart ul);
  elseif base = "bottom":
    anchor := (xpart mate, ypart ll);
  fi;
  anchor
enddef;

tertiarydef a ==> b =
  begingroup
    if (pair a) and (pair b):
      drawarrowpath a -- b;
    elseif ((path a) or (picture a)) and ((path b) or (picture b)):
      save out, in, va, vb; pair out, in, va[], vb[];
      va[1] := llcorner a; va[2] := urcorner a;
      vb[1] := llcorner b; vb[2] := urcorner b;
      if xpart va[2] < xpart vb[1]: % a 在 b 的左侧
        out := 0.5[lrcorner a, urcorner a];
        in  := (xpart vb[1], ypart out);
      elseif xpart va[1] > xpart vb[2]: % a 在 b 的右侧
        out := 0.5[llcorner a, ulcorner a];
        in  := (xpart vb[2], ypart out);
      elseif ypart va[1] > ypart vb[2]: % a 在 b 的上方
        out := 0.5[llcorner a, lrcorner a];
        in  := (xpart out, ypart vb[2]);
      elseif ypart va[2] < ypart vb[1]: % a 在 b 的下方
        out := 0.5[ulcorner a, urcorner a];
        in  := (xpart out, ypart vb[1]);        
      fi;
      drawarrowpath out -- in;
    fi;
  endgroup;
enddef;

def flow = drawarrowpath enddef;
def tagged_flow(expr tag, anchor, c) text p =
  begingroup
    save pos, offset; pair pos; numeric offset;
    pos := point c along (p); offset := .5padding;
    if anchor = "top":
      pos := pos shifted (0, offset);
    elseif anchor = "right":
      pos := pos shifted (offset, 0);
    elseif anchor = "bottom":
      pos := pos shifted (0, -offset);
    elseif anchor = "left":
      pos := pos shifted (-offset, 0);
    fi;
    drawarrowpath (p);
    draw scantokens("thetextext" & "." & anchor)(tag, pos);
enddef;

pair __site__; 
tertiarydef a && b =
  if pair a:
    hide(__site__ := b shifted a) a -- __site__
  elseif path a:
    hide(__site__ := b shifted __site__) a -- __site__
  fi
enddef;

def H expr a = abs(xpart a - xpart __site__) enddef;
def V expr a = abs(ypart a - ypart __site__) enddef;

%%%%%%%%
% 样式
color datacolor, procedurecolor, flowcolor, framecolor, backgroundcolor;
datacolor := .375darkgray;
procedurecolor := darkred;
flowcolor := .9darkgray;
framecolor := .7white;
backgroundcolor := .9white;
drawpathoptions(withpen pencircle scaled 1.5 withcolor flowcolor);

% 留白与间距尺寸
numeric gap, hgap, vgap, margin, padding;
gap := 1.5cm;
hgap := gap; vgap := 0.5gap;
margin := 0.3gap; padding := 4;
```
