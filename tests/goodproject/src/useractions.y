%token Ident Num
%actiontype ()
%%
Start:
  /* Empty */ { () } 
| Ident { () }
| Ident Num { () };