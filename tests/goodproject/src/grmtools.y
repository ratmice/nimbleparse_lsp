%token Ident Num
%%
Start -> ()
:
  /* Empty */ { () } 
| Ident { () }
| Ident Num { () };