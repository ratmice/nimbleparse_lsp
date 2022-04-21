%start Conflicts
%%
Conflicts: | RR | SR;

RR: | "NUM";

expr: "TRUE" | "FALSE";

stmt:
    if_stmt | "IDENT";
stmts:
    stmt ';' stmts;
    
if_stmt:
  "IF" expr "THEN" stmt 
| "IF" expr "THEN" stmt "ELSE" stmt
;

SR: stmt;