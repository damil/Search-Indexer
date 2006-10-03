query : 'term'
      | '+' query
      | '-' query
      | '(' query ')'
      | query 'AND' query
      | query 'OR' query
      | '"' '[^"]+' '"'

     
