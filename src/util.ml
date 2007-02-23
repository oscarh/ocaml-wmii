let split_string str delimiter =
   let rec split str i tokens =
      if i = 0 then 
         str :: tokens
      else if str.[i] = delimiter then
          let token_len = (String.length str) - (i + 1) in
          let token = String.sub str (i + 1) token_len in
          let rest = String.sub str 0 i in
          split rest (i - 1) (token :: tokens)
      else 
         split str (i - 1) tokens in
   split str ((String.length str) - 1) []
