open Types

let rules = function
  | S -> [
      [Syntagme PInd];
      [Syntagme PInd; Syntagme S];
      [Syntagme PInd; Nature ConjC; Syntagme S];
      [Syntagme PSub];
    ]
  | PInd -> [
      [Nature Intj; Syntagme PInd];
      [Nature Intj; Syntagme GP];
      [Nature Adv; Syntagme PInd];
      [Syntagme GN; Syntagme GV];
      [Syntagme GV];
      (* Interrogatives *)
      [Nature Adv; Syntagme GV];         (* Pourquoi mange Paul ? *)
      [Nature Pro; Syntagme GV];         (* Que mange Paul ? *)
      (* Negatives *)
      [Syntagme GN; Nature Pro; Nature V; Nature Adv];  (* Il ne mange pas *)
      [Syntagme GN; Nature Pro; Nature V; Nature Adv; Syntagme GN];  (* Il ne mange pas de pain *)
      [Nature Adv; Nature V; Nature Pro];          (* Pourquoi vient-il ? *)
    ]
  | PSub -> [
      [Nature ConjS ; Syntagme GN; Syntagme GV];    (* Que Marie parte *)
      [Nature ConjS ; Syntagme GV; Syntagme GN];               (* Quand elle arrive *)
    ]
  | GN -> [
      [Nature Pro];
      [Nature Det; Nature N];
      [Nature Det; Syntagme GAdj; Nature N];
      [Nature Det; Nature N; Syntagme GAdj];
      [Nature Det; Nature N; Syntagme GP];
      [Nature Det; Syntagme GAdj; Nature N; Syntagme GAdj];
      [Nature Det; Nature Num; Nature N];
    ]
  | GV -> [
      [Nature V; Syntagme GV]; (*part. passé *)
      [Nature V];
      [Nature V; Syntagme GAdj];
      [Nature V; Syntagme GN];
      [Nature V; Syntagme GP];
      [Nature Pro; Nature V];
      [Nature V; Nature Adv; Syntagme GN];
      [Nature V; Nature Adv; Syntagme GP];
      [Nature V; Nature Adv];
      [Nature Pro; Nature V; Nature Adv];  (* ne mange pas *)
      [Nature V; Syntagme PSub];
    ]
  | GAdj -> [
      [Nature Adv; Syntagme GAdj];
      [Nature Adj; Syntagme GAdj];
      [Nature Adj; Nature ConjC; Nature Adj];
      [Nature Adj];
    ]
  | GP -> [
      [Nature Prep; Syntagme GN];
    ]

let rules_old = function
	| S -> [
			[Syntagme PInd];
			[Syntagme PInd; Syntagme S];
			[Syntagme PInd; Nature ConjC; Syntagme S];
		]
	| PInd -> [
			[Nature Intj; Syntagme GP];
			[Nature Adv; Syntagme PInd];
			[Syntagme GN; Syntagme GV];
			[Syntagme GV];
		]
	| PSub -> [
			[Nature ConjS ; Syntagme S]
	]
	| GN -> [
			[Nature Pro];
			[Nature Det; Nature N];
			[Nature Det; Syntagme GAdj; Nature N];
			[Nature Det; Nature N; Syntagme GAdj];
			[Nature Det; Nature N; Syntagme GP];
			[Nature Det; Syntagme GAdj; Nature N; Syntagme GAdj];
		]
	| GV -> [
			[Nature V];
			[Nature V; Syntagme GAdj];
			[Nature V; Syntagme GN];
			[Nature V; Syntagme GP];
			[Nature Pro; Nature V];
			[Nature V; Nature Adv; Syntagme GN];
			[Nature V; Nature Adv; Syntagme GP];
			[Nature V; Nature Adv];
			[Nature Pro; Nature V; Nature Adv];
	]
	| GAdj -> [
			[Nature Adv; Syntagme GAdj];
			[Nature Adj; Syntagme GAdj];
			[Nature Adj; Nature ConjC; Nature Adj];
			[Nature Adj];
	]
	| GP -> [
			[Nature Prep; Syntagme GN];
	]