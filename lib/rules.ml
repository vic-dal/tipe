open Types

let rules = function
	| S -> [
			[Syntagme GN; Syntagme GV];
		]
	| GN -> [
			[Nature Pro];
			[Nature Det; Nature N];
			[Nature Det; Syntagme GAdj; Nature N];
			[Nature Det; Nature N; Syntagme GAdj];
			[Nature Det; Syntagme GAdj; Nature N; Syntagme GAdj];
		]
	| GV -> [
			[Nature V];
			[Nature V; Syntagme GN];
			[Nature V; Syntagme GP];
			[Nature V; Nature Adv; Syntagme GN];
		]
	| GAdj -> [
			[Nature Adv; Syntagme GAdj];
			[Nature Adj; Syntagme GAdj];
			[Nature Adj];
	]
	| GP -> [
			[Nature Prep; Syntagme GN]

	]