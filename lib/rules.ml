open Types

let rules = function
	| GN -> [
			[Nature N];
			[Nature Det; Nature N];
			[Nature Det; Nature Adj; Nature N];
			[Nature Det; Nature N; Nature Adj];
		]
	| GV -> [
			[Nature V; Syntagme GN];
			[Nature V; Syntagme GAdv];
		]
	| GAdj -> [
			[Nature Adj; Syntagme GAdj];
			[Nature Adj];
	]
	| GAdv -> [
			[Nature Adv; Syntagme GAdv];
			[Nature Adv];
	]
	| S -> [
			[Syntagme GN; Syntagme GV];
		]