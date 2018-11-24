Members:
Sahith Nallapareddy
Matthew Piorko

We only allocate 6 slots for outgoing parameters on the stack for any function call. This means only 10 arguments total allowed.

We used runtime-le.s and sysspim.s given to us by Olin for all our built-ins. Our arrays and strings have the length in the first word. Therefore, an extra
4 bytes were added to array indexing. This also means big endian systems won't necessarily work. 

We wrote compile.sh to compile a tiger program. It is used as "./compile.sh fact.tig" and the output assembly will be in fact.tig.s. 
