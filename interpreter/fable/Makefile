compile:
	dotnet fable
	sed -i 's|import { evalS } from "./interpreter-substitution-semantics.fsx";|import { evalS } from "./interpreter-substitution-semantics.fs.js";|' interpreters-verification-of-equality-fixed.fs.js
	sed -i 's|import { evalC } from "./interpreter-closures-semantics.fsx";|import { evalC } from "./interpreter-closures-semantics.fs.js";|' interpreters-verification-of-equality-fixed.fs.js
	npm run build

run:
	node ombra.js

dev: 
	while inotifywait -e close_write interpreters-verification-of-equality-fixed.fsx; do clear && make compile run; done 
