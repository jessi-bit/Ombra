import { Union } from "./fable_modules/fable-library-js.4.17.0/Types.js";
import { union_type, bool_type, tuple_type, class_type, string_type } from "./fable_modules/fable-library-js.4.17.0/Reflection.js";
import { exp_$reflection } from "./interpreter-types.fs.js";
import { find, add } from "./fable_modules/fable-library-js.4.17.0/Map.js";

export class valueC extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Clos", "Boo"];
    }
}

export function valueC_$reflection() {
    return union_type("Ombra.Interpreter.Closures.valueC", [], valueC, () => [[["Item", tuple_type(string_type, exp_$reflection(), class_type("Microsoft.FSharp.Collections.FSharpMap`2", [string_type, exp_$reflection()]))]], [["Item", bool_type]]]);
}

export function evalC(env_mut, e_mut) {
    evalC:
    while (true) {
        const env = env_mut, e = e_mut;
        switch (e.tag) {
            case 3:
                return new valueC(1, [e.fields[0]]);
            case 1:
                return new valueC(0, [[e.fields[0][0], e.fields[0][2], env]]);
            case 2: {
                const matchValue = evalC(env, e.fields[0][0]);
                if (matchValue.tag === 0) {
                    env_mut = add(matchValue.fields[0][0], e.fields[0][1], matchValue.fields[0][2]);
                    e_mut = matchValue.fields[0][1];
                    continue evalC;
                }
                else {
                    throw new Error("Match failure");
                }
            }
            case 4: {
                const matchValue_1 = evalC(env, e.fields[0][0]);
                let matchResult;
                if (matchValue_1.tag === 1) {
                    if (matchValue_1.fields[0]) {
                        matchResult = 0;
                    }
                    else {
                        matchResult = 1;
                    }
                }
                else {
                    matchResult = 1;
                }
                switch (matchResult) {
                    case 0: {
                        env_mut = env;
                        e_mut = e.fields[0][1];
                        continue evalC;
                    }
                    default: {
                        env_mut = env;
                        e_mut = e.fields[0][2];
                        continue evalC;
                    }
                }
            }
            default: {
                env_mut = env;
                e_mut = find(e.fields[0], env);
                continue evalC;
            }
        }
        break;
    }
}

