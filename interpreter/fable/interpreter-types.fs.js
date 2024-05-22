import { Union } from "./fable_modules/fable-library-js.4.17.0/Types.js";
import { bool_type, tuple_type, string_type, union_type } from "./fable_modules/fable-library-js.4.17.0/Reflection.js";

export class ty extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["BOOL", "FUN"];
    }
}

export function ty_$reflection() {
    return union_type("Ombra.Interpreter.Types.ty", [], ty, () => [[], [["Item1", ty_$reflection()], ["Item2", ty_$reflection()]]]);
}

export class exp extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Lit", "Lam", "App", "Bool", "If"];
    }
}

export function exp_$reflection() {
    return union_type("Ombra.Interpreter.Types.exp", [], exp, () => [[["Item", string_type]], [["Item", tuple_type(string_type, ty_$reflection(), exp_$reflection())]], [["Item", tuple_type(exp_$reflection(), exp_$reflection())]], [["Item", bool_type]], [["Item", tuple_type(exp_$reflection(), exp_$reflection(), exp_$reflection())]]]);
}

