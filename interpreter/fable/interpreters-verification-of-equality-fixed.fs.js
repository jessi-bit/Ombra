import { printf, toText, format, replicate } from "./fable_modules/fable-library-js.4.17.0/String.js";
import { toString } from "./fable_modules/fable-library-js.4.17.0/Types.js";
import { exp, ty } from "./interpreter-types.fs.js";
import { head, sortBy } from "./fable_modules/fable-library-js.4.17.0/Seq.js";
import { newGuid } from "./fable_modules/fable-library-js.4.17.0/Guid.js";
import { comparePrimitives } from "./fable_modules/fable-library-js.4.17.0/Util.js";
import { evalS } from "./interpreter-substitution-semantics.fs.js";
import { evalC } from "./interpreter-closures-semantics.fs.js";
import { empty } from "./fable_modules/fable-library-js.4.17.0/Map.js";
import { ofArray } from "./fable_modules/fable-library-js.4.17.0/List.js";

export function verifyEquality(valueC, valueS) {
    const verifyEqualityInner = (eC_mut, eS_mut) => {
        verifyEqualityInner:
        while (true) {
            const eC = eC_mut, eS = eS_mut;
            let matchResult, l1, l2, e1_1, e2_1, i1_1, i2_1, eC$0027, eC$0027$0027, eS$0027, eS$0027$0027, eCCond, eCElse, eCThen, eSCond, eSElse, eSThen;
            switch (eC.tag) {
                case 0: {
                    if (eS.tag === 0) {
                        matchResult = 1;
                        l1 = eC.fields[0];
                        l2 = eS.fields[0];
                    }
                    else {
                        matchResult = 5;
                    }
                    break;
                }
                case 1: {
                    if (eS.tag === 1) {
                        if (eC.fields[0][0] === eS.fields[0][0]) {
                            matchResult = 2;
                            e1_1 = eC.fields[0][2];
                            e2_1 = eS.fields[0][2];
                            i1_1 = eC.fields[0][0];
                            i2_1 = eS.fields[0][0];
                        }
                        else {
                            matchResult = 5;
                        }
                    }
                    else {
                        matchResult = 5;
                    }
                    break;
                }
                case 2: {
                    if (eS.tag === 2) {
                        matchResult = 3;
                        eC$0027 = eC.fields[0][0];
                        eC$0027$0027 = eC.fields[0][1];
                        eS$0027 = eS.fields[0][0];
                        eS$0027$0027 = eS.fields[0][1];
                    }
                    else {
                        matchResult = 5;
                    }
                    break;
                }
                case 4: {
                    if (eS.tag === 4) {
                        matchResult = 4;
                        eCCond = eC.fields[0][0];
                        eCElse = eC.fields[0][2];
                        eCThen = eC.fields[0][1];
                        eSCond = eS.fields[0][0];
                        eSElse = eS.fields[0][2];
                        eSThen = eS.fields[0][1];
                    }
                    else {
                        matchResult = 5;
                    }
                    break;
                }
                default:
                    if (eS.tag === 3) {
                        matchResult = 0;
                    }
                    else {
                        matchResult = 5;
                    }
            }
            switch (matchResult) {
                case 0:
                    return true;
                case 1:
                    return l1 === l2;
                case 2: {
                    eC_mut = e1_1;
                    eS_mut = e2_1;
                    continue verifyEqualityInner;
                }
                case 3:
                    if (verifyEqualityInner(eC$0027, eS$0027)) {
                        eC_mut = eC$0027$0027;
                        eS_mut = eS$0027$0027;
                        continue verifyEqualityInner;
                    }
                    else {
                        return false;
                    }
                case 4:
                    if (verifyEqualityInner(eCCond, eSCond) && verifyEqualityInner(eCThen, eSThen)) {
                        eC_mut = eCElse;
                        eS_mut = eSElse;
                        continue verifyEqualityInner;
                    }
                    else {
                        return false;
                    }
                default:
                    throw new Error("Match failure: Ombra.Interpreter.Types.exp");
            }
            break;
        }
    };
    let matchResult_1, bC, bS, eC_1, eS_1, idC, idS;
    if (valueC.tag === 0) {
        if (valueS.tag === 1) {
            matchResult_1 = 1;
            eC_1 = valueC.fields[0][1];
            eS_1 = valueS.fields[0][2];
            idC = valueC.fields[0][0];
            idS = valueS.fields[0][0];
        }
        else {
            matchResult_1 = 2;
        }
    }
    else if (valueS.tag === 3) {
        matchResult_1 = 0;
        bC = valueC.fields[0];
        bS = valueS.fields[0];
    }
    else {
        matchResult_1 = 2;
    }
    switch (matchResult_1) {
        case 0:
            return bC === bS;
        case 1: {
            let matchResult_2, x, y;
            if (eC_1.tag === 0) {
                if (eS_1.tag === 0) {
                    matchResult_2 = 0;
                    x = eC_1.fields[0];
                    y = eS_1.fields[0];
                }
                else {
                    matchResult_2 = 1;
                }
            }
            else {
                matchResult_2 = 1;
            }
            switch (matchResult_2) {
                case 0:
                    if (x === y) {
                        return idC === idS;
                    }
                    else {
                        return false;
                    }
                default:
                    if (idC === idS) {
                        return verifyEqualityInner(eC_1, eS_1);
                    }
                    else {
                        return false;
                    }
            }
        }
        default:
            throw new Error("Match failure: Ombra.Interpreter.Closures.valueC");
    }
}

export function toCode(ast, indentation) {
    const ind = () => replicate(indentation, "  ");
    const handleApp = (e) => {
        if (e[1].tag === 3) {
            return format("{0}({1})({2})", ind(), toCode(e[0], 0), toString(e[1].fields[0]));
        }
        else if (e[0].tag === 3) {
            return format("({0})({1})", toString(e[0].fields[0]), toCode(e[1], 0));
        }
        else {
            return format("({0})({1})", toCode(e[0], indentation), toCode(e[1], indentation));
        }
    };
    switch (ast.tag) {
        case 0:
            return format("{0}{1}", ind(), ast.fields[0]);
        case 4: {
            const tupledArg = ast.fields[0];
            const eCond = tupledArg[0];
            let rCond;
            switch (eCond.tag) {
                case 3: {
                    rCond = format("{0}", toString(eCond.fields[0]));
                    break;
                }
                case 0: {
                    rCond = format("{0}", eCond.fields[0]);
                    break;
                }
                default:
                    throw new Error("Match failure: Ombra.Interpreter.Types.exp");
            }
            const rThen = toCode(tupledArg[1], indentation + 1);
            const rElse = toCode(tupledArg[2], indentation + 1);
            return format("{0}if {1}\n{2}\n{3}else\n{4}", ind(), rCond, rThen, ind(), rElse);
        }
        case 1: {
            const tupledArg_1 = ast.fields[0];
            const ident = tupledArg_1[0];
            const e_3 = tupledArg_1[2];
            switch (e_3.tag) {
                case 3:
                    return format("fun {0} {1}", ident, toCode(e_3, indentation));
                case 4:
                    return format("fun {0} \n{1}", ident, toCode(e_3, indentation + 1));
                case 2:
                    return handleApp(e_3.fields[0]);
                default:
                    throw new Error("Match failure: Ombra.Interpreter.Types.exp");
            }
        }
        case 2:
            return handleApp(ast.fields[0]);
        default:
            return format("{0}{1}", ind(), toString(ast.fields[0]));
    }
}

export const ast0 = new exp(2, [[new exp(1, [["c", new ty(0, []), new exp(4, [[new exp(3, [false]), new exp(3, [false]), new exp(3, [true])]])]]), new exp(3, [true])]]);

export const ast1 = new exp(4, [[new exp(3, [true]), new exp(2, [[new exp(1, [["m", new ty(0, []), new exp(3, [false])]]), new exp(3, [false])]]), new exp(3, [true])]]);

export const ast2 = new exp(1, [["z", new ty(1, [new ty(0, []), new ty(1, [new ty(0, []), new ty(1, [new ty(0, []), new ty(0, [])])])]), new exp(4, [[new exp(3, [true]), new exp(3, [true]), new exp(3, [true])]])]]);

export const ast3 = new exp(1, [["v", new ty(1, [new ty(1, [new ty(0, []), new ty(1, [new ty(0, []), new ty(1, [new ty(1, [new ty(0, []), new ty(0, [])]), new ty(0, [])])])]), new ty(0, [])]), new exp(4, [[new exp(3, [true]), new exp(2, [[new exp(1, [["U", new ty(0, []), new exp(3, [true])]]), new exp(3, [true])]]), new exp(2, [[new exp(1, [["G", new ty(0, []), new exp(3, [false])]]), new exp(3, [false])]])]])]]);

export function shuffleG(xs) {
    return sortBy((_arg) => newGuid(), xs, {
        Compare: comparePrimitives,
    });
}

export function verify() {
    let arg_3;
    const ast = head(shuffleG([ast0, ast1, ast2, ast3]));
    const resS = evalS(ast);
    const resC = evalC(empty({
        Compare: comparePrimitives,
    }), ast);
    return ofArray([toText(printf("%A"))(ast), toText(printf("%A"))(resS), toText(printf("%A"))(resC), (arg_3 = verifyEquality(resC, resS), toText(printf("%A"))(arg_3)), toCode(ast, 0)]);
}

