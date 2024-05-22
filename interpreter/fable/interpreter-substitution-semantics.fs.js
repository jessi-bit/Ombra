import { exp } from "./interpreter-types.fs.js";

export function occursFree(x_mut, N_mut) {
    occursFree:
    while (true) {
        const x = x_mut, N = N_mut;
        const N_1 = N;
        let matchResult, y_1, e, y_2, e1, e2, cond, elseE, ifE;
        switch (N_1.tag) {
            case 0: {
                if (N_1.fields[0] === x) {
                    matchResult = 0;
                    y_1 = N_1.fields[0];
                }
                else {
                    matchResult = 4;
                }
                break;
            }
            case 1: {
                matchResult = 1;
                e = N_1.fields[0][2];
                y_2 = N_1.fields[0][0];
                break;
            }
            case 2: {
                matchResult = 2;
                e1 = N_1.fields[0][0];
                e2 = N_1.fields[0][1];
                break;
            }
            case 4: {
                matchResult = 3;
                cond = N_1.fields[0][0];
                elseE = N_1.fields[0][2];
                ifE = N_1.fields[0][1];
                break;
            }
            default:
                matchResult = 4;
        }
        switch (matchResult) {
            case 0:
                return true;
            case 1:
                if (y_2 !== x) {
                    x_mut = x;
                    N_mut = e;
                    continue occursFree;
                }
                else {
                    return false;
                }
            case 2:
                if (occursFree(x, e1)) {
                    return true;
                }
                else {
                    x_mut = x;
                    N_mut = e2;
                    continue occursFree;
                }
            case 3:
                if (occursFree(x, cond) ? true : occursFree(x, ifE)) {
                    return true;
                }
                else {
                    x_mut = x;
                    N_mut = elseE;
                    continue occursFree;
                }
            default:
                return false;
        }
        break;
    }
}

export function substitute(M, x, N) {
    const M_1 = M;
    const N_1 = N;
    switch (M_1.tag) {
        case 0:
            if (M_1.fields[0] === x) {
                return N_1;
            }
            else {
                return M_1;
            }
        case 1:
            if (M_1.fields[0][0] === x) {
                return M_1;
            }
            else {
                return new exp(1, [[M_1.fields[0][0], M_1.fields[0][1], substitute(M_1.fields[0][2], x, N_1)]]);
            }
        case 2:
            return new exp(2, [[substitute(M_1.fields[0][0], x, N_1), substitute(M_1.fields[0][1], x, N_1)]]);
        case 4:
            return new exp(4, [[substitute(M_1.fields[0][0], x, N_1), substitute(M_1.fields[0][1], x, N_1), substitute(M_1.fields[0][2], x, N_1)]]);
        default:
            return M_1;
    }
}

export function evalS(_arg_mut) {
    evalS:
    while (true) {
        const _arg = _arg_mut;
        switch (_arg.tag) {
            case 2: {
                const matchValue = evalS(_arg.fields[0][0]);
                if (matchValue.tag === 1) {
                    _arg_mut = substitute(matchValue.fields[0][2], matchValue.fields[0][0], _arg.fields[0][1]);
                    continue evalS;
                }
                else {
                    throw new Error("Match failure");
                }
            }
            case 4: {
                const matchValue_1 = evalS(_arg.fields[0][0]);
                let matchResult;
                if (matchValue_1.tag === 3) {
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
                        _arg_mut = _arg.fields[0][1];
                        continue evalS;
                    }
                    default: {
                        _arg_mut = _arg.fields[0][2];
                        continue evalS;
                    }
                }
            }
            default:
                return _arg;
        }
        break;
    }
}

