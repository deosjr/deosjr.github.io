function assemble(tokens) {
    const { labels, statements } = parse(tokens);
    const program = statements.map((s) => {
        switch (true) {
            // @LABEL
            case 'label' in s:
                return labels[s.label];
            // @NUMBER
            case 'n' in s:
                return s.n;
            // LISP INSTRUCTION
            case 'keyword' in s:
                return keyword(s.keyword);
            // C-INSTRUCTION
            default:
                return cinstr(s);
        }
    });
    //console.log(program.map((x) => x.toString(16).padStart(4, '0')))
    //return JSON.stringify(statements).replace(/},/g, "},</br>");
    return Uint16Array.from(program);
}
function keyword(k) {
    switch (k) {
        case "SETCAR": return 0b1111001100001000;
        case "SETCDR": return 0b1010111111000000;
        case "EQLM": return 0b1000010111010000;
        case "MCAR": return 0b1111110000010000;
        case "MCDR": return 0b1000011111010000;
        case "ACAR": return 0b1111110000100000;
        case "ACDR": return 0b1000011111100000;
        case "ISPROC": return 0b1001000000010000;
        case "ISSYMB": return 0b1000001011010000;
        case "ISPRIM": return 0b1000001010010000;
        case "ISBUILTIN": return 0b1000001101010000;
        case "ISSPECIAL": return 0b1000001111010000;
        case "ISUSRDEF": return 0b1000001100010000;
        case "ISEMPTY": return 0b1000110000010000;
        case "EMPTYCDR": return 0b1000100000010000;
        case "USRDEFCDR": return 0b1000000100010000;
    }
    return -1;
}
function cinstr(c) {
    let instr = 0b1110000000000000;
    switch (c.jump) {
        case 'JGT':
            instr |= 0b001;
            break;
        case 'JEQ':
            instr |= 0b010;
            break;
        case 'JGE':
            instr |= 0b011;
            break;
        case 'JLT':
            instr |= 0b100;
            break;
        case 'JNE':
            instr |= 0b101;
            break;
        case 'JLE':
            instr |= 0b110;
            break;
        case 'JMP':
            instr |= 0b111;
            break;
    }
    if (c.dest && c.dest.includes('A'))
        instr |= 0b100000;
    if (c.dest && c.dest.includes('D'))
        instr |= 0b010000;
    if (c.dest && c.dest.includes('M'))
        instr |= 0b001000;
    switch (c.comp.join('')) {
        case '0':
            instr |= 0b0000101010000000;
            break;
        case '1':
            instr |= 0b0000111111000000;
            break;
        case 'A':
            instr |= 0b0000110000000000;
            break;
        case 'D':
            instr |= 0b0000001100000000;
            break;
        case 'M':
            instr |= 0b0001110000000000;
            break;
        case '-1':
            instr |= 0b0000111010000000;
            break;
        case '-A':
            instr |= 0b0000110011000000;
            break;
        case '-D':
            instr |= 0b0000001111000000;
            break;
        case '-M':
            instr |= 0b0001110011000000;
            break;
        case '!A':
            instr |= 0b0000110001000000;
            break;
        case '!D':
            instr |= 0b0000001101000000;
            break;
        case '!M':
            instr |= 0b0001110001000000;
            break;
        case 'A+1':
            instr |= 0b0000110111000000;
            break;
        case 'M+1':
            instr |= 0b0001110111000000;
            break;
        case 'D+1':
            instr |= 0b0000011111000000;
            break;
        case 'D+A':
            instr |= 0b0000000010000000;
            break;
        case 'D+M':
            instr |= 0b0001000010000000;
            break;
        case 'A-1':
            instr |= 0b0000110010000000;
            break;
        case 'A-D':
            instr |= 0b0000000111000000;
            break;
        case 'M-1':
            instr |= 0b0001110010000000;
            break;
        case 'M-D':
            instr |= 0b0001000111000000;
            break;
        case 'D-1':
            instr |= 0b0000001110000000;
            break;
        case 'D-A':
            instr |= 0b0000010011000000;
            break;
        case 'D-M':
            instr |= 0b0001010011000000;
            break;
        //case 'D&A': instr |= 0b0000000000000000
        case 'D&M':
            instr |= 0b0001000000000000;
            break;
        case 'D|A':
            instr |= 0b0000010101000000;
            break;
        case 'D|M':
            instr |= 0b0001010101000000;
            break;
    }
    return instr;
}
function parse(tokens) {
    // filter out strings, they are whitespace-only if grammar is correctly defined, and comments
    const t = tokens.filter((e) => typeof (e) === 'object' && e.type !== 'comment');
    // piece back instructions from the grammar
    let assembly = [];
    let c = {};
    let expect = 'dest';
    t.forEach((entry) => {
        if ((expect === 'jump' && entry.type !== 'jump') || (expect === 'op' && entry.type !== 'operator')) {
            assembly.push(c);
            c = {};
            expect = 'dest';
        }
        switch (entry.type) {
            case 'label':
                // labels are guaranteed to start/end with parens because of the grammar regex
                assembly.push({ s: entry.content.slice(1, -1) });
                return;
            case 'address':
                assembly.push({ label: entry.content.slice(1) });
                return;
            case 'number':
                assembly.push({ n: parseInt(entry.content.slice(1)) });
                return;
            case 'hexnum':
                assembly.push({ n: parseInt(entry.content.slice(3), 16) });
                return;
            case 'builtin':
                assembly.push({ keyword: entry.content });
                return;
            case 'symbol':
                switch (expect) {
                    case 'dest':
                        if (c.dest === undefined)
                            c.dest = [];
                        c.dest.push(entry.content);
                        return;
                    case 'comp':
                        if (c.comp === undefined)
                            c.comp = [];
                        c.comp.push(entry.content);
                        expect = 'op';
                        return;
                    case 'operand':
                        c.comp.push(entry.content);
                        expect = 'jump';
                        return;
                }
            case 'operator':
                switch (entry.content) {
                    case '=':
                        expect = 'comp';
                        return;
                    case ';':
                        expect = 'jump';
                        return;
                }
                if (c.comp === undefined)
                    c.comp = [];
                c.comp.push(entry.content);
                expect = 'operand';
                return;
            case 'jump':
                if (c.comp === undefined)
                    c = { comp: c.dest };
                c.jump = entry.content;
                assembly.push(c);
                c = {};
                expect = 'dest';
                return;
        }
    });
    if (Object.keys(c).length > 0) {
        assembly.push(c);
        c = {};
        expect = 'dest';
    }
    // standard labels
    let labels = {
        'SP': 1, 'FREE': 2, 'ENV': 3, 'ARG': 4, 'R0': 0, 'R1': 1, 'R2': 2, 'R3': 3, 'R4': 4, 'R5': 5, 'R6': 6, 'R7': 7, 'R8': 8, 'R9': 9, 'R10': 10, 'R11': 11, 'R12': 12, 'R13': 13, 'R14': 14, 'R15': 15,
    };
    // labels are guaranteed to start/end with parens because of the grammar regex
    let labelsFound = 0;
    assembly.forEach((entry, index) => {
        if ('s' in entry) {
            labels[entry.s] = index - labelsFound;
            labelsFound++;
        }
    });
    return { labels: labels, statements: assembly.filter((e) => !('s' in e)) };
}
export { assemble };
