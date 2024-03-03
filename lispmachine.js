function assemble(tokens) {
    // filter out strings, they are whitespace-only if grammar is correctly defined, and comments
    const t = tokens.filter((e) => typeof (e) === 'object' && e.type !== 'comment');
    console.log(t);
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
                console.log(entry.content);
                assembly.push({ n: parseInt(entry.content.slice(3), 16) });
                return;
            case 'keyword':
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
    console.log(assembly);
    // standard labels
    let labels = {
        'SP': 1, 'FREE': 2, 'ENV': 3, 'ARG': 4, 'R0': 0, 'R1': 1, 'R2': 2, 'R3': 3, 'R4': 4, 'R5': 5, 'R6': 6, 'R7': 7, 'R8': 8, 'R9': 9, 'R10': 10, 'R11': 11, 'R12': 12, 'R13': 13, 'R14': 14, 'R15': 15,
    };
    // labels are guaranteed to start/end with parens because of the grammar regex
    let labelsFound = 0;
    t.forEach((entry, index) => {
        if (entry.type === 'label') {
            labels[entry.content.slice(1, -1)] = index - labelsFound;
            labelsFound++;
        }
    });
    const e = JSON.stringify(tokens[0]);
    return `foobar${e}`;
}
export { assemble };
