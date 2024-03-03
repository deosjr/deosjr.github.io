// from Prism
interface token {
    type: string;
    content: string;
    alias: string;
    length: number;
}

type label = {
    s: string;
}

type ainstruction = {
    label: string;
    n: number;
}

type cinstruction = {
    dest: string[];
    comp: string[];
    jump: string;
    keyword: string;
}

function assemble(tokens:(string|token)[]):string {
    // filter out strings, they are whitespace-only if grammar is correctly defined, and comments
    const t = tokens.filter((e) => typeof(e) === 'object' && e.type !== 'comment') as Array<token>;

    console.log(t)
    
    // piece back instructions from the grammar
    let assembly: (ainstruction|cinstruction|label)[] = [];
    let c: any = {};
    let expect = 'dest'
    t.forEach((entry) => {
        if ((expect === 'jump' && entry.type !== 'jump') || (expect === 'op' && entry.type !== 'operator')) {
            assembly.push(c as cinstruction);
            c = {};
            expect = 'dest'
        }
        switch (entry.type) {
        case 'label':
            // labels are guaranteed to start/end with parens because of the grammar regex
            assembly.push({s:entry.content.slice(1,-1)} as label);
            return
        case 'address':
            assembly.push({label:entry.content.slice(1)} as ainstruction);
            return
        case 'number':
            assembly.push({n:parseInt(entry.content.slice(1))} as ainstruction);
            return
        case 'hexnum':
            assembly.push({n:parseInt(entry.content.slice(3), 16)} as ainstruction);
            return
        case 'keyword':
            assembly.push({keyword:entry.content} as cinstruction);
            return
        case 'symbol':
            switch (expect) {
            case 'dest':
                if (c.dest === undefined) c.dest = []
                c.dest.push(entry.content)
                return
            case 'comp':
                if (c.comp === undefined) c.comp = []
                c.comp.push(entry.content)
                expect = 'op'
                return
            case 'operand':
                c.comp.push(entry.content)
                expect = 'jump'
                return
            }
        case 'operator':
            switch (entry.content) {
            case '=':
                expect = 'comp';
                return
            case ';':
                expect = 'jump';
                return
            }
            if (c.comp === undefined) c.comp = []
            c.comp.push(entry.content);
            expect = 'operand'
            return
        case 'jump':
            if (c.comp === undefined) c = {comp:c.dest};
            c.jump = entry.content
            assembly.push(c as cinstruction);
            c = {};
            expect = 'dest'
            return
        }
    })

    // standard labels
    let labels: { [key: string]: number } = {
        'SP': 1, 'FREE': 2, 'ENV': 3, 'ARG': 4, 'R0': 0, 'R1': 1, 'R2': 2, 'R3': 3, 'R4': 4, 'R5': 5, 'R6': 6, 'R7': 7, 'R8': 8, 'R9': 9, 'R10': 10, 'R11': 11, 'R12': 12, 'R13': 13, 'R14': 14, 'R15': 15,
    }
    // labels are guaranteed to start/end with parens because of the grammar regex
    let labelsFound = 0;
    assembly.forEach((entry, index) => {
        if ('s' in entry) {
            labels[entry.s] = index-labelsFound
            labelsFound++
        }
    })
    assembly = assembly.filter((e) => !('s' in e));

    const e = JSON.stringify(tokens[0]);
    return `foobar${e}`;
}

export { assemble };