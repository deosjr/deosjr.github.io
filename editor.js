import CodeFlask from "https://cdn.jsdelivr.net/npm/codeflask@1.4.1/+esm";
import Prism from "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/+esm";
import { assemble } from './lispmachine.js';

const editor = document.getElementById('editor');
const output = document.getElementById("output");

const prismGrammarASM = {
  comment: {
    pattern: /\/\/.*/,
    greedy: true,
  },
  label: {
    pattern: /(\([A-Z][A-Z]+)\)/,
    alias: 'string'
  },
  jump: {
    pattern: /\b(JEQ|JNE|JMP|JLE|JGE|JLT|JGT)\b/,
    alias: 'keyword',
  },
  keyword:
    /\b(EQLM|MCDR|EMPTYCDR)\b/,
  hexnum: {
    pattern: /(\@0x[0-9]+)/,
    greedy: true,
    alias: 'number',
  },
  number: {
    pattern: /(\@[0-9]+)/,
    greedy: true,
  },
  address: {
    pattern: /(\@[A-Z][A-Z0-9]+)/,
    greedy: true,
    alias: 'string'
  },
  symbol: /([ADM01])(?=[ADM;=+\-&|\s])/,
  operator: /(;|=|\+|-|&|\|)/,
};

const flask = new CodeFlask(editor, {
    language: 'lispassembly',
    lineNumbers: true,
    defaultTheme: false
});
flask.addLanguage("lispassembly", prismGrammarASM);

flask.onUpdate( e => {
    const tokens = Prism.tokenize(e, prismGrammarASM);
    while (output.firstChild) {
        output.removeChild(output.firstChild);
    }
    output.insertAdjacentHTML('beforeend', assemble(tokens))
})

// disable Prism autohighlighting on load
window.Prism = window.Prism || {};
Prism.manual = true;

flask.updateCode(`(ASSQ)
    // assume @R11 = K (key) and @R12 = P (pointer to assoc list)
    @R12
    A=M
    A=M
    D=M
    @R11
    EQLM
    @ASSQCONTINUE
    D;JEQ
    // here K == D !
    @R12
    A=M
    A=M
    MCDR
    @0x6002     // I've used this address to write to tape output
    M=D
    @END
    0;JMP
(ASSQCONTINUE)
    @R12
    A=M
    EMPTYCDR
    @FAILTOFIND
    D;JNE
    @R12
    A=M
    MCDR
    @R12
    M=D
    @ASSQ
    0;JMP
(FAILTOFIND)
    @END
    0;JMP`)

