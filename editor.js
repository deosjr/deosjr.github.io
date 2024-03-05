import CodeFlask from "https://cdn.jsdelivr.net/npm/codeflask@1.4.1/+esm";
import Prism from "https://cdn.jsdelivr.net/npm/prismjs@1.29.0/+esm";
import { loadProgram } from './lispmachine.js';

const editor = document.getElementById('editor');
const output = document.getElementById("output");

const registerA = document.getElementById("a");
const registerD = document.getElementById("d");
const programCounter = document.getElementById("pc");
const cycleCounter = document.getElementById("cycles");
const slider = document.getElementById("stateslider");

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
  builtin: {
    pattern: /\b(EQLM|MCDR|EMPTYCDR)\b/,
    alias: 'keyword',
  },
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
  symbol: /([ADM01])(?=[ADM;=+\-&|\s]|$)/,
  operator: /(;|=|\+|-|&|\|)/,
};

const flask = new CodeFlask(editor, {
    language: 'lispassembly',
    lineNumbers: true,
    defaultTheme: false
});
flask.addLanguage("lispassembly", prismGrammarASM);

let cycles = 0;
let lm = undefined;

flask.onUpdate( e => {
    const tokens = Prism.tokenize(e, prismGrammarASM);
    while (output.firstChild) {
        output.removeChild(output.firstChild);
    }
    lm = loadProgram(tokens)
    lm.run();
    output.insertAdjacentHTML('beforeend', lm.output)
    cycles = lm.history.length
    slider.value = cycles
    slider.max = cycles
    renderButtons()
})

// disable Prism autohighlighting on load
window.Prism = window.Prism || {};
Prism.manual = true;

// UI
function back() {
  if (cycles === 0) return
  cycles -= 1
  slider.value = cycles;
  const diff = lm.history[cycles]
  if ('a' in diff) lm.setA(diff.a.from)
  if ('d' in diff) lm.setD(diff.d.from)
  if ('pc' in diff) lm.setPC(diff.pc.from)
  renderButtons()
}

function forward() {
  if (cycles === lm.history.length) return
  cycles += 1
  slider.value = cycles;
  const diff = lm.history[cycles-1]
  if ('a' in diff) lm.setA(diff.a.to)
  if ('d' in diff) lm.setD(diff.d.to)
  if ('pc' in diff) lm.setPC(diff.pc.to)
  renderButtons()
}

function renderButtons() {
  registerA.textContent = `A: 0x${lm.A().toString(16).padStart(4, '0')}`
  registerD.textContent = `D: 0x${lm.D().toString(16).padStart(4, '0')}`
  programCounter.textContent = `PC: 0x${lm.PC().toString(16).padStart(4, '0')}`
  cycleCounter.textContent = `CYCLES: ${cycles}`
}

var att = document.createAttribute("max")
att.value = 0
slider.setAttributeNode(att);
slider.oninput = function() {
  cycles = parseInt(this.value);
  // todo: look up state in history
  renderButtons()
}

const backbutton = document.getElementById("backbutton")
backbutton.addEventListener("click", back);
const forwardbutton = document.getElementById("forwardbutton")
forwardbutton.addEventListener("click", forward);


// initial code
flask.updateCode(`// change me!
    @42
    D=A
    @0x6002
    M=D
    DM=D-1
    M=D-1
(END)
    @END
    0;JMP`)

/*
// 000c fc20 fc20 fc10 000b 85d0 0010 e302 000c fc20 fc20 87d0 6002 e308 001e ea87 000c fc20 8810 001c e305 000c fc20 87d0 000c e308 0000 ea87 001e ea87 001e ea87
flask.updateCode(`(ASSQ)
    // assume @R11 = K (key)
    // and @R12 = P (pointer to assoc list)
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
    @0x6002     // write to tape output
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
    0;JMP
(END)
    @END
    0;JMP`)
*/