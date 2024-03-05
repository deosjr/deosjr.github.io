import { assemble, token } from './assembler.js'

type entry = {
    k: number;
    v: number;
}

type diff = {
    pc?: number;
    a?: number;
    d?: number;
    car?: entry;
    output?: number;
}

class LispMachine {
    rom: Uint16Array;
    ramCar: Uint16Array;
    ramCdr: Uint16Array;
    // Uint16Array will do the modulo work for us
    registers: Uint16Array;     // A, D, PC
    history: diff[];
    output: number[];

    constructor() {
        this.rom = new Uint16Array(0);
        this.ramCar = new Uint16Array(0x4000);
        this.ramCdr = new Uint16Array(0x4000);
        this.registers = new Uint16Array(3);
        this.history = [];
        this.output = [];
    }

    A(): number { return this.registers[0]; }
    D(): number { return this.registers[1]; }
    PC():number { return this.registers[2]; }

    setA( x: number) { this.registers[0] = x; }
    setD( x: number) { this.registers[1] = x; }
    setPC(x: number) { this.registers[2] = x; }

    // TODO: capture entire run as diffs on memory
    run() {
        let pprev = -1;
        let prev  = -1;
        for (let i = 0; i < 100000; i++) {
            const instr = this.rom[this.PC()];
            let h: diff = {};

            const isAinstr = (instr & 0x8000) === 0;
            if (isAinstr) {
                if (this.A() !== instr) {
                    this.setA(instr);
                    h.a = instr
                }
                this.setPC(this.PC() + 1);
                h.pc = this.PC()
                this.history.push(h);
                if (pprev === this.PC()) break;
                pprev = prev
                prev = this.PC()
                continue
            }

            // assume C-instruction
            const carM = this.ramCar[this.A()];
            const cdrM = this.ramCdr[this.A()]; //todo: lisp instruction support

            // evaluate ALU
            const outM = this.alu(this.D(), mux(this.A(), carM, (instr & 0x1000) !== 0), instr)
            // 2's complement
            const isNegative = (outM & 0x8000) !== 0
            const isZero = outM === 0
            const isPositive = !(isZero || isNegative)
            
            // rest of CPU
            let jump = false
            switch (instr & 0x0007) {
            case 1: jump = isPositive; break;
            case 2: jump = isZero; break;
            case 3: jump = isZero || isPositive; break;
            case 4: jump = isNegative; break;
            case 5: jump = !isZero; break;
            case 6: jump = isZero || isNegative; break;
            case 7: jump = true; break;
            }
            jump ? this.setPC(this.A()) : this.setPC(this.PC() + 1)
            h.pc = this.PC()

            if ((instr & 0x0020) !== 0) {
                this.setA(outM)
                h.a = outM
            }
            if ((instr & 0x0010) !== 0) {
                this.setD(outM)
                h.d = outM
            }
            if ((instr & 0x0008) !== 0) {
                // 0x6002 is the tape output address
                if (this.A() === 0x6002) {
                    this.output.push(outM);
                    h.output = outM
                } else {
                    this.ramCar[this.A()] = outM
                    h.car = {k:this.A(), v:outM}
                }
            }

            this.history.push(h)
            if (pprev === this.PC()) break;
            pprev = prev
            prev = this.PC()
        }
     }

    // instr is 16 bit (0-15), we only use bits 4-9 here
    alu(x: number, y: number, instr: number): number {
        const zxo = mux(x, 0, (instr & 0x0800) !== 0)
        const nxo = mux(zxo, ~zxo, (instr & 0x0400) !== 0)
        const zyo = mux(y, 0, (instr & 0x0200) !== 0)
        const nyo = mux(zyo, ~zyo, (instr & 0x0100) !== 0)
        const fo  = mux(nxo&nyo, nxo+nyo, (instr & 0x0080) !== 0)
        return mux(fo, ~fo, (instr & 0x0040) !== 0)
    }
}

function mux(x:number, y:number, sel:boolean): number {
    return sel ? y : x;
}

function loadProgram(tokens:(string|token)[]): LispMachine {
    let lm = new LispMachine();
    lm.rom = assemble(tokens);
    return lm;
}

export { loadProgram }