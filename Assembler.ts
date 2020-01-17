class Assembler {
    // location that will be used for global references of the next instruction,
    // each time a word is added to instructions, currentOrg is +=4 incremented:
    public currentOrg: number;

    // dictionary for currently defined labels:
    private definedLabels: {[labelName: string]: number} = {}

    // list of instructions (data words are also written here):
    private readonly instructions: number[] = [];

    // tasks used for updating forward references and stuff like that:
    private readonly secondStageTasks: (()=>void)[] = [];

    public flags: { [params: string]: boolean } = {
        errorOnAtUsage: true,
        inDelaySlot: false,
    }

    public static readonly REGISTERS = {
        '$0': 0, '$zero': 0,
        '$1': 1, '$at': 1, // for pseudo instructions.
        '$2': 2, '$v0': 2, // return values v0-v1...
        '$3': 3, '$v1': 3,
        '$4': 4, '$a0': 4, // arguments to functions a0-a7, not preserved by subprograms...
        '$5': 5, '$a1': 5,
        '$6': 6, '$a2': 6,
        '$7': 7, '$a3': 7,
        '$8': 8, '$t0': 8, // temporary registers t0-t8, not preserved by subprograms...
        '$9': 9, '$t1': 9,
        '$10': 10, '$t2': 10,
        '$11': 11, '$t3': 11,
        '$12': 12, '$t4': 12,
        '$13': 13, '$t5': 13,
        '$14': 14, '$t6': 14,
        '$15': 15, '$t7': 15,
        '$16': 16, '$s0': 16, // saved registers s0-s7, preserved by subprograms...
        '$17': 17, '$s1': 17,
        '$18': 18, '$s2': 18,
        '$19': 19, '$s3': 19,
        '$20': 20, '$s4': 20,
        '$21': 21, '$s5': 21,
        '$22': 22, '$s6': 22,
        '$23': 23, '$s7': 23,
        '$24': 24, '$t8': 24, // temporary registers t8-t9, not preserved by subprograms...
        '$25': 25, '$t9': 25,
        '$26': 26, '$k0': 26, // reserved for kernel usage...
        '$27': 27, '$k1': 27,
        '$28': 28, '$gp': 28, // global pointer.
        '$29': 29, '$sp': 29, // stack pointer.
        '$30': 30, '$fp': 30, // frame pointer.
        '$31': 31, '$ra': 31, // return address.
    }

    public static readonly GPR: Array<keyof typeof Assembler.REGISTERS> = [
        '$0',  '$1',  '$2',  '$3',  '$4',  '$5',  '$6',  '$7',  '$8',  '$9',
        '$10', '$11', '$12', '$13', '$14', '$15', '$16', '$17', '$18', '$19',
        '$20', '$21', '$22', '$23', '$24', '$25', '$26', '$27', '$28', '$29',
        '$30', '$31'];

    public constructor({org}: {org: number}) {
        this.currentOrg = org;
    }

    /**
     * Writes program into memory, first instruction will be written into memory[offset], second into memory[offset+1],
     * and so on.
     */
    public assemble(memory?: Uint32Array, offset?: number): Uint32Array {
        for (const task of this.secondStageTasks) {
            task();
        }
        if (memory === undefined) {
            return new Uint32Array(this.instructions);
        }
        memory.set(this.instructions, offset);
        return memory;
    }

    public label(name: string): this {
        if (this.definedLabels[name] !== undefined) {
            throw new Error(`label ${name} already defined`);
        }
        this.definedLabels[name] = this.currentOrg;
        return this;
    }

    /**
     * Opens a new lexical scope block for labels.
     */
    public begin(): this {
        this.definedLabels = Object.create(this.definedLabels);
        return this;
    }

    /**
     * Closes a the innermost lexical scope block. The number of `.begin()` calls should match `.end()`.
     */
    public end(): this {
        const proto = Object.getPrototypeOf(this.definedLabels);
        if (proto === Object.prototype) {
            throw new Error('unexpected end, make sure there is a corresponding begin');
        }
        this.definedLabels = proto;
        return this;
    }

    // supports number or binary strings (each char is in '\x00'..'\xff' range)
    // each string part will be word padded with null bytes.
    public data(...parts: Array<Assembler.Numeric | string>): this {
        for (const part of parts) {
            if (typeof part === 'string') {
                this.pushBinaryString(part);
            } else {
                this.pushData({word: part});
            }
        }
        return this;
    }

    /**
     * GPR[dest] ← GPR[src1] + GPR[src2], trapping on overflow.
     */
    public ADD({src1, src2, dest}: { dest: Assembler.LRegister, src1: Assembler.LRegister, src2: Assembler.LRegister }): this {
        this.flags.inDelaySlot = false;
        return this.pushRInstruction({rs: src1, rt: src2, rd: dest, opcode: 0b100000});
    }

    /**
     * GPR[dest] ← GPR[src1] + sign_extend(imm), trapping on overflow.
     */
    public ADDI({src, dest, imm16}: { dest: Assembler.LRegister, src: Assembler.LRegister, imm16: Assembler.Numeric }): this {
        this.flags.inDelaySlot = false;
        return this.pushIInstruction({opcode: 0b001000, rs: src, rt: dest, imm16});
    }

    /**
     * GPR[dest] ← GPR[src1] + sign_extend(imm), without trap.
     */
    public ADDIU({src, dest, imm16}: { dest: Assembler.LRegister, src: Assembler.LRegister, imm16: Assembler.Numeric }): this {
        this.flags.inDelaySlot = false;
        return this.pushIInstruction({opcode: 0b001001, rs: src, rt: dest, imm16});
    }

    /**
     * GPR[dest] ← GPR[src1] + GPR[src2].
     */
    public ADDU({src1, src2, dest}: { dest: Assembler.LRegister, src1: Assembler.LRegister, src2: Assembler.LRegister }): this {
        this.flags.inDelaySlot = false;
        return this.pushRInstruction({rs: src1, rt: src2, rd: dest, opcode: 0b100001});
    }

    /**
     * GPR[dest] ← GPR[src1] and GPR[src2].
     */
    public AND({src1, src2, dest}: { dest: Assembler.LRegister, src1: Assembler.LRegister, src2: Assembler.LRegister }): this {
        this.flags.inDelaySlot = false;
        return this.pushRInstruction({rs: src1, rt: src2, rd: dest, opcode: 0b100100});
    }

    /**
     * GPR[dest] ← GPR[src] and zero_extend(imm).
     */
    public ANDI({src, dest, imm16}: { dest: Assembler.LRegister, src: Assembler.LRegister, imm16: Assembler.Numeric }): this {
        this.flags.inDelaySlot = false;
        return this.pushIInstruction({opcode: 0b001100, rs: src, rt: dest, imm16});
    }

    /**
     * Pseudo instruction: branch unconditionally.
     * Implemented as: BEQ $zero, $zero, ref
     */
    public B(ref: Assembler.LLabelRef): this {
        return this.forbiddenInDelaySlot('B')
            .BEQ({src1: Assembler.GPR[0], src2: Assembler.GPR[0], ref});
    }

    /**
     * Pseudo instruction: branch and link unconditionally.
     * Implemented as: BAL $zero, ref
     */
    public BAL({ref}: { ref: Assembler.LLabelRef }): this {
        return this.forbiddenInDelaySlot('BAL')
            .pushIInstruction({opcode: 0b000001, rs: Assembler.GPR[0], rt: Assembler.GPR[0b10001], imm16: this.labelRefMap(ref)})
            .setInDelaySlot();
    }

    /**
     * If GPR[src1] == GPR[src2] then relative branch.
     */
    public BEQ({src1, src2, ref}: { src1: Assembler.LRegister, src2: Assembler.LRegister, ref: Assembler.LLabelRef }): this {
        return this.forbiddenInDelaySlot('BEQ')
            .pushIInstruction({opcode: 0b000100, rs: src1, rt: src2, imm16: this.labelRefMap(ref)})
            .setInDelaySlot();
    }

    /**
     * If GPR[src1] == GPR[src2] then relative branch, likely.
     */
    public BEQL({src1, src2, ref}: { src1: Assembler.LRegister, src2: Assembler.LRegister, ref: Assembler.LLabelRef }): this {
        return this.forbiddenInDelaySlot('BEQL')
            .pushIInstruction({opcode: 0b010100, rs: src1, rt: src2, imm16: this.labelRefMap(ref)})
            .setInDelaySlot();
    }

    /**
     * If GPR[src] >= 0 then relative branch.
     */
    public BGEZ({src, ref}: { src: Assembler.LRegister, ref: Assembler.LLabelRef }): this {
        return this.forbiddenInDelaySlot('BGEZ')
            .pushIInstruction({opcode: 0b000001, rs: src, rt: Assembler.GPR[0b00001], imm16: this.labelRefMap(ref)})
            .setInDelaySlot();
    }

    /**
     * If GPR[src] >= 0 then relative branch and link.
     */
    public BGEZAL({src, ref}: { src: Assembler.LRegister, ref: Assembler.LLabelRef }): this {
        return this.forbiddenInDelaySlot('BGEZAL')
            .pushIInstruction({opcode: 0b000001, rs: src, rt: Assembler.GPR[0b10001], imm16: this.labelRefMap(ref)})
            .setInDelaySlot();
    }

    /**
     * If GPR[src] >= 0 then relative branch and link, likely.
     */
    public BGEZALL({src, ref}: { src: Assembler.LRegister, ref: Assembler.LLabelRef }): this {
        return this.forbiddenInDelaySlot('BGEZALL')
            .pushIInstruction({opcode: 0b000001, rs: src, rt: Assembler.GPR[0b10011], imm16: this.labelRefMap(ref)})
            .setInDelaySlot();
    }

    /**
     * If GPR[src] >= 0 then relative branch, likely.
     */
    public BGEZL({src, ref}: { src: Assembler.LRegister, ref: Assembler.LLabelRef }): this {
        return this.forbiddenInDelaySlot('BGEZL')
            .pushIInstruction({opcode: 0b000001, rs: src, rt: Assembler.GPR[0b00011], imm16: this.labelRefMap(ref)})
            .setInDelaySlot();
    }

    /**
     * If GPR[src] > 0 then relative branch.
     */
    public BGTZ({src, ref}: { src: Assembler.LRegister, ref: Assembler.LLabelRef }): this {
        return this.forbiddenInDelaySlot('BGTZ')
            .pushIInstruction({opcode: 0b000111, rs: src, rt: Assembler.GPR[0], imm16: this.labelRefMap(ref)})
            .setInDelaySlot();
    }

    /**
     * If GPR[src] > 0 then relative branch, likely.
     */
    public BGTZL({src, ref}: { src: Assembler.LRegister, ref: Assembler.LLabelRef }): this {
        return this.forbiddenInDelaySlot('BGTZL')
            .pushIInstruction({opcode: 0b010111, rs: src, rt: Assembler.GPR[0], imm16: this.labelRefMap(ref)})
            .setInDelaySlot();
    }

    // TODO: implement BLEZ instruction.
    // TODO: implement BLEZL instruction.
    // TODO: implement BLTZ instruction.
    // TODO: implement BLTZAL instruction.
    // TODO: implement BLTZALL instruction.
    // TODO: implement BLTZL instruction.
    // TODO: implement BNE instruction.
    // TODO: implement BNEL instruction.

    /**
     * Cause a Breakpoint exception.
     */
    public BREAK({code}: { code: Assembler.Numeric } = {code: 0}): this {
        return this.pushExceptionInstruction({code, opcode: 0b001101});
    }

    // TODO: implement CACHE instruction.
    // TODO: implement CFC1 instruction.
    // TODO: implement CFC2 instruction.

    /**
     * Pseudo instruction: GPR[dest] ← 0.
     * Implemented as: OR $dest, $0, $0.
     */
    public CLEAR({dest}: { dest: Assembler.LRegister }): this {
        return this.OR({dest, src1: '$zero', src2: '$zero'});
    }

    // TODO: implement CLO instruction.
    // TODO: implement CLZ instruction.
    // TODO: implement COP2 instruction.
    // TODO: implement CTC1 instruction.
    // TODO: implement CTC2 instruction.
    // TODO: implement DERET instruction.
    // TODO: implement DIV instruction.
    // TODO: implement DIVU instruction.
    // TODO: implement ERET instruction.
    // TODO: implement J instruction.
    // TODO: implement JAL instruction.
    // TODO: implement JALR instruction.
    // TODO: implement JR instruction.

    /**
     * Pseudo instruction: GPR[dest] ← imm32, same as LI.
     * Implemented using LUI and/or ORI.
     */
    public LA({dest, ref}: { dest: Assembler.Register, ref: Assembler.LLabelRef}): this {
        return this.LI({dest, imm32: this.labelRefMap(ref, {relative: false})});
    }

    // TODO: implement LB instruction.
    // TODO: implement LBU instruction.
    // TODO: implement LDC1 instruction.
    // TODO: implement LDC2 instruction.
    // TODO: implement LH instruction.
    // TODO: implement LHU instruction.

    /**
     * Pseudo instruction: GPR[dest] ← imm32.
     * Implemented using LUI and/or ORI.
     */
    public LI({dest, imm32}: { dest: Assembler.Register, imm32: Assembler.Numeric }): this {
        if (typeof imm32 === 'number') {
            const words = Assembler.liEncode(dest, this.currentOrg, imm32);
            this.instructions.push(... words);
            this.currentOrg += words.length * 4;
        } else {
            // data-flow analisys is ignored for usages in closures, immfunc allows us to maintain inferred type.
            const immfunc = imm32;
            const originalOrg = this.currentOrg;
            const index = this.instructions.length;
            this.instructions.push(0, 0); // NOP; NOP
            this.currentOrg += 8;
            this.secondStageTasks.push(() => {
                this.instructions.splice(index, 2, ... Assembler.liEncode(dest, originalOrg, immfunc(this.definedLabels), true));
            });
        }
        return this.setInDelaySlot(false);
    }

    // TODO: implement LL instruction.
    // TODO: implement LUI instruction.
    // TODO: implement LW instruction.
    // TODO: implement LWC1 instruction.
    // TODO: implement LWC2 instruction.
    // TODO: implement LWL instruction.
    // TODO: implement LWR instruction.
    // TODO: implement MADD instruction.
    // TODO: implement MADDU instruction.
    // TODO: implement MFC0 instruction.
    // TODO: implement MFC1 instruction.
    // TODO: implement MFC2 instruction.
    // TODO: implement MFHI instruction.
    // TODO: implement MFLO instruction.
    // TODO: implement MOVE pseudoinstruction.
    // TODO: implement MOVN instruction.
    // TODO: implement MOVZ instruction.
    // TODO: implement MSUB instruction.
    // TODO: implement MSUBU instruction.
    // TODO: implement MTC0 instruction.
    // TODO: implement MTC1 instruction.
    // TODO: implement MTC2 instruction.
    // TODO: implement MTHI instruction.
    // TODO: implement MTLO instruction.
    // TODO: implement MUL instruction.
    // TODO: implement MULT instruction.
    // TODO: implement MULTU instruction.
    // TODO: implement NOP instruction.
    // TODO: implement NOR instruction.

    /**
     * GPR[dest] ← GPR[src1] or GPR[src2].
     */
    public OR({src1, src2, dest}: { dest: Assembler.LRegister, src1: Assembler.LRegister, src2: Assembler.LRegister }): this {
        return this.pushRInstruction({rs: src1, rt: src2, rd: dest, opcode: 0b100101});
    }

    // TODO: implement ORI instruction.
    // TODO: implement PREF instruction.
    // TODO: implement SB instruction.
    // TODO: implement SC instruction.
    // TODO: implement SDBBP instruction.
    // TODO: implement SDC1 instruction.
    // TODO: implement SDC2 instruction.
    // TODO: implement SH instruction.
    // TODO: implement SLL instruction.
    // TODO: implement SLLV instruction.
    // TODO: implement SLT instruction.
    // TODO: implement SLTI instruction.
    // TODO: implement SLTIU instruction.
    // TODO: implement SLTU instruction.
    // TODO: implement SRA instruction.
    // TODO: implement SRAV instruction.
    // TODO: implement SRL instruction.
    // TODO: implement SRLV instruction.
    // TODO: implement SSNOP instruction.
    // TODO: implement SUB instruction.
    // TODO: implement SUBU instruction.
    // TODO: implement SW instruction.
    // TODO: implement SWC1 instruction.
    // TODO: implement SWC2 instruction.
    // TODO: implement SWL instruction.
    // TODO: implement SWR instruction.
    // TODO: implement SYNC instruction.
    // TODO: implement SYSCALL instruction.
    // TODO: implement TEQ instruction.
    // TODO: implement TEQI instruction.
    // TODO: implement TGE instruction.
    // TODO: implement TGEI instruction.
    // TODO: implement TGEIU instruction.
    // TODO: implement TGEU instruction.
    // TODO: implement TLBP instruction.
    // TODO: implement TLBR instruction.
    // TODO: implement TLBWI instruction.
    // TODO: implement TLBWR instruction.
    // TODO: implement TLT instruction.
    // TODO: implement TLTI instruction.
    // TODO: implement TLTIU instruction.
    // TODO: implement TLTU instruction.
    // TODO: implement TNE instruction.
    // TODO: implement TNEI instruction.
    // TODO: implement WAIT instruction.
    // TODO: implement XOR instruction.
    // TODO: implement XORI instruction.

    /**
     * Monadic bind for OptLazy, equivalent to: obj >>= fn.
     */
    private static bindOptLazy<T, U>(fn: (x:T) => Assembler.OptLazy<U>, obj: Assembler.OptLazy<T>): Assembler.OptLazy<U> {
        type LC<U> = Assembler.LazyCallback<Exclude<U, Function>>;
        if (typeof obj !== "function") {
            return fn(obj);
        }
        return (labels): Exclude<U, Function> => {
            // we need to do these dumb casts to LazyCallback because typescript cannot infer never from a Function
            // that doesn't extend Function; thus it never removes the non function alternative from OptLazys obj/res.
            const res = fn((obj as LC<T>)(labels));
            return typeof res === "function" ? (res as LC<U>)(labels) : res;
        };
    }

    /**
     * Converts a LLabelRef into a Numeric relativazing its location from the next instruction
     */
    private labelRefMap(
        lref: Assembler.LLabelRef,
        {relative}: { relative: 'words-from-next' | 'bytes-from-next' | 'bytes-from-current' | false }={relative: 'words-from-next'}
    ): Assembler.Numeric {
        let rel = 0, shift = 0;
        switch (relative) {
            case 'bytes-from-current': rel = this.currentOrg;     shift = 0; break;
            case 'bytes-from-next':    rel = this.currentOrg + 4; shift = 0; break;
            case 'words-from-next':    rel = this.currentOrg + 4; shift = 2; break;
        }
        return Assembler.bindOptLazy((ref: Assembler.LabelRef): Assembler.Numeric => {
            const fn = (labels: {[labelName: string]: number}) => ( labels[ref.label] + ((ref.offset || 0) | 0) - rel ) >> shift;
            return this.definedLabels[ref.label] ? fn(this.definedLabels) : fn;
        }, lref);
    }

    private forbiddenInDelaySlot(instructionName: string): this {
        if (this.flags.inDelaySlot) {
            throw new Error(`${instructionName} instruction not allowed in delay slot, at ${this.currentOrg}`);
        }
        return this;
    }

    private setInDelaySlot(value: boolean = true): this {
        this.flags.inDelaySlot = value;
        return this;
    }

    private static liEncode(dest: Assembler.Register, location: number, n: number, forceTwoWords: boolean = false): number[] {
        if (!(dest in Assembler.REGISTERS)) {
            throw new Error(`Invalid destination register ${dest} at ${location}`);
        }
        if (!Number.isInteger(n) || n < -0x8000_0000 || n > 0xffff_ffff) {
            throw new Error(`Immediate parameter ${n} at ${location} should be a valid 32-bit integer`);
        }
        if (!forceTwoWords) {
            if (-32768 <= n && n <= 32767) {
                return [(0b001101 << 26) | (Assembler.REGISTERS[dest] << 16) | n & 0xffff]; // ORI rt, %zero, n
            } else if ((n & 0xffff) == 0) {
                return [(0b001111 << 26) | (Assembler.REGISTERS[dest] << 16) | (n>>>16)]; // LUI rt, %zero, n>>>16
            }
        }
        return [
            (0b001111 << 26) | (Assembler.REGISTERS[dest] << 16) | (n>>16) & 0xffff, // LUI rt, HI(n)
            (0b001101 << 26) | (Assembler.REGISTERS[dest] << 16) | n & 0xffff, // ORI rt, %zero, n
        ]
    }

    private static registerFormat(name: string, pos: number): Assembler.Field<Assembler.Register> {
        return (args) => {
            if (!(args.unparsedValue in Assembler.REGISTERS)) {
                throw new Error(`Invalid ${name} register ${args.unparsedValue} at ${args.assembler.currentOrg}`);
            }
            const reg: number = Assembler.REGISTERS[args.unparsedValue];
            if (args.assembler.flags.errorOnAtUsage && reg == Assembler.REGISTERS['$at']) {
                throw new Error(`$at register was used with the flag 'errorOnAtUsage' set`);
            }
            return args.instr | (reg << pos);
        }
    }

    private static immediateFormat({name, pos, bits, unsignedAllowed}: {name: string, pos: number, bits: number, unsignedAllowed?: boolean}): Assembler.Field<number> {
        return (args) => {
            const n = args.unparsedValue;
            // if unsignedAllowed then double the limit.
            let maxLimit = ((1 << (bits-1))>>>0) - 1;
            if (typeof unsignedAllowed === 'undefined' || unsignedAllowed) {
                maxLimit = 2 * maxLimit + 1;
            }
            if (!Number.isInteger(n) || n < -((1<<(bits-1))>>>0) || n > maxLimit) {
                throw new Error(`Parameter ${name} at ${args.org} should be a valid ${bits}-bit integer`);
            }
            const mask = bits > 31 ? -1 : (1 << bits) - 1;
            return args.instr | ((n & mask) << pos);
        };
    }

    private static fixedIntFormat({pos, bits}: {pos: number, bits: number}): Assembler.Field<number> {
        return (args) => {
            const n = args.unparsedValue
            if (!Number.isInteger(n) || n < 0 || n >= (1 << bits)) {
                throw new Error(`Invalid ${args.key} value ${n} at ${args.assembler.currentOrg}`);
            }
            return args.instr | (n << pos);
        }
    }

    private pushInstruction<T>(format: Assembler.InstructionFormat<T>): (values: {[K in keyof T]: Assembler.OptLazy<T[K]>}) => this {
        return (values: {[K in keyof T]: Assembler.OptLazy<T[K]>}): this => {
            const originalOrg = this.currentOrg;
            const index = this.instructions.length;
            let word = 0;
            Object.keys(format).forEach((key) => {
                const unparsedValue: Assembler.OptLazy<T[keyof T]> = values[key as keyof T]
                if (typeof unparsedValue === 'function') {
                    this.secondStageTasks.push(() => {
                        this.instructions[index] = format[key as keyof T]({
                            // typescript doesn't realize that:  (X extends Y ? never : X) & Y  :=:  never.
                            unparsedValue: (unparsedValue as Assembler.LazyCallback<T[keyof T]>)(this.definedLabels),
                            instr: this.instructions[index],
                            key,
                            assembler: this,
                            org: originalOrg,
                        });
                    });
                } else {
                    word = format[key as keyof T]({
                        unparsedValue,
                        instr: word,
                        key,
                        assembler: this,
                        org: this.currentOrg,
                    });
                }
            });
            this.instructions.push(word);
            this.currentOrg += 4;
            return this.setInDelaySlot(false);
        };
    }

    private pushIInstruction = this.pushInstruction({
        opcode: Assembler.fixedIntFormat({pos: 26, bits: 6}),
        rs: Assembler.registerFormat('rs', 21),
        rt: Assembler.registerFormat('rt', 16),
        imm16: Assembler.immediateFormat({name: 'imm16', pos: 0, bits: 16}),
    });
    private pushRInstruction = this.pushInstruction({
        rs: Assembler.registerFormat('src1', 21),
        rt: Assembler.registerFormat('src2', 16),
        rd: Assembler.registerFormat('dest', 11),
        opcode: Assembler.fixedIntFormat({pos: 0, bits: 6}),
    });
    private pushExceptionInstruction = this.pushInstruction({
        code: Assembler.immediateFormat({name: 'code', pos: 6, bits: 20}),
        opcode: Assembler.fixedIntFormat({pos: 0, bits: 6}),
    });
    private pushData = this.pushInstruction({
        word: Assembler.immediateFormat({name: 'word', pos: 0, bits: 32})
    });

    private pushBinaryString(str: string): this {
        let word = 0;
        for (let i = 0; i < str.length; i++) {
            const byte = str.charCodeAt(i);
            if (byte > 255) {
                throw new Error(`Invalid ${i}-th charCode ${byte} at ${this.currentOrg}`)
            }
            // in little-endian first char is LSB
            word |= byte << ((i&3)*8);
            if ((i & 3) == 3) {
                this.pushData({word});
                word = 0;
            }
        }
        if (str.length & 3) {
            this.pushData({word});
        }
        return this;
    }
}

module Assembler {
    export type Register = keyof typeof Assembler.REGISTERS;

    export interface LabelRef {
        label: string;
        offset?: number;
    }

    export type Field<V> = (arg: {
        unparsedValue: V,
        instr: number,
        key: string,
        org: number,
        assembler: Assembler,
    }) => number;

    export type InstructionFormat<T> = {
        [K in keyof T]: Field<T[K]>
    }

    // We don't allow function field types since it would be impossible in runtime to distinguish
    // between them and lazy expressions:
    export type LazyCallback<V> = (definedLabels: {[labelName: string]: number}) => V;
    export type OptLazy<V> = Exclude<V, Function> | LazyCallback<Exclude<V, Function>>;

    export type Numeric = OptLazy<number>;
    export type LRegister = OptLazy<Register>;
    export type LLabelRef = OptLazy<LabelRef>;
}

export default Assembler;
