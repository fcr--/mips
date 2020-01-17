import Assembler from '../Assembler'
import { expect } from 'chai';
import 'mocha';

describe('Assembler', () => {
    describe('Constants', () => {
        it('can define numeric constants', () => {
            let mem: Uint32Array;
            // const memory = a.label('foo').begin()
            //     .lw({ dest: 'r0', src: 'r0', offset: 32 })
            //     .addiu({ dest: 'r1', src: {label: 'x', offset: 4} })
            //     .label('x').data(42, 7000, 123)
            // .end().assemble();
            mem = new Assembler({org: 0x0}).data(42).assemble();
            expect(mem).to.eql(Uint32Array.from([42]));

            mem = new Assembler({org: 0x0}).data(0x11223344, -1).assemble();
            expect(mem).to.eql(Uint32Array.from([0x11223344, 0xffff_ffff]));

            mem = new Assembler({org: 0x800})
                .data(42) // located at 0x800
                .label('start')
                .data((s) => s.start, 32) // located at 0x804
                .assemble();
            expect(mem).to.eql(Uint32Array.from([42, 0x804, 32]));
        });

        it('supports string constants', () => {
            let mem: Uint32Array;

            mem = new Assembler({org: 0}).data('Ball').assemble();
            expect(mem).to.eql(Uint32Array.from([0x6c6c6142]));

            mem = new Assembler({org: 0}).data('').assemble();
            expect(mem).to.eql(Uint32Array.from([]));

            mem = new Assembler({org: 0}).data('\x11\x22\x33\x44\x55\x66\x77\x88').assemble();
            expect(mem).to.eql(Uint32Array.from([0x44332211, 0x88776655]));

            mem = new Assembler({org: 0}).data('Ball\x01', ' ', '  ', '', 'XYZ').assemble();
            expect(mem).to.eql(Uint32Array.from([0x6c6c6142, 1, 0x20, 0x2020, 0x5a5958]));

            mem = new Assembler({org: 0}).data(42, '\xff', 7).assemble();
            expect(mem).to.eql(Uint32Array.from([42, 0xff, 7]));
        });

        it('can detect invalid datas', () => {
            const a = new Assembler({org: 0x0});
            for (const invalidValue of [0x1_0000_0000, -0x8000_0001, 0.5]) {
                expect(() => a.data(invalidValue)).to.throw();
                expect(() => a.data(() => invalidValue).assemble()).to.throw();
            }
            expect(() => a.data('\u2665')).to.throw();
        });
    });

    describe('Arithmetic', () => {
        // TODO: CLO, CLZ, L{A,I}, LUI, MOVE, NEGU, SE{B,H}, SUB[U].
        it('Supports ADD', () => {
            let mem: Uint32Array = new Assembler({org: 0})
                .ADD({dest: '$0', src1: '$v1', src2: '$v0'})
                .ADD({dest: '$a0', src1: '$a1', src2: '$a2'})
                .assemble();
            expect(mem).to.eql(Uint32Array.from([0x00620020, 0x00a62020]));
        });

        it('Supports ADDI', () => {
            let mem: Uint32Array = new Assembler({org: 0})
                .ADDI({dest: '$t5', src: '$t4', imm16: 0})
                .ADDI({dest: '$t6', src: '$zero', imm16: 0x8000})
                .ADDI({dest: '$zero', src: '$t7', imm16: 0xfedc})
                .assemble();
            expect(mem).to.eql(Uint32Array.from([0x218d0000, 0x200e8000, 0x21e0fedc]));
        });

        it('Supports ADDIU', () => {
            let mem: Uint32Array = new Assembler({org: 0})
                .ADDIU({dest: '$a0', src: '$a0', imm16: 1})
                .ADDIU({dest: '$v0', src: '$zero', imm16: -1})
                .ADDIU({dest: '$zero', src: '$31', imm16: 0xaa55})
                .assemble();
            expect(mem).to.eql(Uint32Array.from([0x24840001, 0x2402ffff, 0x27e0aa55]));
        });

        it('Supports ADDU', () => {
            let mem: Uint32Array = new Assembler({org: 0})
                .ADDU({dest: '$a3', src1: '$t0', src2: '$t1'})
                .ADDU({dest: '$zero', src1: '$t2', src2: '$t3'})
                .assemble();
            expect(mem).to.eql(Uint32Array.from([0x01093821, 0x014b0021]));
        });
    });

    describe('Atomic Read-Modify-Write', () => {
        // TODO: LL, SC.
    });

    describe('Branches', () => {
        // TODO: B[{GE,LT}Z][AL], B{EQ,NE}[Z], B{GT,LE}Z.
        // TODO: make sure out-of-range destinations are tested
    });

    describe('Conditional', () => {
        // TODO: MOV{N,Z}, SLT[I][U].
    });

    describe('Jumps', () => {
        // TODO: J[AL][R].
    });

    describe('Load and Store', () => {
        // TODO: L{A,I}, L{B,H}U, {L,S}{B,H,W[{L,R}]}, U{L,S}W.
    });

    describe('Logical and Bit-Field', () => {
        // TODO: {AND,[X]OR}[I], NO{P,R,T}, EXT, INS, WSBH
    });

    describe('Multiply, Divide and Accumulator Access', () => {
        // TODO: MUL, {DIV,MULT,M{ADD,SUB}}[U]
    });

    describe('Shift and Rotate', () => {
        // TODO: {ROTR,SLL,SRA,SRL}[V]
    });

    // TODO: add test cases for the other instructions I forgot to add TODOs

    describe('Assembler Flags', () => {
        let a: Assembler = new Assembler({org: 0})
        expect(() => a.ADD({dest: '$0', src1: '$1', src2: '$2'})).to.throw();

        a.flags.errorOnAtUsage = false;
        let mem: Uint32Array = a.ADD({dest: '$0', src1: '$1', src2: '$2'}).assemble();
        expect(mem).to.eql(Uint32Array.from([0x00220020]));
    });
})