#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include "computer.h"
#undef mips			/* gcc already has a def for mips */

unsigned int endianSwap(unsigned int);

void PrintInfo (int changedReg, int changedMem);
unsigned int Fetch (int);
int power(int in);
void Decode (unsigned int, DecodedInstr*, RegVals*);
int Execute (DecodedInstr*, RegVals*);
int Mem(DecodedInstr*, int, int *);
void RegWrite(DecodedInstr*, int, int *);
void UpdatePC(DecodedInstr*, int);
void PrintInstruction (DecodedInstr*);

/*Globally accessible Computer variable*/
Computer mips;
RegVals rVals;

/*
 *  Return an initialized computer with the stack pointer set to the
 *  address of the end of data memory, the remaining registers initialized
 *  to zero, and the instructions read from the given file.
 *  The other arguments govern how the program interacts with the user.
 */
void InitComputer (FILE* filein, int printingRegisters, int printingMemory,
  int debugging, int interactive) {
    int k;
    unsigned int instr;

    /* Initialize registers and memory */

    for (k=0; k<32; k++) {
        mips.registers[k] = 0;
    }
    
    /* stack pointer - Initialize to highest address of data segment */
    mips.registers[29] = 0x00400000 + (MAXNUMINSTRS+MAXNUMDATA)*4;

    for (k=0; k<MAXNUMINSTRS+MAXNUMDATA; k++) {
        mips.memory[k] = 0;
    }

    k = 0;
    while (fread(&instr, 4, 1, filein)) {
	/*swap to big endian, convert to host byte order. Ignore this.*/
        mips.memory[k] = ntohl(endianSwap(instr));
        k++;
        if (k>MAXNUMINSTRS) {
            fprintf (stderr, "Program too big.\n");
            exit (1);
        }
    }

    mips.printingRegisters = printingRegisters;
    mips.printingMemory = printingMemory;
    mips.interactive = interactive;
    mips.debugging = debugging;
}

unsigned int endianSwap(unsigned int i) {
    return (i>>24)|(i>>8&0x0000ff00)|(i<<8&0x00ff0000)|(i<<24);
}

/*
 *  Run the simulation.
 */
void Simulate () {
    char s[40];  /* used for handling interactive input */
    unsigned int instr;
    int changedReg=-1, changedMem=-1, val;
    DecodedInstr d;
    
    /* Initialize the PC to the start of the code section */
    mips.pc = 0x00400000;
    while (1) {
        if (mips.interactive) {
            printf ("> ");
            fgets (s,sizeof(s),stdin);
            if (s[0] == 'q') {
                return;
            }
        }

        /* Fetch instr at mips.pc, returning it in instr */
        instr = Fetch (mips.pc);

        printf ("Executing instruction at %8.8x: %8.8x\n", mips.pc, instr);

        /* 
	 * Decode instr, putting decoded instr in d
	 * Note that we reuse the d struct for each instruction.
	 */
        Decode (instr, &d, &rVals);
        
        /*Print decoded instruction*/
        PrintInstruction(&d);

        /* 
	 * Perform computation needed to execute d, returning computed value 
	 * in val 
	 */
        val = Execute(&d, &rVals);

	    UpdatePC(&d,val);

        /* 
	 * Perform memory load or store. Place the
	 * address of any updated memory in *changedMem, 
	 * otherwise put -1 in *changedMem. 
	 * Return any memory value that is read, otherwise return -1.
         */
        val = Mem(&d, val, &changedMem);

        /* 
	 * Write back to register. If the instruction modified a register--
	 * (including jal, which modifies $ra) --
         * put the index of the modified register in *changedReg,
         * otherwise put -1 in *changedReg.
         */
        RegWrite(&d, val, &changedReg);

        PrintInfo (changedReg, changedMem);
    }
}

/*
 *  Print relevant information about the state of the computer.
 *  changedReg is the index of the register changed by the instruction
 *  being simulated, otherwise -1.
 *  changedMem is the address of the memory location changed by the
 *  simulated instruction, otherwise -1.
 *  Previously initialized flags indicate whether to print all the
 *  registers or just the one that changed, and whether to print
 *  all the nonzero memory or just the memory location that changed.
 */
void PrintInfo ( int changedReg, int changedMem) {
    int k, addr;
    printf ("New pc = %8.8x\n", mips.pc);
    if (!mips.printingRegisters && changedReg == -1) {
        printf ("No register was updated.\n");
    } else if (!mips.printingRegisters) {
        printf ("Updated r%2.2d to %8.8x\n",
        changedReg, mips.registers[changedReg]);
    } else {
        for (k=0; k<32; k++) {
            printf ("r%2.2d: %8.8x  ", k, mips.registers[k]);
            if ((k+1)%4 == 0) {
                printf ("\n");
            }
        }
    }
    if (!mips.printingMemory && changedMem == -1) {
        printf ("No memory location was updated.\n");
    } else if (!mips.printingMemory) {
        printf ("Updated memory at address %8.8x to %8.8x\n",
        changedMem, Fetch (changedMem));
    } else {
        printf ("Nonzero memory\n");
        printf ("ADDR	  CONTENTS\n");
        for (addr = 0x00400000+4*MAXNUMINSTRS;
             addr < 0x00400000+4*(MAXNUMINSTRS+MAXNUMDATA);
             addr = addr+4) {
            if (Fetch (addr) != 0) {
                printf ("%8.8x  %8.8x\n", addr, Fetch (addr));
            }
        }
    }
}

/*
 *  Return the contents of memory at the given address. Simulates
 *  instruction fetch. 
 */
unsigned int Fetch ( int addr) {
    return mips.memory[(addr-0x00400000)/4];
}

/* Decode instr, returning decoded instruction. */
void Decode ( unsigned int instr, DecodedInstr* d, RegVals* rVals) {
    // Decimal to hex
    // int n = 0;
    
    // Get opcode
    int temp;
    d->op = instr >> 26;
    
    printf("opcode: %d\n", d->op);

    if(d->op == 0) { // R-Format
        
        // shift rs here
        temp = instr << 6;
        temp = temp >> 21;
        d->regs.r.rs = temp;
        rVals->R_rs = mips.registers[d->regs.r.rs];
        printf("rs: %d\n", d->regs.r.rs);
        
        // shift rt here
        temp = instr << 11;
        temp = temp >> 16;
        d->regs.r.rt = temp;
        rVals->R_rt = mips.registers[d->regs.r.rt];
        printf("rt: %d\n", d->regs.r.rt);
        
        // shift rd here
        temp = instr << 16;
        temp = temp >> 11;
        d->regs.r.rd = temp;
        rVals->R_rd = mips.registers[d->regs.r.rd];
        printf("rd: %d\n", d->regs.r.rd);
        
        // shift shamt here
        temp = instr << 21;
        temp = temp >> 5;
        d->regs.r.shamt = temp;
        printf("shamt: %d\n", d->regs.r.shamt);
        
        //funct
        temp = instr << 26;
        temp = temp >> 26;
        d->regs.r.funct = temp;
        printf("funct: %d\n", d->regs.r.funct);
        
    } else if(d->op == 2 || d->op == 3) { // J-Format
        // shift target
        temp = instr << 6;
        temp = temp >> 26;
        d->regs.j.target = temp;
        printf("target: %d\n", d->regs.j.target);
    } else if (d->op == 8 || d->op == 12 || d->op == 4 || d->op == 5 || 
               d->op == 15 || d->op == 35 || d->op == 13 || d->op == 43) { // I-Format
        
        // shift rs
        temp = instr << 6;
        temp = temp >> 21;
        d->regs.i.rs = temp;
        rVals->R_rs = mips.registers[d->regs.i.rs];
        printf("i_rs: %d\n", d->regs.i.rs);
        
        // shift rt
        temp = instr << 11; //??
        temp = temp >> 16; //??
        d->regs.i.rt = temp;
        rVals->R_rt = mips.registers[d->regs.i.rt];
        printf("i_rt: %d\n", d->regs.i.rt);
        
        // shift imm
        temp = instr << 16;
        temp = temp >> 16;
        d->regs.i.addr_or_immed = temp;
        printf("i_addr_or_imm: %d\n", d->regs.i.addr_or_immed);
    }
    
    // else{ // other instructions
    //     exit(0);
    // }
}

/*
 *  Print the disassembled version of the given instruction
 *  followed by a newline.
 */
void PrintInstruction ( DecodedInstr* d) {
    // R-format, check funct to print instruction
    if (d->op == 0){
        if (d->regs.r.funct == 33){
            printf("addu\t $%d, ", d->regs.r.rd);
            printf("$%d, ", d->regs.r.rs);
            printf("$%d\n", d->regs.r.rt);
        }
        else if (d->regs.r.funct == 35){
            printf("subu\t $%d, ", d->regs.r.rd);
            printf("$%d, ", d->regs.r.rs);
            printf("$%d\n", d->regs.r.rt);
        }
        else if (d->regs.r.funct == 0){
            printf("sll\t $%d, ", d->regs.i.rs);
            printf("$%d, ", d->regs.i.rt);
            printf("%d\n", d->regs.i.addr_or_immed);
        }
        else if (d->regs.r.funct == 2){
            printf("srl\t $%d, ", d->regs.i.rs);
            printf("$%d, ", d->regs.i.rt);
            printf("%d\n", d->regs.i.addr_or_immed);
        }
        else if (d->regs.r.funct == 36){
            printf("and\t $%d, ", d->regs.r.rd);
            printf("$%d, ", d->regs.r.rs);
            printf("$%d\n", d->regs.r.rt);
        }
        else if (d->regs.r.funct == 37){
            printf("or\t $%d, ", d->regs.r.rd);
            printf("$%d, ", d->regs.r.rs);
            printf("$%d\n", d->regs.r.rt);
        }
        else if (d->regs.r.funct == 42){
            printf("slt\t $%d, ", d->regs.r.rd);
            printf("$%d, ", d->regs.r.rs);
            printf("$%d\n", d->regs.r.rt);
        }
        else if (d->regs.r.funct == 8){
           // printf("jr\t $%d"), d->regs.r.rd; // different case for jr, since it is r-format
        }
    }
    else if (d->op == 2){ // jump instruction
        printf("j\t0x"); 
        printf("%x\n", d->regs.j.target);
    }
}

/* Perform computation needed to execute d, returning computed value */
int Execute ( DecodedInstr* d, RegVals* rVals) {
    int ALUOp, ALUfunct, arg1, arg2;
    ALUOp = d->op;

    if (ALUOp == 0){
        ALUfunct = d->regs.r.funct;
        if (ALUfunct == 33){  // addu
            arg1 = d->regs.r.rs;
            arg2 = d->regs.r.rt;
            return arg1 + arg2;
        }
        else if (ALUfunct == 35){ // subu
            arg1 = d->regs.r.rs;
            arg2 = d->regs.r.rt;
            return arg1 - arg2;
        }
        else if (ALUfunct == 0){ // sll
            arg1 = d->regs.r.rs;
            arg2 = d->regs.r.rt;
            return arg1 << arg2;
        }
        else if (ALUfunct == 2){ // srl
            arg1 = d->regs.r.rs;
            arg2 = d->regs.r.rt;
            return arg1 >> arg2;
        }
        else if (ALUfunct == 36){ // and
            arg1 = d->regs.r.rs;
            arg2 = d->regs.r.rt;
            return arg1 & arg2;
        }
        else if (ALUfunct == 37){ // or
            arg1 = d->regs.r.rs;
            arg2 = d->regs.r.rt;
            return arg1 | arg2;
        }
        else if (ALUfunct == 42){ // slt
            arg1 = d->regs.r.rs;
            arg2 = d->regs.r.rt;
            return arg1 < arg2;
        }
    }
    else if (ALUOp == 9 || ALUOp == 35 || ALUOp == 43){ // addiu, lw, or sw
        arg1 = d->regs.i.rs;
        arg2 = d->regs.i.addr_or_immed;
        return arg1 + arg2;
    }
    else if (ALUOp == 4 || ALUOp == 5){ // beq or bne
        arg1 = d->regs.i.rs;
        arg2 = d->regs.i.rt;
        if (arg1 == arg2){
            return 1; // beq
        }
        else{return 0;} // bne
    }
    else if (ALUOp == 12){ // andi
        arg1 = d->regs.i.rs;
        arg2 = d->regs.i.addr_or_immed;
        return arg1 & arg2;
    }
    else if (ALUOp == 13){ // ori
        arg1 = d->regs.i.rs;
        arg2 = d->regs.i.addr_or_immed;
        return arg1 | arg2;
    }
    else if (ALUOp == 15){ // lui
        arg1 = d->regs.i.addr_or_immed;
        return arg1 << 16;
    }
  return 0;
}

/* 
 * Update the program counter based on the current instruction. For
 * instructions other than branches and jumps, for example, the PC
 * increments by 4 (which we have provided).
 */
void UpdatePC ( DecodedInstr* d, int val) {
    mips.pc+=4;

    if (d->op == 2){ // j
        mips.pc = d->regs.j.target;
    }

    else if (d->op == 3){ // jal
        mips.registers[31] = mips.pc; // store into $ra
        mips.pc = d->regs.j.target;
    }

    else if (d->op == 4){
        if (val == 1){ // beq, 1 = true value
            mips.pc = d->regs.i.addr_or_immed;
        }
        else{ // beq, 0 = false
            return;
        }
    }

    else if (d->op == 5){
        if (val == 0){ // bne, 0 = true value
            mips.pc = d->regs.i.addr_or_immed;
        }
        else{ // bne, 1 = false
            return;
        }
    }

    else if (d->op == 0 && d->regs.r.funct == 8){ // jr
        mips.pc = mips.registers[31]; // jumps to $ra
    }
}

/*
 * Perform memory load or store. Place the address of any updated memory 
 * in *changedMem, otherwise put -1 in *changedMem. Return any memory value 
 * that is read, otherwise return -1. 
 *
 * Remember that we're mapping MIPS addresses to indices in the mips.memory 
 * array. mips.memory[0] corresponds with address 0x00400000, mips.memory[1] 
 * with address 0x00400004, and so forth.
 *
 */
int Mem( DecodedInstr* d, int val, int *changedMem) {
    /* Your code goes here */

  return -1;
}

/* 
 * Write back to register. If the instruction modified a register--
 * (including jal, which modifies $ra) --
 * put the index of the modified register in *changedReg,
 * otherwise put -1 in *changedReg.
 */
void RegWrite( DecodedInstr* d, int val, int *changedReg) {
    /* Your code goes here */
    if (d->op == 0){
        *changedReg = d->regs.r.rd;
        mips.registers[*changedReg]=val;
    }
    else if (d->op == 2){
        *changedReg = -1;
    }
    else if (d->op == 3){
        *changedReg = d->regs.j.target;
        mips.registers[*changedReg]=val;
    }
    else {
        *changedReg = d->regs.i.rt;
        mips.registers[*changedReg]=val;
    }
}
// ./sim -i sample.output
