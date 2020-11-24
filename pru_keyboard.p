#define KEY_GPIO_MODULE        GPIO2
#define PRU0_ARM_INT  19

.macro cleargpior11
        mov r0, 1
        lsl r0, r0, r11
        mov r1, KEY_GPIO_MODULE | GPIO_CLEARDATAOUT
        sbbo r0, r1, 0, 4
.endm

.macro setgpior11
        mov r0, 1
        lsl r0, r0, r11
        mov r1, KEY_GPIO_MODULE | GPIO_SETDATAOUT
        sbbo r0, r1, 0, 4
.endm

.macro load_outc
        mov r10, 0
        lbbo &r10, r10, 0, 1
.endm
.macro load_outpin
        mov r11, 0
        lbbo &r11, r10, 0, 1
.endm

.macro init_keyboard
        load_outc
        load_outpin
        setgpior11
.endm

.macro poll_keyboard
        mov r3, 8
        mov r2, 0

loop ENDLOOP, 8
        mov r13, 0
        lbbo r13, r3, 0, 1
        qblt SKIP_BTN, r13, 31
        getgpio KEY_GPIO_MODULE, r13

        lsl r2, r2, 1
        or r2, r2, r0
SKIP_BTN:
        add r3, r3, 1

ENDLOOP:
        cleargpior11
        mov r0, 15
        lbbo r3, r0, r10, 1 //previuos value
        sbbo r2, r0, r10, 1

        xor r3, r2, r3 //changes

        qbeq END_STORE, r3, 0

        and r4, r3, r2 //pressed
        not r5, r2
        and r5, r5, r3 //depressed

        mov r0, 15+8
        lbbo r2, r0, r10, 1
        or r2, r2, r3
        sbbo r2, r0, r10, 1

        mov r0, 31
        lbbo r2, r0, r10, 1
        or r2, r2, r4
        sbbo r2, r0, r10, 1

        mov r0, 31+8
        lbbo r2, r0, r10, 1
        or r2, r2, r5
        sbbo r2, r0, r10, 1

        MOV r31, 32
        //mov r31.b0, PRU0_ARM_INT+16

END_STORE:

        sub r10, r10, 1
        qbne NEXT_BIT, r10, 0
        load_outc

NEXT_BIT:
        load_outpin
        setgpior11
.endm

