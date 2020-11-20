.origin 0
.entrypoint START

#include "pru_gpio.hp"  // include the GPIO driver
#include "pru_delay.hp" // include the delay driver
#include "pru_keyboard.p"

#define GPIO_MODULE        GPIO1
#define GPIO_PIN           28

#define REG_SYSCFG         C4
#define PRU0_ARM_INTERRUPT 19

.macro rawdelay
.mparam cycles
        mov r1, cycles
        RAWDELAYC:
        sub r1, r1, 1
        qbne RAWDELAYC, r1, 0
.endm

.macro trled
.mparam word
        mov r2, word
        mov r3, 0x800000
TRLEN_IN:       
        and r4, r2, r3
        qbeq TRLED_T0, r4, 0
        tr1
//        setgpio GPIO_MODULE, GPIO_PIN  // set pin low
        qba TRLED_NXT
TRLED_T0:
        tr0
//        cleargpio GPIO_MODULE, GPIO_PIN  // set pin low
TRLED_NXT:
        lsr r3, r3, 1
        qbne TRLEN_IN, r3, 0
.endm


#define T0H 36
#define T0L 78
#define T1H 75
#define T1L 39
.macro tr0
        setgpio GPIO_MODULE, GPIO_PIN  // set pin low
        rawdelay T0H
        cleargpio GPIO_MODULE, GPIO_PIN  // set pin low
        rawdelay T0L
.endm

.macro tr1
        setgpio GPIO_MODULE, GPIO_PIN  // set pin low
        rawdelay T1H
        cleargpio GPIO_MODULE, GPIO_PIN  // set pin low
        rawdelay T1L
.endm

START:
        // Clear SYSCFG[STANDBY_INIT] to enable OCP master port:
        lbco r0, REG_SYSCFG, 4, 4  // These three instructions are required
        clr r0, r0, 4              // to initialize the PRU
        sbco r0, REG_SYSCFG, 4, 4
        MOV     r0, 0x120
        MOV     r1, 0x22028
        SBBO    r0, r1, 0, 4
        MOV r31, 32

        init_keyboard
        poll_keyboard
        poll_keyboard
        poll_keyboard
        poll_keyboard
        poll_keyboard
        poll_keyboard
        poll_keyboard
        poll_keyboard

LED_START:

        mov r8, 0x80 //ws2812 data in pru memory offset
        mov r6, 0
        sbbo &r6, r8, 0, 4

        lbbo &r6, r8, 4, 4
        qblt WAIT_NEXT, r6, 32 //inadequate length
        add r6, r6, 2
        lsl r6, r6, 2
        //now r6 is last address

        mov r5, 8 //offset of data


LED_ITER:
        qbge LED_FIN, r6, r5
        
        lbbo &r7, r8, r5, 4
        trled r7

        add r5, r5, 4
        qba LED_ITER

LED_FIN:
        cleargpio GPIO_MODULE, GPIO_PIN  // set pin low

        // This sends an interrupt to the kernel, which the uio_pruss driver
        // forwards to the pru_blink executable to tell it that the program
        // has finished executing:
//        mov r31.b0, PRU0_ARM_INTERRUPT+16


WAIT_NEXT:
        delayms 10 
        poll_keyboard

        lbbo &r6, r8, 0, 4
        qbeq WAIT_NEXT, r6, 0

        qba LED_START

        // Disable the PRU:
halt
