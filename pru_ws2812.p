.origin 0
.entrypoint START

#include "pru_gpio.hp"  // include the GPIO driver
#include "pru_delay.hp" // include the delay driver

#define GPIO_MODULE        GPIO1 // Modules defined in pru_gpio.hp
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
        qba TRLED_NXT
TRLED_T0:
        tr0
TRLED_NXT:
        lsr r3, r3, 1
        qbne TRLEN_IN, r3, 0
.endm




#define T0H 35
#define T0L 77
#define T1H 73
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
        sbco r0, REG_SYSCFG, 4, 4  //

LED_START:
        //setgpio GPIO_MODULE, GPIO_PIN
        cleargpio GPIO_MODULE, GPIO_PIN
        delayus 60
        //setgpio GPIO_MODULE, GPIO_PIN

        mov r4, 0 //ws2812 data in pru memory offset
        mov r6, 0
        sbbo &r6, r4, 0, 4

        lbbo &r6, r4, 4, 4
        add r6, r6, 2
        lsl r6, r6, 2
        //now r6 is last address

        mov r5, 8 //offset of data

LED_ITER:
        qbge LED_FIN, r6, r5
        
        lbbo &r7, r4, r5, 4
        trled r7

        add r5, r5, 4
        qba LED_ITER

LED_FIN:
        cleargpio GPIO_MODULE, GPIO_PIN  // set pin low


        // This sends an interrupt to the kernel, which the uio_pruss driver
        // forwards to the pru_blink executable to tell it that the program
        // has finished executing:
        mov r31.b0, PRU0_ARM_INTERRUPT+16


WAIT_NEXT:
        lbbo &r6, r4, 0, 4
        qbeq WAIT_NEXT, r6, 0
        qba LED_START


        // Disable the PRU:
        halt