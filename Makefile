# Makefile for pru_blink example program.
# Run:
#  # make
# to assemble the PRU binary and build the C loader.

CFLAGS=-c -Wall -I/usr/local/include
LIBS=-L/usr/local/lib -lpru -lgpio

PASM=pasm
PASMFLAGS=-b -V3

all: pru_ws2812.bin panel
# pru_blink

pru_ws2812.bin: pru_ws2812.p pru_keyboard.p
	$(PASM) $(PASMFLAGS) pru_ws2812.p

panel: panel.o
	cc -o panel panel.o $(LIBS)

clean:
	rm pru_blink.bin pru_blink.o pru_blink

.PHONY: all clean
