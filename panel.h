#ifndef PANEL_H
#define PANEL_H
#include <stdint.h>

#define IDENT_SETLED 0x10
struct setled {
	uint16_t ident; //0x10
	uint16_t button;
	uint32_t color;
	uint32_t colors[];
};

#define IDENT_SETTEXT 0x11
struct settext {
	uint16_t ident; //0x11
	uint16_t button;
	wchar_t line1[16];
	wchar_t line2[16];
};


#endif
