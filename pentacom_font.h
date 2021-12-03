//
// Created by sl on 06.06.17.
//

#ifndef SSD1306_LIBRARY_PENTACOM_FONT_H
#define SSD1306_LIBRARY_PENTACOM_FONT_H

#include <stdint.h>
//#include <wchar.h>

typedef struct {

    uint8_t size;

    uint8_t l[];

} FontChar_t;

const FontChar_t* getCharacter(uint16_t s);
uint16_t FontChar_getPixelTextLength(char *text);
void FontChar_drawInt(uint8_t x, uint8_t y, uint8_t color, int num, int pos);
void FontChar_drawText(uint8_t x, uint8_t y, uint8_t color, char *text);

#endif //SSD1306_LIBRARY_PENTACOM_FONT_H
