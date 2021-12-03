/*
 * Copyright (c) 2017, Stanislav Lakhtin. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. No personal names or organizations' names associated with the
 *    Atomthreads project may be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
 * THE PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef SSD1306_LIBRARY_SSD1306_I2C_H
#define SSD1306_LIBRARY_SSD1306_I2C_H

#include <stdint.h>
#include <stddef.h>

#ifndef true
#define true 1
#define false 0
#endif
typedef uint8_t bool;
#define _swap(a, b) { uint8_t t = a; a = b; b = t; }
#define _bitSet(x) (1 << (x))
#define _bitClear(x) (~(1 << x))
#define _bitCheck(number, x) (number >> x) & 1

// address of device is // 011110+SA0+RW - 0x3C or 0x3D

#define DEFAULT_7bit_OLED_SLAVE_ADDRESS 0b0111100
#define I2C_COMMAND 0x00
#define I2C_DATA 0x40

// Fundamental Command Table
#define SSD1306_SET_CONTROL     0x81  // Double byte command to select 1 out of 256 contrast steps. Contrast increases as the value increases.
#define SSD1306_RESET           0x7F
#define SSD1306_DISPLAY_ON_RAM  0xA4  // Resume to RAM content display (RESET)
#define SSD1306_DISPLAY_NO_RAM  0xA5  // Output ignores RAM content
#define SSD1306_SET_NORMAL      0xA6  // Normal display (RESET)
#define SSD1306_SET_INVERSE     0xA7  // Inverse display
#define SSD1306_SET_DISPLAY_OFF 0xAE  // Display OFF (sleep mode)
#define SSD1306_SET_DISPLAY_ON  0xAF  // Display ON in normal mode
#define SSD1306_SET_VMIRROR_ON  0xC8  // mirror vertically
#define SSD1306_SET_HMIRROR_ON  0xA1  // mirror horizonally

#define _IF_SB(i2c) ((I2C_SR1(i2c) & I2C_SR1_SB) == 0)
#define _IF_BTF(i2c) ((I2C_SR1(i2c) & I2C_SR1_BTF) == 0)
#define _IF_ADDR(i2c) ((I2C_SR1(i2c) & I2C_SR1_ADDR) == 0)
#define _IF_TxE(i2c) (I2C_SR1(i2c) & I2C_SR1_TxE) == 0

// Addressing mode

#ifndef DEFAULTBUFFERLENGTH
#define DEFAULTBUFFERLENGTH 1024
#endif

typedef enum SSD1306_AddressingMode {
  Horizontal  = 0b00,
  Vertical    = 0b01,
  Page        = 0b10, // RESET
  INVALID     = 0b11  // You MUST NOT USE IT
} MODE;

struct lcd {
  uint32_t i2c_oled; //= I2C2;
  uint8_t oled_address; // = DEFAULT_7bit_OLED_SLAVE_ADDRESS;
  uint8_t width; // = 128;
  uint8_t height; // = 32;
  uint16_t sbl; //screenBufferLength; // = DEFAULTBUFFERLENGTH;
  uint8_t *screenRAM;
  void *userdata;
};

extern MODE AddressingMode;

typedef enum SSD1306_COLOR { white = 0, black = 1} Color;
typedef enum SSD1306_WRAP {nowrap, wrapDisplay, wrapCoord} WrapType;

void ssd1306_init(struct lcd *screen, uint32_t i2c, uint8_t address, uint8_t width, uint8_t height, uint8_t *ptr);

// tools

#define DATAONLY (uint8_t)0x40
#define COMMAND  (uint8_t)0x00

int  i2c_master_write(uint32_t port, uint8_t OLED_ADDRESS, uint8_t *buffer, unsigned long length);

void ssd1306_send(struct lcd *screen, uint8_t *buffer, uint32_t len);
void ssd1306_send_data(struct lcd *screen, uint8_t spec, uint8_t data);
//void ssd1306_send(struct lcd *screen, uint8_t spec);

// hardware commands
void ssd1306_setMemoryAddressingMode(struct lcd *screen, MODE mode);
void ssd1306_setColumnAddressScope(struct lcd *screen, uint8_t lower, uint8_t upper);
void ssd1306_setPageAddressScope(struct lcd *screen, uint8_t lower, uint8_t upper);
void ssd1306_setPageStartAddressForPageAddressingMode(struct lcd *screen, uint8_t pageNum);
void ssd1306_setDisplayStartLine(struct lcd *screen, uint8_t startLine);
void ssd1306_setContrast(struct lcd *screen, uint8_t value);
void ssd1306_setPrecharge(struct lcd *screen, uint8_t value);
void ssd1306_setDisplayOn(struct lcd *screen, bool resume); // switch ON/OFF MCU of display
void ssd1306_setInverse(struct lcd *screen, bool inverse);
void ssd1306_chargePump(struct lcd *screen, bool chargePump);
void ssd1306_switchOLEDOn(struct lcd *screen, bool goOn); //switch ON/OFF power switch of the OLED panel
void ssd1306_setDisplayOffset(struct lcd *screen, uint8_t verticalShift);
void ssd1306_adjustVcomDeselectLevel(struct lcd *screen, uint8_t value);
void ssd1306_setOscillatorFrequency(struct lcd *screen, uint8_t value); // you SHOULD use default value (0x80)
void ssd1306_setMultiplexRatio(struct lcd *screen, uint8_t ratio);
void ssd1306_setCOMPinsHardwareConfiguration(struct lcd *screen, uint8_t);
void ssd1306_setPage(struct lcd *screen, uint8_t);
void ssd1306_setColumn(struct lcd *screen, uint8_t);

// paint commands
void ssd1306_clear(struct lcd *screen);
void ssd1306_refresh(struct lcd *screen);
void ssd1306_drawPixel(struct lcd *screen, uint8_t x, uint8_t y, Color c, bool directNoRAM);
void ssd1306_drawVPattern(struct lcd *screen, uint8_t x, int8_t y, uint8_t pattern, uint8_t replace);
void ssd1306_drawWCharStr(struct lcd *screen, uint8_t x, int8_t y, uint8_t maxx, Color color, WrapType wrType, wchar_t *str);



#endif //SSD1306_LIBRARY_SSD1306_I2C_H
