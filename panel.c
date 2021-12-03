#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <err.h>
#include <stdint.h>
#include <fcntl.h>
#include <string.h>
#include <libpru.h>
#include <libgpio.h>
#include <sys/event.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <dev/iicbus/iic.h>
#include <sys/ioctl.h>
#include "ssd1306_i2c.h"
#include <time.h>
#include <pthread.h>
#include <semaphore.h>
#include "panel.h"

#define FWBIN  "pru_ws2812.bin"
#define	AM33XX_MMAP_SIZE	0x00040000
#define ws2812_ram_offset 0x80
//uint8_t buttons[]={16,17,18,19,20,21,31,32,22,23,24,25,1,2,3,4,5,6,7,8,9,10,11};
uint8_t buttons[]={0,1,2,3,4,5,15,16,6,7,8,9,101,102,103,104,105,106,107,108,109,110,111};

sem_t update_lcd0;
sem_t update_lcd1;
sem_t update_lcd2;
struct lcd lcd0;
struct lcd lcd1;
struct lcd lcd2;
struct btnlcd {
	struct lcd *lcd;
	uint8_t x1;
	uint8_t x2;
	uint8_t l1;
	uint8_t l2;
} lcds[] = {
	{ .lcd=&lcd0, .x1=0,  .x2=64,  .l1=24,  .l2=16  },
	{ .lcd=&lcd0, .x1=64, .x2=128, .l1=24,  .l2=16  },
	{ .lcd=&lcd1, .x1=0,  .x2=64,  .l1=24,  .l2=16  },
	{ .lcd=&lcd1, .x1=64, .x2=128, .l1=24,  .l2=16  },

	{ .lcd=&lcd2, .x1=0,  .x2=64,  .l1=56,  .l2=48  },
	{ .lcd=&lcd2, .x1=64, .x2=128, .l1=56,  .l2=48  },

	{ .lcd=&lcd0, .x1=0,  .x2=64,  .l1=0,  .l2=8  },
	{ .lcd=&lcd0, .x1=64, .x2=128, .l1=0,  .l2=8  },
	{ .lcd=&lcd1, .x1=0,  .x2=64,  .l1=0,  .l2=8  },
	{ .lcd=&lcd1, .x1=64, .x2=128, .l1=0,  .l2=8  },
};

void write_lcd(struct btnlcd *bl, wchar_t *line1, wchar_t *line2){
	ssd1306_drawWCharStr(bl->lcd, bl->x1, bl->l1, bl->x2, black, nowrap, line1);
	ssd1306_drawWCharStr(bl->lcd, bl->x1, bl->l2, bl->x2, white, nowrap, line2);
};


int i2c1fd;
#define	I2C1_DEV			"/dev/iic1"
int i2c2fd;
#define	I2C2_DEV			"/dev/iic2"

int i2c_master_write(uint32_t port, uint8_t OLED_ADDRESS, uint8_t *buffer, unsigned long length){
	int error;
	struct iic_msg wrmsg;
	struct iic_rdwr_data rdwrdata;
	wrmsg.buf = buffer;
	wrmsg.len = length;
	wrmsg.slave = OLED_ADDRESS << 1;
	wrmsg.flags = IIC_M_WR;
	rdwrdata.msgs = &wrmsg;
	rdwrdata.nmsgs = 1;
	error = ioctl(port==1?i2c1fd:i2c2fd, I2CRDWR, &rdwrdata);
	if (error) {
		fprintf(stderr, "error %s\n",strerror(errno));
		return 1;
	}

	return 0;
}



void *lcd_updater(void *arg) {
	struct lcd *l=arg;
	sem_t *sem=l->userdata;
	while (1){
		sem_wait(sem);
		int sv;
		while(1){
			sem_getvalue(sem,&sv);
			if(sv>0)
				sem_wait(sem);
			else
				break;
		}
		ssd1306_refresh(l);
	}
	/* the function must return something - NULL will do */
	return NULL;
}


int main(int argc, char* argv[]) {
	size_t i;
	int fd = 0;
	char dev[64];
	size_t mmap_sizes[2] = { AM33XX_MMAP_SIZE };
	int saved_errno = 0;
	int error;
	sranddev();

	snprintf(dev, sizeof(dev), "/dev/pruss%zu", 0);
	fd = open(dev, O_RDWR);
	if (fd == -1 && errno == EACCES)
		err(EXIT_FAILURE, "Cannot open pruss");
	if (fd < 0)
		return EINVAL;
	/* N.B.: The order matters. */
	char *mem;
	uint32_t mem_size;

	for (i = 0; i < sizeof(mmap_sizes)/sizeof(mmap_sizes[0]); i++) {
		mem = mmap(0, mmap_sizes[i], PROT_READ|PROT_WRITE,
				MAP_SHARED, fd, 0);
		saved_errno = errno;
		if (mem != MAP_FAILED) {
			mem_size = mmap_sizes[i];
			break;
		}
	}
	if (mem == MAP_FAILED) {
		printf("mmap failed %d\n", saved_errno);
		errno = saved_errno;
		close(fd);
		return -1;
	}
	printf("mem at %p size %x\n",mem,mem_size);

	uint8_t init_pins_out[]={6,8,10,12};
	mem[0]=4;
	for(int i=mem[0];i>0;i--){
		mem[i]=init_pins_out[i-1];
	}

	uint8_t init_pins_in[]={32,32,25,23,13,11,9,7};
	for(int i=7;i>=0;i--){
		mem[8+i]=init_pins_in[i];
	}
	uint32_t *mem32ws=(uint32_t *)(mem+ws2812_ram_offset);

	printf("read %x %x %x %x\n",mem32ws[0],mem32ws[1],mem32ws[2],mem32ws[3]);

	int gpio2 = gpio_open_device("/dev/gpioc2");
	if (gpio2 == GPIO_INVALID_HANDLE)
		err(EXIT_FAILURE, "Cannot open gpioc");
	for(int i=sizeof(init_pins_in)-1;i>=0;i--){
		printf("config pin %d %d\n",i,init_pins_in[i]);
		if(init_pins_in[i]>31) continue;
		gpio_config_t pin_config;
		pin_config.g_pin = init_pins_in[i];
		pin_config.g_flags = GPIO_PIN_INPUT | GPIO_PIN_PULLDOWN;
		int res = gpio_pin_set_flags(gpio2, &pin_config);
		if (res < 0)
			err(EXIT_FAILURE, "configuration of pin %d "
					"failed (flags=%d)", pin_config.g_pin, 
					pin_config.g_flags);
	}

	for(int i=sizeof(init_pins_out)-1;i>=0;i--){
		printf("config pin %d %d\n",i,init_pins_out[i]);
		gpio_config_t pin_config;
		pin_config.g_pin = init_pins_out[i];
		pin_config.g_flags = GPIO_PIN_OUTPUT;
		int res = gpio_pin_set_flags(gpio2, &pin_config);
		if (res < 0)
			err(EXIT_FAILURE, "configuration of pin %d "
					"failed (flags=%d)", pin_config.g_pin, 
					pin_config.g_flags);
	}



	int handle = gpio_open_device("/dev/gpioc1");
	if (handle == GPIO_INVALID_HANDLE)
		err(EXIT_FAILURE, "Cannot open gpioc");
	gpio_config_t pin_config;
	pin_config.g_pin = 28;
	pin_config.g_flags = GPIO_PIN_OUTPUT;
	int res = gpio_pin_set_flags(handle, &pin_config);
	if (res < 0)
		err(EXIT_FAILURE, "configuration of pin %d "
				"failed (flags=%d)", pin_config.g_pin, 
				pin_config.g_flags);

	for(i=0;i<21;i++){
		mem32ws[i+2]=0x101000;
	}
	mem32ws[1]=21; //led length
	mem32ws[0]=0x01; //data ready

	printf("read %x %x %x %x\n",mem32ws[0],mem32ws[1],mem32ws[2],mem32ws[3]);

	pru_t pru;
	pru=pru_alloc(PRU_TYPE_TI);
	if (pru == NULL) {
		fprintf(stderr, "unable to allocate PRU structure: %s\n", strerror(errno));
		return 3;
	}
	pru_reset(pru, 0);

	error = pru_upload(pru, 0, FWBIN);
	if (error) {
		fprintf(stderr, "unable to upload %s: %s\n", FWBIN, strerror(errno));
		return 5;
	}

	//pru_write_reg(pru, 0, REG_R30, 0);

	error = pru_enable(pru, 0, 0);
	if (error) {
		fprintf(stderr, "unable to enable PRU %d\n", 0);
		return 6;
	}
	pru_free(pru);

	int irqfd = open( "/dev/pruss0.irq2", O_RDONLY);
        if (irqfd == -1) perror("open pru");
	int fifofd = socket(PF_INET6, SOCK_DGRAM, 0);
        if (fifofd == -1) perror("open sock");
	struct sockaddr_in6 su = {.sin6_len = sizeof(struct sockaddr_in6),
		.sin6_family = AF_INET6,
		.sin6_addr   = IN6ADDR_LOOPBACK_INIT,
		.sin6_port=htons(15661)
	};

	error=bind(fifofd, (struct sockaddr*)&su, sizeof(su));
	if(error==-1){ perror("bind"); }

	i2c1fd = open(I2C1_DEV, O_RDWR);
	if (i2c1fd == -1) {
		fprintf(stderr, "Error opening I2C controller (%s) for "
				"scanning: %s\n", I2C1_DEV, strerror(errno));
		return -1;
	}

	i2c2fd = open(I2C2_DEV, O_RDWR);
	if (i2c2fd == -1) {
		fprintf(stderr, "Error opening I2C controller (%s) for "
				"scanning: %s\n", I2C2_DEV, strerror(errno));
		return -1;
	}


	struct iiccmd cmd;
	cmd.slave = (0x3c) << 1;
	cmd.last = 1;
	cmd.count = 0;
	error = ioctl(i2c1fd, I2CRSTCARD, &cmd);
	if (error) {
		fprintf(stderr, "Controller reset failed\n");
		return 1;
	}
	error = ioctl(i2c2fd, I2CRSTCARD, &cmd);
	if (error) {
		fprintf(stderr, "Controller reset failed\n");
		return 1;
	}


	uint8_t framebuffer0[DEFAULTBUFFERLENGTH+1];
	uint8_t framebuffer1[DEFAULTBUFFERLENGTH+1];
	uint8_t framebuffer2[DEFAULTBUFFERLENGTH+1];

	ssd1306_init(&lcd0, 1, DEFAULT_7bit_OLED_SLAVE_ADDRESS, 128, 32, framebuffer0);
	ssd1306_setDisplayOffset(&lcd0, 64);
	ssd1306_send_data(&lcd0, COMMAND, 0xc8); //updown flip
	ssd1306_send_data(&lcd0, COMMAND, 0xA1); //mirror horizonally
	uint8_t lcd0cfg[]={COMMAND, 0xda,0x00};
	ssd1306_send(&lcd0, lcd0cfg, sizeof(lcd0cfg));

	ssd1306_init(&lcd1, 2, DEFAULT_7bit_OLED_SLAVE_ADDRESS, 128, 32, framebuffer1);
	ssd1306_setDisplayOffset(&lcd1, 64);
	ssd1306_send_data(&lcd1, COMMAND, 0xc8); //updown flip
	ssd1306_send_data(&lcd1, COMMAND, 0xA1); //mirror horizonally
	ssd1306_send(&lcd1, lcd0cfg, sizeof(lcd0cfg));
	
	ssd1306_init(&lcd2, 2, DEFAULT_7bit_OLED_SLAVE_ADDRESS+1, 128, 64, framebuffer2);
	ssd1306_send_data(&lcd2, COMMAND, 0xc8); //updown flip
	ssd1306_send_data(&lcd2, COMMAND, 0xA1); //mirror horizonally
	//interlace mode for some 128x64 disaplys
	uint8_t rlcdcfg[]={0xda,0x10};
	ssd1306_send(&lcd2, rlcdcfg, 2);


	ssd1306_clear(&lcd0);
	ssd1306_clear(&lcd1);
	ssd1306_clear(&lcd2);
	lcd0.userdata=&update_lcd0;
	lcd1.userdata=&update_lcd1;
	lcd2.userdata=&update_lcd2;
	sem_init(&update_lcd0, 0, 5);
	sem_init(&update_lcd1, 0, 5);
	sem_init(&update_lcd2, 0, 5);

	/*
	sem_post(&update_lcd0);
	sem_post(&update_lcd1);
	sem_post(&update_lcd2);
	*/
	/*
	write_lcd(&lcds[6], L"\x01\x44 BTN7!  \x01\x64  ", L"Нечто" );
	write_lcd(&lcds[5], L"\x01\x44 BTN6!  \x01\x64  ", L"Нечто" );
	struct btnlcd *bl=&lcds[6];
	ssd1306_drawWCharStr(bl->lcd, bl->x1, bl->l1, bl->x2, black, nowrap, L"\x01\x44 BTN7!  \x01\x64  ");
	ssd1306_drawWCharStr(bl->lcd, bl->x1, bl->l2, bl->x2, white, nowrap, L"Тут что-то можно написать");

	ssd1306_drawWCharStr(&lcd0, 0, 16, 64, white, nowrap, L"Тут что-то можно написать");
	ssd1306_drawWCharStr(&lcd0, 0, 24, 64, black, nowrap, L"\x01\x44 BTN1  \x01\x64  ");

	ssd1306_drawWCharStr(&lcd0, 64, 0, 128, black, nowrap, L"\x01\x44 BTN8!  \x01\x64  ");
	ssd1306_drawWCharStr(&lcd0, 64, 8, 128, white, nowrap, L"Тут что-то можно написать");

	ssd1306_drawWCharStr(&lcd0, 64, 16, 128, white, nowrap, L"Тут что-то можно написать");
	ssd1306_drawWCharStr(&lcd0, 64, 24, 128, black, nowrap, L"\x01\x44 BTN2  \x01\x64  ");
	*/


	pthread_t upd_lcd0;
	if(pthread_create(&upd_lcd0, NULL, lcd_updater, &lcd0)) {
		fprintf(stderr, "Error creating thread\n");
		return 1;
	}

	pthread_t upd_lcd1;
	if(pthread_create(&upd_lcd1, NULL, lcd_updater, &lcd1)) {
		fprintf(stderr, "Error creating thread\n");
		return 1;
	}

	pthread_t upd_lcd2;
	if(pthread_create(&upd_lcd2, NULL, lcd_updater, &lcd2)) {
		fprintf(stderr, "Error creating thread\n");
		return 1;
	}


	int kq=kqueue();
	struct kevent changelist[2];
	EV_SET(&changelist[0], irqfd, EVFILT_READ, EV_ADD, 0, 0, NULL);
	EV_SET(&changelist[1], fifofd, EVFILT_READ, EV_ADD, 0, 0, NULL);
	int ret=kevent(kq, changelist, 2, NULL, 0, NULL);
	if (ret == -1)
		err(EXIT_FAILURE, "kevent register");
	if (changelist->flags & EV_ERROR)
		errx(EXIT_FAILURE, "Event error: %s", strerror(changelist->data));

	struct sockaddr_in6 client_addr;
	client_addr.sin6_family=AF_INET6;
	socklen_t client_addr_len=sizeof(struct sockaddr_in6);

	struct kevent evlist[2];
	for (;;) {
		int nev = kevent(kq, NULL, 0, evlist, 2, NULL);
		if (nev < 0) { err(2, "kevent()"); }
		if (nev == 0) { continue; }

		for(int i=0;i<nev;i++){
			if(evlist[i].ident==irqfd){
				/*
				printf("IRQ\n");
				for(int i=32;i<36;i++){
					printf("%02x-",mem[i]);
					printf("%02x ",mem[i+8]);
					if(i%16==15) printf("\n");
				}
				*/
				for(int j=0;j<4;j++){
					for(int k=0;k<6;k++){
						uint8_t rbtn=k*4+j;
						uint8_t ibtn=buttons[rbtn];
						uint8_t press=mem[32+j] & (1<<k);
						uint8_t rel=mem[40+j] & (1<<k);
						if(press|rel){
							printf("BTN %d (%d, %d)\n",rbtn,j,k);
							if(rbtn >= sizeof(buttons)){
								ibtn=0xff;
							}
							uint16_t btn=(ibtn|(press?(1<<15):0)|(rel?(1<<14):0));
							int l=sendto(fifofd, &btn, 2, 0, (struct sockaddr*)&client_addr, client_addr_len);
							if (l == -1) perror("write");
							printf("sent %04x\n",btn);

						}
					}
				}

				/*
				int l=sendto(fifofd, "xxx", 3, 0, (struct sockaddr*)&client_addr, client_addr_len);
				if (l == -1) perror("write");
				printf("Write %d\n",l);
				*/
				bzero(mem+24,24);
			}else if(evlist[i].ident==fifofd){
				union {
					uint16_t ident;
					struct setled sl;
					struct settext st;
					char raw[2048];
				} buf;
				int l=recvfrom(fifofd, &buf, sizeof(buf), 0, (struct sockaddr*)&client_addr, &client_addr_len);
				printf("Read %d\n",l);
				if (l == -1) perror("read");
				char addrbuf[64];
				inet_ntop(client_addr.sin6_family,&client_addr.sin6_addr,addrbuf,64);
				printf("got from %s:%d\n", addrbuf, htons(client_addr.sin6_port));
				if(buf.ident==IDENT_SETLED){
					printf("setled button %d to %06x\n",buf.sl.button, buf.sl.color);
					mem32ws[buf.sl.button+2]=buf.sl.color;
					l-=sizeof(struct setled);
					printf("%d extra bytes\n",l);
					i=0;
					while(l>=4){
						mem32ws[buf.sl.button+3+i]=buf.sl.colors[i];
						l-=4;
						i++;
					}
					mem32ws[0]=0x01;
				}
				if(buf.ident==IDENT_SETTEXT){
					printf("settext button %d\n",buf.st.button);
					write_lcd(&lcds[buf.st.button],
							buf.st.line1,
							buf.st.line2
							);



					sem_post((sem_t *)lcds[buf.st.button].lcd->userdata);
				}

				/*
				for(int i=2;i<=11;i++){
					switch(rand() % 6){
						case 0: mem32ws[i]=0x330000; break;
						case 1: mem32ws[i]=0x003300; break;
						case 2: mem32ws[i]=0x000033; break;
						case 3: mem32ws[i]=0x333300; break;
						case 4: mem32ws[i]=0x330033; break;
						case 5: mem32ws[i]=0x333333; break;
					}
				}
				mem32ws[1]=0x0a;
				*/
				/*
				mem32ws[0]=0x01;
				switch(rand() % 6){
					case 0: mem32ws[2]=0x330000; break;
					case 1: mem32ws[2]=0x003300; break;
					case 2: mem32ws[2]=0x000033; break;
					case 3: mem32ws[2]=0x333300; break;
					case 4: mem32ws[2]=0x330033; break;
					case 5: mem32ws[2]=0x333333; break;
				}
				*/
			}else {
				printf("unknown fd %d\n",evlist[i].ident);
			}
		}
	}

	return 0;
}
