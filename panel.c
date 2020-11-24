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

#define FWBIN  "pru_ws2812.bin"
#define	AM33XX_MMAP_SIZE	0x00040000
#define ws2812_ram_offset 0x80

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

	uint8_t init_pins_out[]={6,8,10,12,14};
	mem[0]=5;
	for(int i=mem[0];i>0;i--){
		mem[i]=init_pins_out[i-1];
	}

	uint8_t init_pins_in[]={7,9,11,13,32,32,32,32};
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

	for(i=2;i<=12;i++){
		mem32ws[i]=10;
	}
	mem32ws[1]=10; //led length
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
				printf("IRQ\n");
				for(int i=32;i<48;i++){
					printf("%02x ",mem[i]);
					if(i%16==15) printf("\n");
				}
				int l=sendto(fifofd, "xxx", 3, 0, (struct sockaddr*)&client_addr, client_addr_len);
				if (l == -1) perror("write");
				printf("Write %d\n",l);
				bzero(mem+24,24);
			} 
			if(evlist[i].ident==fifofd){
				char* buf[32];
				int l=recvfrom(fifofd, buf, 32, 0, (struct sockaddr*)&client_addr, &client_addr_len);
				printf("Read %d\n",l);
				if (l == -1) perror("read");
				char addrbuf[64];
				inet_ntop(client_addr.sin6_family,&client_addr.sin6_addr,addrbuf,64);
				printf("got from %s:%d\n", addrbuf, htons(client_addr.sin6_port));

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
				mem32ws[0]=0x01;
				switch(rand() % 6){
					case 0: mem32ws[2]=0x330000; break;
					case 1: mem32ws[2]=0x003300; break;
					case 2: mem32ws[2]=0x000033; break;
					case 3: mem32ws[2]=0x333300; break;
					case 4: mem32ws[2]=0x330033; break;
					case 5: mem32ws[2]=0x333333; break;
				}
			}else {
				printf("unknown fd %d\n",evlist[i].ident);
			}
		}
	}

	return 0;
}
