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

#define FWBIN  "pru_ws2812.bin"
#define	AM33XX_MMAP_SIZE	0x00040000
#define ws2812_ram_offset 0x10

int main(int argc, char* argv[]) {
	size_t i;
	int fd = 0;
	char dev[64];
	size_t mmap_sizes[2] = { AM33XX_MMAP_SIZE };
	int saved_errno = 0;
        int error;
        sranddev();

	for (i = 0; i < 4; i++) {
		snprintf(dev, sizeof(dev), "/dev/pruss%zu", 0);
		fd = open(dev, O_RDWR);
		if (fd == -1 && errno == EACCES)
			break;
		if (fd > 0)
			break;
	}
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
	uint32_t *mem32ws=(uint32_t *)(mem+0x80);

	printf("read %x %x %x %x\n",mem32ws[0],mem32ws[1],mem32ws[2],mem32ws[3]);
	if(argc>0){
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

		/*
		for(int i=2;i<=6;i++){
			switch(rand() % 3){
				case 0: mem32ws[i]=0x660000; break;
				case 1: mem32ws[i]=0x006600; break;
				case 2: mem32ws[i]=0x000066; break;
				case 3: mem32ws[i]=0x666600; break;
				case 4: mem32ws[i]=0x660066; break;
				case 5: mem32ws[i]=0x006666; break;
			}
		}
		*/
		mem32ws[1]=0x0a; //led length
		mem32ws[2]=1<<23;
		mem32ws[3]=1<<15;
		mem32ws[4]=1<<7;
		mem32ws[5]=1<<7|1<<15;
		mem32ws[6]=1<<15|1<<23;
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

	}


        
        int x=50;
	while(x-- > 0){
                usleep(500000);
		printf("| read %x %x %x %x\n",mem32ws[0],mem32ws[1],mem32ws[2],mem32ws[3]);
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
		mem32ws[0]=0x01;
		mem32ws[1]=0x0a;
		printf(". read %x %x %x %x\n",mem32ws[0],mem32ws[1],mem32ws[2],mem32ws[3]);

                for(int i=0;i<=31;i++){
                       printf("%02x ",mem[i]); 
                       if(i%16==15) printf("\n");
                }
	}

        /*
        printf("r30: %x\n", pru_read_reg(pru, 0, REG_R30));
        pru_write_reg(pru, 0, REG_R30, 1);
        printf("r30: %x\n", pru_read_reg(pru, 0, REG_R30));
        */

        //sleep(1);
	//mem32ws[5]=0x660000;
	//mem32ws[4]=0x006600;
	//mem32ws[3]=0x000066;
	//mem32ws[2]=0x660066;
	//mem32ws[1]=0x006666;

        //pru_write_reg(pru, 0, REG_R30, 1);

	/*
	 * Use the md_stor field to save the revision.
	 */
#if 0
	if (ti_reg_read_4(pru->mem, AM18XX_INTC_REG) == AM18XX_REV) {
		DPRINTF("found AM18XX PRU @ %p\n", (void *)pru->mem);
		pru->md_stor[0] = AM18XX_REV;
	} else if (ti_reg_read_4(pru->mem, AM33XX_INTC_REG) == AM33XX_REV) {
		DPRINTF("found AM33XX PRU @ %p\n", (void *)pru->mem);
		pru->md_stor[0] = AM33XX_REV;
	} else {
		munmap(pru->mem, pru->mem_size);
		close(fd);
		return EINVAL;
	}
	ti_disable(pru, 0);
	ti_disable(pru, 1);
	ti_reset(pru, 0);
	ti_reset(pru, 1);
	pru->disable = ti_disable;
	pru->enable = ti_enable;
	pru->reset = ti_reset;
	pru->upload_buffer = ti_upload;
	pru->wait = ti_wait;
	pru->check_intr = ti_check_intr;
	pru->deinit = ti_deinit;
	pru->read_imem = ti_read_imem;
	pru->write_imem = ti_write_imem;
	pru->read_mem = ti_read_mem;
	pru->disassemble = ti_disassemble;
	pru->read_reg = ti_read_reg;
	pru->write_reg = ti_write_reg;
	pru->get_pc = ti_get_pc;
	pru->set_pc = ti_set_pc;
	pru->insert_breakpoint = ti_insert_breakpoint;
#endif

	return 0;
}
