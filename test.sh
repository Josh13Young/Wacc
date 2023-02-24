arm-linux-gnueabi-gcc -o temp -mcpu=arm1176jzf-s -mtune=arm1176jzf-s temp.s < /dev/null 
qemu-arm -L /usr/arm-linux-gnueabi/ temp
