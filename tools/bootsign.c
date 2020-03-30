/************************************************
* 			bootsign.c	(C) Jim Lee
*
* Use this tool to create a boot sector
* Usage: <input filename> <output filename>
************************************************/

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>

int main(int argc, char *argv[])
{
	if(argc < 3) 
	{
		fprintf(stderr, "Usage: <input filename> <output filename>\n");
		return -1;
	}

	struct stat st;
	if(stat(argv[1], &st) != 0)
	{
		fprintf(stderr, "Error opening file '%s' : %s\n", argv[1], strerror(errno));
		return -1;
	}

	printf("File '%s' size: %lld bytes\n", argv[1], (long long)st.st_size);

	if(st.st_size > 510)
	{
		fprintf(stderr, "File '%s' size > 510 bytes\n", argv[1]);
		return -1;
	}

	char buf[512];
	memset(buf, 0, sizeof(buf));

	FILE *ifp = fopen(argv[1], "rb");
	int size = fread(buf, 1, st.st_size, ifp);
	if(size != st.st_size)
	{
		fprintf(stderr, "Read File '%s' error, size is %d bytes\n", argv[1], size);
		return -1;
	}
	fclose(ifp);

	buf[510] = 0x55;
	buf[511] = 0xAA;
	FILE *ofp = fopen(argv[2], "wb+");
	size = fwrite(buf, 1, 512, ofp);
	if(size != 512)
	{
		fprintf(stderr, "Write  File '%s' error, size is %d bytes\n", argv[2], size);
		return -1;
	}
	fclose(ofp);

	printf("Build 512 bytes Boot Sector: '%s' Successfully!\n", argv[2]);

	return 0;
}
