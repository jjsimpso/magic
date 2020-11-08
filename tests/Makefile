#---------------------------------------------------------------------------------
# BUILD is the directory where object files & intermediate files will be placed
# SOURCES is a list of directories containing source code
# INCLUDES is a list of directories containing extra header files
# all directories are relative to this makefile
#---------------------------------------------------------------------------------
BUILD		:=	src
SOURCES		:=	src
INCLUDES	:=	include


#---------------------------------------------------------------------------------
# options for code generation
#---------------------------------------------------------------------------------
CC = gcc
COMPILERFLAGS = -Wall -std=c99 -g
MYFLAGS = -DVGA_DEBUG
INCLUDE  = `pkg-config --cflags gtk+-2.0` -I$(CURDIR)/$(INCLUDES) -I$(CURDIR)/$(SOURCES)
CFLAGS = $(COMPILERFLAGS) $(MYFLAGS) $(INCLUDE)

LIBDIRS  = -L$(CURDIR)/$(BUILD) -L$(CURDIR)
LIBS     =  `pkg-config --libs gtk+-2.0`

DEPSDIR	        :=      $(CURDIR)/$(BUILD)
CFILES		:=	$(foreach dir,$(SOURCES),$(notdir $(wildcard $(dir)/*.c)))
OFILES		:=	$(CFILES:.c=.o)

OBJS = $(addprefix $(BUILD)/, $(OFILES))

#---------------------------------------------------------------------------------
# basic rules
#---------------------------------------------------------------------------------
.SUFFIXES: .c .o

%.o : %.c
	@echo $(notdir $<)
	$(CC) $(CFLAGS) -c $< -o $@

#---------------------------------------------------------------------------------
#
#---------------------------------------------------------------------------------
libvga.a : $(OBJS)
	ar rcs $@ $(OBJS)

vgatest : test/main.o
	$(CC) $(CFLAGS) -o vgatest $(LIBDIRS) test/main.o $(LIBS) -lvga

debug : 
	@echo $(CFILES)
	@echo $(OBJS)

install :
	cp $(INCLUDES)/*.h /usr/local/include/libvga
	cp libvga.a /usr/local/lib/

clean:
	@echo clean ...
	rm -f $(BUILD)/*.o
	rm -f libvga.a
