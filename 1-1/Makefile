AS = ca65
LD = ld65
AS_FLAGS =
LD_FLAGS = -C nrom.cfg
OBJ = obj

1-1.nes: $(OBJ) $(OBJ)/1-1.o
	$(LD) $(LD_FLAGS) $(OBJ)/1-1.o -o 1-1.nes

$(OBJ):
	mkdir $(OBJ)

$(OBJ)/1-1.o: 1-1.s 1-1.input chars.inc nrom.cfg
	$(AS) $(AS_FLAGS) 1-1.s -o $(OBJ)/1-1.o

.PHONY: clean
clean:
	rm -rf $(OBJ) 1-1.nes