AS = ca65
LD = ld65
AS_FLAGS =
LD_FLAGS = -C nrom.cfg
OBJ = obj

1-2.nes: $(OBJ) $(OBJ)/1-2.o
	$(LD) $(LD_FLAGS) $(OBJ)/1-2.o -o 1-2.nes

$(OBJ):
	mkdir $(OBJ)

$(OBJ)/1-2.o: 1-2.s 1.input chars.inc nrom.cfg
	$(AS) $(AS_FLAGS) 1-2.s -o $(OBJ)/1-2.o

.PHONY: clean
clean:
	rm -rf $(OBJ) 1-2.nes