AS = ca65
LD = ld65
AS_FLAGS =
LD_FLAGS = -C nrom.cfg
OBJ = obj

2-2.nes: $(OBJ) $(OBJ)/2-2.o
	$(LD) $(LD_FLAGS) $(OBJ)/2-2.o -o 2-2.nes

$(OBJ):
	mkdir $(OBJ)

$(OBJ)/2-2.o: 2-2.s ../inputs/2.input chars.inc nrom.cfg
	$(AS) $(AS_FLAGS) 2-2.s -o $(OBJ)/2-2.o

.PHONY: clean
clean:
	rm -rf $(OBJ) 2-2.nes