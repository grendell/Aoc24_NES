AS = ca65
LD = ld65
AS_FLAGS =
LD_FLAGS = -C nrom.cfg
OBJ = obj

4-1.nes: $(OBJ) $(OBJ)/4-1.o
	$(LD) $(LD_FLAGS) $(OBJ)/4-1.o -o 4-1.nes

$(OBJ):
	mkdir $(OBJ)

$(OBJ)/4-1.o: 4-1.s ../inputs/4.input chars.inc nrom.cfg
	$(AS) $(AS_FLAGS) 4-1.s -o $(OBJ)/4-1.o

.PHONY: clean
clean:
	rm -rf $(OBJ) 4-1.nes