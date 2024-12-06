AS = ca65
LD = ld65
AS_FLAGS =
LD_FLAGS = -C nrom.cfg
OBJ = obj

4-2.nes: $(OBJ) $(OBJ)/4-2.o
	$(LD) $(LD_FLAGS) $(OBJ)/4-2.o -o 4-2.nes

$(OBJ):
	mkdir $(OBJ)

$(OBJ)/4-2.o: 4-2.s ../inputs/4.input chars.inc nrom.cfg
	$(AS) $(AS_FLAGS) 4-2.s -o $(OBJ)/4-2.o

.PHONY: clean
clean:
	rm -rf $(OBJ) 4-2.nes