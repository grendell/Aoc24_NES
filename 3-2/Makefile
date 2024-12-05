AS = ca65
LD = ld65
AS_FLAGS =
LD_FLAGS = -C nrom.cfg
OBJ = obj

3-2.nes: $(OBJ) $(OBJ)/3-2.o
	$(LD) $(LD_FLAGS) $(OBJ)/3-2.o -o 3-2.nes

$(OBJ):
	mkdir $(OBJ)

$(OBJ)/3-2.o: 3-2.s ../inputs/2.input chars.inc nrom.cfg
	$(AS) $(AS_FLAGS) 3-2.s -o $(OBJ)/3-2.o

.PHONY: clean
clean:
	rm -rf $(OBJ) 3-2.nes