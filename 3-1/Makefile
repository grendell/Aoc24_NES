AS = ca65
LD = ld65
AS_FLAGS =
LD_FLAGS = -C nrom.cfg
OBJ = obj

3-1.nes: $(OBJ) $(OBJ)/3-1.o
	$(LD) $(LD_FLAGS) $(OBJ)/3-1.o -o 3-1.nes

$(OBJ):
	mkdir $(OBJ)

$(OBJ)/3-1.o: 3-1.s ../inputs/2.input chars.inc nrom.cfg
	$(AS) $(AS_FLAGS) 3-1.s -o $(OBJ)/3-1.o

.PHONY: clean
clean:
	rm -rf $(OBJ) 3-1.nes